;;; emacs-gc-stats.el --- Collect Emacs GC statistics  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>
;; URL: https://git.sr.ht/~yantar92/emacs-gc-stats
;; Package-Requires: ((emacs "25.1"))

;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package collects Emacs garbage statistics over time and saves
;; it in the format that can be shared with Emacs maintainers.
;;
;;; Usage:
;;
;; Add
;; (require 'emacs-gc-stats)
;; (emacs-gc-stats-mode +1)
;; to your init file to enable the statistics acquiring.
;;
;; When you are ready to share the results, run
;; M-x emacs-gc-stats-save-session
;; and then share the saved `emacs-gc-stats-file'.
;;
;; You can use `emacs-gc-stats-clear' to clear the currently collected
;; session data.

;; 
;;; Code:

(defgroup emacs-gc-stats nil
  "Collect and share Emacs GC statistics."
  :tag "GC stats"
  :group 'development)

(defcustom emacs-gc-stats-file (expand-file-name "emacs-gc-stats.eld" user-emacs-directory)
  "File used to store the statistics across Emacs sessions."
  :type 'file)

(defvar emacs-gc-stats--setting-vars
  '(gc-cons-threshold
    gc-cons-percentage
    memory-limit
    emacs-version
    doom-version
    spacemacs-version
    prelude-tips
    (memory-info)
    (memory-use-counts))
  "List of variable/function symbols to collect.")

(defvar emacs-gc-stats--command-vars
  '(gc-cons-threshold
    gc-cons-percentage
    gcmh-mode
    gc-elapsed
    gcs-done
    this-command
    memory-limit
    (memory-info)
    (memory-use-counts)
    emacs-gc-stats--idle-tic)
  "List of variable/function symbols to collect for each GC or command.")

(defvar emacs-gc-stats--summary-vars
  '(gc-cons-threshold
    gc-cons-percentage
    gc-elapsed
    gcs-done
    memory-limit
    (memory-info)
    emacs-uptime
    (memory-use-counts)
    emacs-gc-stats-idle-delay
    emacs-gc-stats--idle-tic)
  "List of variables to collect at session end.")

(defun emacs-gc-stats--collect (&rest symbols)
  "Collect SYMBOLS values.
If symbol is a variable, collect (symbol . value).
If symbol is a function name, collect (symbol . (funcall symbol)).
If symbol is a list, collect (symbol . (eval symbol)).
Otherwise, collect symbol."
  (let (data)
    (dolist (var symbols)
      (pcase var
        ((pred keywordp) (push var data))
        ((and (pred symbolp) (pred boundp))
         (push (cons var (symbol-value var)) data))
        ((pred functionp)
         (push (cons var (funcall var)) data))
        ((pred listp)
         (push (cons var (eval var)) data))
        (_ (push var data))))
    (nreverse data)))

(defvar emacs-gc-stats--data nil
  "Collected statistics.")

(defun emacs-gc-stats--collect-init ()
  "Collect initial stats."
  (push
   (apply #'emacs-gc-stats--collect
          "Initial stats"
          (current-time-string)
          emacs-gc-stats--setting-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--collect-gc ()
  "Collect single GC stats."
  (push
   (apply #'emacs-gc-stats--collect
          (current-time-string)
          emacs-gc-stats--command-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--collect-init-end ()
  "Collect init.el stats."
  (push
   (apply #'emacs-gc-stats--collect
          "Init.el stats"
          (current-time-string)
          emacs-gc-stats--summary-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--collect-end ()
  "Collect initial stats."
  (push
   (apply #'emacs-gc-stats--collect
          "Session end stats"
          (current-time-string)
          emacs-gc-stats--summary-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats-save-session ()
  "Save stats to disk."
  (interactive)
  (emacs-gc-stats--collect-end)
  (let ((previous-sessions
         (and (file-readable-p emacs-gc-stats-file)
              (with-temp-buffer
                (insert-file-contents emacs-gc-stats-file)
                (ignore-errors (read (current-buffer))))))
        (session (reverse emacs-gc-stats--data)))
    ;; remove end data in case if we continue recording.
    (pop emacs-gc-stats--data)
    (with-temp-file emacs-gc-stats-file
      ;; Override partially saved session.
      (let ((existing (assoc (car session) previous-sessions)))
        (if existing
            (setcdr (cdr existing) (cdr session))
          (push session previous-sessions)))
      (prin1 previous-sessions (current-buffer)))))

(defun emacs-gc-stats-clear ()
  "Clear GC stats collected so far."
  (interactive)
  (setq emacs-gc-stats--data nil)
  ;; Restart.
  (when emacs-gc-stats-mode
    (emacs-gc-stats-mode -1)
    (emacs-gc-stats-mode +1)))

(defvar emacs-gc-stats-idle-delay 300
  "Delay in seconds to count idle time.")

(defvar emacs-gc-stats--idle-tic 0
  "Idle counter.")
(defvar emacs-gc-stats--idle-timer nil
  "Time counting idle time.")
(defun emacs-gc-stats-idle-tic ()
  "Increase idle counter."
  (when (and (current-idle-time)
             (> (time-to-seconds (current-idle-time)) emacs-gc-stats-idle-delay))
    (cl-incf emacs-gc-stats--idle-tic)))

;;;###autoload
(define-minor-mode emacs-gc-stats-mode
  "Toggle collecting Emacs GC statistics."
  :global t
  (if emacs-gc-stats-mode
      (progn
        (unless emacs-gc-stats--data
          (emacs-gc-stats--collect-init))
        ;; 5 minutes counter.
        (setq emacs-gc-stats--idle-timer
              (run-with-timer
               emacs-gc-stats-idle-delay
               emacs-gc-stats-idle-delay
               #'emacs-gc-stats-idle-tic))
        (add-hook 'post-gc-hook #'emacs-gc-stats--collect-gc)
        (add-hook 'after-init-hook #'emacs-gc-stats--collect-init-end)
        (add-hook 'kill-emacs-hook #'emacs-gc-stats-save-session))
    (when (timerp emacs-gc-stats--idle-timer)
      (cancel-timer emacs-gc-stats--idle-timer))
    (remove-hook 'post-gc-hook #'emacs-gc-stats--collect-gc)
    (remove-hook 'after-init-hook #'emacs-gc-stats--collect-init-end)
    (remove-hook 'kill-emacs-hook #'emacs-gc-stats-save-session)))

(provide 'emacs-gc-stats)
;;; emacs-gc-stats.el ends here
