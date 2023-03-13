;;; emacs-gc-stats.el --- Collect Emacs GC statistics  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>

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
;; M-x emacs-gc-stats-submit

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
    emacs-version)
  "List of variable/function symbols to collect.")

(defvar emacs-gc-stats--command-vars
  '(gc-cons-threshold
    gc-cons-percentage
    gcmh-mode
    gc-elapsed
    gcs-done
    this-command
    memory-limit
    memory-info)
  "List of variable/function symbols to collect for each GC or command.")

(defvar emacs-gc-stats--summary-vars
  '(gc-cons-threshold
    gc-cons-percentage
    gc-elapsed
    gcs-done
    memory-limit
    emacs-uptime)
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
                (ignore-errors (read (current-buffer)))))))
    (with-temp-file emacs-gc-stats-file
      ;; Override partially saved session.
      (setf (alist-get (car emacs-gc-stats--data) previous-sessions)
            (cdr emacs-gc-stats--data))
      (prin1 previous-sessions (current-buffer)))))

(define-minor-mode emacs-gc-stats-mode
  "Toggle collecting Emacs GC statistics."
  :global t
  (if emacs-gc-stats-mode
      (progn
        (unless emacs-gc-stats--data
          (emacs-gc-stats--collect-init))
        (add-hook 'post-gc-hook #'emacs-gc-stats--collect-gc)
        (add-hook 'kill-emacs-hook #'emacs-gc-stats-save-session))
    (remove-hook 'post-gc-hook #'emacs-gc-stats--collect-gc)
    (remove-hook 'kill-emacs-hook #'emacs-gc-stats-save-session)))

(provide 'emacs-gc-stats)
;;; emacs-gc-stats.el ends here
