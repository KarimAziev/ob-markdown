;;; ob-markdown.el --- org-babel functions for markdown evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Takahiro Noda

;; Author: Takahiro Noda
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://gitbub.com/tnoda/ob-markdown/
;; Version: 0.01
;; URL: https://github.com/KarimAziev/ob-markdown
;; Package-Requires: ((emacs "24.4"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating markdown code

;;; Requirements:

;; - pandoc

;;; Code:
(require 'ob)
(require 'ob-eval)

;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("markdown" . "text"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:markdown '((:cmdline . "-f gfm")
                                                 (:results . "raw")))
(defvar org-babel-command:markdown "pandoc"
  "Command run by ob-markdown.")


;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:markdown' function below.
(defun org-babel-expand-body:markdown (body params &optional _processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-expand-body:generic body params))

(defun ob-markdown-babel-eval (command query)
  "Run COMMAND on QUERY.

Return standard output produced by COMMAND.  If COMMAND exits
with a non-zero code or produces error output, show it with
`org-babel-eval-error-notify'.

Writes QUERY into a temp-buffer that is processed with
`org-babel--shell-command-on-region'."
  (let ((error-buffer (get-buffer-create " *Org-Babel Error*"))
        exit-code)
    (with-current-buffer error-buffer (erase-buffer))
    (with-temp-buffer
      ;; Ensure trailing newline.  It is required for cmdproxy.exe.
      (insert query "\n")
      (setq exit-code
            (org-babel--shell-command-on-region
             command error-buffer))
      (let ((stderr (with-current-buffer error-buffer (buffer-string))))
        (if (or (not (numberp exit-code))
                (> exit-code 0)
                (not (string-empty-p stderr)))
            (progn
              (org-babel-eval-error-notify exit-code stderr)
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but
                    ;; Babel expects the buffer modifiable.
                    (setq buffer-read-only nil))))
              ;; Return output, if any.
              (buffer-string))
          (pcase (ob-markdown-out-type-from-cmdline command)
            ("org" (save-excursion
                     (goto-char (point-min))
                     (let ((case-fold-search t))
                       (while (re-search-forward
                               ":PROPERTIES:[\n]+[\s\t]+:CUSTOM_ID:[\s\t][^\n]+[\n]+[\s\t]+:END:[\n]"
                               nil t
                               1)
                         (replace-match ""))))))
          (buffer-string))))))


(defun ob-markdown-out-type-from-cmdline (cmdline)
  "Extract output type from CMDLINE using predefined flags.

Argument CMDLINE is a string containing the command line to be parsed."
  (ob-markdown-get-arg-value cmdline
                             "-t" "-w" "--to" "--write"))

(defun ob-markdown-get-arg-value (cmdline &rest args)
  "Extract argument value from a command line string.

Argument CMDLINE is a string representing the command line to be parsed.

Remaining arguments ARGS are strings representing the arguments to search for in
CMDLINE."
  (with-temp-buffer (insert cmdline)
                    (goto-char (point-min))
                    (when (re-search-forward (regexp-opt args) nil t 1)
                      (when (looking-at "[\s=]")
                        (forward-char 1)
                        (when (re-search-forward
                               "[a-z]+"
                               nil t
                               1)
                          (match-string-no-properties 0))))))


(defun org-babel-execute:markdown (body params)
  "Execute a block of Markdown code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Markdown source code block")
  (let* ((expanded-body (org-babel-expand-body:markdown body params))
         (cmdline (cdr (assoc :cmdline params)))
         (outtype-from-args (and cmdline
                                 (ob-markdown-out-type-from-cmdline cmdline)))
         (result-type (cdr (assq :result-params params))))
    (cond ((and cmdline
                (ob-markdown-get-arg-value cmdline "-f" "--from")))
          (t
           (setq cmdline (string-join (delq nil (list "-f" "gfm" cmdline)) " "))))
    (cond (outtype-from-args)
          ((member "html" result-type)
           (setq cmdline (string-join (delq nil (list "-t" "html" cmdline)) " ")))
          ((member "file" result-type)
           nil)
          (t
           (setq cmdline (string-join (delq nil (list "-t" "org" cmdline)) " "))))
    (ob-markdown-babel-eval (if cmdline
                                (concat org-babel-command:markdown
                                        " "
                                        cmdline)
                              org-babel-command:markdown)
                            expanded-body)))


;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:markdown (_session _params)
  "Return an error if the :session header argument is Set.
Markdown does not support sessions."
  (error "Markdown sessions are nonsensical."))

(provide 'ob-markdown)
;;; ob-markdown.el ends here
