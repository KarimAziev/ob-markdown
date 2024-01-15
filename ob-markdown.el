;;; ob-markdown.el --- Functions for markdown evaluation in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Takahiro Noda

;; Author: Takahiro Noda
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Keywords: outlines
;; Version: 0.01
;; URL: https://github.com/KarimAziev/ob-markdown
;; Package-Requires: ((emacs "27.1"))

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

;; - pandoc (optional)


;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-block-regexp)

;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("markdown" . "text"))

(declare-function markdown-code-block-lang "markdown-mode")
(declare-function markdown-find-next-prop "markdown-mode")
(declare-function markdown-syntax-propertize "markdown-mode")

(defcustom ob-markdown-pandoc-pre-compile-hook '(ob-markdown-add-used-langs)
  "Hook run before Pandoc compilation in ob-markdown.

A hook that runs before Pandoc compilation in Org Babel Markdown blocks.

Hooks added to this list are called with no arguments and can be used to perform
setup tasks before Pandoc processes the Markdown source code.

Functions should be designed to operate in the buffer containing the Markdown
source code that will be passed to Pandoc."
  :group 'ob-markdown
  :type '(hook :options
          (ob-markdown-add-used-langs)))

(defcustom ob-markdown-pandoc-post-compile-hook '(ob-markdown-strip-custom-id-props
                                                  ob-markdown-sync-block-languages-in-org)
  "Hooks to run after Pandoc compilation.

Functions added to this hook are called with no arguments and can be used to
perform additional processing on the results of the Pandoc compilation.

Each function should be a symbol referring to a function that will be called
after the Pandoc compilation process completes."
  :group 'ob-markdown
  :type '(hook :options
          (ob-markdown-strip-custom-id-props
           ob-markdown-sync-block-languages-in-org)))

(defvar-local ob-markdown-out-type nil
  "Output type for Markdown code block execution.")

(defvar-local ob-markdown-extracted-langs nil
  "List of languages extracted from Markdown code blocks.")

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:markdown '((:results . "raw")
                                                 (:cmdline . "--from gfm")))

(defmacro ob-markdown-defvar (sym &rest body)
  "Define a namespaced variable with markdown suffix.

It is used to supress package lint warnings about a non-standard separator
and package prefix.

Argument SYM is the symbol to define as a variable.

Remaining arguments BODY are the forms that make up the body of the variable
definition."
  (declare (doc-string 3)
           (indent 2))
  `(defvar ,(intern (concat (symbol-name sym) ":markdown"))
     ,@body))

(ob-markdown-defvar org-babel-command "pandoc"
  "Command run by ob-markdown.")

(defmacro ob-markdown-defun (sym &rest body)
  "Define a new function with a `:markdown' suffix.

It is used to supress package lint warnings about a non-standard separator
and package prefix.

Argument SYM is the symbol to which the markdown function will be bound.

Remaining arguments BODY are the forms that make up the body of the function."
  (declare (doc-string 3)
           (indent 2))
  `(defun ,(intern (concat (symbol-name sym) ":markdown"))
       ,@body))

;; ;; This function expands the body of a source code block by doing
;; ;; things like prepending argument definitions to the body, it should
;; ;; be called by the `org-babel-execute:markdown' function below.
(ob-markdown-defun org-babel-expand-body (body params &optional
                                               _processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-expand-body:generic body params))

(defun ob-markdown-get-output-type-from-cmdline (cmdline)
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
      (setq ob-markdown-out-type (ob-markdown-get-output-type-from-cmdline command))
      (run-hooks 'ob-markdown-pandoc-pre-compile-hook)
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
          (run-hooks 'ob-markdown-pandoc-post-compile-hook)
          (buffer-string))))))

(defun ob-markdown--strip-custom-id-props ()
  "Remove custom ID properties from text."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward
              ":PROPERTIES:[\n]+[\s\t]+:CUSTOM_ID:[\s\t][^\n]+[\n]+[\s\t]+:END:[\n]"
              nil t
              1)
        (replace-match "")))))

(defun ob-markdown-strip-custom-id-props ()
  "Remove CUSTOM_ID properties from Org-mode blocks."
  (when (equal "org" ob-markdown-out-type)
    (ob-markdown--strip-custom-id-props)))

(defun ob-markdown-add-used-langs ()
  "Add used languages from Org buffer to a `ob-markdown-extracted-langs'."
  (when (equal "org" ob-markdown-out-type)
    (setq ob-markdown-extracted-langs
          (ob-markdown--extract-code-languages
           (buffer-substring-no-properties (point-min)
                                           (point-max))))))

(defun ob-markdown--extract-code-languages (body)
  "Return list of used languages from a markdown BODY text.

This function parses the BODY of a Markdown document to identify code blocks
and their respective languages.

The result is a list of elements that can be either strings (representing a
language) or nil if the code block has no specified language.

The order of the list matches the sequence of code blocks in the markdown BODY."
  (require 'markdown-mode)
  (with-temp-buffer
    (insert body)
    (markdown-syntax-propertize
     (point-min)
     (point-max))
    (goto-char (point-min))
    (let ((langs))
      (cl-loop with prop = 'markdown-gfm-block-begin
               for pos-prop = (markdown-find-next-prop prop)
               while pos-prop
               for lang = (markdown-code-block-lang pos-prop)
               do
               (progn
                 (push lang langs)
                 (goto-char
                  (next-single-property-change
                   (point)
                   prop))))
      (nreverse langs))))

(defun ob-markdown-sync-block-languages-in-org ()
  "Synchronize Org code block languages."
  (when (and
         ob-markdown-extracted-langs
         (equal "org" ob-markdown-out-type))
    (ob-markdown--sync-block-languages ob-markdown-extracted-langs)))

(defun ob-markdown--sync-block-languages (languages)
  "Ensure Org document code blocks reflect specified LANGUAGES.

LANGUAGES should be a list reflecting the sequence of code languages used in the
original markdown content, with each element corresponding to a particular
block's language or nil if no language is specified."
  (require 'org)
  (let* ((langs (reverse languages))
         (count (length langs))
         (case-fold-search t))
    (goto-char (point-max))
    (while (re-search-backward org-block-regexp nil t 1)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (save-excursion
          (when
              (when (re-search-forward "#\\+begin_" nil t 1)
                (looking-at (regexp-opt (list "src" "example"))))
            (let* ((lang (pop langs))
                   (example (looking-at (regexp-opt (list "example")))))
              (cond ((and lang example)
                     (save-excursion
                       (goto-char end)
                       (delete-region (line-beginning-position) end)
                       (insert "#+END_SRC"))
                     (goto-char beg)
                     (delete-region beg (line-end-position))
                     (insert "#+BEGIN_SRC " lang)))
              (setq count (1- count)))))))))

(defun org-babel-execute:markdown (body params)
  "Execute a block of Markdown code with org-babel.

Argument BODY is the content of the code block to be executed.

Argument PARAMS is an association list of parameters for the code block."
  (message "executing Markdown source code block")
  (if
      (not (equal org-babel-command:markdown "pandoc"))
      (org-babel-eval
       (if-let ((cmdline (cdr (assoc :cmdline params))))
           (concat org-babel-command:markdown " " cmdline)
         org-babel-command:markdown)
       (org-babel-expand-body:markdown body
                                       params))
    (let* ((expanded-body (org-babel-expand-body:markdown body params))
           (cmdline (cdr (assoc :cmdline params)))
           (outtype-from-args
            (and cmdline
                 (ob-markdown-get-output-type-from-cmdline cmdline)))
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
                              expanded-body))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:markdown (_session _params)
  "Return an error if the :session header argument is Set.
Markdown does not support sessions."
  (error "Markdown sessions are nonsensical"))

(provide 'ob-markdown)
;;; ob-markdown.el ends here
