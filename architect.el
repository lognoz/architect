;; architect.el --- Architect File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: architect

;; This file is not part of GNU Emacs.

;; This Emacs config is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This Emacs config is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this Emacs config. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'subr-x)

;;; Contextual Architect constant.

(defconst architect-base-directory (file-name-directory load-file-name)
  "The directory Architect was loaded from.")

;;; Contextual Architect variables.

(defvar architect-source-directory (concat architect-base-directory "src/")
  "The directory of source templates.")

(defvar architect-template-variables nil
  "The list of template variables.")

(defvar architect-template-replacements nil
  "The list of template replatement.")

(defvar architect-template-destination nil
  "The destination directory template.")

(defvar architect-template-default-directory nil
  "The template default directory.")

;;; Internal Architect face.

(defface architect-error-face
  '((t :foreground "#ce5555"))
  "Face when in user prompt when input doesn't match.")

;;; Internal Architect functions.

(defun architect-template-candidates ()
  "This function return the list of directories located into
`architect-source-directory' to provide prompt candidates."
  (let ((list))
    (dolist (f (directory-files architect-source-directory))
      (let ((path (concat architect-source-directory f)))
        (when (and (file-directory-p path)
                   (not (equal f "."))
                   (not (equal f "..")))
          (push (file-name-nondirectory path) list))))
    list))

(defun architect-load-configuration (path)
  "This function load project configuration by path."
  (let ((path (concat path "/template.el")))
    (if (file-exists-p path)
        (load path nil 'nomessage)
      (error (format "File '%s' not found." path)))))

(defun architect-define-variable (&rest args)
  "Define variables for a template. It's generally used in
`template.el' file."
  (setq architect-template-variables
        (append architect-template-variables (list args))))

(defun architect-define-default-directory (directory)
  "Define directory for a template. It's generally used in
`template.el' file."
  (when (file-directory-p directory)
    (setq architect-template-default-directory directory)))

(defun architect-plist-get (plist key)
  "This function is used to get plist value."
  (eval (plist-get plist key)))

(defun architect-read-string (plist)
  "This function is used to get user input string. It require an)
non-empty string before to return it."
  (let ((answer) (valid) (prompt-text) (prompt-error-text)
        (input (architect-plist-get plist :input))
        (input-error (architect-plist-get plist :input-error))
        (regex (architect-plist-get plist :regex)))
    (unless regex (setq regex ".+"))
    (unless input-error (setq input-error "require"))
    (setq prompt-text (concat input ": ")
          prompt-error-text
            (format "%s (%s): " input
                    (propertize input-error 'face
                                'architect-error-face)))
    (while (not valid)
      (setq answer (string-trim (read-string prompt-text answer)))
      (if (string-match-p regex answer)
          (setq valid t)
        (setq prompt-text prompt-error-text)))
    answer))

(defun architect-set-directory (target)
  "This function provide a prompt input to ask where the project
will be created."
  (let* ((base-directory
           (read-directory-name
             "Directory: "
             architect-template-default-directory
             nil t))
         (project-directory
           (architect-read-string
             '(:regex "^[-_A-Za-z0-9]+$"
               :input (concat "Destination " base-directory)
               :input-error "alphanumeric"))))
    (setq architect-template-destination
          (concat base-directory project-directory))))

(defun architect-set-variables ()
  "This function loop into `architect-template-variables' and))))
execute an user prompt. It return an assosiative array that will
be used to replace in template file."
  (let ((variables) (selector) (value))
    (dolist (args architect-template-variables)
      (setq selector (architect-plist-get args :variable))
      (setq value (architect-read-string args))
      (push (cons selector value) architect-template-replacements))))

(defun architect-recursive-directory ()
  "This function return this list of recursive files and
directories by path."
  (split-string
    (shell-command-to-string "find .")))

(defun architect-replace-filename (path)
  "This function apply filename replacement based on template
variables."
  (let ((return-path path))
    (dolist (replacement architect-template-replacements)
      (let ((variable (car replacement))
            (value (cdr replacement)))
        (when (string-match-p variable path)
          (setq return-path
            (replace-regexp-in-string
              variable value return-path nil 'literal))
          (rename-file path return-path))))))

(defun architect-apply-replacement (path)
  "This function scan file content to apply replacement based on
template variables."
  (with-temp-file path
    (insert-file-contents-literally path)
    (dolist (replacement architect-template-replacements)
      (goto-char 0)
      (while (search-forward (car replacement) nil t)
        (replace-match (cdr replacement)))
      replacement)))

(defun architect-create-project (template-name)
  "This function is used as action to ivy prompt executed in main
`architect' function."
  (let* ((path (concat architect-source-directory template-name)))
    (architect-load-configuration path)
    (architect-set-directory template-name)
    (architect-set-variables)
    (shell-command (format "cp -r %s %s" path architect-template-destination))
    (message (format "Creating %s..." architect-template-destination))
    (let ((default-directory architect-template-destination))
      (dolist (path (architect-recursive-directory))
        (architect-replace-filename path))
      (dolist (path (architect-recursive-directory))
        (message (format "Checking %s for replacement..." path))
        (unless (file-directory-p path)
          (architect-apply-replacement path))))
    (dired architect-template-destination)
    (message "")))

;;; External Architect functions.

;;;###autoload
(defun architect ()
  "Provide functionality to create project template quickly. If
you want to create a new template, please add it to Architect src
directory."
  (interactive)
  (setq architect-template-variables nil
        architect-template-replacements nil
        architect-template-default-directory nil)
  (ivy-read "Create project: "
            (architect-template-candidates)
            :require-match t
            :action 'architect-create-project))

(provide 'architect)
