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

;;; Contextual Architect constants.

(defconst architect-base-directory (file-name-directory load-file-name)
  "The directory Architect was loaded from.")

(defconst architect-source-directory (concat architect-base-directory "src/")
  "The directory of source templates.")

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

(defun architect-template-recursive-source (path)
  "This function return this list of recursive files and
directories by path."
  (split-string
    (shell-command-to-string
      (format "find %s" path) "\n" t)))

(defun architect-load-configuration (path)
  "This function load project configuration by path."
  (let ((path (concat path "/project.el")))
    (if (file-exists-p path)
        (load path nil 'nomessage)
      (error (format "File '%s' not found." path)))))

(defun architect-read-directory (target)
  "This function provide a prompt input to ask where the project
will be created."
  (let* ((variable (format "architect-%s-default-directory" target))
         (prompt-default-directory (architect-default-directory variable)))
    (concat
      (read-directory-name "Directory: " prompt-default-directory nil t)
      (architect-read-string "Project directory"  "^[-_A-Za-z0-9]+$"  "alphanumeric") "/")))

(defun architect-default-directory (variable-name)
  "This function return the default directory defined variable)))))
value. If it doesn't exists, it return none."
  (let ((variable (intern variable-name))
        (directory))
    (when (boundp variable)
      (setq variable (symbol-value variable))
      (when (file-directory-p variable)
        (setq directory variable)))
    directory))

(defun architect-read-args (variable)
  "This function loop into project variables and execute an user
prompt. It return an assosiative array that will be used to
replace in template file."
  (let ((args))
    (mapc
      (lambda (entry)
        (let* ((replacement (car entry))
               (text (cadr entry))
               (value))
          (setq value
            (if (equal (length entry) 4)
                (architect-read-string text (caddr entry) (cadddr entry))
              (architect-read-string text)))
         (push (cons replacement value) args)))
      (symbol-value (intern variable)))
    args))

(cl-defun architect-read-string (text &optional (regex ".+") (regex-error "require"))
  "This function is used to get user input string. It require an
non-empty string before to return it."
  (let ((answer) (not-empty)
        (input (format "%s: " text))
        (input-error
         (format "%s (%s): " text
                 (propertize regex-error 'face
                             'architect-error-face))))
    (while (not not-empty)
      (setq answer (string-trim (read-string input answer)))
      (if (string-match-p regex answer)
          (setq not-empty t)
        (setq input input-error)))
    answer))

(defun architect-create-project (template-name)
  "This function is used as action to ivy prompt executed in main
`architect' function."
  (let* ((directory) (args)
         (path (concat architect-source-directory template-name))
         (variable (format "architect-%s-variables" template-name)))
    (architect-load-configuration path)
    (unless (boundp (intern variable))
      (error (format "Variable '%s' not found." variable)))
    (setq directory (architect-read-directory template-name))
    (when (file-directory-p directory)
      (error (format "Directory '%s' already exists." directory)))
    (setq args (architect-read-args variable))))

;;; External Architect functions.

;;;###autoload
(defun architect ()
  "Provide functionality to create project template quickly. If
you want to create a new template, please add it to Architect src
directory."
  (interactive)
  (ivy-read "Create project: "
            (architect-template-candidates)
            :require-match t
            :action 'architect-create-project))

(provide 'architect)
