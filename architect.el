;; architect.el --- Architect File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: architect

;; This file is not part of GNU Emacs.

;; This Emacs config is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This Emacs config is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this Emacs config. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;; Contextual Architect constants.

(defconst architect-base-directory (file-name-directory load-file-name)
  "The directory Architect was loaded from.")

(defconst architect-source-directory (concat architect-base-directory "src/")
  "The directory of source templates.")

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
      (error (format "File %s not found." path)))))

(defun architect-default-directory (variable-name)
  "This function return the default directory defined variable
value. If it doesn't exists, it return none."
  (let ((variable (intern variable-name))
        (directory))
    (when (boundp variable)
      (setq variable (symbol-value variable))
      (when (file-directory-p variable)
        (setq directory variable)))
    directory))

(defun architect-read-directory (target)
  "This function provide a prompt input to ask where the project
will be created."
  (let* ((variable (format "architect-%s-default-directory" target))
         (prompt-default-directory (architect-default-directory variable)))
    (read-directory-name "Directory: " prompt-default-directory nil t)))

(defun architect-create-project (template-name)
  "This function is used as action to ivy prompt executed in main
`architect' function"
  (let* ((path (concat architect-source-directory template-name))
         (directory))
    (architect-load-configuration path)
    (setq directory (architect-read-directory template-name))))

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
