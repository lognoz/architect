;; architect.el --- Architect File  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/architect
;; Keywords: project architect
;; Package-Version: 0.1
;; Package-Requires: ((emacs "26.3"))

;; This file is not part of GNU Emacs.

;;; License: GNU General Public License v3.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate project rapidly by using template.
;; https://github.com/lognoz/architect

;; This project is inspired by project-archetypes by Magnar Sveen.
;; https://github.com/magnars/.emacs.d/tree/master/project-archetypes

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Contextual constant.

(defgroup achitect nil
  "Provide functionality to create project template quickly."
  :prefix "architect"
  :group 'tools
  :link '(url-link "https://github.com/lognoz/architect"))

(defgroup architect-faces nil
  "Faces used in Architect."
  :group 'architect
  :group 'faces)

(defcustom architect-directory nil
  "The directory of source templates."
  :group 'architect
  :type 'string)

(defface architect-error-face
  '((t :foreground "#ce5555"))
  "Face when in user prompt when input doesn't match."
  :group 'architect-faces)

(defconst architect-base-directory (file-name-directory load-file-name)
  "The directory Architect was loaded from.")

(defvar architect-template-variables nil
  "The list of template variables.")

(defvar architect-template-replacements nil
  "The list of template replacements.")

(defvar architect-template-commits nil
  "The list of template commits.")

(defvar architect-template-shell-command nil
  "The list of template shell command.")

(defvar architect-template-destination nil
  "The destination directory template.")

(defvar architect-template-default-directory nil
  "The template default directory.")

(defvar architect-variable-keywords
  '((:variable :type "string" :require t)
    (:after-function :type "symbol" :function t)
    (:input :type "string")
    (:input-error :type "string")
    (:regex :type "string")
    (:value :type "string"))
  "The available keywords in `architect-variable' function.")

(defvar architect-commit-keywords
  '((:message :type "string" :require t)
    (:add :type "string" :require t))
  "The available keywords in `architect-commit' function.")

(defvar architect-shell-command-keywords
  '((:command :type "string" :require t)
    (:before :type "string" :in ("commit" "replacement")))
  "The available keywords in `architect-shell-command' function.")

;;; Internal functions.

(defun architect--validate-directory ()
  "Check if the user given directory is valid."
  (let ((directory architect-directory))
    (unless (stringp directory)
      (error (format "Variable architect-directory malformed, value need to be a string")))
    (setq directory (concat (string-trim-right directory "/") "/"))
    (unless (file-directory-p directory)
      (error "Directory %S does not exist" directory))
    (setq architect-directory directory)))

(defun architect--template-candidates ()
  "Provide candidates to `architect' prompt.
Return a list of directories located into `architect-directory'."
  (architect--validate-directory)
  (let ((list))
    (dolist (f (directory-files architect-directory))
      (let ((path (concat architect-directory f)))
        (when (and (file-directory-p path)
                   (file-exists-p (concat path "/architect.el"))
                   (not (equal f "."))
                   (not (equal f "..")))
          (push (file-name-nondirectory path) list))))
    list))

(defun architect--load-configuration (path)
  "This function load project configuration by PATH."
  (let ((path (concat path "/architect.el")))
    (if (file-exists-p path)
        (load path nil 'nomessage)
      (error (format "File '%s' not found." path)))))

(defun architect--set-directory ()
  "Provide prompt to ask where the project will be created."
  (let* ((path
           (read-directory-name
             "Directory: "
             architect-template-default-directory)))
    (setq path (concat (string-trim-right path "/") "/"))
    (when (file-directory-p path)
      (setq path
        (concat path
          (architect--read-string
            (list :regex "^[-_A-Za-z0-9]+$"
                  :input (concat "Destination " path)
                  :input-error "alphanumeric")))))
    (setq architect-template-destination
          (string-trim-right path "/"))))

(defun architect--set-variables ()
  "Fetch into `architect-template-variables' and execute prompt.
This function return an assosiative array that will be used to
replace in template file."
  (let ((selector) (after-function) (value))
    (dolist (args architect-template-variables)
      (setq selector (plist-get args :variable)
            after-function (plist-get args :after-function)
            value (plist-get args :value))
      (unless value
        (setq value (architect--read-string args)))
      (when after-function
        (setq value (funcall after-function value)))
      (push (cons selector value) architect-template-replacements))))

(defun architect--replace-filename (path)
  "Apply filename replacement based on template variables.
This function expect to recive a PATH that will be used to fetch
recursively."
  (let ((return-path path))
    (dolist (replacement architect-template-replacements)
      (let ((variable (car replacement))
            (value (cdr replacement)))
        (when (string-match-p variable path)
          (setq return-path
            (replace-regexp-in-string
              variable value return-path nil 'literal))
          (rename-file path return-path))))
    return-path))

(defun architect--apply-replacement (file)
  "Scan FILE content to apply replacement based on template variables."
  (with-temp-file file
    (insert-file-contents-literally file)
    (dolist (replacement architect-template-replacements)
      (goto-char 0)
      (while (search-forward (car replacement) nil t)
        (replace-match (cdr replacement)))
      replacement)))

(defun architect--copy-template (path)
  "Create directory recursively and copy template content.
This function expect to recive PATH argument."
  (let ((directory (file-name-directory architect-template-destination)))
    (make-directory directory t))
  (copy-directory path architect-template-destination))

(defun architect--initialize-git ()
  "Fetch `architect-template-commits' and stage file to commit them."
  (shell-command-to-string "git init .")
  (dolist (commit architect-template-commits)
    (let ((stage (plist-get commit :add))
          (message (plist-get commit :message)))
      (shell-command-to-string
        (format "git add %s" stage))
      (shell-command-to-string
        (format "git commit -m \"%s\"" message)))))

(defun architect--rename-files (directory)
  "Rename files in DIRECTORY by replacing variables define in path."
  (dolist (f (directory-files directory))
    (unless (or (equal f ".") (equal f ".."))
      (let ((path (concat directory "/" f)))
        (setq path (architect--replace-filename path))
        (when (file-directory-p path)
          (architect--rename-files path))))))

(defun architect--read-string (plist)
  "Provide string input by defined PLIST.
It require an non-empty string before to return it."
  (let ((answer) (valid) (prompt-text) (prompt-error-text)
        (input (plist-get plist :input))
        (input-error (plist-get plist :input-error))
        (regex (plist-get plist :regex)))
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

(defun architect--completing-read ()
  "Read and return a template name."
  (let ((candidates (architect--template-candidates)))
    (list (completing-read "Create project: " candidates nil t))))

(defun architect--validate (args validation prefix-error)
  "Throw error if given ARGS didn't pass the VALIDATION.
This function checks if value is the same as TYPE argument.
If it's not it print an error message based on PREFIX-ERROR."
  (dolist (parameters validation)
    (let* ((keyword (car parameters))
           (validation (cdr parameters))
           (value (plist-get args keyword))
           (type-expected (plist-get validation :type))
           (type-value (symbol-name (type-of value))))
      (when (and (plist-get validation :require) (not value))
        (error (format "%s expects to receive %s" prefix-error keyword)))
      (when (and value (not (equal type-value type-expected)))
        (error (format "%s expects %s to be a %s" prefix-error keyword type-expected)))
      (when (and value (plist-get validation :function) (not (fboundp value)))
        (error (format "%s can't found '%s' function in %s"
                       prefix-error (symbol-name value) keyword)))
      (let ((whitelist (plist-get validation :in)))
        (when (and value whitelist (not (cl-position value whitelist :test 'equal)))
          (error (format "%s %s expects to receive %s"
                         prefix-error keyword
                        (mapconcat 'identity whitelist " or "))))))))

(defun architect--execute-shell-command (&optional before)
  "Fetch `architect-template-shell-command' and execute define script.
This function receive BEFORE argument to restrict when to apply
the shell command."
  (dolist (process architect-template-shell-command)
    (let ((command (plist-get process :command))
          (execute-before (plist-get process :before)))
      (when (equal execute-before before)
        (shell-command-to-string command)))))

(defun architect--create-project (template-name)
  "Create project by argument TEMPLATE-NAME.
This function is executed after `architect' function prompt."
  (let* ((path (expand-file-name (concat architect-directory template-name))))
    (architect--load-configuration path)
    (architect--set-directory)
    (architect--set-variables)
    (architect--copy-template path)
    (let ((default-directory architect-template-destination))
      (architect--execute-shell-command "replacement")
      (architect--rename-files architect-template-destination)
      (delete-file "architect.el")
      (dolist (path (directory-files-recursively "." ""))
        (unless (file-directory-p path)
          (message (format "Replace variables in %s" path))
          (architect--apply-replacement path)))
      (architect--execute-shell-command "commit")
      (architect--initialize-git)
      (architect--execute-shell-command))
    (dired architect-template-destination)
    (message "")))

;;; External Architect functions.

;;;###autoload
(defun architect-variable (&rest args)
  "Define variable that will be fetch in `architect--set-variables'.

  (architect-variable
     [:keyword [ARGS]]...)

:variable        String used to be replace with the value.
:value           String used as `:variable' replacement.
:after-function  Symbol of a function executed after user define the value.

:input           String that will be used as label.
                 If input is defined, its mean that Architect will provide
                 a prompt to the user.
:input-error     String used as prompt error if it's not valid.
:regex           String regex used to check if the value is valid or not."
  (let ((prefix-error "Architect: architect-variable"))
    (architect--validate args architect-variable-keywords prefix-error)
    (let ((value (plist-get args :value))
          (input (plist-get args :input)))
      (when (or (and (not value) (not input))
                (and value input))
        (error (format "%s expects to receive :input or :value" prefix-error))))
    (setq architect-template-variables
          (append architect-template-variables (list args)))))

;;;###autoload
(defun architect-commit (&rest args)
  "Define commit that will be executed into `architect--initialize-git'.

  (architect-commit
     [:keyword [ARGS]]...)

:add      String used in git command to stage changes.
:message  String used in git command to commit staged changes."
  (let ((prefix-error "Architect: architect-commit"))
    (architect--validate args architect-commit-keywords prefix-error))
  (setq architect-template-commits
        (append architect-template-commits (list args))))

;;;###autoload
(defun architect-shell-command (&rest args)
  "Define shell command that will be executed into `architect--execute-command'.

  (architect-shell-command
     [:keyword [ARGS]]...)

:command  String used as shell command.
:before   Reference of the step user want to execute the shell command."
  (let ((prefix-error "Architect: architect-shell-command"))
    (architect--validate args architect-shell-command-keywords prefix-error))
  (setq architect-template-shell-command
        (append architect-template-shell-command (list args))))

;;;###autoload
(defun architect-default-directory (directory)
  "Define DIRECTORY that will be used in `architect--set-directory'.
This command is used in `architect.el' template file."
  (when (file-directory-p directory)
    (setq architect-template-default-directory directory)))

;;;###autoload
(defun architect (template)
  "Provide functionality to create project quickly.
This function provide a prompt to choose which TEMPLATE you want
to create."
  (interactive (architect--completing-read))
  (setq architect-template-variables nil
        architect-template-replacements nil
        architect-template-commits nil
        architect-template-shell-command nil
        architect-template-default-directory nil)
  (architect--create-project template))

(provide 'architect)

;;; architect.el ends here
