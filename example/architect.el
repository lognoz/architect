;; template.el --- Architect Template Example -*- lexical-binding: t; -*-

(architect-define-default-directory "~/")

(architect-define-variable
  :variable "__date__"
  :value (format-time-string "%Y-%m-%d"))

(architect-define-variable
  :variable "__filename__"
  :regex "^[-_A-Za-z0-9]+$"
  :input "Filename"
  :input-error "alphanumeric"
  :after-function 'downcase)

(architect-define-variable
  :variable "__print__"
  :input "Print message"
  :after-function 'capitalize)

(architect-define-commit
  :add "."
  :message "Add initial commit")
