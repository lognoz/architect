;; template.el --- Architect Template Example -*- lexical-binding: t; -*-

(architect-default-directory "~/document")

(architect-variable
  :variable "__date__"
  :value (format-time-string "%Y-%m-%d"))

(architect-variable
  :variable "__filename__"
  :regex "^[-_a-z0-9]+$"
  :input "Filename"
  :input-error "alphanumeric")

(architect-variable
  :variable "__print__"
  :input "Print message"
  :after-function 'capitalize)

(architect-commit
  :add "."
  :message "Add initial commit")

(architect-shell-command
  :command "make")
