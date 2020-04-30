;; autoload.el --- Architect Autoload -*- lexical-binding: t; -*-

(defun generate-autoload (path)
  "Generate autoload file by given PATH."
  (let ((generated-autoload-file (expand-file-name "autoload.el")))
    (update-file-autoloads path t generated-autoload-file)))

(mapc #'generate-autoload '("architect.el"))
