;; compile.el --- Architect Byte Compile -*- lexical-binding: t; -*-

(add-to-list 'load-path default-directory)
(mapc #'byte-compile-file '("architect.el"))
