;; architect-test.el --- Architect Testing File  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: project architect

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

;;; Code:

(require 'ert)
(require 'architect)

(ert-deftest architect-variable/variable-absent ()
  :expected-result :failed
  (architect-variable))

(ert-deftest architect-variable/variable-unexpected-type ()
  :expected-result :failed
  (architect-variable
    :variable t))

(ert-deftest architect-variable/after-function-unexpected-type ()
  :expected-result :failed
  (architect-variable
    :variable "__filename__"
    :input "filename"
    :after-function "function"))

(ert-deftest architect-variable/success ()
  :expected-result :passed
  (architect-variable
    :variable "__filename__"
    :input "filename"
    :after-function 'downcase))

(ert-deftest architect-commit/message-unexpected-type ()
  :expected-result :failed
  (architect-commit
    :add "."
    :message 2))

(ert-deftest architect-commit/success ()
  :expected-result :passed
  (architect-commit
    :add "."
    :message "Add initial commit"))

(ert-deftest architect-shell-command/success ()
  :expected-result :passed
  (architect-shell-command
    :command "touch a"
    :before "commit"))

(ert-deftest architect-shell-command/before-unexpected-value ()
  :expected-result :failed
  (architect-shell-command
    :command "touch a"
    :before "not-commit"))

(provide 'architect-test)

;;; architect-test.el ends here
