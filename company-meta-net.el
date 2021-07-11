;;; company-meta-net.el --- Company completion for C# project using meta-net  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 21:19:45

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Company completion for C# project using meta-net
;; Keyword: csproj csharp company
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (company "0.8.12") (meta-net "1.1.0"))
;; URL: https://github.com/emacs-vs/company-meta-net

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Company completion for C# project using meta-net
;;

;;; Code:

(require 'subr-x)

(require 'company)
(require 'meta-net)

(defgroup company-meta-net nil
  "Company completion for C# project using meta-net."
  :prefix "meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/company-meta-net"))

(defcustom company-meta-net-active-modes
  '(csharp-mode csharp-tree-sitter-mode)
  "Major modes that allow completion."
  :type 'list
  :group 'meta-view)

;; These keywords are grab from `csharp-mode'
(defconst company-meta-net--csharp-keywords
  (append
   '("class" "interface" "struct")
   '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
     "long" "ulong" "short" "ushort" "void" "object" "string" "var")
   '("typeof" "is" "as")
   '("enum" "new")
   '("using")
   '("abstract" "default" "final" "native" "private" "protected"
     "public" "partial" "internal" "readonly" "static" "event" "transient"
     "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
     "fixed" "override" "params" "async" "await" "extern" "unsafe"
     "get" "set" "this" "const" "delegate")
   '("select" "from" "where" "join" "in" "on" "equals" "into"
     "orderby" "ascending" "descending" "group" "when"
     "let" "by" "namespace")
   '("do" "else" "finally" "try")
   '("for" "if" "switch" "while" "catch" "foreach" "fixed" "checked"
     "unchecked" "using" "lock")
   '("break" "continue" "goto" "throw" "return" "yield")
   '("true" "false" "null" "value")
   '("base" "operator"))
  "Some C# keywords to eliminate namespaces")

(defvar-local company-meta-net--namespaces nil
  "Where store all the parsed namespaces.")

;;
;; (@* "Util" )
;;

(defun company-meta-net--re-seq (regexp string)
  "Get a list of all REGEXP match in a STRING."
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

;;
;; (@* "Core" )
;;

(defun company-meta-net--prepare ()
  "Prepare this package wit meta-net."
  (when (memq major-mode company-meta-net-active-modes)
    (meta-net-read-project)))

(defun company-meta-net--grab-namespaces ()
  "Parsed namespaces from current buffer."
  (setq company-meta-net--namespaces nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[,.:]*[ \t\n]*\\([a-z-A-Z0-9_-]+\\)[ \t\n]*[.;{]+" nil t)
      (save-excursion
        (forward-symbol -1)
        (unless (company-in-string-or-comment)
          (when-let ((symbol (thing-at-point 'symbol)))
            (push symbol company-meta-net--namespaces))))))
  (setq company-meta-net--namespaces (delete-dups (reverse company-meta-net--namespaces))))

;;
;; (@* "Company" )
;;

(defun company-meta-net--prefix ()
  "Return the string represent the prefix."
  (when (and (not (company-in-string-or-comment)) (company-meta-net--prepare))
    (or (company-grab-symbol-cons "\\." 1) 'stop)))

(defun company-meta-net--candidates ()
  ""
  )

(defun company-meta-net (command &optional arg &rest ignored)
  "Company backend for VS C# project.

Arguments COMMAND, ARG and IGNORED are standard arguments from `company-mode`."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-meta-net))
    (prefix (company-meta-net--prefix))
    (annotation (company-meta-net--candidates))
    (candidates )
    (doc-buffer )
    (kind )))

(provide 'company-meta-net)
;;; company-meta-net.el ends here
