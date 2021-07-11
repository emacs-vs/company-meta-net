;;; company-meta-net.el --- Company completion for C# project using meta-net  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 21:19:45

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Company completion for C# project using meta-net
;; Keyword: csproj csharp company
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (company "0.8.12") (meta-net "1.1.0"))
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

(defvar-local company-meta-net--namespaces nil
  "Where store all the parsed namespaces.")

(defvar company-meta-net-show-debug nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun company-meta-net-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when company-meta-net-show-debug (apply 'message fmt args)))

;;
;; (@* "Core" )
;;

(defun company-meta-net--prepare ()
  "Prepare this package wit meta-net."
  (when (memq major-mode company-meta-net-active-modes)
    (meta-net-read-project)
    meta-net-csproj-current))

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

(defun company-meta-net--match-name (type)
  "Return non-nil, if the TYPE match the current namespace list.

The argument TYPE is a list of namespace in string.  For instance,

   using System.Collections;  => '(System Collections)

We use this to eliminate not possible candidates."
  (let ((match t) (len (length type)) (index 0) item)
    (while (and match (< index len))
      (setq item (nth index type)
            index (1+ index)
            match (member item company-meta-net--namespaces)))
    match))

;;
;; (@* "Company" )
;;

(defun company-meta-net--prefix ()
  "Return the string represent the prefix."
  (when (and (not (company-in-string-or-comment)) (company-meta-net--prepare))
    (or (company-grab-symbol-cons "\\." 1) 'stop)))

(defun company-meta-net--candidates ()
  "Return a possible candidates from current point."
  (company-meta-net--grab-namespaces)
  (let* ((xmls (meta-view--all-xmls))  ; Get the list of xml files from current project
         (xmls-len (length xmls))      ; length of the xmls
         (xml-index 0)                 ; index search through all `xmls`
         (project meta-net-csproj-current)
         meta-view--namespaces  ; list namespace that displays on top, under `endregion'
         xml          ; current xml path as key
         break        ; flag to stop
         type         ; xml assembly type
         comp-name    ; name of the type, the last component from the type
         splits       ; temporary list to chop namespace, use to produce `comp-name`
         candidates)  ; final results
    (while (and (not break) (< xml-index xmls-len))
      (setq xml (nth xml-index xmls)
            xml-index (1+ xml-index))
      (let* ((types (meta-net-xml-types xml))
             (types-len (length types))
             (type-index 0))
        (while (and (not break) (< type-index types-len))
          (setq type (nth type-index types)
                type-index (1+ type-index)
                splits (split-string type "\\.")
                comp-name (nth (1- (length splits)) splits))
          ;; Check if all namespaces appears in the buffer,
          ;;
          ;; We use `butlast' to get rid of the component name because we do
          ;; allow the same level candidates. For example, `NamespaceA` contains
          ;; `classA` and `classB`, and we ignore the check of classA and classB
          ;; in order to let them appears in our candidates list!
          (when (company-meta-net--match-name (butlast splits))
            (setq candidates (append candidates splits))
            (company-meta-net-debug "\f")
            (company-meta-net-debug "xml: %s" xml)
            (company-meta-net-debug "Type: %s" type)
            (company-meta-net-debug "Name: %s" comp-name)
            ))))
    (setq candidates (delete-dups candidates))
    candidates))

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
