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

(require 'company)
(require 'meta-net)

(defgroup company-meta-net nil
  "Company completion for C# project using meta-net."
  :prefix "meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/company-meta-net"))



(provide 'company-meta-net)
;;; company-meta-net.el ends here
