;; -*- mode: eask; lexical-binding: t -*-

(package "company-meta-net"
         "0.1.0"
         "Company completion for C# project using meta-net")

(website-url "https://github.com/emacs-vs/company-meta-net")
(keywords "convenience" "csproj" "csharp" "company")

(package-file "company-meta-net.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "jcs-elpa")
(source "melpa")

(depends-on "emacs" "25.1")
(depends-on "company")
(depends-on "meta-net")
(depends-on "ht")

(development
 (depends-on "ert-runner"))
 
(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
