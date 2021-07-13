[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![CI](https://github.com/emacs-vs/company-meta-net/workflows/CI/badge.svg)

# company-meta-net
> Company completion for C# project using meta-net

<p align="center">
  <img src="./etc/demo.png" width="611" height="317" />
</p>

## :floppy_disk: Quickstart

```el
(use-package company-meta-net
  :ensure t
  :hook (csharp-mode . (lambda ()
                         (add-to-list 'company-backends 'company-meta-net))))
```

## :hammer: Configurations

#### `company-meta-net-active-modes`

Major modes that allow completion.

#### `company-meta-net-display-annotation`

Display type annotation in company-mode's annotation command.

#### `company-meta-net-display-document`

Display document in company-mode's doc-buffer command.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
