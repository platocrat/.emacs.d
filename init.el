(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired. See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need nor want to do this.
  ;;(and-to-list `package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck indium xref-js2 js2-refactor js2-mode company-lsp yasnippet lsp-ui lsp-metals lsp-mode fly-check sbt-mode scala-mode use-package solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "/Users/platocrat/.emacs.d/elpa/")

;; Enable SLIME for Quicklisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Common Lisp implementation for Slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Add mode for Dockerfile syntax highlighting
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Remove default top-nav toolbar
(tool-bar-mode -1)

;; Normal erase for backspace key is opposite of default
(normal-erase-is-backspace-mode 1)

;; For solarized Emacs theme
(load-theme 'solarized-dark t)


;; Scala's Metals IDE for Emacs, thanks to the `lsp-mode` package.
;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code: https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
	     :mode "\\.s\\(cala\\|bt\\)$")

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
	     :commands sbt-start sbt-command
	     :config
	     ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
	     ;; allows using SPACE when in the minibuffer
	     (substitute-key-definition
	      'minibuffer-complete-word
	      'self-insert-command
	      minibuffer-local-completion-map)
	     ;; sbt-supershell kills sbt-mode: https://github.com/hvesalai/emacs-sbt-mode/issues/152
	     (setq sbt:program-options '("-Dsbt.supershell=false"))
	     )

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
	     :init (global-flycheck-mode))

(use-package lsp-mode
	     ;; Optional - enable lsp-mode automatically in scala files
	     :hook (scala-mode . lsp)
	     (lsp-mode . lsp-lens-mode)
	     :config (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals
             :config
             (setq lsp-metals-treeview-show-when-views-received t)
	     )

;; Enable nice rendering of documentation on hover
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for  them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Add company-lsp backend for metals
(use-package company-lsp)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
	     ;; Posframe is a pop-up tool that must be manually installed for dap-mode
	     )
(use-package dap-mode
	     :hook
	     (lsp-mode . dap-mode)
	     (lsp-mode . dap-ui-mode)
	     )

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs)

;; on MacOS, `ls` doesn't support the `--dired` option, while on Linux, it is supported.
;; Reference:
;; https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;;; .emacs ends here
