;;; package --- Summary
;;; Commentary:
;;; For installing MELPA.
(require 'package)

;;; Code:
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
   '(rainbow-delimiters smartparens company lsp-docker flycheck fly-check use-package solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "/Users/platocrat/.emacs.d/elpa/")

;; Enable SLIME for Quicklisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;; Install `flycheck`
(package-install 'flycheck)
(global-flycheck-mode)
;; required for flycheck on MacOS
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)
;; To provide simple syntax to declare and configure packages in your init file
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Package to enable multi-cursor on-click
(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this)
	 ("M-," . mc/unmark-next-like-this)
	 ("<s-mouse-1>" . mc/add-cursor-on-click)))

;; Keeping parentheses balanced
(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;; Highlight parens etc. for improved readability
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; JavaScript
(setq-default js-indent-level 2)

;;; Personals:
;; Remove default top-nav toolbar
(tool-bar-mode -1)

;; Normal erase for backspace key is opposite of default
(normal-erase-is-backspace-mode 1)

;; For solarized Emacs theme
(load-theme 'solarized-dark t)

;;; init.el ends here
