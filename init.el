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
 '(column-number-mode nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mail-face 'default)
 '(display-time-mode t)
 '(package-selected-packages
   '(exec-path-from-shell which-key org-roam-bibtex org-beautify-theme org-link-beautify org-dashboard org-sidebar org-roam org-roam-server all-the-icons-dired neotree all-the-icons yasnippet lsp-ui rainbow-delimiters smartparens company lsp-docker flycheck fly-check use-package solarized-theme))
 '(spaceline-all-the-icons-clock-always-visible t)
 '(spaceline-all-the-icons-eyebrowse-display-name nil)
 '(spaceline-all-the-icons-flycheck-alternate nil)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-git-ahead 'commit)
 '(spaceline-all-the-icons-icon-set-modified 'toggle)
 '(spaceline-all-the-icons-icon-set-window-numbering 'solid)
 '(spaceline-all-the-icons-separator-type 'arrow)
 '(spaceline-all-the-icons-slim-render t)
 '(spaceline-all-the-icons-window-number-always-visible t)
 '(user-mail-address "platocrat@tuta.io"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(mode-line ((t (:background "#21252B" :foreground "#9DA5B4" :box (:line-width (1 . 1) :color "#181A1F"))))))



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



;;; lsp-mode -- for the n'th time...
;;; "`(use-package foo)` is a macro call that expands to `(require 'foo)`
;;; plus some house-keeping like reporting long load times."
;;;
;;;     Following lsp-mode code taken from:
;;;     https://github.com/MatthewZMD/.emacs.d#lsp
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldec-hook nil)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((js-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  :defer t
  :custom (company-lsp-cache-candidates 'auto))



;;; Enabling `all-the-icons` package in various elements of Emacs
;; Utility package to collect various Icon Fonts and propertize them within Emacs
(use-package all-the-icons)

;; Enable `all-the-icons` in dired-mode
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; A tree plugin like NerdTree
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))



;;; Org-roam
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Ensure `sglite3` is located on `exec-path`
(executable-find "sqlite3")

;; Set directory for `org-roam`
(setq org-roam-directory "~/.org-roam")

;; Start global minor mode `org-roam-mode` on startup
(add-hook 'after-init-hook 'org-roam-mode)



;;; Smart tab: `smart-tab` attempts to expand the text before the point or indent
;; the current line or selection.
(use-package smart-tab)
(global-smart-tab-mode 1)



;;; Spaceline
(use-package spaceline-all-the-icons)
(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (spaceline-all-the-icons-theme))


;;; Git gutter
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))



;;; Use fancy-battery
(use-package fancy-battery
  :init
  (fancy-battery-mode))

;;; Use which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))


;;; Initialize line-spacing
(setq-default line-spacing 0.3)



;;; Personals:
;; Remove default top-nav toolbar
(tool-bar-mode -1)

;; Normal erase for backspace key is opposite of default
(normal-erase-is-backspace-mode 1)

;; Load Emacs theme
(load-theme 'atom-one-dark t)
;; Load org-mode theme
(load-theme 'org-beautify t)

;;; init.el ends here
