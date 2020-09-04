;;; org-link-beautify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-link-beautify" "org-link-beautify.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-link-beautify.el

(autoload 'org-link-beautify-enable "org-link-beautify" "\
Enable `org-link-beautify'." nil nil)

(autoload 'org-link-beautify-disable "org-link-beautify" "\
Disable `org-link-beautify'." nil nil)

(defvar org-link-beautify-mode nil "\
Non-nil if Org-Link-Beautify mode is enabled.
See the `org-link-beautify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-link-beautify-mode'.")

(custom-autoload 'org-link-beautify-mode "org-link-beautify" nil)

(autoload 'org-link-beautify-mode "org-link-beautify" "\
A minor mode that beautify Org links with colors and icons.

If called interactively, enable Org-Link-Beautify mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-link-beautify" '("org-link-beautify"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-link-beautify-autoloads.el ends here
