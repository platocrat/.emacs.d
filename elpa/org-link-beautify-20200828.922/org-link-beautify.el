;;; org-link-beautify.el --- Beautify Org Links -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-08-28 17:22:45 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "27.1") (all-the-icons "4.0.0"))
;; Package-Commit: be208d2aceb0cc71fcd54a8127af25800461b94e
;; Package-Version: 20200828.922
;; Package-X-Original-Version: 1.0
;; Keywords: hypermedia
;; homepage: https://github.com/stardiviner/org-link-beautify

;; org-link-beautify is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; org-link-beautify is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; (org-link-beautify-mode 1)

;;; Code:

(require 'ol)
(require 'org-element)
(require 'all-the-icons)

(defgroup org-link-beautify nil
  "Customize group of org-link-beautify-mode."
  :prefix "org-link-beautify-"
  :group 'org)

(defcustom org-link-beautify-exclude-modes '(org-agenda-mode)
  "A list of excluded major modes which wouldn't enable `org-link-beautify'."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-thumbnail-dir "~/.cache/thumbnails/"
  "The directory of generated thumbnails."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-thumbnail-size 512
  "The video thumbnail image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-types-list '("avi" "rmvb" "ogg" "ogv" "mp4" "mkv" "flv")
  "A list of video file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defun org-link-beautify--get-element (position)
  "Return the org element of link at the `POSITION'."
  (save-excursion (goto-char position) (org-element-context)))

(defun org-link-beautify--get-link-description-fast (position)
  "Get the link description at `POSITION' (fuzzy but faster version)."
  (save-excursion
    (goto-char position)
    (and (org-in-regexp org-link-bracket-re) (match-string 2))))

(defun org-link-beautify--warning (path)
  "Use `org-warning' face if link PATH does not exist."
  (if (and (not (file-remote-p path))
           (file-exists-p (expand-file-name path)))
      'org-link 'org-warning))

(defun org-link-beautify (start end path bracket-p)
  "Display icon for the link type based on PATH from START to END."
  ;; (message
  ;;  (format "start: %s, end: %s, path: %s, bracket-p: %s" start end path bracket-p))
  (unless (memq major-mode org-link-beautify-exclude-modes)
    ;; detect whether link is normal, jump other links in special places.
    (when (eq (car (org-link-beautify--get-element start)) 'link)
      (save-match-data
        (let* ((link-element (org-link-beautify--get-element start))
               ;; (link-element-debug (message link-element))
               (raw-link (org-element-property :raw-link link-element))
               ;; (raw-link-debug (message raw-link))
               (type (org-element-property :type link-element))
               (extension (or (file-name-extension (org-link-unescape path)) "txt"))
               ;; (ext-debug (message extension))
               (description (or (and (org-element-property :contents-begin link-element) ; in raw link case, it's nil
                                     (buffer-substring-no-properties
                                      (org-element-property :contents-begin link-element)
                                      (org-element-property :contents-end link-element)))
                                ;; when description not exist, use raw link for raw link case.
                                raw-link))
               ;; (desc-debug (message description))
               (icon (pcase type
                       ("file"
                        (cond
                         ((file-remote-p path) ; remote file
                          (all-the-icons-faicon "server" :face 'org-warning))
                         ((not (file-exists-p (expand-file-name path))) ; not exist file
                          (all-the-icons-faicon "exclamation-triangle" :face 'org-warning))
                         ((file-directory-p path) ; directory
                          (all-the-icons-icon-for-dir
                           "path"
                           :face (org-link-beautify--warning path)
                           :v-adjust 0))
                         (t (all-the-icons-icon-for-file ; file
                             (format ".%s" extension)
                             :face (org-link-beautify--warning path)
                             :v-adjust 0))))
                       ("file+sys" (all-the-icons-faicon "link"))
                       ("file+emacs" (all-the-icons-icon-for-mode 'emacs-lisp-mode))
                       ("http" (all-the-icons-icon-for-url (concat "http:" path) :v-adjust -0.05))
                       ("https" (all-the-icons-icon-for-url (concat "https:" path) :v-adjust -0.05))
                       ("ftp" (all-the-icons-faicon "link"))
                       ("custom-id" (all-the-icons-faicon "hashtag"))
                       ("coderef" (all-the-icons-faicon "code"))
                       ("id" (all-the-icons-fileicon ""))
                       ("attachment" (all-the-icons-faicon "puzzle-piece"))
                       ("elisp" (all-the-icons-icon-for-mode 'emacs-lisp-mode :v-adjust -0.05))
                       ("shell" (all-the-icons-icon-for-mode 'shell-mode))
                       ("eww" (all-the-icons-icon-for-mode 'eww-mode))
                       ("mu4e" (all-the-icons-faicon "envelope-o" :v-adjust -0.05))
                       ("git" (all-the-icons-octicon "git-branch"))
                       ("orgit" (all-the-icons-octicon "git-branch"))
                       ("orgit-rev" (all-the-icons-octicon "git-commit"))
                       ("orgit-log" (all-the-icons-icon-for-mode 'magit-log-mode))
                       ("pdfview" (all-the-icons-icon-for-file ".pdf"))
                       ("grep" (all-the-icons-icon-for-mode 'grep-mode))
                       ("occur" (all-the-icons-icon-for-mode 'occur-mode))
                       ("man" (all-the-icons-icon-for-mode 'Man-mode))
                       ("info" (all-the-icons-icon-for-mode 'Info-mode))
                       ("help" (all-the-icons-icon-for-mode 'Help-Mode))
                       ("rss" (all-the-icons-material "rss_feed"))
                       ("elfeed" (all-the-icons-material "rss_feed"))
                       ("telnet" (all-the-icons-faicon "compress"))
                       ("wikipedia" (all-the-icons-faicon "wikipedia-w"))
                       ("mailto" (all-the-icons-material "email" :v-adjust -0.05))
                       ("doi" (all-the-icons-fileicon "isabelle"))
                       ("eaf" (all-the-icons-faicon "linux" :v-adjust -0.05)))))
          (when bracket-p (ignore))
          (cond
           ;; video thumbnails
           ((and (equal type "file") (member extension org-link-beautify-video-types-list))
            (let* ((video (expand-file-name (org-link-unescape path)))
                   (thumbnails-dir (file-name-directory
                                    (or org-link-beautify-thumbnail-dir "~/.cache/thumbnails/")))
                   (thumbnail-size (or org-link-beautify-thumbnail-size 512))
                   
                   (thumbnail (expand-file-name (format "%s%s.jpg" thumbnails-dir (file-name-base video)))))
              ;; (message (format "ffmpegthumbnailer -f -i %s -s %s -o %s"
              ;;                  (shell-quote-argument video) thumbnail-size (shell-quote-argument thumbnail)))
              (shell-command
               (format "ffmpegthumbnailer -f -i %s -s %s -o %s"
                       (shell-quote-argument video) thumbnail-size (shell-quote-argument thumbnail)))
              ;; (message "start: %s, end: %s" start end)
              ;; (message "%s" thumbnail)
              (put-text-property
               start end
               'display (create-image thumbnail nil nil :ascent 'center :max-height thumbnail-size))))
           ;; general icons
           (t (put-text-property
               start end
               'display
               (propertize
                (concat
                 (propertize "[" 'face '(:inherit nil :underline nil :foreground "orange"))
                 (propertize description 'face '(:underline t :foreground "dark cyan"))
                 (propertize "]" 'face '(:inherit nil :underline nil :foreground "orange"))
                 (propertize "(" 'face '(:inherit nil :underline nil :foreground "orange"))
                 (propertize icon 'face '(:inherit nil :underline nil :foreground "gray"))
                 (propertize ")" 'face '(:inherit nil :underline nil :foreground "orange"))))))))))))

(defun org-link-beautify-toggle-overlays ()
  "Toggle the display of `org-link-beautify'."
  (let ((point (point-min))
        (bmp (buffer-modified-p)))
    (while (setq point (next-single-property-change point 'display))
	    (when (and (get-text-property point 'display)
		             (eq (get-text-property point 'face) 'org-link))
	      (remove-text-properties
	       point (setq point (next-single-property-change point 'display))
	       '(display t))))
    (set-buffer-modified-p bmp))
  (org-restart-font-lock))

;;;###autoload
(defun org-link-beautify-enable ()
  "Enable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (org-link-set-parameters link-type :activate-func #'org-link-beautify))
  (org-link-beautify-toggle-overlays))

;;;###autoload
(defun org-link-beautify-disable ()
  "Disable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (org-link-set-parameters link-type :activate-func t))
  (org-link-beautify-toggle-overlays))

;;;###autoload
(define-minor-mode org-link-beautify-mode
  "A minor mode that beautify Org links with colors and icons."
  :init-value nil
  :lighter nil
  :group 'org-link-beautify
  :global t
  (if org-link-beautify-mode
      (org-link-beautify-enable)
    (org-link-beautify-disable)))



(provide 'org-link-beautify)

;;; org-link-beautify.el ends here
