;;; mode-line.el --- Description -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------------
;; Copyright (C) 2021 Devin Sackett
;;
;; Author: Devin Sackett <https://github.com/dsac>
;; Created: February 19, 2021
;; Modified: February 19, 2021
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;; ---------------------------------------------------------------------;;
;;; Commentary:

;;; Code:

(defgroup yiglas '()
  "Faces and colors for the modeline")

(defcustom yiglas-color-foreground "#ECEFF4"
  "Nord6."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-background "#2E3440"
  "Nord0."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-highlight "#3B4252"
  "Nord1."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-critical "#EBCB8B"
  "Nord11."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-salient "#81A1C1"
  "Nord9."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-strong "#ECEFF4"
  "Nord6."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-popout "#D08770"
  "Nord12."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-subtle "#434C5E"
  "Nord2."
  :type 'color
  :group 'yiglas)

(defcustom yiglas-color-faded "#677691"
  "."
  :type 'color
  :group 'yiglas)


(defface yiglas-face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'yiglas)

(defface yiglas-face-header-popout nil
  "Popout face for the header line"
  :group 'yiglas)

(defface yiglas-face-header-critical nil
  "Critical face for the header line"
  :group 'yiglas)

(defface yiglas-face-header-faded nil
  "Faded face for the header line"
  :group 'yiglas)

(defface yiglas-face-header-default nil
  "Default face for the header line"
  :group 'yiglas)

(defface yiglas-face-header-strong nil
  "Strong face for the header line"
  :group 'yiglas)


(set-face-attribute 'yiglas-face-header-popout nil
                    :foreground yiglas-color-background
                    :background yiglas-color-popout
                    :box `(:line-width 1
                           :color ,yiglas-color-background
                           :style nil))

(set-face-attribute 'yiglas-face-header-critical nil
                    :foreground yiglas-color-background
                    :background yiglas-color-critical
                    :box `(:line-width 1
                           :color ,yiglas-color-background
                           :style nil))

(set-face-attribute 'yiglas-face-header-faded nil
                    :foreground yiglas-color-background
                    :background yiglas-color-faded
                    :box `(:line-width 1
                           :color ,yiglas-color-background
                           :style nil))

(set-face-attribute 'yiglas-face-header-default nil
                    :foreground yiglas-color-foreground
                    :background yiglas-color-subtle
                    :box `(:line-width 1
                           :color ,yiglas-color-background
                           :style nil))

(set-face-attribute 'yiglas-face-header-strong nil
                    :foreground yiglas-color-strong
                    :background yiglas-color-subtle
                    :inherit 'yiglas-face-strong
                    :box `(:line-width 1
                           :color ,yiglas-color-background
                           :style nil))

(set-face-attribute 'yiglas-face-subtle nil
                    :background yiglas-color-subtle)


;; -------------------------------------------------------------------
(defun vc-branch ()
  "."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `MAX-LENGTH' characters of a directory name `DIR'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; -------------------------------------------------------------------
(defun yiglas-modeline-compose (status name primary secondary)
  "Compose a string with provided information.
`STATUS` `NAME` `PRIMARY` `SECONDARY`"
  (let* ((char-width (window-font-width nil 'header-line))
         (window (get-buffer-window (current-buffer)))
         (space-up +0.15)
         (space-down -0.20)
         (prefix (cond ((string= status "RO") (propertize " RO " 'face 'yiglas-face-header-popout))
                       ((string= status "**") (propertize " ** " 'face 'yiglas-face-header-critical))
                       ((string= status "RW") (propertize " RW " 'face 'yiglas-face-header-faded))
                       (t (propertize status 'face 'yiglas-face-header-popout))))
         (left (concat
                (propertize " "  'face 'yiglas-face-header-default 'display `(raise ,space-up))
                (propertize name 'face 'yiglas-face-header-strong)
                (propertize " "  'face 'yiglas-face-header-default 'display `(raise ,space-down))
                (propertize primary 'face 'yiglas-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width)
                             (length prefix)
                             (length left)
                             (length right)
                             (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat
     prefix
     left
     (propertize (make-string available-width ?\ ) 'face 'yiglas-face-header-default)
     (propertize right 'face `(:inherit yiglas-face-header-default
                               :foreground ,yiglas-color-faded)))))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-mu4e-dashboard-mode-p ()
  "."
  (bound-and-true-p mu4e-dashboard-mode))

(defun yiglas-modeline-mu4e-dashboard-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Mail" (yiglas-modeline-mu4e-context) ""))

;; ---------------------------------------------------------------------

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the yiglas-modeline function to set
;; the header format in a notebook buffer.  Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.
(with-eval-after-load 'ein
  (defun yiglas-modeline-ein-notebook-mode ()
    (let ((buffer-name (format-mode-line "%b")))
      (yiglas-modeline-compose (if (ein:notebook-modified-p) "**" "RW") buffer-name "" (ein:header-line))))
  (setq ein:header-line-format '((:eval (yiglas-modeline-ein-notebook-mode)))))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-elfeed-search-mode-p ()
  "."
  (derived-mode-p 'elfeed-search-mode))

(defun yiglas-modeline-elfeed-search-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Elfeed" (concat "(" (elfeed-search--header)  ")") ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-elfeed-show-mode-p ()
  "."
  (derived-mode-p 'elfeed-show-mode))

(defun yiglas-modeline-elfeed-show-mode ()
  "."
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tags-str (mapconcat #'symbol-name tags ", "))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (plist-get (elfeed-feed-meta feed) :title)))
    (yiglas-modeline-compose (yiglas-modeline-status) (s-truncate 40 title "…") (concat "(" tags-str ")") feed-title)))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-calendar-mode-p ()
  "."
  (derived-mode-p 'calendar-mode))

(defun yiglas-modeline-calendar-mode () "." "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative 'header-line
                             `(:overline ,(face-foreground 'default)
                               :height 0.5
                               :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-org-capture-mode-p ()
  "."
  (bound-and-true-p org-capture-mode))

(defun yiglas-modeline-org-capture-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Capture" "(org)" ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    (message nil))
  (add-hook 'org-capture-mode-hook #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)

(defun yiglas-modeline-info-breadcrumbs ()
  "."
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
        (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
        line)
    (while (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                                 crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
             (if (not (equal node "Top")) node
               (format "%s"
                       (if (stringp Info-current-file)
                           (file-name-sans-extension (file-name-nondirectory Info-current-file))
                         Info-current-file)))))
        (setq line (concat line
                           (if (null line) "" " > ")
                           (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun yiglas-modeline-info-mode-p ()
  "."
  (derived-mode-p 'Info-mode))

(defun yiglas-modeline-info-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Info" (concat "(" (yiglas-modeline-info-breadcrumbs) ")") ""))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-org-agenda-mode-p ()
  "."
  (derived-mode-p 'org-agenda-mode))

(defun yiglas-modeline-org-agenda-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Agenda" "" (format-time-string "%H:%M")))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-term-mode-p ()
  "."
  (derived-mode-p 'term-mode))

(defun yiglas-modeline-vterm-mode-p ()
  "."
  (derived-mode-p 'vterm-mode))

(defun yiglas-modeline-term-mode ()
  "."
  (yiglas-modeline-compose " >_ " "Terminal" (concat "(" shell-file-name ")") (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-mu4e-main-mode-p ()
  "."
  (derived-mode-p 'mu4e-main-mode))

(defun yiglas-modeline-mu4e-main-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Mail" (yiglas-modeline-mu4e-context) (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-mu4e-headers-mode-p ()
  "."
  (derived-mode-p 'mu4e-headers-mode))

(defun yiglas-modeline-mu4e-headers-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) (mu4e~quote-for-modeline mu4e~headers-last-query) "" ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (yiglas-modeline)))

;; ---------------------------------------------------------------------
(setq mu4e-modeline-max-width 72)

(defun yiglas-modeline-mu4e-view-mode-p ()
  "."
  (derived-mode-p 'mu4e-view-mode))

(defun yiglas-modeline-mu4e-view-mode ()
  "."
  (let* ((msg (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from (mu4e~headers-contact-str (mu4e-message-field msg :from))))
    (yiglas-modeline-compose (yiglas-modeline-status) (s-truncate 40 subject "…") "" from)))

(add-hook 'mu4e-view-mode-hook
          (lambda () (setq header-line-format "%-")
            (face-remap-add-relative 'header-line
                                     '(:background "#ffffff"
                                       :underline nil
                                       :box nil
                                       :height 1.0))))


;; ---------------------------------------------------------------------
(defun yiglas-modeline-yiglas-help-mode-p ()
  "."
  (derived-mode-p 'yiglas-help-mode))

(defun yiglas-modeline-yiglas-help-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Emacs" "(help)" ""))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-message-mode-p ()
  "."
  (derived-mode-p 'message-mode))

(defun yiglas-modeline-message-mode ()
  "."
  (yiglas-modeline-compose (yiglas-modeline-status) "Message" "(draft)" ""))


;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)

(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            '(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun yiglas-modeline-org-clock-mode-p ()
  "."
  org-mode-line-string)

(defun yiglas-modeline-org-clock-mode ()
  "."
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name (format-mode-line "%m"))
          (branch (vc-branch)))
      (yiglas-modeline-compose
       (yiglas-modeline-status)
       buffer-name
       (concat "(" mode-name (if branch (concat ", ") (propertize branch 'face 'italic)) ")")
       org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-docview-mode-p ()
  "."
  (derived-mode-p 'doc-view-mode))

(defun yiglas-modeline-docview-mode ()
  "."
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name (format-mode-line "%m"))
        (branch (vc-branch))
        (page-number (concat
                      (number-to-string (doc-view-current-page))
                      "/"
                      (or (ignore-errors (number-to-string (doc-view-last-page-number))) "???"))))
    (yiglas-modeline-compose
     (yiglas-modeline-status)
     buffer-name
     (concat "(" mode-name (if branch (concat ", ") (propertize branch 'face 'italic)) ")")
     page-number)))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-pdf-view-mode-p ()
  "."
  (derived-mode-p 'pdf-view-mode))

(defun yiglas-modeline-pdf-view-mode ()
  "."
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name (format-mode-line "%m"))
        (branch (vc-branch))
        (page-number (concat
                      (number-to-string (pdf-view-current-page))
                      "/"
                      (or (ignore-errors (number-to-string (pdf-cache-number-of-pages))) "???"))))
    (yiglas-modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name (if branch (concat ", ")) (propertize branch 'face 'italic) ")")
     page-number)))

;; ---------------------------------------------------------------------
(defun buffer-menu-mode-header-line ()
  "."
  (face-remap-add-relative 'header-line `(:background ,(face-background 'yiglas-face-subtle))))

(add-hook 'Buffer-menu-mode-hook #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun yiglas-modeline-completion-list-mode-p ()
  "."
  (derived-mode-p 'completion-list-mode))

(defun yiglas-modeline-completion-list-mode ()
  "."
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name (format-mode-line "%m"))
          (position (format-mode-line "%l:%c")))
      (yiglas-modeline-compose (yiglas-modeline-status) buffer-name "" position)))

;; ---------------------------------------------------------------------
(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun yiglas-modeline-deft-mode-p ()
  "."
  (derived-mode-p 'deft-mode))

(defun yiglas-modeline-deft-mode ()
  "."
  (let ((prefix " DEFT ")
        (primary "Search")
        (filter (if deft-filter-regexp
                    (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (yiglas-modeline-compose prefix primary filter matches)))


;; ---------------------------------------------------------------------
(defun yiglas-modeline-prog-mode-p ()
  "."
  (derived-mode-p 'prog-mode))

(defun yiglas-modeline-text-mode-p ()
  "."
  (derived-mode-p 'text-mode))

(defun yiglas-modeline-default-mode ()
  "."
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name (format-mode-line "%m"))
          (branch (vc-branch))
          (position (format-mode-line "%l:%c")))
      (yiglas-modeline-compose
       (yiglas-modeline-status)
       buffer-name
       (concat "(" mode-name (if branch (concat ", " (propertize branch 'face 'italic)) ")"))
       position)))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)."
  (let ((read-only buffer-read-only)
        (modified (and buffer-file-name (buffer-modified-p))))
    (cond
     (modified "**")
     (read-only "RO")
     (t "RW"))))

;; ---------------------------------------------------------------------
(defun yiglas-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."
  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))

;; ---------------------------------------------------------------------
(defun yiglas-modeline ()
  "Install a header line whose content is dependend on the major mode."
  (interactive)
  (setq-default header-line-format
                '((:eval
                   (cond
                    ((yiglas-modeline-prog-mode-p) (yiglas-modeline-default-mode))
                    ((yiglas-modeline-message-mode-p) (yiglas-modeline-message-mode))
                    ((yiglas-modeline-elfeed-search-mode-p) (yiglas-modeline-elfeed-search-mode))
                    ((yiglas-modeline-elfeed-show-mode-p) (yiglas-modeline-elfeed-show-mode))
                    ((yiglas-modeline-deft-mode-p) (yiglas-modeline-deft-mode))
                    ((yiglas-modeline-info-mode-p) (yiglas-modeline-info-mode))
                    ((yiglas-modeline-calendar-mode-p) (yiglas-modeline-calendar-mode))
                    ((yiglas-modeline-org-capture-mode-p) (yiglas-modeline-org-capture-mode))
                    ((yiglas-modeline-org-agenda-mode-p) (yiglas-modeline-org-agenda-mode))
                    ((yiglas-modeline-org-clock-mode-p) (yiglas-modeline-org-clock-mode))
                    ((yiglas-modeline-term-mode-p) (yiglas-modeline-term-mode))
                    ((yiglas-modeline-vterm-mode-p) (yiglas-modeline-term-mode))
                    ((yiglas-modeline-mu4e-dashboard-mode-p) (yiglas-modeline-mu4e-dashboard-mode))
                    ((yiglas-modeline-mu4e-main-mode-p) (yiglas-modeline-mu4e-main-mode))
                    ((yiglas-modeline-mu4e-headers-mode-p) (yiglas-modeline-mu4e-headers-mode))
                    ((yiglas-modeline-text-mode-p) (yiglas-modeline-default-mode))
                    ((yiglas-modeline-pdf-view-mode-p) (yiglas-modeline-pdf-view-mode))
                    ((yiglas-modeline-docview-mode-p) (yiglas-modeline-docview-mode))
                    ((yiglas-modeline-completion-list-mode-p) (yiglas-modeline-completion-list-mode))
                    ((yiglas-modeline-yiglas-help-mode-p) (yiglas-modeline-yiglas-help-mode))
                    (t (yiglas-modeline-default-mode)))))))


;; ---------------------------------------------------------------------
(defun yiglas-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."
  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t))
          (eq (window-in-direction 'below) (minibuffer-window))
        (not (window-in-direction 'below)))
      (with-current-buffer (window-buffer window)
        (setq mode-line-format (list "")))
      (with-current-buffer (window-buffer window)
        (setq mode-line-format nil)))))

(add-hook 'window-configuration-change-hook 'yiglas-modeline-update-windows)

(setq eshell-status-in-modeline nil)

(yiglas-modeline)

(provide 'mode-line)
;;; mode-line.el ends here
