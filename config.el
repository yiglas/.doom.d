;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Devin Sackett"
      user-mail-address "dsac@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
(set-face-attribute 'default nil :font "JetBrains Mono")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(setq mac-command-modifier 'super
      ns-command-modifier 'super
      ns-right-command-modifier 'super
      mac-option-modifier 'meta
      ns-option-modifier 'meta
      ns-right-option-modifier 'meta)

(map! "<escape>" 'doom/escape)

(defun +yiglas/make-frame ()
  "Make a new frame and switch that frame to the scratch buffer."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(map! "M-n" '+yiglas/make-frame)
(map! "C-c <left>" 'windmove-left)
(map! "C-c <right>" 'windmove-right)
(map! "C-c <up>" 'windmove-up)
(map! "C-c <down>" 'windmove-down)

(map! "M-<return>" 'toggle-frame-maximized)
(map! "C-x k" 'kill-current-buffer)
(map! "s-x" 'kill-region)

(map! (:when (featurep! :ui treemacs) "C-x t" 'treemacs))

(defun +yiglas/open-config ()
  (interactive)
  (find-file "~/.doom.d/config.el"))

(map! "s-," '+yiglas/open-config)

(after! treemacs
  (treemacs-follow-mode t))

(after! doom-modeline
  (setq doom-modeline-height 15
        doom-modeline-bar-width 6
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-persp-name nil
        doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-encoding nil)
  (custom-set-faces '(mode-line ((t (:height 0.90))))
                    '(mode-line-inactive ((t (:height 0.93))))))

(map! "s-." 'company-search-candidates)

(use-package! dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

(defun +yiglas/set-frame-dimensions ()
  (interactive)
  (when-let (dims (doom-store-get 'last-frame-size))
    (cl-destructuring-bind ((left . top) width height) dims
      (message "left %d top %d width %d height %d" left top width height)
      (set-frame-position (selected-frame) left top)
      (set-frame-size (selected-frame) width height))))

(add-hook! 'emacs-startup-hook #'+yiglas/set-frame-dimensions)

(defun +yiglas/save-frame-dimensions ()
  (interactive)
  (doom-store-put 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height))))

(add-hook 'kill-emacs-hook #'+yiglas/set-frame-dimensions)

(setq confirm-kill-emacs nil)

;; delete the selection when pasting
(delete-selection-mode 1)

;; delete by moving to trash
(setq delete-by-moving-to-trash t)

(after! org-mode
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(after! lsp-mode
  (setq lsp-lens-enable t)
  (map! "<f12>" 'lsp-find-definition
        "C-<f12>" 'lsp-find-implementation
        "C--" 'pop-global-mark)
  (dolist (dir
           '("[/\\\\]tools\\'"
             "[/\\\\].docz\\'"
             "[/\\\\]TestOutput\\'"
             "[/\\\\]Terraform\\'"
             "[/\\\\]QA Automation\\'"
             "[/\\\\]certs\\'"
             "[/\\\\]LoadTests\\'"
             "[/\\\\]Artifacts\\'"
             "[/\\\\]Database\\'"
             "[/\\\\].azuredevops\\'"
             "[/\\\\].docker\\'"
             "[/\\\\].log\\'"
             "[/\\\\].vs\\'"
             "[/\\\\]Dependencies\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))

(after! lsp-ui
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package! eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

;; make the titlebar completely transparent
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(add-hook! 'prog-mode-hook 'format-all-mode)
(add-hook! 'prog-mode-hook 'rainbow-delimiters-mode)
