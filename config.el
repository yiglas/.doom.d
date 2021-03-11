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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

(defun yiglas-make-frame ()
  "."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun yiglas-delete-frame-or-kill-emacs ()
  "Delete the current frame or completely kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

(map! "C-c <left>" 'windmove-left
      "C-c <right>" 'windmove-right
      "C-c <up>" 'windmove-up
      "C-c <down>" 'windmove-down
      "C--" 'pop-global-mark
      "s-x" 'kill-region
      "<f12>" 'lsp-find-definition
      "C-<f12>" 'lsp-find-implementation
      "M-<return>" 'toggle-frame-maximized
      "M-n" 'yiglas-make-frame
      "C-x k" 'kill-current-buffer
      "C-c C-k" 'yiglas-delete-frame-or-kill-emacs
      "C-c r" 'recentf-open-files
      "C-x t" 'treemacs
      "s-." 'company-search-candidates)


(setq projectile-project-search-path '("~/code/"))
(setq enable-dir-local-variables t)
(setq require-final-newline t)
(setq mode-require-final-newline t)

;; disable lines numbers in the following modes
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; get rid of that pesky modeline
;; NOTE: this might go away if doom-modeline can change to the header line
(load-file "~/.doom.d/mode-line.el")

;; give some space around the buffer:
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(setq default-frame-alist
      (append (list
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe . 0)
               '(right-fringe . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(set-face-attribute 'default nil :font "JetBrains Mono")

;; save the screen size and location when closing eamcs
(when-let (dims (doom-store-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
          (append initial-frame-alist
                  `((left . ,left)
                    (top . ,top)
                    (width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen))))))

(defun yiglas-save-frame-dimensions ()
  (doom-store-put 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'yiglas-save-frame-dimensions)

;; apply rainbow-delimiters to all program modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)

;; setup unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "."
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; default shell in term
(setq-default shell-file-name "/bin/zsh")
(setq explicit-shell-file-name "/bin/zsh")

;; set the indenting for JavaScript
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq typescript-indent-level 2)

;; eshell-toggle allows me to open an Eshell window below the current buffer for the path (or project path) of the buffer.
(use-package! eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package! dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package! prettier-js
  :hook ('typescript-mode-hook 'prettier-js-mode))
