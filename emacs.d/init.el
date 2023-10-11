;;; Emacs basic configuration.
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq visible-bell nil)
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(setq visible-cursor nil)
(setq ring-bell-function 'ignore)
(column-number-mode)
(global-display-line-numbers-mode t)
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\{ . ?\})))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;; Tab configuration
(defvar tab-width-size 8)
(setq-default c-basic-offset tab-width-size
              tab-width tab-width-size
              indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; File configuration.
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode t)
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;;; Font configuration.
(defvar efs/default-font-family "Hack Nerd Font Mono")
(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)
(set-face-attribute 'default nil
		    :family efs/default-font-family
		    :height efs/default-font-size
		    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
		    :family efs/default-font-family
		    :height efs/default-font-size
		    :weight 'regular)
(set-face-attribute 'variable-pitch nil
		    :family efs/default-font-family
		    :height efs/default-variable-font-size
		    :weight 'regular)
(setq-default line-spacing 0.0)

;;; Keymap configuration.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer)

;;; Melpa package manager & auto renew configuration.
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                       ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;; Installed package configuration.

(use-package restart-emacs
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t))

(use-package all-the-icons
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ace-window
  :ensure t)
(global-set-key (kbd "M-o") 'ace-window)

(use-package hungry-delete
  :ensure t
  :bind (("C-c DEL" . hungry-delete-backward)
	 ("C-c d" . hungry-delete-forward)))
(global-hungry-delete-mode)

(use-package drag-stuff
  :ensure t
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))


;;; ivy & counsel
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package general
  :ensure t
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq
   company-minimum-prefix-length 0
   company-idle-delay 0.0))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package eglot
  :hook ((c-mode
          c++-mode
          java-mode
          ;; rust-mode
          ) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e a" . #'eglot-code-actions)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  (defun eglot-actions-before-save()
    (add-hook 'before-save-hook
              (lambda ()
                (call-interactively #'eglot-format)
                (call-interactively #'eglot-code-action-organize-imports))))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))
