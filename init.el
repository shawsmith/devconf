;;; 调整性能相关参数.
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(defalias 'yes-or-no-p 'y-or-n-p)

;;; 启动时展示启动时间及垃圾回收次数.
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;;; 设置字体及大小、行高
(defvar efs/default-font-family "IBM 3270")
(defvar efs/default-font-size 160)
(defvar efs/default-variable-font-size 160)

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

;;; 禁用自动备份、保存
(setq make-backup-files nil
      auto-save-default nil)

;;; Auto reload buffer when file content changed.
(global-auto-revert-mode t)

;;; 设置三方包源
(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; 设置软件包自动更新
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:00"))

;;; 设置一些界面上的UI样式
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell nil)
(column-number-mode)
(global-display-line-numbers-mode t)
(electric-pair-mode 1)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\{ . ?\})))
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default c-basic-offset 8
	      tab-width 8
              indent-tabs-mode nil)

;;; 保存文件时删除首尾空白字符
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;;; 键盘映射
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; 代码格式化
;;;(use-package clang-format
;;;  :ensure t)
(global-set-key (kbd "M-s-l") 'lsp-format-buffer)

(use-package ace-window
  :ensure t)
(global-set-key (kbd "M-o") 'ace-window)

(use-package restart-emacs)

;;; 设置主题
(use-package zenburn-theme
  :init (load-theme 'zenburn t))

;;; 设置Modeline
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward)
	 ("C-c d" . hungry-delete-forward)))
(global-hungry-delete-mode)

(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;;; Which-Key提示
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
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package general
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 0)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; 缩放字体
(use-package hydra
  :defer t
  :bind(("C-c t s" . hydra-text-scale/body)))

(defhydra hydra-text-scale (:timeout 10)
  "scale text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/codebase")
    (setq projectile-project-search-path '("~/codebase")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :config
  (global-company-mode)
  (setq
   company-minimum-prefix-length 0
   company-idle-delay 0.0
   company-show-quick-access t))

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package rust-mode
  :mode
  (("\\.rs\\'" . rust-mode))
  :hook
  (rust-mode . hs-minor-mode) ;; 折叠模式
  (rust-mode . eldoc-mode) ;; 代码追踪
  (rust-mode . company-mode) ;; 自动填充
  (rust-mode . (lambda () (setq indent-tabs-mode nil))) ;; 设置缩进
  :config
  (setq rust-format-on-save t))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . efs/lsp-mode-setup)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (java-mode . lsp-deferred)
	 (rust-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(defun lsp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'lsp-mode-hook #'lsp-install-save-hooks)

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
	 (dap-session-created . (lambda (&_rest) (dap-hydra)))
	 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package dap-java
  :ensure nil)

(use-package lsp-java
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         30
          treemacs-width-is-initially-locked     nil
          treemacs-workspace-switch-cleanup      nil)
    (treemacs-resize-icons 18)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(with-eval-after-load 'treemacs
  (defun treemacs-custom-filter (file _)
    (or (s-ends-with? ".class" file)
	(s-ends-with? ".log" file)
	(s-ends-with? ".lock" file)))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))
(setq lsp-treemacs-sync-mode 1)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(add-hook 'lsp-mode-hook #'lsp-lens-mode)

;;; Setup java shortcut keys
(global-set-key (kbd "M-s-b") 'lsp-find-definition)
(global-set-key (kbd "M-s-t") 'lsp-find-type-definition)
(global-set-key (kbd "<M-s-left>") 'switch-to-prev-buffer)
(global-set-key (kbd "<M-s-right>") 'switch-to-next-buffer)
(global-set-key (kbd "M-s-r") 'lsp-rename)
(global-set-key (kbd "M-s-o") 'lsp-organize-imports)
(global-set-key (kbd "M-RET") 'lsp-java-add-import)

(use-package lsp-ui
:ensure t
:after (lsp-mode)
:bind (:map lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
:init (setq lsp-ui-doc-enable nil
	    lsp-ui-sideline-enable nil))
(add-hook 'lsp-ui-doc-mode-hook #'(lambda()(display-line-numbers-mode -1)))

(use-package helm
:ensure t
:init
(helm-mode 1)
(progn (setq helm-buffers-fuzzy-matching t))
:bind
(("C-c h" . helm-command-prefix))
(("C-c b" . helm-bookmarks))
(("C-c f" . helm-recentf))   ;; Add new key to recentf
(("C-c g" . helm-grep-do-git-grep)))

(use-package helm-descbinds
:ensure t
:bind ("C-h b" . helm-descbinds))

(use-package lsp-ivy
  :after lsp-mode)
