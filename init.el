;; -*- lexical-binding: t; -*-

;;; This file is generated from confiig.org file in this repository

;;; --- Basic Configuration ---

(setq inhibit-startup-message t)
(tab-bar-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq scroll-margin 10)

;;; --- Elpaca Bootstrap ---
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; (setq use-package-always-ensure t)

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'macchiato)
  (catppuccin-reload)
  (load-theme 'catppuccin :no-confirm))

(use-package evil
  :demand t
  :ensure t
  :init
  ;; Pre load configuration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'elpaca-log-mode 'emacs)
  (evil-mode 1)
  (evil-global-set-key 'insert "<hangul>" 'toggle-input-method)
  )

(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (evil-collection-init))

(use-package evil-numbers
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preview-current t)
  (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (completion-at-point-functions
	(list (
	       cape-capf-debug #'cape-dict)))
  :init
  (global-corfu-mode)
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;(vertico-multiform-mode)
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  ;; (vertico-multiform-commands
  ;;  '((consult-imenu buffer indexed)
  ;;    (execute-extended-command unobtrusive)))
  ;; (vertico-multiform-categories
  ;;  '((file grid)
  ;;    (consult-grep buffer)))
  )

(use-package emacs
  :custom
  ;; (context-menu-mode t)
  ;; (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; (minibuffer-prompt-properties
   ;; '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (text-mode-ispell-word-completion nil)
  )

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t)
  :config
  (orderless-define-completion-style dh/orderless-flex
    (orderless-matching-styles '(orderless-flex
				 orderless-literal
				 orderless-regexp)))
  (setq completion-category-overrides '(
					(command (styles dh/orderless-flex))
					(symbol (styles dh/orderless-flex))
					(variable (styles dh/orderless-flex))
					(file (styles partial-completion)))))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register consult--source-recent-file consult--source-project-recent-file :preview-key '(:debounce 0.4 any)))

(use-package transient
  :ensure t)
(use-package magit
  :ensure t
  :after transient)

(use-package which-key
  :diminish
  :ensure t
  :config
  (which-key-mode 1))

;; auto pair
(electric-pair-mode 1)
(savehist-mode 1)
(recentf-mode 1)

(setq completion-ignore-caes t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(save-place-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ;

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(set-face-attribute 'default nil
		    :height 150
		    :family "JetBrainsMono Nerd Font")

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(global-set-key (kbd "<hangul>") 'toggle-input-method)

(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setenv "GTK_IM_MODULE" "fcitx")
(setenv "QT_IM_MODULE" "fcitx")
(setenv "XMODIFIERS" "@im=fcitx")

(use-package org
  :ensure t
  :bind
  (
   :map org-mode-map
   ("C-c <up>" . org-priority-up)
   ("C-c <down>" . org-priority-down))
  )

(use-package org-super-agenda :ensure t)
(use-package comment-tags :ensure t)

(setq org-agenda-files '("~/org")) ; tell agenda where files are

(setq org-log-done 'time) ; TODO
(setq org-return-follows-link t) ; RET

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-capture-templates
    '(
	("j" "Work Log Entry"
	 entry (file+datetree "~/org/work-log.org")
	 "* %?"
	 :empty-lines 0)
	("n" "Note"
	 entry (file+headline "~/org/notes.org" "Random notes")
	 "** %?"
	 :empty-lines 0)
	("g" "General To-Do"
	 entry (file+headline "~/org/todos.org" "General Tasks")
	 "* TODO [#B] %?\n:Created: %T\n "
	 :empty-lines 0)
	))

(setq org-todo-keywords
      '((sequence 
	 "TODO(t)"
	 "PLANNING(p)"
	 "IN-PROGRESS(i@/!)"
	 "VERIFYING(v!)"
	 "|"
	 "DONE(d)"
	 "OBE(o@!)"
	 "WONT-DO(w@/!)")))

(setq org-todo-keyword-faces
      '(
	("TODO" . (:foreground "GoldenRod" :weight bold))
	("PLANNING" . (:foreground "DeepPink" :weight bold))
	("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
	("VERIFYING" . (:foreground "DarkOrange" :weight bold))
	("BLOCKED" . (:foreground "Red" :weight bold))
	("DONE" . (:foreground "LimeGreen" :weight bold))
	("OBE" . (:foreground "LimeGreen" :weight bold))
	("WONT-DO" . (:foreground "LimeGreen" :weight bold)
	 )))
