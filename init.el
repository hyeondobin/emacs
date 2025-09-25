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
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(setq ring-bell-function #'ignore)

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

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  ;; (general-evil-setup)
  (general-define-key
   :keymaps 'override
   :states '(insert normal hybrid motion visual operator emacs)
   :prefix-map '+prefix-map
   :prefix-command '+prefix-map
   :prefix ","
   :global-prefix "M-,")
  
  (general-create-definer dh-global-def
    :wk-full-keys nil
    :keymaps '+prefix-map)
  (dh-global-def
   "SPC" 'execute-extended-command
   "h" (general-simulate-key "C-h" :which-key "help")
   "!" 'shell-command
   ":" 'eval-expression
   "." 'repeat
   "z" '((lambda (local) (interactive "p")
  	 (unless repeat-mode (repeat-mode))
  	 (let ((local current-prefix-arg)
  	       (current-prefix-arg nil))
  	   (call-interactively (if local #'text-scale-adjust #'global-text-scale-adjust))))
         :which-key "Zoom")
   "?" 'which-key-show-top-level
   )
  (general-create-definer dh-global-leader
    :keymaps 'override
    :states '(insert normal hybrid motion virual operator)
    :prefix ", m"
    :non-normal-prefix "M-, m"
    "" '(:ignore t
         :which-key
         (lambda (arg)
  	 (cons (cadr (split-string (car arg) " "))
  	       (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))))
    )
  (defmacro +general-global-menu! (name prefix-key &rest body)
    "Create a definer named +general-global-NAME wrapping dh-global-def.
    Create prefix map: +general-global-NAME-map. Prefix bindings in BODY with PREFIX-KEY."
    (declare (indent 2))
    (let* ((n (concat "+general-global-" name))
  	 (prefix-map (intern (concat n "-map"))))
      `(progn
         (general-create-definer ,(intern n)
  	 :wrapping dh-global-def
  	 :prefix-map (quote ,prefix-map)
  	 :prefix ,prefix-key
  	 :wk-full-keys nil
  	 "" '(:ignore t :which-key ,name))
         (,(intern n) ,@body)))
    )
  (+general-global-menu! "application" "a"
    "p" '(:ignore t "elpaca")
    "pb" 'elpaca-browse
    "pr" '(
  	 (lambda () (interactive)
  	   (let ((current-prefix-arg (not current-prefix-arg))
  		 (this-command 'elpaca-rebuild))
  	     (call-interactively #'elpaca-rebuild)))
  	 :which-key "rebuild")
    "pm" 'elpaca-manager
    "pl" 'elpaca-log
    "pi" 'elpaca-info
    "pI" '((lambda () (interactive) (info "Elpaca"))
  	 :which-key "elpaca-info")
    "ps" 'elpaca-status
    "pt" 'elpaca-try
    "pv" 'elpaca-visit
    )
  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer
    "i" 'consult-buffer
    "o" '((lambda () (interactive) (switch-to-buffer nil))
  	:which-key "other-buffer")
    "p" 'previous-buffer
    "r" 'rename-buffer
    "R" 'revert-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
  	:which-key "messages-buffer")
    "n" 'next-buffer
    "s" 'scratch-buffer
    "TAB" '((lambda () (interactive) (switch-to-buffer nil))
  	  :whick-key "other-buffer")
    )
  (+general-global-menu! "bookmark" "B")
  (+general-global-menu! "eval" "e"
    "b" 'eval-buffer
    "d" 'eval-defun
    "e" 'eval-expression
    "p" 'pp-eval-last-sexp ; pp = pretty print
    "s" 'eval-last-sexp)
  (+general-global-menu! "file" "f"
    "d" '((lambda (&optional arg) (interactive "P")
  	  (let ((buffer (when arg (current-buffer))))
  	    (diff-buffer-with-file buffer)))
  	:which-key "diff-with-file")
    "e" '(:ignore t :which-key "edit")
    "ec" '((lambda () (interactive) (find-file "~/.emacs.d/config.org"))
  	:which-key "Open Emacs Config")
    "ed" '((lambda () (interactive) (find-file-existing literate-file)(widen))
  	 :which-key "dotfile")
    "f" 'find-file
    "p" 'find-function-at-point
    "P" 'find-function
    "r" 'consult-recent-file
    "R" 'rename-file-and-buffer
    "s" 'save-buffer
    "v" 'find-variable-at-point
    "V" 'find-variable
    )
  (dh-global-def
    "q" '(:ignore t :which-key "quit")
    "qr" 'restart-emacs
    "qs" 'save-buffers-kill-terminal
    ))

(use-package evil
  :demand t
  :ensure t
  :init
  ;; Pre load configuration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil))
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'elpaca-log-mode 'emacs)
  (evil-mode 1)
  (when (fboundp #'general-define-key)
    (general-define-key
     :states '(motion)))
  ;; (evil-global-set-key 'insert (kbd<hangul>) 'toggle-input-method)
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (evil-collection-init))

(use-package evil-numbers
  :ensure t
  :after (general)
  :init
  (general-define-key :states '(normal)
   "C-a" 'evil-numbers/inc-at-pt
   "C-s" 'evil-numbers/dec-at-pt))

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
  :bind (
	 :map corfu-map
	      (""))
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

(desktop-save-mode 1)

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
  (:map org-mode-map
   ("C-c <up>" . org-priority-up)
   ("C-c <down>" . org-priority-down))
  :config
  (dh-global-leader
    :keymaps '(org-mode-map)
    "'" '(org-edit-special
  	:which-key "edit code block")
    "b" '(:ignore t :which-key "Babel")
    "bt" 'org-babel-tangle
    "c" 'org-capture
    "h" 'consult-org-heading
    "s" '(:ignore t :which-key "Src")
    "st" 'org-insert-structure-template
    )
  (dh-global-leader
    :keymaps '(org-src-mode-map lisp-mode-shared-map)
    "'" 'org-edit-src-exit
    "k" 'org-edit-src-abort
    )
  (with-eval-after-load 'org
    (add-to-list 'org-structure-template-alist
  	       '("se" . "src emacs-lisp\n")))
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
