;;; -*- lexical binding: t -*-
;; managing completion related configs. 

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package consult
  :bind
  (
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
   ("C-x C-b" . consult-buffer) ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark
   ("C-M-#" . consult-register)

   ("M-y" . consult-yank-pop) ;; orig. yank-pop

   ;; M-g `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g r" . consult-grep-match)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ("M-s d" . consult-find)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)

   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-iserach-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ;; Minibuffer history
   ("M-s" . consult-history)
   ("M-r" . consult-history)
   )
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<"))

(defun dobin/select-and-insert ()
  (interactive)
  (corfu-next)
  (corfu-insert))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (global-corfu-minibuffer nil)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("S-SPC" . corfu-insert-separator)
	;; use M-[n, p] to select completions and C-[n, p] to move cursor
	([remap next-line] . nil)
	([remap previous-line] . nil)
	;; I want C-a or C-e to act as default
	([remap move-beginning-of-line] . nil)
	([remap move-end-of-line] . nil)
	("RET" . nil)
	("C-y" . dobin/select-and-insert)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (setq orderless-matching-styles '(orderless-flex)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun dobin/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  
  (defun dobin/lsp-mode-setup-completion ()
    (self (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless)))
  (add-hook 'orderless-style-dispatchers #'dobin/orderless-dispatch-flex-first nil 'local)
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-completion-provider :none)
  
  (lsp-format-buffer-on-save t)
  (lsp-format-buffer-on-save-list '(nix-mode nix-ts-mode))
  (lsp-diagnostics-mode t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-diagnostics-mode)
  (lsp-completion-mode . dobin/lsp-mode-setup-completion))

(use-package lsp-ui
  ;; :custom
  :commands lsp-ui-mode)

(use-package lsp-nix
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'completion)
