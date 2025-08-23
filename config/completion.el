(defun dobin/select-and-insert ()
    (interactive)
  (corfu-next)
  (corfu-insert))
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :config
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "C-n") nil)
  (define-key corfu-map (kbd "C-y") 'dobin/select-and-insert))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (setq orderless-matching-styles '(orderless-flex)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-completion-provider :none)
  :hook
  (lsp-completion-mode . (lambda ()
			   (self (alist-get 'lsp-capf completion-category-defaults)
				 '((styles . (orderless))))))
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(provide 'completion)
