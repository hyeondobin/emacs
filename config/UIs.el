(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'after-init-hook 'toggle-frame-maximized)

(setq scroll-margin 10)

;; rainbow delimiter : 괄호 무지개 색으로 묶어서 색칠해줌
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

 (provide 'UIs)
