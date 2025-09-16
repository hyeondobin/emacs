;;; keybinds.el --- My personal key binding -*- lexical-binding: t -*-

(defun dobin/open-init-file ()
  "Open init.el."
  (interactive)
  (find-file user-init-file))

;; join line with C-M-j like C-j in vim's normal mode Note that C-u --
;; is passed as `-1'
(global-set-key (kbd "C-M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; use C-z as leader key
(define-prefix-command 'C-z-map)
(global-set-key (kbd "C-z") 'C-z-map)
(global-set-key (kbd "C-z s") 'eshell)
(global-set-key (kbd "C-z C-s") 'eshell)
(global-set-key (kbd "C-z C-o") 'delete-other-windows)
;; https://stackoverflow.com/a/12558095/29108843
(global-set-key (kbd "C-z C-f o") (lambda() (interactive) (find-file "~/org")))
(global-set-key (kbd "C-z C-f i") 'dobin/open-init-file)
(global-set-key (kbd "C-z C-f r") 'consult-recent-file)
(global-set-key (kbd "C-z C-i") 'consult-buffer)
(global-set-key (kbd "C-z d") 'dired-jump)
(global-set-key (kbd "C-z C-f n") (lambda() (interactive) (find-file "/etc/nixos/flake.nix")))
(global-set-key (kbd "C-z t") 'vterm)

(provide 'keybinds)

;;; keybinds.el ends here
