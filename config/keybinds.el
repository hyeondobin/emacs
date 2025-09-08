(defun dobin/open-init-file ()
  "Open init.el"
  (interactive)
  (find-file user-init-file))

(defun dobin/open-nixos-flake ()
  "Open NixOS flake file"
  (interactive)
  (find-file "/etc/nixos/flake.nix"))

(global-set-key (kbd "C-z i") 'dobin/open-init-file)

(global-set-key (kbd "C-z s") 'eshell)
(global-set-key (kbd "C-z C-s") 'eshell)
(global-set-key (kbd "C-z C-o") 'delete-other-windows)

;; use C-z as leader key
(define-prefix-command 'C-z-map)
(global-set-key (kbd "C-z") 'C-z-map)
(global-set-key (kbd "C-z C-f r") 'consult-recent-file)
(global-set-key (kbd "C-z C-i") 'ibuffer)
(global-set-key (kbd "C-z d") 'dired-jump)
(global-set-key (kbd "C-z C-e n") 'dobin/open-nixos-flake)

(provide 'keybinds)
