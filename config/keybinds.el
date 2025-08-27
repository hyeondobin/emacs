(defun dobin/open-init-file ()
  "Open init.el"
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'dobin/open-init-file)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c C-s") 'eshell)

(global-set-key (kbd "C-c d") 'dired)

(provide 'keybinds)
