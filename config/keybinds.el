(defun dobin/open-init-file ()
  "Open init.el"
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'dobin/open-init-file)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c C-s") 'eshell)
(global-set-key (kbd "C-c C-o") 'delete-other-windows)

;; use C-, as leader key
(global-set-key (kbd "C-, C-f r") 'recentf-open-files)
(global-set-key (kbd "C-, C-i") 'ibuffer)
(global-set-key (kbd "C-, d") 'dired)

(provide 'keybinds)
