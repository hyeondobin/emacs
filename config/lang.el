;; languages

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook
  (nix-ts-mode . lsp-deferred))

;; (use-package nix-mode
    ;; :mode "\\.nix\\'")

(provide 'lang)
