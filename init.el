(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1 :inherit ignore
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let* ((buffer (pop-to-buffer-same-window "*elpacr-bootstrap*"))
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

;; use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Elpaca Bootstrap done

;; Maximize screen on startup

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'colors)
(require 'completion)
(require 'keybinds)
(require 'inputs)
(require 'UIs)
(require 'fonts)
(require 'lang)
(require 'orgs)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(setenv "EDITOR" "emacs")
(setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.bitwarden-ssh-agent.sock"))

(recentf-mode 1)

(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(nix "https://github.com/nix-community/tree-sitter-nix")))
	
(use-package treesit-auto
  :ensure t
:custom
(treesit-auto-install 'prompt)
:config
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode))

(use-package transient :ensure t)
(use-package magit :ensure t :after transient
;;:bind
;;("C-x g" . magit-status)
;;:config
)

(use-package sudo-edit :ensure t)
(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))

(require 'tramp)
(setq tramp-remote-path
      '(tramp-own-remote-path
	"/run/current-system/sw/bin"
	"/usr/local/bin" "/usr/bin" "/bin"))
(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
(setq tramp-default-remote-shell "/run/current-system/sw/bin/bash")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

(setq tramp-verbose 6)

(use-package vterm
  :ensure t
  :bind
  ("C-c t" . vterm)
  )
  
