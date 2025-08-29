;; enable org package
(use-package org)

;; packages recommended by tutorial (https://github.com/james-stoup/emacs-org-mode-tutorial)
(use-package org-super-agenda
  :ensure t)
(use-package comment-tags
  :ensure t)

;; Tell agenda where the org files are
(setq org-agenda-files '("~/org"))

;; When a TODO is set to a done state, record a timestamp
(setq org-log-done 'time)

;; Follow the links
(setq org-return-follows-link t)

;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make the indentation look nicer
(add-hook 'org-mode-hook 'org-indent-mode)

;; Remap the change priority keys to use the UP or DOWN key
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)

;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; When you want to change the level of an org item, use SMR
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)

;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;; Wrap the lines in org mode so that things are easier to read
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
	))



(provide 'orgs)
