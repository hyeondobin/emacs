;; enable org package
(use-package org
  :bind
  (:map org-mode-map
	;; Remap the change priority keys to use the UP or DOWN key
	("C-c <up>" . org-priority-up)
	("C-c <down>" . org-priority-down)
	;; When you want to change the level of an org item, use SMR
	("C-c C-g C-r" . org-shiftmetaright)
	))


;; packages recommended by tutorial (https://github.com/james-stoup/emacs-org-mode-tutorial)
(use-package org-super-agenda)
(use-package comment-tags)


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



;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)



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
	("g" "General To-Do"
	 entry (file+headline "~/org/todos.org" "General Tasks")
	 "* TODO [#B] %?\n:Created: %T\n "
	 :empty-lines 0)
	))

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)" "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
	))
;; TODO Colors
(setq org-todo-keyword-faces
      '(
	("TODO" . (:foreground "GoldenRod" :weight bold))
	("PLANNING" . (:foreground "DeepPink" :weight bold))
	("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
	("VERIFYING" . (:foreground "DarkOrange" :weight bold))
	("BLOCKED" . (:foreground "Red" :weight bold))
	("DONE" . (:foreground "LimeGreen" :weight bold))
	("OBE" . (:foreground "LimeGreen" :weight bold))
	("WONT-DO" . (:foreground "LimeGreen" :weight bold))
	))

(provide 'orgs)
