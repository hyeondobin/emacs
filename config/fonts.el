(setq face-font-rescale-alist '((".D2Coding." . 1.26)))
(add-to-list 'default-frame-alist '(font . "D2CodingLigature Nerd Font"))

(set-face-attribute 'default nil
		    :height 140
		    :family "D2CodingLigature Nerd Font")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
		  '("D2Coding" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
		  '("D2Coding" . "iso10646-1"))

(provide 'fonts)
