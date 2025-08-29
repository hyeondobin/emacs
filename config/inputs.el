(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(global-set-key (kbd "<hangul>") 'toggle-input-method)

(setenv "GTK_IM_MODULE")
(setenv "QT_IM_MODULE")
(setenv "XMODIFIERS" "@im=fcitx")

(provide 'inputs)

