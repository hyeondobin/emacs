(set-face-attribute 'default nil 
		    :height 160  ;; 기본 베이스가 되는 크기 설정
		    :family "JetBrainsMono Nerd Font")

;;(add-to-list 'default-frame-alist '(font . "D2CodingLigature Nerd Font"))

;;(set-fontset-font nil 'hangul (font-spec :family "D2Coding" :pixelsize 12))
(setq face-font-rescale-alist '((".*D2Coding.*" . 1.24)))
(setq-default line-spacing 3)
(set-fontset-font "fontset-default" '(#x1100 . #xffdc) ;; 유니코드 한글 영역 1001234560798
		  (font-spec :family "D2CodingLigature Nerd Font" :pixelsize 15 :registry "iso10646-1"))
		  ;; '("D2CodingLigature Nerd Font" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e) ;; 유니코드 사용자 영역
		  '("D2Coding" . "iso10646-1"))

(provide 'fonts)

;; | 1564 |
;; | 한글 | 이것은 한글 입력 예시Is Korean Tall입니다er than English. |
;; | abcd | this is sample text input yes|




