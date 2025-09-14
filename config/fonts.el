;; config about fonts
(defvar dobin/default-font (font-spec 
			    ;; :family "JetBrainsMono Nerd Font"
			    :family "NanumGothicCoding"
			    :height 1
			    ))

(defvar dobin/kor-font (font-spec
			:family "D2CodingLigature Nerd Font"
			;; :family "D2Coding"
			:registry "unicode-bmp"
			;; :width 'wide
			:size 20
			))

(set-face-attribute 'default nil 
		    :height 150  ;; 베이스가 기본 되는 크기 설정 
		    :family "JetBrainsMono Nerd Font"
		    ;; :family "D2Coding"
		    )
(set-fontset-font "fontset-default" '(#xac00 . #xd7a3) dobin/kor-font)
		  ;; dobin/kor-font)

(set-fontset-font "fontset-default" '(#x1100 . #xffdc) ;; 유니코드 한글 영역 1001234560798
		  ;; (font-spec :family "NanumGothicCoding")
		  dobin/kor-font
		  )

(add-to-list 'face-font-rescale-alist '(".*D2Coding.*" . 1.23))
(add-to-list 'face-font-rescale-alist '(".*NanumGothic.*" . 1.25))

(setq-default line-spacing 4)

(provide 'fonts)
;; | 1564 |
;; | 한글 | 이것은 한글 입력 예시Is Korean Tall입니다er than English. |
;; | abcd | this is sample text input yes|

;; 01234567890123456789012345678901234567890
;; The quick brown fox jumps over a lazy dog.
;; 다람쥐 헌 쳇바퀴에 타고파

;; All questions asked by five watch experts amazed the judge.
;; 키스의 고유 조건은 입술끼리 만나야 하고 특별한 요령은 필요치 않다.

;; SIMILAR = "oO08ㅇ iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="


;;(add-to-list 'default-frame-alist '(font . "D2CodingLigature Nerd Font"))

;; (set-fontset-font nil 'hangul (font-spec :family "D2Coding" :pixelsize 12))
;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
		  ;; (font-spec :family "D2Coding") )
		  ;; '("D2CodingLigature Nerd Font" . "iso10646-1"))
		  ;; '("JetBrainsMono Nerd Font" . "iso10646-1"))

;; (setq face-font-rescale-alist '((".*D2Coding.*" . 1.24)))






