;starsky v0.5r1 RSK (+ GEL support)
;starsky-gen bases on script-fu-stars-in-the-sky
;
;==============================================================================================
;          Данный код распространяется на условиях лицензии Creative Commons BY-SA
;==============================================================================================
;
;
;===============================================================================================================
;Version 1.0 (initial version)
;October 2010
;Creates a new layer sparkling stars.
;This script is under CC-BY-SA license, you can find it here:
;http://creativecommons.org/licenses/by-sa/2.0/fr/deed.en
;Copyleft 2010 Harfangdesneiges
;===============================================================================================================
;RSS Edition Header
;===============================================================================================================
;Version history:
;===============================================================================================================
;ver. 0.1 (December 18 2010)
; - RSS initail release;
;===============================================================================================================
;ver. 0.5 (February 13 2011)
; - improved color control;
; - stable release;
;===============================================================================================================

;Main function
(define (rsk-starsky img new_switch aspect width height tone_switch tone_color tone_opc color_switch color_intensity spikes-lenght flare-intensity)

  (rsk-begin-handle)

  (cond
    ((= aspect 1) (set! height width))
    ((= aspect 2) (set! height (/ width 1.33)))
    ((= aspect 3) (set! height (/ width 1.77)))
    ((= aspect 4) (set! height (/ width 2.22)))
  )

  (if (= new_switch TRUE)
    (set! img (car (gimp-image-new width height RGB)))
    (begin
      (set! height (car (gimp-image-height img)))
      (set! width (car (gimp-image-width img)))
    )
  )

  (let* (
	(layer (car (gimp-layer-new img width height 0 "Текстура" 100 NORMAL-MODE)))
	(background-color (gimp-context-get-background))
	(tone_layer)
	(color_layer)
	)
	(gimp-image-undo-disable img)
	(gel-image-insert-layer img layer 0)
	(gimp-context-set-background '(0 0 0) )
	(gimp-drawable-fill layer BACKGROUND-FILL)
	(plug-in-hsv-noise 1 img layer 4 32 10 194)
	(gimp-brightness-contrast layer -48 42)
	(plug-in-sparkle 1 img layer 0.009 flare-intensity spikes-lenght 4 15 1 0 0 0 FALSE FALSE FALSE 0)
	(if (= color_switch TRUE)
	  (begin
	    (set! color_layer (car (gimp-layer-new img width height 0 "Шум RGB" 100 NORMAL-MODE)))
	    (gel-image-insert-layer img color_layer -1)
	    (plug-in-rgb-noise 1 img color_layer TRUE FALSE 0.2 0.2 0.2 0)
	    (gimp-layer-set-mode color_layer 13)
	    (gimp-layer-set-opacity color_layer color_intensity)
	    (gimp-hue-saturation color_layer 0 0 0 100)
	    (plug-in-gauss-rle2 1 img color_layer (/ width 250) (/ width 250))
	    (set! layer (car (gimp-image-merge-down img color_layer 0)))
	  )
	)
	(if (and (= tone_switch TRUE) (> tone_opc 0))
	  (begin
	    (set! tone_layer (car (gimp-layer-copy layer FALSE)))
	    (gel-image-insert-layer img tone_layer -1)
	    (plug-in-colorify 1 img tone_layer tone_color)
	    (gimp-layer-set-mode tone_layer 13)
	    (gimp-layer-set-opacity tone_layer tone_opc)
	    (set! layer (car (gimp-image-merge-down img tone_layer 0)))
	  )
	)
	(gimp-context-set-background (car background-color))
	(gimp-image-undo-enable img)
	(if (= new_switch TRUE)
	  (gimp-display-new img)
	  (gimp-displays-flush)
	)
  )

  (rsk-end-handle
    (string-append
    "Звездное небо 0.5 RSK"
    "\nРазмеры текстуры: " (number->string width) "x" (number->string height) "px"
    (if (= tone_switch TRUE) "\nТонировка включена" "")
    (if (= color_switch TRUE) "\nНаложение цветов включено" "")
    )
  )

)

;Registration function
(script-fu-register
"rsk-starsky"
(string-append rsk-reg-defpath "Звездное небо")
"Создание текстуры звездного неба."
(string-append "Harfangdesneiges\n" rsk-reg-author)
"Harfangdesneiges"
rsk-reg-date
""
SF-IMAGE	"Изображение"		0
SF-TOGGLE	"Новое изображение"	TRUE
SF-OPTION	"Соотношение сторон"	'("Определено пользователем" "1:1" "4:3" "16:9" "22:10")
SF-VALUE	"Ширина текстуры"	"1280"
SF-VALUE	"Высота текстуры"	"1280"
SF-TOGGLE	"Тонировка"		FALSE
SF-COLOR	"Цвет тонировки"	'(168 229 255)
SF-ADJUSTMENT	"Плотность тонирования"	'(100 0 100 5 15 1 0)
SF-TOGGLE	"Разноцветные звезды"	FALSE
SF-ADJUSTMENT	"Интенсивность цвета"	'(15 0 100 5 15 1 0)
SF-VALUE	"Размер искр"		"25"
SF-ADJUSTMENT	"Видимость искр"	'(0.2 0.0 1.0 0.1 0.3 1 0)
)