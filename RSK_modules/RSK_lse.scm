;LSE v2.0 RSK (+ GEL support)
;
;==============================================================================================
;                  Данный код распространяется на условиях лицензии RSDLv1
;==============================================================================================
;
;LSE (Light Saber Effect) is a script for creating lightsaber effect
;Version history:
;==================================================================
;ver. 0.3 (September 2009)
; - working script without layer merging;
;==================================================================
;ver. 0.6 (October 2009)
; - add layers merging and achieved final result;
;==================================================================
;ver. 0.8 (November 2009)
; - sabsize variable extended to 350 (for very big photos);
;==================================================================
;ver. 0.9 (November 2009)
; - first public release;
;==================================================================
;ver. 1.0 (December 2009)
; - "sun" mode;
; - script rebuild and optimization;
; - better glow quality by addition gauss blur;
; - support of undo/rebo;
;==================================================================
;ver. 1.0r1 (February 2010)
; - optioanal undo/rebo support.
; - bugfix for final layers merging.
;==================================================================
;ver. 1.0r2 (September 30 2010)
; - separate image processing and complete undo suppor.
;==================================================================
;ver. 2.0 RSK (April 18 2011)
; - faster core (experimental);
; - separate color for saber core;
; - color presets;
;==================================================================

(define rsk-lse-presets
  '(
    "Цвета пользователя"
    "Цвет ядра => Цвет свечения"
    "Цвет свечения => Цвет ядра"
    "Классический синий"
    "Классический зеленый"
    "Классический красный"
    "Новый синий"
    "Новый зеленый"
    "Новый красный"
    "Фиолетовый"
    "Оранжевый"
    "Серебрянный"
    "Изумрудный"
  )
)

;List of input variables:
;image - processed image;
;layer - processed layer;
;core_arb - choose LSE core;
;color_preset - choose color preset for sabers;
;coreSize - blur size for saber's core;
;sabSize - glow size;
;sabColor - glow color;
;sky_switch - toggle for "sky" mode;
(define (rsk-lse-main image layer core_arb color_preset coreSize sabSize coreColor sabColor sky_switch)

  (rsk-begin-handle)

  (if (= (car (gimp-drawable-has-alpha layer)) FALSE)
    (begin
      (gimp-message "Выбранный слой не имеет альфа-канала\nВыберите другой.")
      (rsk-quit-handle)
    )
  )

  (let* (
	(imh (car (gimp-image-height image)))
	(imw (car (gimp-image-width image)))
	(sep-image (car (gimp-image-new imw imh 0)))
	(r_blade)
	(r_over)
	)

	(gimp-image-undo-group-start image)
	(gimp-image-undo-disable sep-image)

	;Color processing
	(cond
	  (
	    (= color_preset 1)
	    (set! sabColor coreColor)
	  )
	  (
	    (= color_preset 2)
	    (set! coreColor sabColor)
	  )
	  (
	    (= color_preset 3) 
	    (begin
	      (set! coreColor '(42 170 255))
	      (set! sabColor '(42 156 255))
	    )
	  )
	  (
	    (= color_preset 4) 
	    (begin
	      (set! coreColor '(30 255 252))
	      (set! sabColor '(30 255 35))
	    )
	  )
	  (
	    (= color_preset 5) 
	    (begin
	      (set! coreColor '(255 41 102))
	      (set! sabColor '(255 41 41))
	    )
	  )
	  (
	    (= color_preset 6) 
	    (begin
	      (set! coreColor '(30 173 255))
	      (set! sabColor '(30 120 255))
	    )
	  )
	  (
	    (= color_preset 7) 
	    (begin
	      (set! coreColor '(152 255 30))
	      (set! sabColor '(41 255 30))
	    )
	  )
	  (
	    (= color_preset 8) 
	    (begin
	      (set! coreColor '(255 95 70))
	      (set! sabColor '(255 30 30))
	    )
	  )
	  (
	    (= color_preset 9) 
	    (begin
	      (set! coreColor '(210 30 255))
	      (set! sabColor '(184 30 255))
	    )
	  )
	  (
	    (= color_preset 10) 
	    (begin
	      (set! coreColor '(255 170 35))
	      (set! sabColor '(255 144 35))
	    )
	  )
	  (
	    (= color_preset 11) 
	    (begin
	      (set! coreColor '(159 172 195))
	      (set! sabColor coreColor)
	    )
	  )
	  (
	    (= color_preset 12) 
	    (begin
	      (set! coreColor '(120 174 94))
	      (set! sabColor '(94 174 96))
	    )
	  )
	)

	(cond
	  ((= core_arb 0) (set! r_blade (rsk-lse-oldcore sep-image layer coreSize sabSize coreColor sabColor TRUE)))
	  ((= core_arb 1) (set! r_blade (rsk-lse-newcore sep-image layer coreSize sabSize coreColor sabColor TRUE FALSE)))
	  ((= core_arb 2) (set! r_blade (rsk-lse-newcore sep-image layer coreSize sabSize coreColor sabColor TRUE TRUE)))
	)

	(set! r_blade (car (gimp-layer-new-from-drawable r_blade image)))
	(gel-image-insert-layer image r_blade -1)
	(gel-item-set-name r_blade "LSE Готовый меч")
	(gimp-layer-set-mode r_blade 4)
	(gimp-brightness-contrast r_blade 0 25)
	(set! r_over 
	  (car
	    (gimp-layer-copy r_blade TRUE)
	  )
	)
	(gel-image-insert-layer image r_over -1)
	(gel-item-set-name r_over "LSE Оверлей")
	(cond
	  (
	    (= core_arb 0)
	    (if (= sky_switch TRUE)
	      (begin 
		(gimp-layer-set-mode r_over 0)
		(gimp-layer-set-opacity r_over 23)
	      )
	      (gimp-layer-set-mode r_over 5)
	    )
	  )
	  (
	    (= core_arb 1)
	    (begin 
	      (gimp-layer-set-mode r_over 7)
	      (plug-in-gauss-rle2 1 image r_over (/ imw 60) (/ imw 60))
	    )
	  )
	  (
	    (= core_arb 2)
	    (begin 
	      (gimp-layer-set-mode r_over 13)
	      (plug-in-gauss-rle2 1 image r_over (/ imw 60) (/ imw 60))
	    )
	  )
	)
	(gimp-image-undo-group-end image)
	(gimp-image-delete sep-image)

	;Refresh display
	(gimp-displays-flush)
  )

  (rsk-end-handle
    (string-append
    "LSE 2.0 RSK"
    (cond
      ((= core_arb 0) "\nОбработано старым ядром")
      ((= core_arb 1) "\nОбработано новым ядром (обычный режим)")
      ((= core_arb 2) "\nОбработано новым ядром (альтернативный режим)")
    )
    (if (= sky_switch TRUE) (if (= core_arb 0) "\nРежим белого наложения применен" "\nРежим белого наложения проигнорирован") "")
    "\nЦветовой профиль: " (list-ref rsk-lse-presets color_preset)
    (if (= color_preset 0)
      (string-append
      "\nЦвет ядра: '("
      (number->string (car coreColor)) " "
      (number->string (cadr coreColor)) " "
      (number->string (caddr coreColor)) ")"
      "\nЦвет свечения: '("
      (number->string (car sabColor)) " "
      (number->string (cadr sabColor)) " "
      (number->string (caddr sabColor)) ")"
      )
      ""
    )
    )
  )
)

(script-fu-register
"rsk-lse-main"
(string-append rsk-reg-defpath "Световой меч")
"Создание светового меча из слоя с силуэтом лезвия"
rsk-reg-author
(string-append rsk-reg-copyright rsk-reg-license)
rsk-reg-date
"*"
SF-IMAGE	"Изображение"				0
SF-DRAWABLE	"Слой"					0
SF-OPTION	"Режим LSE (экспериментально)"		'(
							"Старое ядро"
							"Новое ядро (обычный режим)"
							"Новое ядро (альтернативный режим)"
							)
SF-OPTION	"Установить цветовой комплект"		rsk-lse-presets
SF-ADJUSTMENT	"Размер ядра свечения"			'(4 1 120 1 3 1 0)
SF-ADJUSTMENT	"Размер внешнего свечения"		'(15 0 450 10 30 1 0)
SF-COLOR	"Цвет ядра"				'(80 150 255)
SF-COLOR	"Цвет свечения"				'(30 78 255)
SF-TOGGLE	"Режим белого наложения"		FALSE
)

(define (rsk-lse-oldcore input_image layer coreSize sabSize coreColor sabColor pass)

  ;begin of script and declaim local variables
  (let* (
	(off_count 4)
	(blade)
	(big_glow)
	(big_glow_2)
	(big_glow_3)
	(soft_blade)
	(blade_glow)
	(blade_glow_2)
	(big_size)
	)

	;begin processing

	(set! blade (car (gimp-layer-new-from-drawable layer input_image)))
	(gel-item-set-visible layer FALSE)
	(gel-image-insert-layer input_image blade -1)
	(gel-item-set-name blade "Blade_FX")
	(set! soft_blade 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gel-image-insert-layer input_image soft_blade -1)
	(gel-item-set-name soft_blade "Soft_Blade")
	(plug-in-gauss-rle2 1 input_image soft_blade sabSize sabSize)
	(set! big_glow 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gel-image-insert-layer input_image big_glow -1)
	(gel-item-set-name big_glow "Big_Glow")
	(plug-in-colorify 1 input_image big_glow sabColor)
	(set! blade_glow 
	  (car 
	    (gimp-layer-copy soft_blade TRUE)
	  )
	)
	(set! big_size (* 3 sabSize))
	(plug-in-gauss-rle2 1 input_image big_glow big_size big_size)
	(set! big_glow_2 
	  (car 
	    (gimp-layer-copy big_glow TRUE)
	  )
	)
	(set! big_glow_3 
	  (car 
	    (gimp-layer-copy big_glow TRUE)
	  )
	)
	(gel-image-insert-layer input_image big_glow_2 -1)
	(gel-image-insert-layer input_image big_glow_3 -1)
	(gel-item-set-name big_glow_2 "Big_Glow_2")
	(gel-item-set-name big_glow_3 "Big_Glow_3")
	(gel-image-insert-layer input_image blade_glow -1)
	(gel-item-set-name blade_glow "Blade_Glow")
	(plug-in-colorify 1 input_image blade_glow coreColor)
	(set! blade_glow_2 
	  (car 
	    (gimp-layer-copy blade_glow TRUE)
	  )
	)
	(gel-image-insert-layer input_image blade_glow_2 -1)
	(gel-item-set-name blade_glow_2 "Blade_Glow_2")
	(while (>= off_count 0)
	  (gel-image-raise-item input_image soft_blade)
	  (gel-image-raise-item input_image blade)
	  (set! off_count (- off_count 1))
	)
	(plug-in-gauss-rle2 1 input_image blade coreSize coreSize)

	;Merge layers into one result layer
	(set! blade 
	  (car 
	    (gimp-image-merge-down input_image soft_blade 0)
	  )
	)
	(set! blade_glow 
	  (car 
	    (gimp-image-merge-down input_image blade_glow_2 0)
	  )
	)
	(set! big_glow_2 
	  (car 
	    (gimp-image-merge-down input_image big_glow_3 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down input_image big_glow_2 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down input_image blade_glow 0)
	  )
	)
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 input_image big_glow (* coreSize 2) (* coreSize 2))
	)
	(set! blade (car (gimp-image-merge-visible-layers input_image 0)))
	
	;Returning layer
	blade
  )
)

(define (rsk-lse-newcore input_image layer coreSize sabSize coreColor sabColor pass core_mode)
  (let* (
	(input_fore (car (gimp-context-get-foreground)))
	(input_back (car (gimp-context-get-background)))
	(imh (car (gimp-image-height input_image)))
	(imw (car (gimp-image-width input_image)))
	(orig_layer (car (gimp-layer-new-from-drawable layer input_image)))
	(blade)
	(big_glow)
	(soft_blade)
	(blade_glow)
	(blade_buffer)
	)

	(if (= core_mode TRUE)
	  (gimp-context-set-background '(255 255 255))
	  (gimp-context-set-background '(0 0 0))
	)
	(gel-item-set-visible layer FALSE)

	(gel-image-insert-layer input_image orig_layer -1)
	(gel-image-select-item input_image 0 orig_layer)
	(set! blade_buffer (car (gimp-selection-save input_image)))
	(gimp-image-remove-layer input_image orig_layer)
	(set! blade (car (gimp-layer-new input_image imw imh 1 "Blade_FX" 100 0)))
	(gel-image-insert-layer input_image blade -1)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-selection-feather input_image coreSize)
	(gimp-edit-fill blade 3)
	(gimp-edit-fill blade 0)
	(gimp-selection-none input_image)
	(gel-image-select-item input_image 0 blade_buffer)
	(set! soft_blade (car (gimp-layer-new input_image imw imh 1 "Soft_Blade" 100 0)))
	(gel-image-insert-layer input_image soft_blade -1)
	(gimp-selection-feather input_image sabSize)
	(gimp-edit-fill soft_blade 3)
	(gimp-edit-fill soft_blade 0)
	(gimp-selection-none input_image)
	(set! big_glow (car (gimp-layer-new input_image imw imh 1 "Big_Glow" 100 0)))
	(gel-image-insert-layer input_image big_glow -1)
	(gel-image-select-item input_image 0 blade_buffer)
	(gimp-selection-feather input_image (* sabSize 3))
	(gimp-context-set-foreground sabColor)
	(gimp-edit-fill big_glow 3)
	(gimp-edit-fill big_glow 0)
	(set! blade_glow (car (gimp-layer-new input_image imw imh 1 "Blade_Glow" 100 0)))
	(gel-image-insert-layer input_image blade_glow -1)
	(gimp-selection-none input_image)
	(gel-image-select-item input_image 0 blade_buffer)
	(gimp-selection-feather input_image sabSize)
	(gimp-context-set-foreground coreColor)
	(gimp-edit-fill blade_glow 3)
	(gimp-edit-fill blade_glow 0)
	(gimp-selection-none input_image)
	(gel-item-set-visible blade FALSE)
	(gel-item-set-visible soft_blade FALSE)

	(set! big_glow (car (gimp-image-merge-visible-layers input_image 0)))
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 input_image big_glow (* coreSize 2) (* coreSize 2))
	)

	(gel-item-set-visible blade TRUE)
	(gel-item-set-visible soft_blade TRUE)
	(gimp-layer-set-opacity soft_blade 15)
	(gel-image-raise-item input_image soft_blade)
	(gel-image-raise-item input_image blade)
	(set! blade (car (gimp-image-merge-down input_image blade 0)))
	(set! blade (car (gimp-image-merge-down input_image soft_blade 0)))
	(gimp-context-set-background input_back)
	(gimp-context-set-foreground input_fore)

	;Returning layer
	blade
  )
)