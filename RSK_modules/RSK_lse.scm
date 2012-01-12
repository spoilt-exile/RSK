;LSE v1.0r2 RSK
;
;LSE is a part of RSK (RSS Script Kit)
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

;List of input variables:
;image - processed image
;layer - processed layer
;coreSize - blur size for saber's core
;sabSize - glow size
;sabColor - glow color
;sky_switch - toggle for "sky" mode
;pass - additional gauss bluring for best quality of the glow
;undo - enable/disable undo stack support (not recomended for big pictures)
(define (rsk-lse image layer coreSize sabSize sabColor sky_switch pass)

(rsk-api-check)

  ;begin of script and declaim local variables
  (let* (
	(imh (car (gimp-image-height image)))
	(imw (car (gimp-image-width image)))
	(sep-image (car (gimp-image-new imw imh 0)))
	(blade)
	(big_glow)
	(big_glow_2)
	(big_glow_3)
	(soft_blade)
	(blade_glow)
	(blade_glow_2)
	(r_blade)
	(r_over)
	(big_size)
	(off_count 4)
	)

	;begin processing
	(gimp-image-undo-group-start image)
	(gimp-image-undo-disable sep-image)
	(set! blade (car (gimp-layer-new-from-drawable layer sep-image)))
	(gimp-drawable-set-visible layer FALSE)
	(gimp-image-add-layer sep-image blade -1)
	(gimp-drawable-set-name blade "Blade_FX")
	(set! soft_blade 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gimp-image-add-layer sep-image soft_blade -1)
	(gimp-drawable-set-name soft_blade "Soft_Blade")
	(plug-in-gauss-rle2 1 sep-image soft_blade sabSize sabSize)
	(set! big_glow 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gimp-image-add-layer sep-image big_glow -1)
	(gimp-drawable-set-name big_glow "Big_Glow")
	(plug-in-colorify 1 sep-image big_glow sabColor)
	(set! blade_glow 
	  (car 
	    (gimp-layer-copy soft_blade TRUE)
	  )
	)
	(set! big_size (* 3 sabSize))
	(plug-in-gauss-rle2 1 sep-image big_glow big_size big_size)
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
	(gimp-image-add-layer sep-image big_glow_2 -1)
	(gimp-image-add-layer sep-image big_glow_3 -1)
	(gimp-drawable-set-name big_glow_2 "Big_Glow_2")
	(gimp-drawable-set-name big_glow_3 "Big_Glow_3")
	(gimp-image-add-layer sep-image blade_glow -1)
	(gimp-drawable-set-name blade_glow "Blade_Glow")
	(plug-in-colorify 1 sep-image blade_glow sabColor)
	(set! blade_glow_2 
	  (car 
	    (gimp-layer-copy blade_glow TRUE)
	  )
	)
	(gimp-image-add-layer sep-image blade_glow_2 -1)
	(gimp-drawable-set-name blade_glow_2 "Blade_Glow_2")
	(while (>= off_count 0)
	  (gimp-image-raise-layer sep-image soft_blade)
	  (gimp-image-raise-layer sep-image blade)
	  (set! off_count (- off_count 1))
	)
	(plug-in-gauss-rle2 1 sep-image blade coreSize coreSize)

	;Merge layers into one result layer
	(set! blade 
	  (car 
	    (gimp-image-merge-down sep-image soft_blade 0)
	  )
	)
	(set! blade_glow 
	  (car 
	    (gimp-image-merge-down sep-image blade_glow_2 0)
	  )
	)
	(set! big_glow_2 
	  (car 
	    (gimp-image-merge-down sep-image big_glow_3 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down sep-image big_glow_2 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down sep-image blade_glow 0)
	  )
	)
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 image big_glow (* coreSize 2) (* coreSize 2))
	)
	(set! blade (car (gimp-image-merge-visible-layers sep-image 0)))

	;End of processing
	(set! r_blade (car (gimp-layer-new-from-drawable blade image)))
	(gimp-image-add-layer image r_blade -1)
	(gimp-drawable-set-name r_blade "LSE Готовый меч")
	(gimp-layer-set-mode r_blade 4)
	(gimp-brightness-contrast r_blade 0 25)
	(set! r_over 
	  (car
	    (gimp-layer-copy r_blade TRUE)
	  )
	)
	(gimp-image-add-layer image r_over -1)
	(gimp-drawable-set-name r_over "LSE Оверлей")
	(if (= sky_switch FALSE)
	  (gimp-layer-set-mode r_over 5)
	  (begin 
	    (gimp-layer-set-mode r_over 0)
	    (gimp-layer-set-opacity r_over 23)
	  )
	)
	(gimp-image-undo-group-end image)
	(gimp-image-delete sep-image)

	;Refresh display
	(gimp-displays-flush)
  )
)

(script-fu-register
"rsk-lse"
"LSE"
"Создание светового меча из слоя с силуэтом лезвия"
"Непочатов Станислав"
"GNU GPLv3"
"7 Октября 2010"
"*"
SF-IMAGE	"Изображение"				0
SF-DRAWABLE	"Слой"					0
SF-ADJUSTMENT	"Размер ядра свечения"			'(4 1 120 1 3 1 0)
SF-ADJUSTMENT	"Размер внешнего свечения"		'(15 0 450 10 30 1 0)
SF-COLOR	"Цвет свечения"				'(30 78 255)
SF-TOGGLE	"Режим белого наложения"		FALSE
SF-TOGGLE	"Улучшенное качество"			FALSE
)

(script-fu-menu-register
"rsk-lse"
_"<Image>/Filters/RSK R1"
)