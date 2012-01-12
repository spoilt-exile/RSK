;glare v0.5 RSK (+ GEL support)
;
;==============================================================================================
;                  Данный код распространяется на условиях лицензии Free
;==============================================================================================
;
;Glare effect script.
;
;Version history:
;==================================================================
;ver. 0.3 (March 7th 2011)
; - initial release;
;==================================================================
;ver. 0.5 (March 9th 2011)
; - vertical glare;
; - glare optical axis flip;
; - additional glare amplification;
;==================================================================

(define (rsk-glarefx-core image layer glSize glFact glEdge glVert glFlip glColor glOverColor srcswitch viz)

  (rsk-begin-handle)

  (gimp-image-undo-group-start image)

  (if (= viz TRUE)
    (begin
      (let* (
	    (source (car (gimp-image-get-active-layer image)))
	    (visible)
	    )
	    (gimp-edit-copy-visible image)
	    (set! visible
	      (car
		(gimp-edit-paste source TRUE)
	      )
	    )
	    (gimp-floating-sel-to-layer visible)
	    (gel-item-set-name visible "Viz-src")
	    (gel-image-raise-item-to-top image visible)
	    (gimp-image-set-active-layer image visible)
      )
    )
  )

  (let* (
	(src (car (gimp-image-get-active-layer image)))
	(glayer (car (gimp-layer-copy src FALSE)))
	(imh (car (gimp-image-height image)))
	(imw (car (gimp-image-width image)))
	(clayer)
	(fore (car (gimp-context-get-foreground)))
	(mlayer)
	(reslayer)
	)

	(gel-image-insert-layer image glayer -2)
	(gel-item-set-name glayer "Glow")
	(if (> glOverColor 0)
	  (begin
	    (set! clayer (car (gimp-layer-new image imw imh 1 "Color overlay" glOverColor 0)))
	    (gel-image-insert-layer image clayer -1)
	    (gimp-context-set-foreground glColor)
	    (gimp-edit-fill clayer 0)
	    (gimp-layer-set-mode clayer 13)
	    (set! glayer (car (gimp-image-merge-down image clayer 0)))
	    (gimp-context-set-foreground fore)
	  )
	)
	(gimp-levels glayer 0 glEdge 255 1.0 0 255)
	(if (= glVert FALSE)
	  (plug-in-gauss-iir2 1 image glayer (* glSize (/ imw 100)) (* glFact (/ imh 100)))
	  (plug-in-gauss-iir2 1 image glayer (* glFact (/ imw 100)) (* glSize (/ imh 100)))
	)
	(gimp-levels glayer 0 0 (- 255 (/ glEdge 2)) 1.0 0 255)
	(if (= glFlip TRUE)
	  (begin
	    (gel-item-transform-flip-simple glayer 0 TRUE 0 TRUE)
	    (gel-item-transform-flip-simple glayer 1 TRUE 0 TRUE)
	  )
	)
	(gimp-layer-set-mode glayer 4)
	(set! mlayer (car (gimp-layer-copy glayer FALSE)))
	(gel-item-set-name mlayer "Hard light")
	(gel-image-insert-layer image mlayer -3)
	(gimp-layer-set-mode mlayer 7)
	(gimp-layer-set-opacity mlayer 45)

	(if (= srcswitch FALSE)
	  (begin
	    (gimp-edit-copy-visible image)
	    (set! reslayer
	      (car
		(gimp-edit-paste glayer TRUE)
	      )
	    )
	    (gimp-floating-sel-to-layer reslayer)
	    (gel-item-set-name reslayer "Result")
	    (gimp-image-remove-layer image glayer)
	    (gimp-image-remove-layer image mlayer)
	  )
	)

	(if (= viz TRUE)
	  (gimp-image-remove-layer image src)
	)
	(gimp-image-undo-group-end image)
	(gimp-displays-flush)
  )

  (rsk-end-handle
    (string-append
    "Блеск 0.5"
    (if (= glFlip TRUE) "\nОтблески отражены" "\nОтблески не отражены")
    (if (= srcswitch TRUE) "\nПромежуточный слои сохраненны" "\nСлои совмещены")
    (if (= viz TRUE) "\nИсточник = Видимое" "\nИсточник = Копия")
    )
  )

)

(script-fu-register
"rsk-glarefx-core"
(string-append rsk-reg-defpath "Бле_ск")
"Создание оптического эффекта блеска"
rsk-reg-author
(string-append rsk-reg-copyright rsk-reg-license)
rsk-reg-date
"*"
SF-IMAGE	"Input image"			0
SF-DRAWABLE	"Input layer"			0
SF-ADJUSTMENT	"Величина свечения %"		'(50 0 100 10 10 1 0)
SF-ADJUSTMENT	"Вторичное размытие %"		'(5 0 10 1 2 1 0)
SF-ADJUSTMENT	"Граница свечения"		'(0 0 255 15 45 1 0)
SF-TOGGLE	"Вертикальное свечение"		FALSE
SF-TOGGLE	"Оптическое отражение"		FALSE
SF-COLOR	"Цвет свечегия"			'(134 164 225)
SF-ADJUSTMENT	"Степень тонировки свечения"	'(45 0 100 10 10 1 0)
SF-TOGGLE	"Сохранить исходные слои"	FALSE
SF-TOGGLE	"Работать с видимым"		FALSE
)