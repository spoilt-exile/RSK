;Photostamp v0.5 RSK
;
;Photostamp is a part of RSK (RSS Script Kit)
;
;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;http://www.gnu.org/licenses/gpl-3.0.html
;
;Version history:
;===============================================================================================================
;ver. 0.1 (September 2 2010)
; - original script reconstruuction;
;===============================================================================================================
;ver. 0.2 (September 4 2010)
; - double marking;
; - new marking algorythm;
; - true invisible marking;
; - reveal procedure;
;===============================================================================================================
;ver. 0.4 (September 25 2010)
; - script was renamed in "Photostamp";
; - stamp processing;
; - global pathes;
;===============================================================================================================
;ver. 0.5 (September 29 2010)
; - stamp control improvement;
;===============================================================================================================

;Reveal call for external calling
(define reveal-call FALSE)

;Path variables
(define g-mark-path (string-append gimp-directory rsk-sys-sep "patterns" rsk-sys-sep "rsk-watermark.pat"))
(define g-stamp-path (string-append gimp-directory rsk-sys-sep "patterns" rsk-sys-sep "rsk-stamp.pat"))

;ps-create-mark
;Input variables:
;STRING - mark text;
;STRING - font name for marking;
;INTEGER - font size for marking;
;BOOLEAN - switch for showing final pattern;
(define (ps-create-wmark text font size show_switch)
(rsk-api-check)
  (let* (
	(image (car (gimp-image-new 256 256 RGB)))
	(old_fore (car (gimp-context-get-foreground)))
	(text_layer 0)
	)

	(gimp-context-set-foreground '(255 255 255))
	(set! text_layer (car (gimp-text-fontname image -1 0 0 text 5 TRUE size PIXELS font)))
	(script-fu-util-image-resize-from-layer image text_layer)
	(file-pat-save RUN-NONINTERACTIVE image text_layer g-mark-path g-mark-path "RSK-Watermark")
	(gimp-patterns-refresh)
	(gimp-context-set-foreground old_fore)
	(if (= show_switch FALSE)
	  (gimp-image-delete image)
	  (gimp-display-new image)
	)
  )
)

;ps-create-mask regestring
(script-fu-register 
"ps-create-wmark"
_"<Image>/Filters/RSK R1/Фотоштамп Создать _метку"
"Создать невидимую метку"
"Гурейси Монтейро <guaracy.bm@gmail.com>\nНепочатов Станислав <spoilt_exile[at]mail.ru>"
"Гурейси Монтейро"
"7 Октября 2010"
""
SF-STRING		"Текст метки"			"Моя метка"
SF-FONT			"Шрифт"				"Arial Bold"
SF-ADJUSTMENT		"Кегль (в пикселях)"		'(18 2 200 1 10 0 1)
SF-TOGGLE		"Просмотреть метку"		FALSE
)

;ps-create-stamp
;Input variables:
;INTEGER - stamp width;
;INTEGER - stamp height;
;INTEGER - aspect preset number;
(define (ps-create-stamp imw imh aspect)
(rsk-api-check)
(define image)
(define layer)
  (if (> aspect 0)
    (cond
      ((= aspect 1) (set! imh (/ imw (/ 4 3))))
      ((= aspect 2) (set! imh (/ imw (/ 16 9))))
      ((= aspect 3) (set! imh (/ imw (/ 16 10))))
    )
  )
  (set! image (car (gimp-image-new imw imh 0)))
  (set! layer (car (gimp-layer-new image imw imh 1 "layer" 100 NORMAL)))
  (gimp-image-add-layer image layer -1)
  (gimp-display-new image)
)

;ps-create-stamp regestring
(script-fu-register 
"ps-create-stamp"
_"<Image>/Filters/RSK R1/Фотоштамп Создать _штамп"
"Создание штампа"
"Непочатов Станислав <spoilt_exile[at]mail.ru>"
"Непочатов Станислав"
"7 Октября 2010"
""
SF-VALUE		"Ширина"			"1280"
SF-VALUE		"Высота"			"720"
SF-OPTION		"Установить соотношение сторон"	'("Выключено" "4:3" "16:9" "16:10")
)

;ps-save-stamp
;Input variables:
;IMAGE - image to save;
;LAYER - layer to save;
(define (ps-save-stamp image layer)
(rsk-api-check)
  (set! layer (car (gimp-image-merge-visible-layers image 0)))
  (file-pat-save RUN-NONINTERACTIVE image layer g-stamp-path g-stamp-path "RSK-Stamp")
  (gimp-patterns-refresh)
)

;ps-save-stamp regestring
(script-fu-register 
"ps-save-stamp"
_"<Image>/Filters/RSK R1/Фотоштамп Со_хранить штамп"
"Сохранение штампа для его дальнейшего использования"
"Непочатов Станислав <spoilt_exile[at]mail.ru>"
"Непочатов Станислав"
"7 Октября 2010"
"RGBA"
SF-IMAGE		"Изображение"			0
SF-DRAWABLE		"Слой"				0
)

;ps-apply-mark
;Input variables:
;IMAGE - process image;
;LAYER - process layer;
;BOOLEAN - stamp activation switch;
;INTEGER - stamp size in percents;
;INTEGER - stamp align mode;
;INTEGER - stamp combination mode;
;INTEGER - stamp opacity;
;BOOLEAN - invisible mark switch;
;BOOLEAN - double mark switch;
(define (ps-apply-mark image layer st_switch st_percent st_off st_align st_mode st_opc imark_switch dmark_switch)
(rsk-api-check)
  (let* (
	(old_pattern (car (gimp-context-get-pattern)))
	(mark_layer)
	(copy_layer (car (gimp-layer-copy layer FALSE)))
	(copy_mask)
	(invmark_layer)
	(invmark_mask)
	(width (car (gimp-image-width image)))
	(height (car (gimp-image-height image)))
	(st_layer)
	(low_percent (if (> width height) (/ height 100) (/ width 100)))
	(st_w)
	(st_h)
	(st_aspect)
	(st_size (* st_percent low_percent))
	(st_x)
	(st_y)
	)

	(gimp-context-push)
	(gimp-image-undo-group-start image)
	(gimp-image-add-layer image copy_layer -1)
	(if (= st_switch TRUE)
	  (if (= 1 (car (gimp-patterns-get-list "RSK-Stamp")))
	    (begin
	      (set! st_layer (car (gimp-file-load-layer 1 image g-stamp-path)))
	      (gimp-image-add-layer image st_layer -1)
	      (set! st_w (car (gimp-drawable-width st_layer)))
	      (set! st_h (car (gimp-drawable-height st_layer)))
	      (set! st_aspect (/ st_w st_h))
	      (if (> st_w st_size)
		(gimp-layer-scale-full st_layer st_size (/ st_size st_aspect) 1 3)
	      )

	      (set! st_w (car (gimp-drawable-width st_layer)))
	      (set! st_h (car (gimp-drawable-height st_layer)))

	      (cond

		;Low-right corner rule
		(
		  (= st_align 0)
		  (begin
		    (set! st_x (- width (+ st_w (* st_off low_percent))))
		    (set! st_y (- height (+ st_h (* st_off low_percent))))
		  )
		)

		;Lower-left corner rule
		(
		  (= st_align 1)
		  (begin
		    (set! st_x (+ 0 (* st_off low_percent)))
		    (set! st_y (- height (+ st_h (* st_off low_percent))))
		  )
		)

		;Upper-right corner rule
		(
		  (= st_align 2)
		  (begin
		    (set! st_x (- width (+ st_w (* st_off low_percent))))
		    (set! st_y (+ 0 (* st_off low_percent)))
		  )
		)

		;Upper-left corner rule
		(
		  (= st_align 3)
		  (begin
		    (set! st_x (+ 0 (* st_off low_percent)))
		    (set! st_y (+ 0 (* st_off low_percent)))
		  )
		)
	      )
	      (gimp-layer-set-offsets st_layer st_x st_y)
	      (cond
		((= st_mode 1) (gimp-layer-set-mode st_layer 4))
		((= st_mode 2) (gimp-layer-set-mode st_layer 7))
		((= st_mode 3) (gimp-layer-set-mode st_layer 6))
	      )
	      (gimp-layer-set-opacity st_layer st_opc)
	      (set! copy_layer (car (gimp-image-merge-down image st_layer 0)))
	      (gimp-drawable-set-name copy_layer "Слой со штампом")
	    )
	    (gimp-message "Текстура штампа не найдена.\nCоздайте необходимую текстуру:\n [Filters/RSK R1/Фотоштамп Создать штамп]\n")
	  )
	)
	(if (= imark_switch TRUE)
	  (if (= 1 (car (gimp-patterns-get-list "RSK-Watermark")))
	    (begin
	      (set! copy_mask (car (gimp-layer-create-mask copy_layer 5)))
	      (set! mark_layer (car (gimp-layer-new image width height RGBA-IMAGE "Watermark" 3 NORMAL-MODE)))
	      (gimp-image-add-layer image mark_layer -1)
	      (gimp-context-set-pattern "PS-Watermark")
	      (gimp-displays-flush image)
	      (gimp-selection-all image)
	      (gimp-edit-fill mark_layer PATTERN-FILL)
	      (gimp-layer-add-mask mark_layer copy_mask)
	      (if (= dmark_switch TRUE)
		(begin
		  (set! invmark_layer (car (gimp-layer-copy mark_layer TRUE)))
		  (gimp-image-add-layer image invmark_layer -1)
		  (gimp-invert invmark_layer)
		  (set! invmark_mask (car (gimp-layer-get-mask invmark_layer)))
		  (gimp-invert invmark_mask)
		)
	      )
	      (set! copy_layer (car (gimp-image-merge-down image mark_layer 0)))
	      (if (= dmark_switch TRUE)
		(set! copy_layer (car (gimp-image-merge-down image invmark_layer 0)))
	      )
	      (gimp-drawable-set-name copy_layer "Защищенный слой")
	      (gimp-selection-clear image)
	      (gimp-context-set-pattern old_pattern)
	    )
	    (gimp-message "Текстура метки не найдена.\nCоздайте необходимую текстуру:\n [Filters/RSK R1/Фотоштамп Создать метку]\n")
	  )
	)
	(gimp-image-undo-group-end image)
	(gimp-context-pop)
	(gimp-displays-flush)
  )
)

;ps-apply-mark regestring
(script-fu-register 
"ps-apply-mark"
_"<Image>/Filters/RSK R1/Фотоштамп _Применить метки"
"Пометка изображения с помощью штампов и меток"
"Гурейси Монтейро <guaracy.bm@gmail.com>\nНепочатов Станислав <spoilt_exile[at]mail.ru>"
"Гурейси Монтейро"
"7 Октября 2010"
"RGB,RGBA*"
SF-IMAGE		"Изображение"			0
SF-DRAWABLE		"Слой"				0
SF-TOGGLE		"Отметить штампом"		TRUE
SF-ADJUSTMENT		"Размер штампа в %"		'(30 15 45 5 10 1 0)
SF-ADJUSTMENT		"Сдвиг штампа в %"		'(1.5 1 5 0.5 1 1 0)
SF-OPTION		"Выровнять штамп к"		'(
							"Нижний правый угол"
							"Нижний левый угол"
							"Верхний правый угол"
							"Верхний левый уголr"
							)
SF-OPTION		"Режим наложения штампа"	'("Нормальный" "Экран" "Добавление" "Разница")
SF-ADJUSTMENT		"Непрозрачность штампа"		'(100 0 100 10 25 1 0)
SF-TOGGLE		"Отметить невидимой меткой"	FALSE
SF-TOGGLE		"Двойная метка"			TRUE
)

;ps-reveal-mark
;Input variables:
;IMAGE - process image;
;LAYER - process layer;
;LAYER - original layer (without mark);
(define (ps-reveal-mark image layer orig_layer post)
(rsk-api-check)
  (let* (
	(copy_layer (car (gimp-layer-copy orig_layer FALSE)))
	(over_layer (car (gimp-layer-copy layer FALSE)))
	)

	(if (= reveal-call FALSE)
	  (gimp-image-undo-group-start image)
	)
	(gimp-image-add-layer image copy_layer -1)
	(gimp-image-add-layer image over_layer -1)
	(gimp-layer-set-mode over_layer 20)
	(set! over_layer (car (gimp-image-merge-down image over_layer 0)))
	(plug-in-c-astretch 1 image over_layer)
	(gimp-drawable-set-name over_layer "Проявленная метка")
	(gimp-desaturate-full over_layer 1)
	(if (= post TRUE)
	  (gimp-levels over_layer 0 25 100 1.0 0 255)
	)
	(if (= reveal-call TRUE)
	  (gimp-image-raise-layer-to-top image over_layer)
	)
	(gimp-displays-flush image)
	(if (= reveal-call FALSE)
	  (gimp-image-undo-group-end image)
	)
  )
)

;ps-reveal-mark regestring
(script-fu-register 
"ps-reveal-mark"
_"<Image>/Filters/RSK R1/Фотоштамп Про_явить метку"
"Проявить невидимую метку"
"Непочатов Станислав <spoilt_exile[at]mail.ru>"
"Непочатов Станислав"
"7 Октября 2010"
"RGB,RGBA*"
SF-IMAGE		"Изображение"			0
SF-DRAWABLE		"Слой"				0
SF-DRAWABLE		"Выбрать слой с оригиналом"	5
SF-TOGGLE		"Постобработка метки"		FALSE
)

(define (ps-reveal-image image prc_layer org_name post)
(rsk-api-check)
  (let* (
	(org_layer (car (gimp-file-load-layer 1 image org_name)))
	(org_h (car (gimp-drawable-height org_layer)))
	(org_w (car (gimp-drawable-width org_layer)))
	(prc_h (car (gimp-drawable-height prc_layer)))
	(prc_w (car (gimp-drawable-width prc_layer)))
	)

	(gimp-image-undo-group-start image)
	(if (or (= org_h prc_h) (= org_w prc_w))
	  (begin
	    (gimp-image-add-layer image org_layer -1)
	    (gimp-image-lower-layer image org_layer)
	    (set! reveal-call TRUE)
	    (ps-reveal-mark image prc_layer org_layer post)
	    (set! reveal-call FALSE)
	  )
	  (gimp-message "Размеры изображений не одинаковы. Проявление метки невозможно.")
	)
	(gimp-image-undo-group-end image)
  )
)

;ps-reveal-image regestring
(script-fu-register 
"ps-reveal-image"
_"<Image>/Filters/RSK R1/Фотоштамп Проявить и_зображение"
"Проявить невидимую метку из файла изображения"
"Непочатов Станислав <spoilt_exile[at]mail.ru>"
"Непочатов Станислав"
"7 Октября 2010"
"RGB,RGBA*"
SF-IMAGE		"Изображение"			0
SF-DRAWABLE		"Слой"				0
SF-FILENAME 		"Сравниваемое изображение" 	""
SF-TOGGLE		"Постобработка метки"		FALSE
)

(define (ps-batch-mark dir_in input_format dir_out out_format st_switch st_percent st_off st_align st_mode st_opc imark_switch dmark_switch resize_switch resize_dimension resize_value)
(rsk-api-check)
(define input-ext)
  (cond
    ((= input_format 0) (set! input-ext "*"))
    ((= input_format 1) (set! input-ext "[jJ][pP][gG]"))
    ((= input_format 2) (set! input-ext "[bB][mM][pP]"))
    ((= input_format 3) (set! input-ext "[xX][cC][fF]"))
  )

  (define out-ext)
  (cond
    ((= out_format 0) (set! out-ext "jpg"))
    ((= out_format 1) (set! out-ext "png"))
    ((= out_format 2) (set! out-ext "tif"))
    ((= out_format 3) (set! out-ext "bmp"))
  )

  (let*	(
	(pattern (string-append dir_in rsk-sys-sep "*." input-ext))
	(filelist (cadr (file-glob pattern 1)))
	(run_mode 1)
	)
	(while (not (null? filelist))
	  (let* (
		(cur_target (car filelist))
		(image (car (gimp-file-load 1 cur_target cur_target)))
		(imh (car (gimp-image-height image)))
		(imw (car (gimp-image-width image)))
		(aspect_ratio (/ imw imh))
		(rs_imh (if (= resize_dimension 0) resize_value (/ resize_value aspect_ratio)))
		(rs_imw (if (= resize_dimension 0) (* resize_value aspect_ratio) resize_value))
		(srclayer)
		(filename (car (gimp-image-get-filename image)))
		(target_out)
		(origin_out)
		(file)
		(res_layer)
		)
		(set! file (substring filename (string-length dir_in) (- (string-length filename) 4 )))
		(set! srclayer (car (gimp-image-flatten image)))
		(if (= resize_switch TRUE)
		  (begin
		    (gimp-image-scale-full image rs_imw rs_imh 2)
		    (set! origin_out (string-append dir_out "/" file "_оригинал." out-ext))
		    (cond
		      ((= out_format 0) (file-jpeg-save 1 image srclayer origin_out origin_out 1 0 1 1 "" 2 1 0 0))
		      ((= out_format 1) (file-png-save-defaults 1 image srclayer origin_out origin_out))
		      ((= out_format 2) (file-tiff-save 1 image srclayer origin_out origin_out 1))
		      ((= out_format 3) (file-bmp-save 1 image srclayer origin_out origin_out))
		    )
		  )
		)
		(ps-apply-mark image srclayer st_switch st_percent st_off st_align st_mode st_opc imark_switch dmark_switch)
		(set! res_layer (car (gimp-image-merge-visible-layers image 0)))
		(set! target_out (string-append dir_out "/" file "_защищено." out-ext))
		(cond
		  ((= out_format 0) (file-jpeg-save 1 image res_layer target_out target_out 1 0 1 1 "" 2 1 0 0))
		  ((= out_format 1) (file-png-save-defaults 1 image res_layer target_out target_out))
		  ((= out_format 2) (file-tiff-save 1 image res_layer target_out target_out 1))
		  ((= out_format 3) (file-bmp-save 1 image res_layer target_out target_out))
		)
		(gimp-image-delete image)
	  )
	  (set! filelist (cdr filelist))
	)
  )
)

;ps-batch-mark regestring
(script-fu-register 
"ps-batch-mark"
_"<Image>/Filters/RSK R1/Фотоштамп Пакетный режим"
"Пометка изображений в пакетном режиме"
"Непочатов Станислав <spoilt_exile[at]mail.ru>"
"Непочатов Станислав"
"7 Октября 2010"
""
SF-DIRNAME		"Каталог-источник"		""
SF-OPTION		"Входной формат"		'(
							"*"
							"JPG"
							"TIFF"
							"XCF"
							)
SF-DIRNAME		"Каталог-назначение"		""
SF-OPTION		"Формат сохранения"		'(
							"JPG"
							"PNG"
							"TIF"
							"BMP"
							)
SF-TOGGLE		"Отметить штампом"		TRUE
SF-ADJUSTMENT		"Размер штампа в %"		'(30 15 45 5 10 1 0)
SF-ADJUSTMENT		"Сдвиг штампа в %"		'(1.5 1 5 0.5 1 1 0)
SF-OPTION		"Выровнять штамп к"		'(
							"Нижний правый угол"
							"Нижний левый угол"
							"Верхний правый угол"
							"Верхний левый уголr"
							)
SF-OPTION		"Режим наложения штампа"	'("Нормальный" "Экран" "Добавление" "Разница")
SF-ADJUSTMENT		"Непрозрачность штампа"		'(100 0 100 10 25 1 0)
SF-TOGGLE		"Отметить невидимой меткой"	FALSE
SF-TOGGLE		"Двойная метка"			TRUE
SF-TOGGLE		"Изменить размеры изображения"	FALSE
SF-OPTION		"Использовать"			'(
							"Высоту"
							"Ширину"
							)
SF-VALUE		"Изменить до"			"1280"
)