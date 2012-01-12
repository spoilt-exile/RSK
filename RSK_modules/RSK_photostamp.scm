;Photostamp v0.8 RSK (+ GEL support)
;
;==============================================================================================
;                Данный код распространяется на условиях лицензии GNU GPLv3
;==============================================================================================
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
;ver. 0.7 (December 5 2010)
; - GAL implementation;
; - stamp and mark buffering while batch process;
; - additional improvements;
;===============================================================================================================
;ver. 0.8 (February 14 2011)
; - stable release;
;===============================================================================================================
;ver. 0.8 RSK (April 18 2011)
; - unified procedure for stamp/mark saving;
; - saving stamp and mark at the same time;
;===============================================================================================================

(define rsk-photostamp-version "Фотоштамп 0.8 RSK")

;Path variables
(define rsk-mark-path (string-append gimp-directory rsk-sys-sep "patterns" rsk-sys-sep "rsk-watermark.pat"))
(define rsk-stamp-path (string-append gimp-directory rsk-sys-sep "patterns" rsk-sys-sep "rsk-stamp.pat"))

;Global buffer images
(define rsk-stamp-image)
(define rsk-mark-image)

;Global buffer layers
(define rsk-stamp-layer)
(define rsk-mark-layer)

;rsk-create-mark
;Input variables:
;STRING - mark text;
;STRING - font name for marking;
;INTEGER - font size for marking;
;BOOLEAN - switch for showing final pattern;
(define (rsk-create-automark text font size show_switch mk_backup_switch mk_backup_prefix mk_backup_path)

  (rsk-begin-handle)
  (define xcf-path)

  (let* (
	(image (car (gimp-image-new 256 256 RGB)))
	(old_fore (car (gimp-context-get-foreground)))
	(text_layer 0)
	(prefix)
	)

	(if (equal? mk_backup_prefix "")
	  (set! prefix 
	    (string-append 
	    (number->string (cadddr (time))) 
	    "h " 
	    (number->string (cadddr (cdr (time)))) 
	    "m " 
	    (number->string (cadr (time))) 
	    "." 
	    (number->string (caddr (time))) 
	    "." 
	    (number->string (- (car (time)) 100)) 
	    "_"
	    )
	  )
	  (set! prefix (string-append mk_backup_prefix "_"))
	)
	(set! xcf-path (string-append mk_backup_path rsk-sys-sep prefix "rsk-mark-backup.xcf"))
	(gimp-context-set-foreground '(255 255 255))
	(set! text_layer (car (gimp-text-fontname image -1 0 0 text 5 TRUE size PIXELS font)))
	(script-fu-util-image-resize-from-layer image text_layer)
	(if (= mk_backup_switch TRUE)
	  (gimp-file-save 0 image text_layer xcf-path xcf-path)
	)
	(file-pat-save RUN-NONINTERACTIVE image text_layer rsk-mark-path rsk-mark-path "RSK-Watermark")
	(gimp-patterns-refresh)
	(gimp-context-set-foreground old_fore)
	(if (= show_switch FALSE)
	  (gimp-image-delete image)
	  (gimp-display-new image)
	)
  )

  (rsk-end-handle
    (string-append
    rsk-photostamp-version
    "\n(Мастер создания меток)"
    "\nТекст: " "\"" text "\""
    "\nРезервная копия: " (if (= mk_backup_switch TRUE) (string-append "Есть\nПуть копии: " xcf-path) "Нет")
    )
  )
)

;rsk-create-mask regestring
(script-fu-register 
"rsk-create-automark"
"<Image>/File/Create/Фотоштамп/Мастер с_оздания меток"
"Создать невидимую метку в автоматическом режиме"
(string-append "Guaracy Monteiro <guaracy.bm@gmail.com>\n" rsk-reg-author)
"Guaracy Monteiro"
rsk-reg-date
""
SF-STRING		"Текст метки"			"Моя метка"
SF-FONT			"Шрифт"				"Arial Bold"
SF-ADJUSTMENT		"Кегль (в пикселях)"		'(18 2 200 1 10 0 1)
SF-TOGGLE		"Просмотреть метку"		FALSE
SF-TOGGLE		"Сохранить копию метки"		FALSE
SF-STRING		"Префикс для копии"		""
SF-DIRNAME		"Сохранить штамп в"		""
)

;rsk-create-stamp
;Input variables:
;INTEGER - stamp width;
;INTEGER - stamp height;
;INTEGER - aspect preset number;
(define (rsk-create-uni imw imh aspect)

  (rsk-begin-handle)
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
  (gel-image-insert-layer image layer -1)
  (gimp-display-new image)
  (rsk-end-handle
    (string-append
    rsk-photostamp-version
    "\n(Создание заготовки)"
    "\nРазмер заготовки: " (number->string imw) "x" (number->string imh)
    )
  )
)

;rsk-create-stamp regestring
(script-fu-register 
"rsk-create-uni"
"<Image>/File/Create/Фотоштамп/Создать заготовку"
"Создание заготовку штампа или метки"
rsk-reg-author
rsk-reg-copyright
rsk-reg-date
""
SF-VALUE		"Ширина"			"1280"
SF-VALUE		"Высота"			"720"
SF-OPTION		"Установить соотношение сторон"	'("Выключено" "4:3" "16:9" "16:10")
)

;rsk-save-stamp
;Input variables:
;IMAGE - image to save;
;LAYER - layer to save;
;BOOLEAN - save stamp switch;
;BOOLEAN - backup stamp switch;
;STRING - stamp backup file prefix;
;STRING - stamp backup file path;
(define (rsk-save-uni image layer saver_opt st_backup_switch st_backup_prefix st_backup_path)

  (rsk-begin-handle)
  (define sep-image)
  (define prefix)
  (if (equal? st_backup_prefix "")
    (set! prefix 
      (string-append 
      (number->string (cadddr (time)))
      "h " 
      (number->string (cadddr (cdr (time)))) 
      "m " 
      (number->string (cadr (time))) 
      "." 
      (number->string (caddr (time))) 
      "." 
      (number->string (- (car (time)) 100)) 
      "_"
      )
    )
    (set! prefix (string-append st_backup_prefix "_"))
  )
  (define xcf-path 
    (string-append 
    st_backup_path 
    rsk-sys-sep prefix 
    "rsk-" 
    (cond
      (
	(= saver_opt 1)
	"stamp"
      )
      (
	(= saver_opt 2)
	"mark"
      )
      (
	(= saver_opt 3)
	"uni"
      )
    )
    "-backup.xcf"
    )
  )
  (if (and (= saver_opt 0) (= st_backup_switch FALSE))
    (rsk-quit-handle)
  )
  (if (= st_backup_switch TRUE)
    (gimp-file-save 0 image layer xcf-path xcf-path)
  )
  (set! sep-image (car (gimp-image-duplicate image)))
  (set! layer (car (gimp-image-merge-visible-layers sep-image 0)))
  (if (or (= saver_opt 1) (= saver_opt 3))
    (file-pat-save RUN-NONINTERACTIVE sep-image layer rsk-stamp-path rsk-stamp-path "RSK-Stamp")
  )
  (if (or (= saver_opt 2) (= saver_opt 3))
    (file-pat-save RUN-NONINTERACTIVE sep-image layer rsk-mark-path rsk-mark-path "RSK-Watermark")
  )
  (gimp-image-delete sep-image)
  (gimp-patterns-refresh)
  
  (rsk-end-handle
    (string-append
    rsk-photostamp-version
    "\n(Использование готового изображения)"
    "\nСохранен: " 
    (cond ((= saver_opt 1) "Штамп") ((= saver_opt 2) "Метка") ((= saver_opt 3) "Штамп и метка"))
    (if (= st_backup_switch TRUE) (string-append "\nРезервная копия: " xcf-path) "")
    )
  )
)

;rsk-save-stamp regestring
(script-fu-register 
"rsk-save-uni"
"<Image>/File/Create/Фотоштамп/Использовать изображение"
"Использовать текущее изображение как штамп или метку"
rsk-reg-author
rsk-reg-copyright
rsk-reg-date
"RGBA"
SF-IMAGE		"Изображение"			0
SF-DRAWABLE		"Слой"				0
SF-OPTION		"Сохранить как"			'("Не сохранять" "Штамп" "Метка" "Штамп и метка")
SF-TOGGLE		"Сохранить копию штампа"	FALSE
SF-STRING		"Префикс для копии"		""
SF-DIRNAME		"Сохранить штамп в"		""
)

;rsk-apply-mark
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
(define (rsk-apply-mark image layer st_switch st_percent st_off st_align st_mode st_opc imark_switch dmark_switch)

  (if (= rsk-batch-call-state FALSE)
    (rsk-begin-handle)
  )

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
	(st_blur_size (* low_percent 15))
	)

	(gimp-context-push)
	(gimp-image-undo-group-start image)
	(gel-image-insert-layer image copy_layer -1)
	(if (= st_switch TRUE)
	  (if (= 1 (car (gimp-patterns-get-list "RSK-Stamp")))
	    (begin
	      (if (= rsk-batch-call-state TRUE)
		(begin
		  (set! st_layer (car (gimp-layer-new-from-drawable rsk-stamp-layer image)))
		  (gel-image-insert-layer image st_layer -1)
		)
		(begin
		  (set! st_layer (car (gimp-file-load-layer 1 image rsk-stamp-path)))
		  (gel-image-insert-layer image st_layer -1)
		)
	      )
	      (set! st_w (car (gimp-drawable-width st_layer)))
	      (set! st_h (car (gimp-drawable-height st_layer)))
	      (set! st_aspect (/ st_w st_h))
	      (if (> st_w st_size)
		(gimp-layer-scale st_layer st_size (/ st_size st_aspect) 1)
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
	      (gel-item-set-name copy_layer "Слой со штампом")
	    )
	    (gimp-message "Текстура штампа не найдена.\nCоздайте необходимую текстуру:\n [Файл/Создать/Фотоштамп/Создать заготовку]\n")
	  )
	)
	(if (= imark_switch TRUE)
	  (if (= 1 (car (gimp-patterns-get-list "RSK-Watermark")))
	    (begin
	      (set! copy_mask (car (gimp-layer-create-mask copy_layer 5)))
	      (set! mark_layer (car (gimp-layer-new image width height RGBA-IMAGE "Watermark" 3 NORMAL-MODE)))
	      (gel-image-insert-layer image mark_layer -1)
	      (gimp-context-set-pattern "RSK-Watermark")
	      (gimp-displays-flush image)
	      (gimp-selection-all image)
	      (gimp-edit-fill mark_layer PATTERN-FILL)
	      (gimp-layer-add-mask mark_layer copy_mask)
	      (if (= dmark_switch TRUE)
		(begin
		  (set! invmark_layer (car (gimp-layer-copy mark_layer TRUE)))
		  (gel-image-insert-layer image invmark_layer -1)
		  (gimp-invert invmark_layer)
		  (set! invmark_mask (car (gimp-layer-get-mask invmark_layer)))
		  (gimp-invert invmark_mask)
		)
	      )
	      (set! copy_layer (car (gimp-image-merge-down image mark_layer 0)))
	      (if (= dmark_switch TRUE)
		(set! copy_layer (car (gimp-image-merge-down image invmark_layer 0)))
	      )
	      (gel-item-set-name copy_layer "Защищенный слой")
	      (gimp-selection-none image)
	      (gimp-context-set-pattern old_pattern)
	    )
	    (gimp-message "Текстура метки не найдена.\nCоздайте необходимую текстуру:\n [Файл/Создать/Фотоштамп/Мастер создания меток]\n")
	  )
	)
	(gimp-image-undo-group-end image)
	(gimp-context-pop)
	(gimp-displays-flush)
  )

  (if (= rsk-batch-call-state FALSE)
    (rsk-end-handle
      (string-append
      rsk-photostamp-version
      "\n(Применение штампов и меток)"
      (if (= st_switch TRUE) (string-append "\nШтамп: ЕСТЬ\nПрозрачность штампа: " (number->string st_opc)) "")
      (if (= imark_switch TRUE) (string-append "\nМетка: ЕСТЬ" (if (= dmark_switch TRUE) "\nДвойная метка: ЕСТЬ" "")) "")
      )
    )
  )

)

;rsk-apply-mark regestring
(script-fu-register 
"rsk-apply-mark"
(string-append rsk-reg-defpath "Фотоштамп _Применить метки")
"Пометка изображения с помощью штампов и меток"
(string-append "Guaracy Monteiro <guaracy.bm@gmail.com>\n" rsk-reg-author)
"Guaracy Monteiro"
rsk-reg-date
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

;rsk-reveal-mark
;Input variables:
;IMAGE - process image;
;LAYER - process layer;
;LAYER - original layer (without mark);
(define (rsk-reveal-mark image layer orig_layer post)

  (rsk-begin-handle)

  (let* (
	(copy_layer (car (gimp-layer-copy orig_layer FALSE)))
	(over_layer (car (gimp-layer-copy layer FALSE)))
	)

	(gimp-image-undo-group-start image)
	(gel-image-insert-layer image copy_layer -1)
	(gel-image-insert-layer image over_layer -1)
	(gimp-layer-set-mode over_layer 20)
	(set! over_layer (car (gimp-image-merge-down image over_layer 0)))
	(plug-in-c-astretch 1 image over_layer)
	(gel-item-set-name over_layer "Проявленная метка")
	(gimp-desaturate-full over_layer 1)
	(if (= post TRUE)
	  (gimp-levels over_layer 0 25 100 1.0 0 255)
	)
	(gimp-displays-flush image)
	(gimp-image-undo-group-end image)
  )

  (rsk-end-handle 
    (string-append
    rsk-photostamp-version
    "\n(Проявка метки)"
    )
  )

)

;rsk-reveal-mark regestring
(script-fu-register 
"rsk-reveal-mark"
(string-append rsk-reg-defpath "Фотоштамп Про_явить метку")
"Проявить невидимую метку"
rsk-reg-author
rsk-reg-copyright
rsk-reg-date
"RGB,RGBA*"
SF-IMAGE		"Изображение"			0
SF-DRAWABLE		"Слой"				0
SF-DRAWABLE		"Выбрать слой с оригиналом"	5
SF-TOGGLE		"Постобработка метки"		FALSE
)

(define (rsk-batch-mark dir_in input_format dir_out out_format st_switch st_percent st_off st_align st_mode st_opc imark_switch dmark_switch resize_switch resize_value)

  (rsk-begin-handle)
  (set! rsk-batch-call-state TRUE)
  (define pr-counter)

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

	(set! pr-counter (length filelist))

	;Creating buffers
	(if (= (car (gimp-patterns-get-list "RSK-Stamp")) TRUE)
	  (begin
	    (set! rsk-stamp-image (car (gimp-file-load 1 rsk-stamp-path rsk-stamp-path)))
	    (set! rsk-stamp-layer (car (gimp-image-get-active-layer rsk-stamp-image)))
	  )
	  (gimp-message "Текстура штампа не найдена.\nCоздайте необходимую текстуру:\n [Filters/RSS/Фотоштамп Создать штамп]\n")
	)
	(if (= (car (gimp-patterns-get-list "RSK-Watermark")) TRUE)
	  (begin
	    (set! rsk-mark-image (car (gimp-file-load 1 rsk-mark-path rsk-mark-path)))
	    (set! rsk-mark-layer (car (gimp-image-get-active-layer rsk-mark-image)))
	  )
	  (gimp-message "Текстура метки не найдена.\nCоздайте необходимую текстуру:\n [Filters/RSS/Фотоштамп Создать метку]\n")
	)

	(while (not (null? filelist))
	  (let* (
		(cur_target (car filelist))
		(image (car (gimp-file-load 1 cur_target cur_target)))
		(imh (car (gimp-image-height image)))
		(imw (car (gimp-image-width image)))
		(aspect_ratio (/ imw imh))
		(rs_imh (if (> imh imw) resize_value (/ resize_value aspect_ratio)))
		(rs_imw (if (> imh imw) (* resize_value aspect_ratio) resize_value))
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
		    (gimp-image-scale image rs_imw rs_imh)
		    (if (= imark_switch TRUE)
		      (begin
			(set! origin_out (string-append dir_out "/" file "_оригинал." out-ext))
			(cond
			  ((= out_format 0) (file-jpeg-save 1 image srclayer origin_out origin_out 1 0 1 1 "" 2 1 0 0))
			  ((= out_format 1) (file-png-save-defaults 1 image srclayer origin_out origin_out))
			  ((= out_format 2) (file-tiff-save 1 image srclayer origin_out origin_out 1))
			  ((= out_format 3) (file-bmp-save 1 image srclayer origin_out origin_out))
			)
		      )
		    )
		  )
		)
		(rsk-apply-mark image srclayer st_switch st_percent st_off st_align st_mode st_opc imark_switch dmark_switch)
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

	;Delete buffers
	(gimp-image-delete rsk-stamp-image)
	(gimp-image-delete rsk-mark-image)

  )
  
  (set! rsk-batch-call-state FALSE)
  (rsk-end-handle
    (string-append
    rsk-photostamp-version
    "\n(Пакетный режим)"
    "\nПапка-источник: " dir_in
    "\nПапка-назначение: " dir_out
    "\nКоличество файлов: " (number->string pr-counter)
    (if (= st_switch TRUE) (string-append "\nШтамп: ЕСТЬ\nПрозрачность штампа: " (number->string st_opc)) "")
    (if (= imark_switch TRUE) (string-append "\nМетка: ЕСТЬ" (if (= dmark_switch TRUE) "\nДвойная метка: ЕСТЬ" "")) "")
    )
  )

)

;rsk-batch-mark regestring
(script-fu-register 
"rsk-batch-mark"
(string-append rsk-reg-defpath "Фотоштамп Пакетный режим")
"Пометка изображений в пакетном режиме"
rsk-reg-author
rsk-reg-copyright
rsk-reg-date
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
SF-VALUE		"Изменить до (большая сторона)"	"1280"
)