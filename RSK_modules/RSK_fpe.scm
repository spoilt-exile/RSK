;FPE (Fast Photo Enhance) v0.9 RSK
;
;FPE is a part of RSK (RSS Script Kit)
;
;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;http://www.gnu.org/licenses/gpl-3.0.html
;
;СУФ (Срочное улучшение фото)
;Данный скрипт требует наличия таких плагинов:
;-Fix-CA
;-greycstoration
;========================================================================
;История версий:
;========================================================================
;ver. 0.1 (16 октября 2009)
; - заготовка скрипта без единого движка.
;========================================================================
;ver. 0.3 (5 января 2010)
; - реализация единого движка (ядро).
; - портирование и модернизация highpass sharpening.
;========================================================================
;ver. 0.5 (5 февраля 2010)
; - портирование и модернизация fake hdr.
; - реализация входных и выходных форматов.
; - добавление поддержки путей Windows.
;========================================================================
;ver. 0.7 (14 марта 2010)
; - мелкие исправления и подготовка к релизу.
; - добавление сообщений в случае отсутствия плагинов.
;========================================================================
;ver. 0.8 (9 мая 2010)
; - fake hdr заменен на shadow recovery.
;========================================================================
;ver. 0.81 (3 июня 2010)
; - регистронезависимые расширения графическийх файлов в конвейере.
;========================================================================
;ver. 0.9 (30 сентября 2010)
; - подавдение хроматических шумов.
;========================================================================

(define (rsk-fpe-core image layer ex_chrome ex_denoise dn_mode dn_chrome ex_sharp sharp_opc s_rec s_opc)
(rsk-api-check)
  (let*	(
	(boost (/ (* 128 (+ 100 0)) 200))
	(radius 10)
	(run_mode 1)
	(target (car (gimp-layer-copy layer FALSE)))
	(layer_blur)
	(layer_grey)
	(layer_boost)
	(ShadowMask)
	(ShadowLayer)
	(chrome_overlay)
	(chrome_mask)
	)
	(gimp-context-push)
	(gimp-image-undo-disable image)
	(gimp-image-add-layer image target -1)
	(gimp-drawable-set-name target "FPE")
	(if (= ex_chrome TRUE)
	  (if (= rsk-fixca-def TRUE)
	    (Fix-CA 0 image target 0.0 0.0 1 0.0 0.0 0.0 0.0)
	    (rsk-dep_warn-handle 1)
	  )
	)
	(if (= dn_chrome TRUE)
	  (if (= rsk-gmic-def TRUE)
	    (plug-in-gmic 1 image target 1 "-apply_channels \"-blur_xy 3,3,1\",8")
	    (rsk-dep_warn-handle 0)
	  )
	)
	(if (= ex_denoise TRUE)
	  (if (= rsk-greycstoration-def TRUE)
	    (begin
	      (cond
		((= dn_mode 0) (plug-in-greycstoration 1 image target 20 0.8 0.17 1.0 1.1 0.8 30 1 2 1 1))
		((= dn_mode 1) (plug-in-greycstoration 1 image target 50 0.8 0.17 1.3 1.1 0.8 30 1 2 1 1))
		((= dn_mode 2) (plug-in-greycstoration 0 image target 50 0.8 0.17 1.3 1.1 0.8 30 1 2 1 1))
	      )
	    )
	    (rsk-dep_warn-handle 2)
	  )
	)
	(if (= ex_sharp TRUE)
	  (begin
	    ;Код был взят из скрипта highpass sharpening, автор - Andreas Schönfelder
	    ;http://registry.gimp.org/node/21165
	    (set! layer_grey (car (gimp-layer-copy target FALSE)))
	    (gimp-image-add-layer image layer_grey -1)
	    (gimp-desaturate layer_grey)
	    (set! layer_blur (car (gimp-layer-copy layer_grey FALSE)))
	    (gimp-image-add-layer image layer_blur -1)
	    (plug-in-gauss-rle 1 image layer_blur radius 1 1)
	    (gimp-invert layer_blur)
	    (gimp-layer-set-opacity layer_blur 50)
	    (set! layer_grey (car (gimp-image-merge-down image layer_blur 0)))
	    (gimp-levels layer_grey HISTOGRAM-VALUE boost (- 255 boost) 1 0 255)
	    (gimp-curves-spline layer_grey HISTOGRAM-VALUE 10 #(95 0 127 128 154 184 222 240 255 255))
	    (gimp-layer-set-opacity layer_grey sharp_opc)
	    (gimp-layer-set-mode layer_grey OVERLAY-MODE)
	    (set! layer_boost (car (gimp-layer-copy layer_grey FALSE)))
	    (gimp-image-add-layer image layer_boost -1)
	    (set! target (car (gimp-image-merge-down image layer_grey 0)))
	    (set! target (car (gimp-image-merge-down image layer_boost 0)))
	  )
	)
	(if (= s_rec TRUE)
	  (begin
	    ;Код был взят из скрипта Shadow recovery, автор - Martin Egger
	    ;http://registry.gimp.org/node/112
	    (set! ShadowLayer (car (gimp-layer-copy target TRUE)))
	    (gimp-image-add-layer image ShadowLayer -1)
	    (gimp-desaturate ShadowLayer)
	    (gimp-invert ShadowLayer)
	    (set! ShadowMask (car (gimp-layer-create-mask ShadowLayer 5)))
	    (gimp-layer-add-mask ShadowLayer ShadowMask)
	    (gimp-layer-set-mode ShadowLayer OVERLAY-MODE)
	    (gimp-layer-set-opacity ShadowLayer s_opc)
	    (set! target (car (gimp-image-merge-down image ShadowLayer 0)))
	  )
	)
	(gimp-image-set-active-layer image target)
	(gimp-image-undo-enable image)
	(gimp-context-pop)
	(gimp-displays-flush)
  )
)

(define (rsk-fpe-batch dir_in input_format dir_out out_format ex_chrome ex_denoise dn_mode dn_chrome ex_sharp sharp_opc s_rec s_opc)
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
    ((= out_format 4) (set! out-ext "xcf"))
    ((= out_format 5) (set! out-ext "psd"))
  )

  (let*	(
	(pattern (string-append dir_in rsk-sys-sep "*." input-ext))
	(filelist (cadr (file-glob pattern 1)))
	(run_mode 1)
	)
	(while (not (null? filelist))
	  (let* (
		(cur_target (car filelist))
		(img (car (gimp-file-load 1 cur_target cur_target)))
		(srclayer)
		(filename (car (gimp-image-get-filename img)))
		(target_out)
		(file)
		(res_layer)
		)
		(if (> input_format 2)
		  (begin
		    (set! srclayer (car (gimp-image-get-active-layer img)))
		    (gimp-edit-copy-visible img)
		    (set! srclayer (car (gimp-edit-paste srclayer TRUE)))
		    (gimp-floating-sel-to-layer srclayer)
		    (gimp-drawable-set-name srclayer "Viz-src")
		    (gimp-image-raise-layer-to-top img srclayer)
		  )
		  (set! srclayer (car (gimp-image-get-active-layer img)))
		)
		(fpe-core img srclayer ex_chrome ex_denoise dn_mode dn_chrome ex_sharp sharp_opc s_rec s_opc)
		(if (< out_format 4)
		  (set! res_layer (car (gimp-image-merge-visible-layers img 0)))
		  (set! res_layer (car (gimp-image-get-active-layer img)))
		)
		(set! file (substring filename (string-length dir_in) (- (string-length filename) 4 )))
		(set! target_out (string-append dir_out "/" file "_FPE." out-ext))
		(cond
		  ((= out_format 0) (file-jpeg-save 1 img res_layer target_out target_out 1 0 1 1 "" 2 1 0 0))
		  ((= out_format 1) (file-png-save-defaults 1 img res_layer target_out target_out))
		  ((= out_format 2) (file-tiff-save 1 img res_layer target_out target_out 1))
		  ((= out_format 3) (file-bmp-save 1 img res_layer target_out target_out))
		  ((= out_format 4) (gimp-xcf-save 1 img res_layer target_out target_out))
		  ((= out_format 5) (file-psd-save 1 img res_layer target_out target_out 1 0))
		)
		(gimp-image-delete img)
	  )
	  (set! filelist (cdr filelist))
	)
  )
)

(define fpe-credits
  (list
  "Непочатов Станислав"
  "GPLv3"
  "7 Октября 2010"
  )
)

(define fpe-controls
  (list
  SF-TOGGLE	"Подавление хроматической абберации (*dep)"	FALSE
  SF-TOGGLE	"Подавление шумов (*dep)"			FALSE
  SF-OPTION	"Установки шумоподавления"			'(
								"Легкий шум"
								"Средний шум"
								"Указано пользователем..."
								)
  SF-TOGGLE	"Подавление хроматического шума (*dep)"		FALSE
  SF-TOGGLE	"Увеличение резкости"				TRUE
  SF-ADJUSTMENT	"Степень резкости"				'(45 0 100 10 20 1 0)
  SF-TOGGLE	"Проявить тени"					FALSE
  SF-ADJUSTMENT	"Степень проявки"				'(60 0 100 10 20 1 0)
  )
)

(apply script-fu-register
  (append
    (list
    "rsk-fpe-core"
    _"<Image>/Filters/RSK R1/С_УФ"
    "Срочное улучшение фотографий"
    )
    fpe-credits
    (list "*"
    SF-IMAGE		"Изображение"		0
    SF-DRAWABLE		"Слой"			0
    )
    fpe-controls
  )
)

(apply script-fu-register
  (append
    (list
    "rsk-fpe-batch"
    _"<Image>/Filters/RSK R1/СУФ К_онвейер"
    "Конвейерное улучшение фотографий"
    )
    fpe-credits
    (list ""
    SF-DIRNAME	"Папка-источник"	"/home/spoilt/Документы/Batch/IN"
    SF-OPTION	"Входящий формат"	'(
					"*"
					"JPG"
					"TIFF"
					"XCF"
					)
    SF-DIRNAME	"Папка-назначение"	"/home/spoilt/Документы/Batch/OUT"
    SF-OPTION	"Формат сохранения"	'(
					"JPG"
					"PNG"
					"TIF"
					"BMP"
					"XCF"
					"PSD"
					)
    )
    fpe-controls
  )
)