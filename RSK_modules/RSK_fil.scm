;FIL v1.7.1 RSK (+ GEL support)
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
;FIL = ЛИПС (Лаборатория Имитации Пленочных Снимков);
;
;Version history:
;===============================================================================================================
;ver. 0.3 (December 19 2009)
; - working script with small amount of procedures;
; - FIL 0.3 specifications definition;
;===============================================================================================================
;ver. 0.5 (December 22 2009)
; - separate execution of color and grain processes;
; - option indicaion output into final layer's name;
; - specs modification;
; - module classes introduction;
;===============================================================================================================
;ver 0.8 (December 24 2009)
; - new core (NG);
; - vignette as pre-process;
; - grian amplification as part of core (not recomended with Simple Grian process);
; - work woth visible;
; - new grain process (Grain+);
;===============================================================================================================
;ver. 1.0 (January 11 2010)
; - core independ process execution enhancement;
; - bugfixes;
; - color process and etc modification;
; - grain amplification in grain process;
; - border blur (like bad lenses);
; - interface modification;
; - vignette radius (may be increased);
;===============================================================================================================
;ver. 1.0r1 (February 17 2010)
; - vignette process modification;
; - vignette softness support;
; - rsk-fil-source-handle improved;
;===============================================================================================================
;ver. 1.0r2 (March 24 2010)
; - some fixes in sepia process;
; - relative blur in Grian+;
; - significant optimization in Grain+ process;
;===============================================================================================================
;ver 1.0r3 (March 26 2010)
; - first public release;
;===============================================================================================================
;ver 1.5.0 (May 3 2010)
; - total revision of all core stages;
; - new specs;
; - register procedures for color and grain processes;
; - whole core variables classification;
; - new color process Duotone (based on Split Studio 3);
; - exposure correction as new pre-process;
;===============================================================================================================
;ver 1.5.1 (June 5 2010)
; - FIL core batch execution;
; - new grain process "Sulfide";
;===============================================================================================================
;ver 1.6.0 (August 6 2010)
; - script's core modification;
; - optional option output;
; - launching processes with custome options;
; - Grunge filter included in Sulfide process.
; - stable release status;
;===============================================================================================================
;ver 1.7.0 (August 30 2010)
; - undo support by separate image processing;
; - binary plugin integration (G'MIC and Fix-CA);
; - new proceesses ("Vintage", "Photochrom" and "Dram");
; - plugins checker added;
; - film scratches in "Sulfide" process added;
; - chromatic abberation in "Border Blur" pre-process added;
;===============================================================================================================
;ver 1.7.1 (November 30 2010)
; - "B/W" and "Sepia" process was replaced by "Monochrome" color process;
; - "SOV" and "Lomo" processes has been updated;
; - "Simple Grain" was removed;
; - new process "Dram grain";
; - batch version improvments;
;===============================================================================================================
;ver 1.7.1 RSK (April 18 2011)
; - code cleanup for RSK;
;===============================================================================================================
;Procedures:			Status		Revision	Specs version
;==========================================CORE PROCEDURES======================================================
;rsk-fil-spe-core		stable		---		1.7
;rsk-fil-stage-handle		stable		---		---
;rsk-fil-source-handle		stable		---		---
;rsk-fil-spe-batch		stable		---		---
;===========================================PRE-PROCESSES=======================================================
;rsk-fil-pre-xps		stable		r0		1.7
;rsk-fil-pre-vignette		stable		r4		1.7
;rsk-fil-prefx-badblur		stable		r3		1.7
;==========================================COLOR PROCESSES======================================================
;rsk-fil-clr-sov		stable		r7		1.7
;rsk-fil-clr-monochrome		stable		r4		1.7
;rsk-fil-clr-lomo		stable		r4		1.7
;rsk-fil-clr-duo		stable		r2		1.7
;rsk-fil-clr-vintage		stable		r1		1.7
;rsk-fil-clr-chrome		stable		r2		1.7
;rsk-fil-clr-dram		stable		r1		1.7
;==========================================GRAIN PROCESSES======================================================
;rsk-fil-grn-adv_grain		stable		r5		1.7
;rsk-fil-grnfx-sulfide		stable		r6		1.7
;rsk-fil-grnfx-dram		stable		r2		1.7
;=====================================FIL processes classification==============================================
; -pre - pre-process.
; -clr - color process.
; -grn - grain process.
; -prefx - pre-process which uses plugins.
; -clrfx - color process which uses plugins.
; -grnfx - grain process which uses plugins.
;=================================FIL 1.7 modules requirements list:============================================
; * process can use binary plugin procedures by using core permissions in rsk-fk-*-def variables.
; * if process can't work without some plugin then message should appear (via rsk-fil-dep_warn-handle).
; * process can call to binary plugin only in NON-INTERACTIVE mode.
; * processes shouldn't call other FIL processes from itself but it can call private additional procedures.
; * processes shouldn't change image dimensions or it's color depth.
; * procceses able to take some image option from FIL core by itself (variable class fc_*).
; * register stage should be defined by it's variable and should be included in rsk-fk-stages-list.
; * processes (except pre-proccesses) should be register in rsk-fk-clr-stage and rsk-fk-grain-stage variables.
; * processes should return final layer to core (if processes use many layers).
; * processes could have special launch options (for creating profiles).
;========================================FIL 1.7 core stages====================================================
;Stage			Register?			Stage number (stage_id)
;pre-stage		NO				0
;rsk-fk-clr-stage	YES				1
;rsk-fk-grain-stage	YES				2
;===============================================================================================================

;Core global variables

;FIL version
(define rsk-fil-version "ЛИПС 1.7.1 RSK")

;Core stage counter
(define rsk-fk-stage-counter 0)

;Separate image variable
(define rsk-fk-sep-image)

;Core stage register with stage_id=1 (color stage);
(define rsk-fk-clr-stage)
(set! rsk-fk-clr-stage
  (list
    
    ;Process "SOV: normal" with proc_id=0
    (list "СОВ: обычный" 		TRUE	(quote (rsk-fil-clr-sov rsk-fk-sep-image fio_uni_layer fc_imh fc_imw 60 65)))

    ;Process "SOV: light" with proc_id=1
    (list "СОВ: легкий" 		TRUE	(quote (rsk-fil-clr-sov rsk-fk-sep-image fio_uni_layer fc_imh fc_imw 30 35)))

    ;Process "Monochrome (dev)" with proc_id=2
    (list "Монохром: ч/б"		TRUE	(quote (rsk-fil-clr-monochrome rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 0)))

    ;Process "Monochrome (dev)" with proc_id=3
    (list "Монохром: сепия"		TRUE	(quote (rsk-fil-clr-monochrome rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 1)))

    ;Process "Lomo: XPro Green" with proc_id=4
    (list "Ломо: XPro Зеленый" 		FALSE	(quote (rsk-fil-clr-lomo rsk-fk-sep-image fio_uni_layer 0)))

    ;Process "Lomo: XPro Autumn" with proc_id=5
    (list "Ломо: желтоватый" 		FALSE	(quote (rsk-fil-clr-lomo rsk-fk-sep-image fio_uni_layer 1)))

    ;Process "Lomo: Old Red" with proc_id=6
    (list "Ломо: золотая осень" 	FALSE	(quote (rsk-fil-clr-lomo rsk-fk-sep-image fio_uni_layer 2)))

    ;Process "Duotone: normal" with proc_id=7
    (list "Двутон: обычный" 		TRUE	(quote (rsk-fil-clr-duo rsk-fk-sep-image fio_uni_layer 75 '(200 175 140) '(80 102 109))))

    ;Process "Duotone: soft" with proc_id=8
    (list "Двутон: мягкий" 		TRUE	(quote (rsk-fil-clr-duo rsk-fk-sep-image fio_uni_layer 30 '(200 175 140) '(80 102 109))))

    ;Process "Duotone: user colors" with proc_id=9
    (list "Двутон: свои цвета" 		TRUE	(quote (rsk-fil-clr-duo rsk-fk-sep-image fio_uni_layer 55 fc_fore fc_back)))

    ;Process "Vintage" with proc_id=10
    (list "Винтаж"			TRUE	(quote (rsk-fil-clr-vintage rsk-fk-sep-image fio_uni_layer fc_imh fc_imw 17 20 59 TRUE)))

    ;Process "Photochrom: normal" with proc_id=11
    (list "Фотохром: обычный"		TRUE	(quote (rsk-fil-clr-chrome rsk-fk-sep-image fio_uni_layer fc_imh fc_imw '(255 128 0) '(255 68 112) 60 60 0 100 FALSE FALSE)))

    ;Process "Photochrom: retro" with proc_id=12
    (list "Фотохром: ретро"		TRUE	(quote (rsk-fil-clr-chrome rsk-fk-sep-image fio_uni_layer fc_imh fc_imw '(255 128 0) '(255 68 112) 60 60 0 100 FALSE TRUE)))

    ;Process "Photochrom: bleach" with proc_id=13
    (list "Фотохром: блеклый"		TRUE	(quote (rsk-fil-clr-chrome rsk-fk-sep-image fio_uni_layer fc_imh fc_imw '(255 128 0) '(255 68 112) 60 60 0 100 TRUE FALSE)))

    ;Process "Photochrom: user colors" with proc_id=14
    (list "Фотохром: свои цвета"	TRUE	(quote (rsk-fil-clr-chrome rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore fc_back 60 60 0 100 FALSE FALSE)))

    ;Process "Dram: normal" with proc_id=15
    (list "Драм: обычный"		TRUE	(quote (rsk-fil-clr-dram rsk-fk-sep-image fio_uni_layer '(93 103 124))))

    ;Process "Dram: user colors" with proc_id=16
    (list "Драм: свои цвета"		TRUE	(quote (rsk-fil-clr-dram rsk-fk-sep-image fio_uni_layer fc_fore)))
  )
)

;Core stage register with stage_id=2 (grain stage);
(define rsk-fk-grain-stage)
(set! rsk-fk-grain-stage
  (list

    ;Process "Grain+: normal" with proc_id=0
    (list "Зерно+: обычный" 		TRUE	(quote (rsk-fil-grn-adv_grain rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore FALSE)))

    ;Process "Grain+: amplified" with proc_id=1
    (list "Зерно+: усиленный" 		TRUE	(quote (rsk-fil-grn-adv_grain rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore TRUE)))

    ;Process "Sulfide: normal" with proc_id=2
    (list "Сульфид: обычный"		TRUE	(quote (rsk-fil-grnfx-sulfide rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 2.5 FALSE FALSE)))

    ;Process "Sulfide: large scale" with proc_id=3
    (list "Сульфид: крупный"		TRUE	(quote (rsk-fil-grnfx-sulfide rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 3.1 FALSE FALSE)))

    ;Process "Sulfide: grunge" with proc_id=4
    (list "Сульфид: гранж"		TRUE	(quote (rsk-fil-grnfx-sulfide rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 2.7 TRUE FALSE)))

    ;Process "Sulfide; scratches" with proc_id=5
    (list "(G'MIC) Сульфид: царапины"	TRUE	(quote (rsk-fil-grnfx-sulfide rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 2.5 FALSE TRUE)))

    ;Process "Dram Grain: normal" with proc_id=6
    (list "(G'MIC) Драм-зерно: обычный"	TRUE	(quote (rsk-fil-grnfx-dram rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 30 TRUE)))

    ;Process "Dram Grain: light" with proc_id=7
    (list "Драм-зерно: легкий"		TRUE	(quote (rsk-fil-grnfx-dram rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fc_fore 25 FALSE)))
  )
)

;Global stage list
(define rsk-fk-stages-list 
  (list 
    FALSE			;Pre-process stage marked as FALSE (not register stage);
    rsk-fk-clr-stage		;Color process stage;
    rsk-fk-grain-stage		;Grain process stage;
  )
)

;FIL core procedure
(define (rsk-fil-spe-core	;procedure name;

	;Launching main atributes
	fm_image		;image variable;

	;Color stage control
	fm_clr_flag		;color proceess execution switch;
	fm_clr_id		;color process number;

	;Grain stage control
	fm_grain_flag		;grain proceess execution switch;
	fm_grain_id		;grain process number;

	;Pre-process control
	fm_pre_vign_flag	;vignette activation switch;
	fm_pre_vign_rad		;vignette radius in percents;
	fm_pre_vign_soft	;vignette softness;
	fm_pre_vign_opc		;vingette opacity;
	fm_pre_blur_step	;border blur control;
	fm_pre_xps_control	;exposure correction control;

	;Additional options
	fm_misc_logout		;option output swtitch;
	fm_misc_visible		;visible switch;
	)

  ;Stage counter force reset
  (set! rsk-fk-stage-counter 0)

  ;Empty launck prevent
  (if (and (= fm_clr_flag FALSE) (= fm_grain_flag FALSE) (= fm_pre_vign_flag FALSE) (= fm_pre_xps_control 0))
    (rsk-quit-handle)
  )

  ;Core start
  (if (= rsk-batch-call-state FALSE)
      (begin
	(rsk-begin-handle)
	(gimp-image-undo-group-start fm_image)
      )
      (begin
	(gimp-context-push)
	(gimp-image-undo-disable fm_image)
	(set! rsk-fk-sep-image fm_image)
      )
  )

  (define f-res-str)

  ;Variables declaration;
  (let* (

	;System variables
	(fc_imh (car (gimp-image-height fm_image)))		;image height system variable
	(fc_imw (car (gimp-image-width fm_image)))		;image width system variable
	(fc_fore (car (gimp-context-get-foreground)))		;foreground color system variable
	(fc_back (car (gimp-context-get-background)))		;background color system variable

	;Stages control
	(fl_pre_flag FALSE)					;pre-stage execution flag

	;Result variables recieved from stage handler
	(fx_clr_list)						;List of variables recived from rsk-fil-stage-handle while color stage
	(fx_clr_exp)						;Color stage execution code block
	(fx_grain_list)						;List of variables recived from rsk-fil-stage-handle while grain stage
	(fx_grain_exp)						;grain stage execution code block

	;I/O variables
	(fio_uni_layer)						;single layer for all stages
	(fio_return_flag)					;flag for automatic layer returning

	;Option indication string prefixes
	(fs_pref_pre "-п ")					;pre-stage option prefix
	(fs_pref_clr "-ц ")					;color stage option prefix
	(fs_pref_grain "-з ")					;grain stage option prefix

	;Additional string variables
	(fs_clr_str)						;color stage layer name
	(fs_grain_str)						;grain stage layer name
	(fs_res_str "")						;final layer name
	(fs_xps_str "Эксп. ")					;exposure correction string mark
	(fs_vign_str "(В) ")					;vignette string mark
	(fs_blur_str "Разм. x")					;border blur (bad lenses) string mark
	(fs_default_str "Результат обработки")			;final layer default string
	)

	;Pre-stage activation section
	(cond
	  ((> fm_pre_blur_step 0) (set! fl_pre_flag TRUE))
	  ((and (= fm_pre_vign_flag TRUE) (> fm_pre_vign_opc 0)) (set! fl_pre_flag TRUE))
	  ((not (= fm_pre_xps_control 0)) (set! fl_pre_flag TRUE))
	)

	;Pre-stage initalization
	(if (= fl_pre_flag TRUE)
	  (begin

	    ;Copying layer
	    (set! fio_uni_layer (rsk-fil-source-handle fm_image fm_misc_visible))
	    (set! fs_res_str (string-append fs_res_str fs_pref_pre))

	    ;Exposure correction launching
	    (if (not (= fm_pre_xps_control 0))
	      (begin
		(rsk-fil-pre-xps rsk-fk-sep-image fio_uni_layer fm_pre_xps_control)
		(set! fs_xps_str (string-append fs_xps_str (if (> fm_pre_xps_control 0) "+" "-") (number->string fm_pre_xps_control)))
		(if (> (string-length fs_xps_str) 10)
		  (set! fs_xps_str (substring fs_xps_str 0 11))
		)
		(set! fs_res_str (string-append fs_res_str fs_xps_str " "))
	      )
	    )

	    ;Vignette launching
	    (if (= fm_pre_vign_flag TRUE)
	      (if (> fm_pre_vign_opc 0)
		(begin
		  (set! fio_uni_layer (rsk-fil-pre-vignette rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fm_pre_vign_opc fm_pre_vign_rad fm_pre_vign_soft fc_fore))
		  (set! fs_res_str (string-append fs_res_str fs_vign_str))
		)
	      )
	    )
	    
	    ;Blur launching
	    (if (> fm_pre_blur_step 0)
	      (begin
		(rsk-fil-prefx-badblur rsk-fk-sep-image fio_uni_layer fc_imh fc_imw fm_pre_blur_step)
		(set! fs_res_str (string-append fs_res_str fs_blur_str (number->string (+ fm_pre_blur_step 1)) " "))
	      )
	    )
	  )
	)

	;Stage counter correction
	(set! rsk-fk-stage-counter (+ rsk-fk-stage-counter 1))

	;Color stage initalization
	(if (= fm_clr_flag TRUE)
	  (begin

	    ;Recieved layer checking
	    (if (null? fio_uni_layer)
	      (set! fio_uni_layer (rsk-fil-source-handle fm_image fm_misc_visible))
	    )

	    ;Process list initalization
	    (set! fx_clr_list (rsk-fil-stage-handle FALSE rsk-fk-stage-counter fm_clr_id))
	    (set! fs_clr_str (car fx_clr_list))
	    (set! fio_return_flag (cadr fx_clr_list))
	    (set! fx_clr_exp (caddr fx_clr_list))

	    ;Color process execution
	    (if (= fio_return_flag TRUE)
	      (set! fio_uni_layer (eval fx_clr_exp))
	      (eval fx_clr_exp)
	    )

	    ;String modification and layer renaming
	    (set! fs_res_str (string-append fs_res_str fs_pref_clr fs_clr_str " "))
	  )
	)

	;Stage counter correction
	(set! rsk-fk-stage-counter (+ rsk-fk-stage-counter 1))

	;Grain stage initalization
	(if (= fm_grain_flag TRUE)
	  (begin

	    ;Recieved layer checking
	    (if (null? fio_uni_layer)
	      (set! fio_uni_layer (rsk-fil-source-handle fm_image fm_misc_visible))
	    )

	    ;Process list initalization
	    (set! fx_grain_list (rsk-fil-stage-handle FALSE rsk-fk-stage-counter fm_grain_id))
	    (set! fs_grain_str (car fx_grain_list))
	    (set! fio_return_flag (cadr fx_grain_list))
	    (set! fx_grain_exp (caddr fx_grain_list))

	    ;Grain process execution
	    (if (= fio_return_flag TRUE)
	      (set! fio_uni_layer (eval fx_grain_exp))
	      (eval fx_grain_exp)
	    )

	    ;String modification and layer renaming
	    (set! fs_res_str (string-append fs_res_str fs_pref_grain fs_grain_str))
	  )
	)

	;Returning original foreground and background colors
	(gimp-context-set-foreground fc_fore)
	(gimp-context-set-background fc_back)
	(set! fio_uni_layer (car (gimp-layer-new-from-drawable fio_uni_layer fm_image)))
	(gel-image-insert-layer fm_image fio_uni_layer -1)
	(if (= fm_misc_logout TRUE)
	  (gel-item-set-name fio_uni_layer fs_res_str)
	  (gel-item-set-name fio_uni_layer fs_default_str)
	)
	(set! f-res-str fs_res_str)
	(gimp-displays-flush)
  )

  ;Stage counter reset
  (set! rsk-fk-stage-counter 0)

  ;End of execution
  (if (= rsk-batch-call-state FALSE)
    (begin
      (gimp-image-undo-group-end fm_image)
      (gimp-image-delete rsk-fk-sep-image)
      (rsk-end-handle (string-append rsk-fil-version ": " f-res-str))
    )
    (begin
      (gimp-image-undo-enable fm_image)
      (gimp-context-pop)
    )
  )
)

;rsk-fil-stage-handle
;CORE MODULE
;Input variables:
;BOOLEAN - TRUE returning name list of specified stage / FALSE returning list with execution variables;
;INTEGER - core stage number (required option);
;INTEGER - selected process number (zero if need to return list of process);
;Returning variables:
;LIST - list with strings of the processes names / list with process name and with block of code;
(define (rsk-fil-stage-handle param stage_id proc_id)
(define stage-handle)
  (let* (
	(stage_error (string-append "ЛИПС не нашел стадию с указанным номером:\nstage_id=" (number->string stage_id)))
	(proc_error (string-append "ЛИПС не нашел процесс с указанным номером:\nstage_id=" (number->string stage_id) "\nproc_id=" (number->string proc_id)))
	(curr_error (string-append "Текущая стадия ЛИПС не является региструруемой:\nstage_id=" (number->string stage_id)))
	(stage_counter -1)
	(proc_counter -1)
	(current_stage_list)
	(name_list '())
	(proc_list)
	(temp_list)
	(temp_entry)
	)
	(set! temp_list rsk-fk-stages-list)

	;Recieving list of current stage
	(if (not (or (< stage_id 0) (> stage_id (- (length rsk-fk-stages-list) 1))))
	  (while (< stage_counter stage_id)
	    (begin
	      (set! current_stage_list (car temp_list))
	      (set! stage_counter (+ stage_counter 1))
	      (set! temp_list (cdr temp_list))
	    )
	  )
	  (begin
	    (gimp-message stage_error)
	    (rsk-quit-handle)
	  )
	)

	;Error message if current stage return FALSE
	(if (not (list? current_stage_list))
	  (begin
	    (gimp-message curr_error)
	    (rsk-quit-handle)
	  )
	)

	;Stage processing
	(if (= param TRUE)

	  ;Nmae list generation
	  (begin
	    (while (not (null? current_stage_list))
	      (set! temp_entry (car current_stage_list))
	      (set! name_list (append name_list (list (car temp_entry))))
	      (set! current_stage_list (cdr current_stage_list))
	    )
	    (set! stage-handle name_list)
	  )

	  ;Recieving list with name of process and code block
	  (begin
	    (if (not (or (< proc_id 0) (> proc_id (- (length current_stage_list) 1))))
	      (begin
		(while (< proc_counter proc_id)
		  (set! proc_list (car current_stage_list))
		  (set! proc_counter (+ proc_counter 1))
		  (set! current_stage_list (cdr current_stage_list))
		)
	      )
	      (begin
		(gimp-message proc_error)
		(rsk-quit-handle)
	      )
	    )
	    (set! stage-handle proc_list)
	  )
	)
  )
stage-handle
)

;rsk-fil-source-handle
;CORE MODULE
;Input variables:
;IMAGE - processing image;
;BOOLEAN. - fm_misc_visible raw value;
;Returned variables:
;LAYER - ready layer;
(define (rsk-fil-source-handle image viz)
(define exit)
  (let* (
	(active (car (gimp-image-get-active-layer image)))
	(exit-layer)
	(temp-layer)
	)

	(if (= rsk-batch-call-state FALSE)
	  (begin
	    (set! rsk-fk-sep-image (car (gimp-image-duplicate image)))
	    (gimp-image-undo-disable rsk-fk-sep-image)
	    (if (= viz TRUE)
	      (begin
		(gimp-edit-copy-visible image)
		(set! temp-layer 
		  (car
		    (gimp-edit-paste active TRUE)
		  )
		)
		(gimp-floating-sel-to-layer temp-layer)
		(gel-item-set-name temp-layer "Источник = Видимое")
		(set! exit-layer
		  (car
		    (gimp-layer-new-from-drawable temp-layer rsk-fk-sep-image)
		  )
		)
		(gel-image-insert-layer rsk-fk-sep-image exit-layer -1)
		(gimp-image-remove-layer image temp-layer)
	      )
	      (begin
		(set! exit-layer 
		  (car 
		    (gimp-layer-new-from-drawable active rsk-fk-sep-image)
		  )
		)
		(gel-image-insert-layer rsk-fk-sep-image exit-layer -1)
		(gel-item-set-name exit-layer "Источник = Копия")
	      )
	    )
	  )
	  (begin
	    (if (= viz TRUE)
	      (begin
		(gimp-edit-copy-visible image)
		(set! exit-layer 
		  (car
		    (gimp-edit-paste active TRUE)
		  )
		)
		(gimp-floating-sel-to-layer exit-layer)
		(gel-item-set-name exit-layer "Источник = Видимое")
		(gel-image-raise-item-to-top image exit-layer)
	      )
	      (begin
		(set! exit-layer (car (gimp-layer-copy active FALSE)))
		(gel-image-insert-layer image exit-layer -1)
		(gel-item-set-name exit-layer "Источник = Копия")
	      )
	    )
	  )
	)
	(set! exit exit-layer)
  )
exit
)

;FIL registation part resposible for author rights
(define rsk-fil-credits
  (list
  rsk-reg-author
  rsk-reg-copyright
  rsk-reg-date
  )
)

;FIL registation part responsible for procedure tuning
(define rsk-fil-controls
  (list
  SF-TOGGLE	"Стадия цветокорректировки"	TRUE
  SF-OPTION 	"Цветовой процесс" 		(rsk-fil-stage-handle TRUE 1 0)
  SF-TOGGLE	"Стадия зернистости"		TRUE
  SF-OPTION	"Процесс зернистости"		(rsk-fil-stage-handle TRUE 2 0)
  SF-TOGGLE	"Включить виньетирование"	FALSE
  SF-ADJUSTMENT	"Радиус виньетирования (%)"	'(100 85 125 5 10 1 0)
  SF-ADJUSTMENT	"Мягкость виньетирования (%)"	'(33 20 45 2 5 1 0)
  SF-ADJUSTMENT	"Плотность виньетирования"	'(100 0 100 10 25 1 0)
  SF-OPTION	"Cтепень размытия краев"	'("Отключено" "x1" "x2" "x3")
  SF-ADJUSTMENT	"Коррекция экспозиции"		'(0 -2 2 0.1 0.3 1 0)
  SF-TOGGLE	"Записать опции в имя слоя"	FALSE
  )
)

;rsk-fil-spe-core procedure registration
(apply script-fu-register
  (append
    (list
    "rsk-fil-spe-core"
    (string-append rsk-reg-defpath "_ЛИПС")
    "Лаборатория имитации пленочных снимков"
    )
    rsk-fil-credits
    (list
    "RGB,RGBA*"
    SF-IMAGE	"Изображение"			0
    )
    rsk-fil-controls
    (list
    SF-TOGGLE	"Работать с видимым"		FALSE
    )
  )
)

;Batch core procedure
(define (rsk-fil-spe-batch		;procedure name

	;Batch execution control
	fb_dir_in		;input directory address
	fb_input_format		;input format;
	fb_dir_out		;output directory address;
	fb_out_format		;output format;

	;Color stage control
	fbm_clr_flag		;color proceess execution switch;
	fbm_clr_id		;color process number;

	;Grain stage control
	fbm_grain_flag		;grain proceess execution switch;
	fbm_grain_id		;grain process number;

	;Pre-processes control
	fbm_pre_vign_flag	;vignette activation switch;;
	fbm_pre_vign_rad	;vignette radius in percents;
	fbm_pre_vign_soft	;vignette softness;
	fbm_pre_vign_opc	;vingette opacity;
	fbm_pre_blur_step	;border blur control;
	fbm_pre_xps_control	;exposure correction control;
	
	;Additional options
	fbm_misc_logout		;option output swtitch;
	fbm_misc_random		;random mode switch;
	)

  ;Input format definition
  (define input-ext)
  (cond
    ((= fb_input_format 0) (set! input-ext "*"))
    ((= fb_input_format 1) (set! input-ext "[jJ][pP][gG]"))
    ((= fb_input_format 2) (set! input-ext "[bB][mM][pP]"))
    ((= fb_input_format 3) (set! input-ext "[xX][cC][fF]"))
  )

  ;Output format definition
  (define out-ext)
  (cond
    ((= fb_out_format 0) (set! out-ext "jpg"))
    ((= fb_out_format 1) (set! out-ext "png"))
    ((= fb_out_format 2) (set! out-ext "tif"))
    ((= fb_out_format 3) (set! out-ext "bmp"))
    ((= fb_out_format 4) (set! out-ext "xcf"))
    ((= fb_out_format 5) (set! out-ext "psd"))
  )

  ;Declaration of variables
  (let*	(
	(rsk-sys-sep (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))
	(pattern (string-append fb_dir_in rsk-sys-sep "*." input-ext))
	(filelist (cadr (file-glob pattern 1)))
	(prog_length (length filelist))
	(prog_counter 0)
	)

	;Going into batch state
	(rsk-begin-handle)
	(set! rsk-batch-call-state TRUE)
	(set! rsk-batch-warn-lock FALSE)

	;Cycle begin
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

		;Progress bar setting up
		(set! prog_counter (+ prog_counter 1))
		(gimp-progress-set-text (string-append "Файл " (number->string prog_counter) " из " (number->string prog_length)))
		(gimp-progress-update (/ prog_counter prog_length))

		;Preliminary layer merging
		(if (> fb_input_format 2)
		  (begin
		    (set! srclayer (car (gimp-image-get-active-layer img)))
		    (gimp-edit-copy-visible img)
		    (set! srclayer (car (gimp-edit-paste srclayer TRUE)))
		    (gimp-floating-sel-to-layer srclayer)
		    (gel-item-set-name srclayer "Viz-src")
		    (gel-image-raise-item-to-top img srclayer)
		  )
		  (set! srclayer (car (gimp-image-get-active-layer img)))
		)

		;rsk-fil-spe-core launching
		(if (= fbm_misc_random TRUE)
		  (rsk-fil-spe-core
		    img							;>>fm_image
		    fbm_clr_flag					;>>fm_clr_flag
		    (random (length rsk-fk-clr-stage))			;>>fm_clr_id
		    fbm_grain_flag					;>>fm_grain_flag
		    (random (length rsk-fk-grain-stage))		;>>fm_grain_id
		    (random 1)						;>>fm_pre_vign_flag
		    fbm_pre_vign_rad					;>>fm_pre_vign_rad
		    fbm_pre_vign_soft					;>>fm_pre_vign_soft
		    fbm_pre_vign_opc					;>>fm_pre_vign_opc
		    fbm_pre_blur_step					;>>fm_pre_blur_step
		    fbm_pre_xps_control					;>>fm_pre_xps_control
		    fbm_misc_logout					;>>fm_misc_logout
		    FALSE						;>>fm_misc_visible
		  )
		  (rsk-fil-spe-core 
		    img							;>>fm_image
		    fbm_clr_flag					;>>fm_clr_flag
		    fbm_clr_id						;>>fm_clr_id
		    fbm_grain_flag					;>>fm_grain_flag
		    fbm_grain_id					;>>fm_grain_id
		    fbm_pre_vign_flag					;>>fm_pre_vign_flag
		    fbm_pre_vign_rad					;>>fm_pre_vign_rad
		    fbm_pre_vign_soft					;>>fm_pre_vign_soft
		    fbm_pre_vign_opc					;>>fm_pre_vign_opc
		    fbm_pre_blur_step					;>>fm_pre_blur_step
		    fbm_pre_xps_control					;>>fm_pre_xps_control
		    fbm_misc_logout					;>>fm_misc_logout
		    FALSE						;>>fm_misc_visible
		  )
		)

		;Final layers merging
		(if (< fb_out_format 4)
		  (set! res_layer (car (gimp-image-merge-visible-layers img 0)))
		  (set! res_layer (car (gimp-image-get-active-layer img)))
		)

		;String proceessting and construction output path
		(set! file (substring filename (string-length fb_dir_in) (- (string-length filename) 4 )))
		(set! target_out (string-append fb_dir_out "/" file "_ЛИПС." out-ext))

		;File saving
		(cond
		  ((= fb_out_format 0) (file-jpeg-save 1 img res_layer target_out target_out 1 0 1 1 "" 2 1 0 0))
		  ((= fb_out_format 1) (file-png-save-defaults 1 img res_layer target_out target_out))
		  ((= fb_out_format 2) (file-tiff-save 1 img res_layer target_out target_out 1))
		  ((= fb_out_format 3) (file-bmp-save 1 img res_layer target_out target_out))
		  ((= fb_out_format 4) (gimp-xcf-save 1 img res_layer target_out target_out))
		  ((= fb_out_format 5) (file-psd-save 1 img res_layer target_out target_out 1 0))
		)

		;Image remocing
		(gimp-image-delete img)
	  )

	  ;List offset and cycle's stage ending
	  (set! filelist (cdr filelist))
	)

	;Going out from batch state
	(rsk-end-handle 
	  (string-append 
	  rsk-fil-version " (пакетная обработка):"
	  "\nПапка-источник: " fb_dir_in
	  "\nПапка-назначение: " fb_dir_out
	  "\nКоличество файлов: " (number->string prog_counter)
	  )
	)
	(set! rsk-batch-call-state FALSE)
	(set! rsk-batch-warn-lock FALSE)
  )
)

;rsk-fil-spe-batch procedure registration
(apply script-fu-register
  (append
    (list
    "rsk-fil-spe-batch"
    (string-append rsk-reg-defpath "ЛИПС Кон_вейер")
    "Конвейерное исполнение ЛИПС"
    )
    rsk-fil-credits
    (list
    ""
    SF-DIRNAME	"Папка-источник"	""
    SF-OPTION	"Входящий формат"	'(
					"*"
					"JPG"
					"TIFF"
					"XCF"
					)
    SF-DIRNAME	"Папка-назначение"	""
    SF-OPTION	"Формат сохранения"	'(
					"JPG"
					"PNG"
					"TIF"
					"BMP"
					"XCF"
					"PSD"
					)
    )
    rsk-fil-controls
    (list
    SF-TOGGLE	"Случайный режим"	FALSE
    )
  )
)

;Core section end

;rsk-fil-pre-xps
;PRE-PROCESS
;Input variables:
;LAYER - processing layer;
;INTEGER - exposure correction value;
(define (rsk-fil-pre-xps image layer control)
  (let* (
	(low_input (- 0 (* control 25)))
	(high_input (- 255 (* control 25)))
	)
	(if (> high_input 255)
	  (set! high_input 255)
	)
	(if (< low_input 0)
	  (set! low_input 0)
	)
	(gimp-levels layer 0 low_input high_input 1.0 0 255)
  )
)

;rsk-fil-pre-vignette
;PRE-PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;INTEGER - vignette opacity value;
;INTEGER - vignette softness value;
;INTEGER - vignette radius value;
;COLOR - foreground color;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-pre-vignette image src imh imw vign_opc vign_rad vign_soft fore)
(define vign-exit)
  (let* (
	(p_imh (* (/ imh 100) vign_rad))
	(p_imw (* (/ imw 100) vign_rad))
	(soft_min (if (> imw imh) imh imw))
	(p_soft (* (/ soft_min 100) vign_soft))
	(off_x)
	(off_y)
	(p_big)
	(d_diff)
	(vign (car (gimp-layer-new image imw imh 1 "Виньетирование" 100 0)))
	(norm_vign)
	)
	(if (> p_imh p_imw)
	  (begin
	    (set! p_big p_imh)
	    (set! d_diff (/ (- p_imh p_imw) 2))
	    (if (< vign_rad 100)
	      (begin
		(set! off_x (- (/ (- imw p_imw) 2) d_diff))
		(set! off_y (/ (- imh p_imh) 2))
	      )
	      (begin
		(set! off_x (- (/ (- p_imw imw) -2) d_diff))
		(set! off_y (/ (- p_imh imh) -2))
	      )
	    )
	  )
	  (begin
	    (set! p_big p_imw)
	    (set! d_diff (/ (- p_imw p_imh) 2))
	    (if (< vign_rad 100)
	      (begin
		(set! off_x (/ (- imw p_imw) 2))
		(set! off_y (- (/ (- imh p_imh) 2) d_diff))
	      )
	      (begin
		(set! off_x (/ (- p_imw imw) -2))
		(set! off_y (- (/ (- p_imh imh) -2) d_diff))
	      )
	    )
	  )
	)
	(gel-image-insert-layer image vign -1)
	(gimp-drawable-fill vign 3)
	(gimp-context-set-foreground '(0 0 0))
	(gel-image-select-ellipse image off_x off_y p_big p_big 0 TRUE TRUE 0)
	(gimp-selection-invert image)
	(gimp-selection-feather image p_soft)
	(gimp-edit-bucket-fill vign 0 0 100 0 FALSE 0 0)
	(gimp-selection-none image)
	(gimp-context-set-foreground fore)
	(set! norm_vign (car (gimp-layer-copy vign TRUE)))
	(gel-image-insert-layer image norm_vign -1)
	(gimp-layer-set-mode vign 5)
	(gimp-layer-set-mode norm_vign 3)
	(gimp-layer-set-opacity vign vign_opc)
	(gimp-layer-set-opacity norm_vign (/ vign_opc 5))
	(set! src
	  (car
	    (gimp-image-merge-down image vign 0)
	  )
	)
	(set! src
	  (car
	    (gimp-image-merge-down image norm_vign 0)
	  )
	)
	(plug-in-autocrop-layer 1 image src)
	(set! vign-exit src)
  )
vign-exit
)

;rsk-fil-prefx-badblur
;PRE-PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;INTEGER - blur step value;
(define (rsk-fil-prefx-badblur image layer imh imw ext)
  (set! ext (+ ext 1))
  (if (= rsk-fixca-def TRUE)
    (begin
      (Fix-CA 1 image layer (+ 1.5 ext) (- -1.5 ext) 1 0 0 0 0)
    )
  )
  (plug-in-mblur 1 image layer 2 (/ (+ (/ imh (/ 3500 ext)) (/ imw (/ 3500 ext))) 2) 0 (/ imw 2) (/ imh 2))
)

;rsk-fil-clr-sov
;COLOR PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;INTEGER - tone opacity value;
;INTEGER - red veil opacity value;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-clr-sov image layer imh imw opc_tone opc_red)
(define sov-exit)
  (let* (
	(first (car (gimp-layer-copy layer FALSE)))
	(red (car (gimp-layer-new image imw imh 0 "Тон маски" 100 0)))
	(red_mask)
	)
	(gimp-hue-saturation layer 0 5 0 -30)
	(gel-image-insert-layer image first -1)
	(gel-image-insert-layer image red -1)
	(set! red_mask
	  (car
	    (gimp-layer-create-mask layer 5)
	  )
	)
	(gimp-layer-add-mask red red_mask)
	(gimp-colorize first 48 18 0)
	(gimp-colorize red 0 75 20)
	(gimp-levels red_mask 0 33 120 1.0 0 255)
	(gimp-levels red 0 0 255 1.0 20 255)
	(gimp-invert red_mask)
	(gimp-layer-set-opacity first opc_tone)
	(gimp-layer-set-opacity red opc_red)
	(set! layer
	  (car
	    (gimp-image-merge-down image first 0)
	  )
	)
	(set! layer
	  (car
	    (gimp-image-merge-down image red 0)
	  )
	)
	(gimp-hue-saturation layer 0 0 0 30)
	(gimp-curves-spline layer 0 6 #(0 0 105 125 255 255))
	(set! sov-exit layer)
  )
sov-exit
)

;rsk-fil-clr-monochrome
;COLOR PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;COLOR - foreground color;
;INTEGER - action handler;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-clr-monochrome image layer imh imw foreground act_id)
(define monochrome-exit)
  (let* (
	(paper 0)
	)

	(set! paper (car (gimp-layer-new image imw imh 0 "Фотобумага" 100 0)))
	(gel-image-insert-layer image paper -1)
	(gimp-context-set-foreground '(224 213 184))
	(gimp-drawable-fill paper 0)
	(gel-image-lower-item image paper)
	(gimp-layer-set-mode layer 9)
	(set! layer
	  (car
	    (gimp-image-merge-down image layer 0)
	  )
	)
	(gimp-context-set-foreground foreground)

	(cond
	  ((= act_id 0) (plug-in-colors-channel-mixer 1 image layer TRUE 0.1 0.3 0.7 0 0 0 0 0 0))
	  ((= act_id 1)
	    (begin
	      (gimp-colorize layer 34 20 0)
	      (gimp-curves-spline layer 1 4 #(0 40 255 255))
	      (gimp-curves-spline layer 2 4 #(0 12 255 255))
	    )
	  )
	)

	(gimp-curves-spline layer 0 4 #(0 0 220 255))

	(set! monochrome-exit layer)
  )
monochrome-exit
)

;rsk-fil-clr-lomo
;COLOR PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - color process number;
;This procedure bases on Lomo Script by Elsamuko (http://registry.gimp.org/node/7870)
;code by Donncha O Caoimh (donncha@inphotos.org) and Elsamuko (elsamuko@web.de)
(define (rsk-fil-clr-lomo image layer cid)
  (if (= cid 0)
    (begin
      (gimp-curves-spline layer 1 10 #(0 0 80 84 149 192 191 248 255 255))
      (gimp-curves-spline layer 2 8 #(0 0 70 81 159 220 255 255))
      (gimp-curves-spline layer 3 4 #(0 27 255 213))
    )
  )
  (if (= cid 1)
    (begin
      (gimp-curves-spline layer 0 8 #(0 0 39 53 146 124 225 255))
      (gimp-curves-spline layer 1 10 #(0 0 34 20 66 56 127 175 255 255))
      (gimp-curves-spline layer 2 8 #(0 0 51 35 99 133 255 255))
      (gimp-curves-spline layer 3 10 #(0 0 23 32 47 57 159 115 220 255))
    )
  )
  (if (= cid 2)
    (begin
      (gimp-curves-spline layer 0 6 #(10 0 103 134 243 255))
      (gimp-curves-spline layer 1 6 #(0 0 125 153 255 255))
      (gimp-curves-spline layer 2 6 #(0 19 133 114 255 255))
      (gimp-curves-spline layer 3 6 #(0 27 176 111 255 255))
    )
  )
)

;rsk-fil-clr-duo
;COLOR PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - affect opacity value;
;COLOR - light area color;
;COLOR - dark area color;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-clr-duo image layer opc_affect light_color dark_color)
(define duo-exit)
  (let* (
	(affect (car (gimp-layer-copy layer FALSE)))
	(light (car (gimp-layer-copy layer FALSE)))
	(dark (car (gimp-layer-copy layer FALSE)))
	(lightmask)
	)
	(gel-image-insert-layer image dark -1)
	(gel-image-insert-layer image affect -1)
	(gel-image-insert-layer image light -1)
	(gimp-desaturate affect)
	(gimp-levels affect 0 60 195 1.0 0 255)
	(set! lightmask
	  (car
	    (gimp-layer-create-mask affect 5)
	  )
	)
	(gimp-layer-add-mask light lightmask)
	(plug-in-colorify 1 image light light_color)
	(plug-in-colorify 1 image dark dark_color)
	(gimp-layer-set-mode light 13)
	(gimp-layer-set-mode dark 13)
	(gimp-layer-set-mode affect 5)
	(gimp-hue-saturation light 0 0 0 100)
	(gimp-layer-set-opacity affect opc_affect)
	(set! layer (car (gimp-image-merge-down image dark 0)))
	(set! layer (car (gimp-image-merge-down image affect 0)))
	(set! layer (car (gimp-image-merge-down image light 0)))
	(gimp-hue-saturation layer 0 0 0 25)
	(set! duo-exit layer)
  )
duo-exit
)

;rsk-fil-clr-vintage
;COLOR PROCESSES
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;INTEGER - cyan layer opacity;
;INTEGER - magneta layer opacity;
;INTEGER - yellow layer opacity;
;BOOLEAN - overlay switch;
;Returned variables:
;LAYER - processed layer;
;This procedure bases on Vintage Look script by Michael Maier (http://registry.gimp.org/node/1348)
;code by Michael Maier (info@mmip.net) and Elsamuko (elsamuko@web.de)
(define (rsk-fil-clr-vintage img drw imh imw VarCyan VarMagenta VarYellow Overlay)
(define vint-exit)
  (let* (
	(overlay-layer (car (gimp-layer-copy drw FALSE)))
	(cyan-layer)
	(magenta-layer)
	(yellow-layer)
	)

	;Bleach Bypass
	(if(= Overlay TRUE)
	  (begin
	    (gel-image-insert-layer img overlay-layer -1)
	    (gimp-desaturate-full overlay-layer DESATURATE-LUMINOSITY)
	    (plug-in-gauss TRUE img overlay-layer 1 1 TRUE)
	    (plug-in-unsharp-mask 1 img overlay-layer 1 1 0)
	    (gimp-layer-set-mode overlay-layer OVERLAY-MODE)
	  )
	)

	;Yellow Layer
	(set! yellow-layer (car (gimp-layer-new img imw imh RGB "Yellow" 100  MULTIPLY-MODE)))
	(gel-image-insert-layer img yellow-layer -1)
	(gimp-context-set-background '(251 242 163))
	(gimp-drawable-fill yellow-layer BACKGROUND-FILL)
	(gimp-layer-set-opacity yellow-layer VarYellow)

	;Magenta Layer
	(set! magenta-layer (car (gimp-layer-new img imw imh RGB "Magenta" 100  SCREEN-MODE)))
	(gel-image-insert-layer img magenta-layer -1)
	(gimp-context-set-background '(232 101 179))
	(gimp-drawable-fill magenta-layer BACKGROUND-FILL)
	(gimp-layer-set-opacity magenta-layer VarMagenta)

	;Cyan Layer 
	(set! cyan-layer (car (gimp-layer-new img imw imh RGB "Cyan" 100  SCREEN-MODE)))
	(gel-image-insert-layer img cyan-layer -1)
	(gimp-context-set-background '(9 73 233))
	(gimp-drawable-fill cyan-layer BACKGROUND-FILL)
	(gimp-layer-set-opacity cyan-layer VarCyan)

	;End
	(if (= Overlay TRUE)
	  (set! drw (car (gimp-image-merge-down img overlay-layer 0)))
	)
	(set! drw (car (gimp-image-merge-down img yellow-layer 0)))
	(set! drw (car (gimp-image-merge-down img magenta-layer 0)))
	(set! drw (car (gimp-image-merge-down img cyan-layer 0)))
	(set! vint-exit drw)
  )
vint-exit
)

;rsk-fil-clr-chrome
;COLOR PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;COLOR - screen merge color;
;COLOR - multiply color;
;INTEGER - contrast opacity variable;
;INTEGER - b/w opacity variable;
;INTEGER - gradient begin offset;
;INTEGER - gradient end offset;
;BOOLEAN - b/w dodging switch;
;BOOLEAN - retro mode switch;
;Returned variables:
;LAYER - processed layer;
;This procedure bases on Photochrom script by Elsamuko (http://registry.gimp.org/node/24197)
;code by Elsamuko (elsamuko@web.de)
(define (rsk-fil-clr-chrome image layer imh imw color1 color2 contrast bw-merge num1 num2 gray retro)
(define chrome-exit)
  (let* (
	(offset1 (* imh (/ num1 100)))
	(offset2 (* imh (/ num2 100)))
	(dodge-layer (car (gimp-layer-copy layer FALSE)))
	(contrast-layer1 (car (gimp-layer-copy layer FALSE)))
	(contrast-layer2 (car (gimp-layer-copy layer FALSE)))
	(bw-screen-layer (car (gimp-layer-copy layer FALSE)))         
	(bw-merge-layer (car (gimp-layer-copy layer FALSE)))         
	(extra-layer)
	(merge-layer (car (gimp-layer-new image imw imh RGBA-IMAGE "Grain Merge" 50 GRAIN-MERGE-MODE)))
	(merge-mask (car (gimp-layer-create-mask merge-layer ADD-WHITE-MASK)))
	(screen-layer (car (gimp-layer-new image imw imh RGBA-IMAGE "Screen" 10 SCREEN-MODE)))
	(screen-mask (car (gimp-layer-create-mask screen-layer ADD-WHITE-MASK)))
	(multiply-layer (car (gimp-layer-new image imw imh RGBA-IMAGE "Multiply" 10 MULTIPLY-MODE)))
	(multiply-mask (car (gimp-layer-create-mask multiply-layer ADD-WHITE-MASK)))
	(retro-layer (car (gimp-layer-new image imw imh RGBA-IMAGE "Retro 1" 60 MULTIPLY-MODE)))
	(floatingsel)
	(retro-mask (car (gimp-layer-create-mask retro-layer ADD-WHITE-MASK)))
	(retro-layer2 (car (gimp-layer-new image imw imh RGBA-IMAGE "Retro 2" 20 SCREEN-MODE)))
	(gradient-layer (car (gimp-layer-new image imw imh RGBA-IMAGE "Gradient Overlay" 100 OVERLAY-MODE)))
	)

	;set BW screen layer
	(gel-image-insert-layer image bw-screen-layer -1)
	(gimp-layer-set-mode bw-screen-layer SCREEN-MODE)
	(gimp-layer-set-opacity bw-screen-layer 50)
	(gimp-desaturate-full bw-screen-layer DESATURATE-LUMINOSITY)

	;set BW merge layer
	(gel-image-insert-layer image bw-merge-layer -1)
	(gimp-layer-set-mode bw-merge-layer GRAIN-MERGE-MODE)
	(gimp-layer-set-opacity bw-merge-layer bw-merge)
	(gimp-desaturate-full bw-merge-layer DESATURATE-LUMINOSITY)
	(gimp-curves-spline bw-merge-layer HISTOGRAM-VALUE 6 #(0 144 88 42 255 255))

	;set contrast layers
	(gel-image-insert-layer image contrast-layer1 -1)
	(gimp-layer-set-mode contrast-layer1 OVERLAY-MODE)
	(gimp-layer-set-opacity contrast-layer1 contrast)
	(gimp-desaturate-full contrast-layer1 DESATURATE-LUMINOSITY)

	(gel-image-insert-layer image contrast-layer2 -1)
	(gimp-layer-set-mode contrast-layer2 OVERLAY-MODE)
	(gimp-layer-set-opacity contrast-layer2 contrast)
	(gimp-desaturate-full contrast-layer2 DESATURATE-LUMINOSITY)
    
	;set dodge layer
	(gel-image-insert-layer image dodge-layer -1)
	(gimp-layer-set-mode dodge-layer DODGE-MODE)
	(gimp-layer-set-opacity dodge-layer 50)
    
	;set merge layer
	(gel-image-insert-layer image merge-layer -1)
	(gimp-selection-all image)
	(gimp-context-set-foreground color1)
	(gimp-edit-bucket-fill merge-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
	(gimp-layer-add-mask merge-layer merge-mask)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-context-set-background '(0 0 0))
	(gimp-edit-blend merge-mask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 0 TRUE 0 offset1 0 offset2)
    
	;set screen layer
	(gel-image-insert-layer image screen-layer -1)
	(gimp-selection-all image)
	(gimp-context-set-foreground color1)
	(gimp-edit-bucket-fill screen-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
	(gimp-layer-add-mask screen-layer screen-mask)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-context-set-background '(0 0 0))
	(gimp-edit-blend screen-mask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 0 TRUE 0 offset1 0 offset2)

	;set multiply layer
	(gel-image-insert-layer image multiply-layer -1)
	(gimp-selection-all image)
	(gimp-context-set-foreground color2)
	(gimp-edit-bucket-fill multiply-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
	(gimp-layer-add-mask multiply-layer multiply-mask)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-context-set-background '(0 0 0))
	(gimp-edit-blend multiply-mask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 0 TRUE 0 offset1 0 offset2)
    
	;optional retro colors
	(if(= retro TRUE)
	  (begin

	    ;yellow with mask
	    (gel-image-insert-layer image retro-layer -1)
	    (gimp-selection-all image)
	    (gimp-context-set-foreground '(251 242 163))
	    (gimp-edit-bucket-fill retro-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
	    (gimp-layer-add-mask retro-layer retro-mask)
	    (gimp-edit-copy contrast-layer1)
	    (set! floatingsel (car (gimp-edit-paste retro-mask TRUE)))
	    (gimp-floating-sel-anchor floatingsel)
           
	    ;rose
	    (gel-image-insert-layer image retro-layer2 -1)
	    (gimp-selection-all image)
	    (gimp-context-set-foreground '(232 101 179))
	    (gimp-edit-bucket-fill retro-layer2 FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)

	    ;gradient overlay
	    (gel-image-insert-layer image gradient-layer -1)
	    (gimp-context-set-foreground '(255 255 255))
	    (gimp-context-set-background '(0 0 0))
	    (gimp-edit-blend gradient-layer FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE 0 offset1 0 offset2)

	    ;deactivate orange layers
	    (gel-item-set-visible merge-layer FALSE)
	    (gel-item-set-visible screen-layer FALSE)
	    (gel-item-set-visible multiply-layer FALSE)
	  )
	)
    
	;make source layer gray
	(if(= gray TRUE)
	    (gimp-hue-saturation layer 0 0 0 -70)
	)

	;layers merging
	(set! layer (car (gimp-image-merge-down image bw-screen-layer 0)))
	(set! layer (car (gimp-image-merge-down image bw-merge-layer 0)))
	(set! layer (car (gimp-image-merge-down image contrast-layer1 0)))
	(set! layer (car (gimp-image-merge-down image contrast-layer2 0)))
	(set! layer (car (gimp-image-merge-down image dodge-layer 0)))
	(set! layer (car (gimp-image-merge-down image merge-layer 0)))
	(set! layer (car (gimp-image-merge-down image screen-layer 0)))
	(set! layer (car (gimp-image-merge-down image multiply-layer 0)))
	(if (= retro TRUE)
	  (begin
	    (set! layer (car (gimp-image-merge-down image retro-layer 0)))
	    (set! layer (car (gimp-image-merge-down image retro-layer2 0)))
	    (set! layer (car (gimp-image-merge-down image gradient-layer 0)))
	  )
	)
	(set! chrome-exit layer)
  )
chrome-exit
)

;rsk-fil-clr-dram
;COLOR PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;COLOR - overlay and tone color;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-clr-dram image layer input_color)
(define dram-c-exit)
  (let* (
	(color_layer 0)
	(over_layer 0)
	)
	(set! over_layer (car (gimp-layer-copy layer FALSE)))
	(gel-image-insert-layer image over_layer -1)
	(plug-in-colorify 1 image over_layer input_color)
	(gimp-layer-set-mode over_layer 5)
	(set! color_layer (car (gimp-layer-copy over_layer FALSE)))
	(gel-image-insert-layer image color_layer -1)
	(gimp-layer-set-mode color_layer 13)
	(gimp-layer-set-opacity color_layer 40)
	(set! layer (car (gimp-image-merge-down image over_layer 0)))
	(set! layer (car (gimp-image-merge-down image color_layer 0)))
	(set! dram-c-exit layer)
  )
dram-c-exit
)

;rsk-fil-grn-adv_grain
;GRAIN PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;COLOR - foreground color;
;BOOLEAN - grain amplification switch;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-grn-adv_grain image clr_res imh imw foreground boost)
(define adv-exit)
  (let* (
	(rel_step (if (> imh imw) (/ imh 800) (/ imw 800)))
	(grain_boost)
	(grain)
	(grain_mask)
	)
	(set! grain 
	  (car 
	    (gimp-layer-new image imw imh 0 "Зерно+" 100 0)
	  )
	)
	(gel-image-insert-layer image grain -1)
	(gimp-context-set-foreground '(128 128 128))
	(gimp-drawable-fill grain 0)
	(plug-in-hsv-noise 1 image grain 2 3 0 25)
	(gimp-brightness-contrast grain 0 80)
	(gimp-layer-set-mode grain 5)
	(gimp-brightness-contrast grain 0 55)
	(gimp-context-set-foreground foreground)
	(set! grain_mask
	  (car
	    (gimp-layer-create-mask clr_res 5)
	  )
	)
	(gimp-layer-add-mask grain grain_mask)
	(gimp-curves-spline grain_mask 0 6 #(0 80 128 128 255 80))
	(gimp-brightness-contrast grain_mask 50 60)
	(if (= boost TRUE)
	  (begin
	    (set! grain_boost (car (gimp-layer-copy grain FALSE)))
	    (gel-image-insert-layer image grain_boost -1)
	  )
	)
	(set! clr_res (car (gimp-image-merge-down image grain 0)))
	(if (= boost TRUE)
	  (set! clr_res (car (gimp-image-merge-down image grain_boost 0)))
	)
	(plug-in-gauss-iir2 1 image clr_res rel_step rel_step)
	(set! adv-exit clr_res)
  )
adv-exit
)

;rsk-fil-grnfx-sulfide
;GRAIN PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;COLOR - foreground color;
;REAL - grain scale;
;BOOLEAN - grunge-mode switch;
;BOOLEAN - scratch-mode switch;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-grnfx-sulfide image layer imh imw foreground scale_step grunge_switch scratch_switch)
(define sulf-exit)
  (let* (
	(sc_imh (/ imh scale_step))
	(sc_imw (/ imw scale_step))
	(scale_layer)
	(grain_layer)
	(grunge_layer)
	(grain_mask)
	(rel_step (if (> imh imw) (/ imh 1100) (/ imw 1100)))
	)

	(if (= scratch_switch TRUE)
	  (if (= rsk-gmic-def TRUE)
	    (plug-in-gmic 1 image layer 1 "-apply_channels \"-stripes_y 2\",7")
	    (rsk-dep_warn-handle 0)
	  )
	)

	(set! scale_layer
	  (car 
	    (gimp-layer-new image sc_imw sc_imh 0 "Слой масштаба" 100 0)
	  )
	)
	(gel-image-insert-layer image scale_layer -1)
	(gimp-context-set-foreground '(128 128 128))
	(gimp-drawable-fill scale_layer 0)
	(plug-in-hsv-noise 1 image scale_layer 2 3 0 25)
	(gimp-layer-set-mode scale_layer 5)
	(gimp-brightness-contrast scale_layer 0 75)
	(set! grain_mask
	  (car
	    (gimp-layer-create-mask layer 5)
	  )
	)
	(set! grain_layer 
	  (car 
	    (gimp-layer-new image imw imh 0 "Нормальное зерно" 100 0)
	  )
	)
	(gel-image-insert-layer image grain_layer -1)
	(gimp-layer-add-mask grain_layer grain_mask)
	(gimp-curves-spline grain_mask 0 6 #(0 80 128 128 255 80))
	(gimp-drawable-fill grain_layer 0)
	(plug-in-hsv-noise 1 image grain_layer 2 3 0 25)
	(gimp-brightness-contrast grain_layer 0 80)
	(gimp-layer-set-mode grain_layer 5)
	(gimp-layer-scale scale_layer imw imh FALSE)
	(gimp-brightness-contrast scale_layer 0 35)
	(gimp-layer-set-opacity scale_layer 45)
	(gimp-layer-resize-to-image-size scale_layer)
	(gimp-context-set-foreground foreground)

	(if (= grunge_switch TRUE)
	  (begin
	    (set! grunge_layer 
	      (car 
		(gimp-layer-new image imw imh 0 "Гранж" 100 0)
	      )
	    )
	    (gel-image-insert-layer image grunge_layer -1)
	    (gel-image-lower-item image grunge_layer)
	    (gel-image-lower-item image grunge_layer)
	    (plug-in-plasma 1 image grunge_layer 0 5.0)
	    (gimp-desaturate grunge_layer)
	    (gimp-layer-set-mode grunge_layer 5)
	    (gimp-layer-set-opacity grunge_layer 35)
	    (set! layer
	      (car
		(gimp-image-merge-down image grunge_layer 0)
	      )
	    )
	  )
	)

	(set! layer
	  (car
	    (gimp-image-merge-down image scale_layer 0)
	  )
	)
	(set! layer
	  (car
	    (gimp-image-merge-down image grain_layer 0)
	  )
	)
	(plug-in-gauss-iir2 1 image layer rel_step rel_step)
	(set! sulf-exit layer)
  )
sulf-exit
)

;rsk-fil-grnfx-dram
;GRAIN PROCESS
;Input variables:
;IMAGE - processing image;
;LAYER - processing layer;
;INTEGER - image height value;
;INTEGER - image width value;
;COLOR - foreground color;
;INTEGER - sharping value;
;BOOLEAN - scratch-mode switch;
;Returned variables:
;LAYER - processed layer;
(define (rsk-fil-grnfx-dram image layer imh imw foreground sharp_opc scratch_switch)
(define dram-g-exit)

  (let* (
	(sc_imh (/ imh 3.5))
	(sc_imw (/ imw 3.5))
	(sc_imd1 sc_imw)
	(sc_imd2 sc_imh)
	(r_imd1 imw)
	(r_imd2 imh)
	(scale_layer)
	(scale_mask)
	(scratch_layer)
	(scratch_mask)
	(rotate_temp FALSE)
	(blur_layer)
	(grey_layer)
	(boost_layer)
	(boost (/ (* 128 (+ 100 0)) 200))
	)

	(if (< imh imw)
	  (begin
	    (gimp-image-rotate image 0)
	    (set! rotate_temp TRUE)
	    (set! r_imd1 imh)
	    (set! r_imd2 imw)
	    (set! sc_imd1 sc_imh)
	    (set! sc_imd2 sc_imw)
	  )
	)

	(if (= scratch_switch TRUE)
	  (begin
	    (gimp-context-set-foreground '(128 128 128))
	    (set! scratch_layer (car (gimp-layer-new image r_imd1 r_imd2 0 "Царапины" 100 0)))
	    (gel-image-insert-layer image scratch_layer -1)
	    (if (= rsk-gmic-def TRUE)
	      (plug-in-gmic 1 image scratch_layer 1 "-stripes_y 2")
	      (rsk-dep_warn-handle 0)
	    )
	    (set! scratch_mask (car (gimp-layer-create-mask scratch_layer 1)))
	    (gimp-layer-add-mask scratch_layer scratch_mask)
	    (plug-in-plasma 1 image scratch_mask 0 4.5)
	    (gimp-curves-spline scratch_mask 0 4 #(0 25 255 255))
	    (gimp-layer-set-mode scratch_layer 7)
	    (set! layer (car (gimp-image-merge-down image scratch_layer 0)))
	  )
	)

	(set! scale_layer
	  (car 
	    (gimp-layer-new image sc_imd1 sc_imd2 0 "Слой масштаба" 100 0)
	  )
	)
	(gel-image-insert-layer image scale_layer -1)
	(gimp-context-set-foreground '(128 128 128))
	(gimp-drawable-fill scale_layer 0)
	(plug-in-hsv-noise 1 image scale_layer 2 3 0 25)
	(gimp-layer-set-mode scale_layer 5)
	(gimp-brightness-contrast scale_layer 0 75)
	(set! scale_mask
	  (car
	    (gimp-layer-create-mask layer 5)
	  )
	)
	(gimp-layer-scale scale_layer r_imd1 r_imd2 FALSE)
	(gimp-brightness-contrast scale_layer 0 35)
	(gimp-layer-set-opacity scale_layer 60)
	(gimp-layer-resize-to-image-size scale_layer)
	(gimp-layer-add-mask scale_layer scale_mask)
	(gimp-curves-spline scale_mask 0 6 #(0 80 128 128 255 80))
	(gimp-context-set-foreground foreground)

	(set! layer
	  (car
	    (gimp-image-merge-down image scale_layer 0)
	  )
	)

	(if (> sharp_opc 0)
	  (begin
	    ;Код был взят из скрипта highpass sharpening, автор - Andreas Schönfelder
	    ;http://registry.gimp.org/node/21165
	    (set! grey_layer (car (gimp-layer-copy layer FALSE)))
	    (gel-image-insert-layer image grey_layer -1)
	    (gimp-desaturate grey_layer)
	    (set! blur_layer (car (gimp-layer-copy grey_layer FALSE)))
	    (gel-image-insert-layer image blur_layer -1)
	    (plug-in-gauss-rle 1 image blur_layer 10 1 1)
	    (gimp-invert blur_layer)
	    (gimp-layer-set-opacity blur_layer 50)
	    (set! grey_layer (car (gimp-image-merge-down image blur_layer 0)))
	    (gimp-levels grey_layer HISTOGRAM-VALUE boost (- 255 boost) 1 0 255)
	    (gimp-curves-spline grey_layer HISTOGRAM-VALUE 10 #(95 0 127 128 154 184 222 240 255 255))
	    (gimp-layer-set-opacity grey_layer sharp_opc)
	    (gimp-layer-set-mode grey_layer OVERLAY-MODE)
	    (set! boost_layer (car (gimp-layer-copy grey_layer FALSE)))
	    (gel-image-insert-layer image boost_layer -1)
	    (set! layer (car (gimp-image-merge-down image grey_layer 0)))
	    (set! layer (car (gimp-image-merge-down image boost_layer 0)))
	  )
	)

	(if (= rotate_temp TRUE)
	  (gimp-image-rotate image 2)
	)

	(set! dram-g-exit layer)
  )
dram-g-exit
)