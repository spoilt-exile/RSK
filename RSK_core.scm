;RSK core script v0.8
;
;==============================================================================================
;                  Данный код распространяется на условиях лицензии Free
;==============================================================================================
;
;
;Version history:
;==============================================================================================
;ver. 0.3 (October 7 2010)
; - basic RSK core implementation;
;==============================================================================================
;ver. 0.5 (April 18 2011)
; - initialization system added;
; - begin/end procedure handlers with additional error handling;
; - RSK global core states;
; - RSK development core state for improvment debug;
;==============================================================================================
;
;==============================================================================================
;Procedure name:		Status:			Revision:
;==============================================================================================
;rsk-selftest-return		stable			r4
;rsk-selftest-handle		stable			r0
;rsk-dep_warn-handle		stable			r2
;rsk-begin-handle		stable			r0
;rsk-end-handle			stable			r1
;rsk-quit-handle		stable			r3
;rsk-debug-message		stable			r0
;rsk-init			stable			r5
;==============================================================================================

;Release digit
(define rsk-init-release 2)

;Release string
(define rsk-release (string-append "RSK R" (number->string rsk-init-release) ""))

;RSK recomended GIMP version
(define rsk-gimp-branch "2.6")

;RSK recomended GIMP version
(define rsk-gimp-version (string-append rsk-gimp-branch ".11"))

;OS directory separator
(define rsk-sys-sep (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))

;Author registration string
(define rsk-reg-author "Непочатов Станислав <spoilt.exile[at]gmail.com>")

;Copyright registration string
(define rsk-reg-copyright "© RED STORM STUDIO 2011")

;RSK license registration string
(define rsk-reg-license "\nРаспротранение на условиях свободной лицензии (Free for any purpose)")

;Date registration string
(define rsk-reg-date "18 Сентября 2011")

;Default RSK menu path string
(define rsk-reg-defpath (string-append "<Image>/Filters/" rsk-release "/"))

;Plugin activation area

;G'MIC define variable
(define rsk-gmic-def (if (defined? 'plug-in-gmic) TRUE FALSE))

;Fix-CA define variable
(define rsk-fixca-def (if (defined? 'Fix-CA) TRUE FALSE))

;greycstoration define variable
(define rsk-greycstoration-def (if (defined? 'plug-in-greycstoration) TRUE FALSE))

;Plugin information global list
(define rsk-plugs-list
  (list

    ;G'MIC info entry
    (list "G'MIC" "http://registry.gimp.org/node/13469" rsk-gmic-def)

    ;Fix-CA info entry
    (list "Fix-CA" "http://registry.gimp.org/node/3726" rsk-fixca-def)
  )
)

;RSK Core states

;Core state for batch mode
(define rsk-batch-call-state FALSE)

;Core state for rsk-dep_warn-handle lock
(define rsk-batch-warn-lock FALSE)

;Core state for ongoing evaluation
(define rsk-core-run-state FALSE)

;Core variable for begin time
(define rsk-core-begin-time)

;Core variable for end time
(define rsk-core-end-time)

;Core debug mode variable
(define rsk-core-dev-state FALSE)

;Core init error state
(define rsk-core-init-error FALSE)

;rsk-selftest-return
;Selftest generation procedure
;Has no arguments
;RETURNS:
;STRING - selftest message string;
(define (rsk-selftest-return)
  (let* (
	(finded "найден.")
	(not_finded "не найден.\nПожалуйста установите плагин используя ссылку:")
	(line "\n")
	(space " ")
	(temp_list)
	(temp_entry)
	(plug_name)
	(plug_url)
	(plug_var)
	(test_message "Проверка целостности пакета RSK\n\nПроверка версии GIMP:")
	(plug_report "")
	(plug_fail FALSE)
	(version_string (car (gimp-version)))
	)
	(set! temp_list rsk-plugs-list)

	;GIMP version check
	(if (not (equal? (substring version_string 0 3) rsk-gimp-branch))
	  (begin
	    (set! test_message
	      (string-append
	      test_message
	      " (ОШИБКА!)\nТекущая версия GIMP (" version_string ") не является рекомендуемой версией для пакета " rsk-release "."
	      "\nПакет RSK наиболее совместим с GIMP " rsk-gimp-branch ".x."
	      "\nРекомендуемой версией GIMP является " rsk-gimp-version "." 
	      )
	    )
	    (set! rsk-core-init-error TRUE)
	  )
	  (set! test_message
	    (string-append
	    test_message
	    " (ПРОЙДЕНО)\nТекущая версия GIMP (" version_string ") полностью совместима с пакетом " rsk-release "."
	    )
	  )
	)

	;Pligin report generation
	(set! test_message (string-append test_message "\n\nПроверка расширений:"))
	(while (not (null? temp_list))
	  (set! temp_entry (car temp_list))
	  (set! plug_name (car temp_entry))
	  (set! plug_url (cadr temp_entry))
	  (set! plug_var (caddr temp_entry))
	  (if (= plug_var TRUE)
	    (set! plug_report (string-append plug_report plug_name space finded))
	    (begin
	      (set! plug_report (string-append plug_report plug_name space not_finded line plug_url))
	      (set! rsk-core-init-error TRUE)
	      (set! plug_fail TRUE)
	    )
	  )
	  (set! plug_report (string-append plug_report line))
	  (set! temp_list (cdr temp_list))
	)
	(if (= plug_fail TRUE)
	  (set! test_message
	    (string-append
	    test_message
	    " (ОШИБКА!)\n"
	    plug_report
	    )
	  )
	  (set! test_message
	    (string-append
	    test_message
	    " (ПРОЙДЕНО)\n"
	    plug_report
	    )
	  )
	)

	;Returning string
	test_message
  )
)

;rsk-selftest-handle
;Selftest info procedure
;Has no arguments
(define (rsk-selftest-handle)
  (define out-string)
  (set! out-string (rsk-selftest-return))
  (set! out-string (string-append out-string "\n" rsk-release "\n" "© RED STORM STUDIO 2011"))
  (gimp-message out-string)
)

(script-fu-register 
"rsk-selftest-handle"
(string-append rsk-reg-defpath "RSK Диагностика пакета")
"Проверить наличие необходимых для RSK расширений"
rsk-reg-author
(string-append rsk-reg-copyright rsk-reg-license)
rsk-reg-date
""
)

;rsk-dep_warn-handle
;Plugin warning procedure
;LIST OF ARGUMENTS:
;INTEGER - dependency number in rsk-plugs-list;
(define (rsk-dep_warn-handle dep_id)
  (let* (
	(temp_list rsk-plugs-list)
	(temp_entry)
	(temp_id 0)
	(dep_name)
	(dep_url)
	(dep_var)
	)

	(if (< dep_id (length rsk-plugs-list))
	  (begin
	    (while (< temp_id (length rsk-plugs-list))
	      (if (= temp_id dep_id)
		(begin
		  (set! temp_entry (car temp_list))
		  (set! dep_name (car temp_entry))
		  (set! dep_url (cadr temp_entry))
		  (set! dep_var (caddr temp_entry))
		)
	      )
	      (set! temp_id (+ temp_id 1))
	      (set! temp_list (cdr temp_list))
	    )
	    (if (= dep_var FALSE)
	      (begin
		(if (= rsk-batch-warn-lock FALSE)
		  (gimp-message 
		    (string-append "Выбранное вами действие требует наличия расширения " dep_name ", которое не установленно на данный момент." 
		    "\nВы можете самостоятельно установить необходимые расширения воспользовавшись адресом:\n" dep_url
		    "\n\n" rsk-release
		    )
		  )
		)
		(if (= rsk-batch-call-state TRUE)
		  (rsk-quit-handle FALSE)
		  (set! rsk-batch-warn-lock TRUE)
		)
	      )
	      (gimp-message 
		(string-append "Выявлена неправильная работа интеграции с бинарным расширением " dep_name
		".\nСвяжитесь с разработчиком RSK для устранения ошибки."
		"\nВыполнение продолжится без дополнительных эффектов."
		"\n\n" rsk-release
		)
	      )
	    )
	  )
	  #f
	)
  )
)

;rsk-begin-handle
;Script begin procedure
;Has no arguments
(define (rsk-begin-handle)
  (if (= rsk-core-run-state FALSE)
    (begin
      (set! rsk-core-begin-time (realtime))
      (set! rsk-core-run-state TRUE)
    )
    (gimp-message 
      (string-append 
      "Выполнение предыдущей операции скрипта RSK закончилось ошибкой."
      "\nРекомендуется перезапустить GIMP."
      "\nСвяжитесь с разработчиками для разрешения проблемы (контактная информация доступна в документации).\n"
      rsk-release
      )
    )
  )
)

;rsk-end-handle
;Script end procedure
;LIST OF ARGUMENTS:
;STRING - end report from procedure;
(define (rsk-end-handle input_string)
  (if (= rsk-core-run-state FALSE)
    (begin
      (rsk-debug-message
	(string-append
	"Ядро RSK находилось в неактивном состоянии.\nПроверьте код завешрение и начала операции."
	"\nДескриптор операции:\n========================================\n"
	input_string
	"\n========================================\n"
	rsk-release
	)
      )
      (rsk-quit-handle)
    )
  )
  (set! rsk-core-end-time (realtime))
  (set! rsk-core-run-state FALSE)
  (rsk-debug-message 
    (string-append 
    "Операция завершена.\n"
    "Дескриптор операции:\n========================================\n"
    input_string
    "\n========================================\nВремя выполнения: "
    (number->string (- rsk-core-end-time rsk-core-begin-time))
    " сек.\n"
    rsk-release
    )
  )
)

;rsk-quit-handle
;Quit handler
;Has no arguments
(define (rsk-quit-handle)
  (set! rsk-core-run-state FALSE)
  (set! rsk-core-begin-time nil)
  (set! rsk-core-end-time nil)
  (rsk-debug-message (string-append "Выполнение скрипта аварийно завершено.\n" rsk-release))
  (quit)
)

;rsk-debug-message
;Debug message procedure
;LIST OF ARGUMENTS:
;STRING - debug message string;
(define (rsk-debug-message input_string)
  (if (= rsk-core-dev-state TRUE)
    (begin 
      (gimp-message input_string)
      #t
    )
    #f
  )
)

;rsk-init
;RSK package initialization function
;Has no arguments
(define (rsk-init)
  (let* (
	(parasite_glist (if (defined? 'gimp-get-parasite-list) (gimp-get-parasite-list) (gimp-parasite-list)))
	(parasite_count (car parasite_glist))
	(parasite_templist (cadr parasite_glist))
	(current_parasite_name)
	(current_parasite)
	(parasite_name)
	(parasite_value)
	(init_parasite FALSE)
	(init_flag FALSE)
	(init_message "")
	)

	;Finding init parasite
	(if (> parasite_count 0)
	  (while (not (or (= init_parasite TRUE) (null? parasite_templist)))
	    (set! current_parasite_name (car parasite_templist))
	    (if (equal? current_parasite_name "rsk-init")
	      (set! init_parasite TRUE)
	      (set! parasite_templist (cdr parasite_templist))
	    )
	  )
	)

	;Generation update message
	(if (= init_parasite TRUE)
	  (begin
	    (set! current_parasite (if (defined? 'gimp-get-parasite) (car (gimp-get-parasite current_parasite_name)) (car (gimp-parasite-find current_parasite_name))))
	    (set! parasite_value (caddr current_parasite))

	    ;Checking RSK release number
	    (if (not (equal? (string->number parasite_value) rsk-init-release))
	      (begin
		(set! init_flag TRUE)
		(set! init_message 
		  (string-append 
		  init_message
		  "Инициализация пакета RSK...\n\n"
		  "Внимание! Вы сменили версию RSK:\n"
		  "R" parasite_value " => R" (number->string rsk-init-release)
		  (if (< (string->number parasite_value) rsk-init-release)
		    "\nВерсия пакета обновлена."
		    (string-append 
		    "\n**************************"
		    "\nВНИМАНИЕ!"
		    "\nПакет RSK был заменен на более старую версию!"
		    "\nЕсли вы исптываете трудности с новой версией, вам следует связатся с разработчиками!"
		    "\n**************************"
		    )
		  )
		  (if (= rsk-core-dev-state TRUE)
		    "\n\nПримечание!\nДанная версия находится в разработке и может оказатся нестабильной."
		  )
		  "\n\n"
		  )
		)
	      )
	    )
	  )
	)

	;Generation init message
	(if (or (= init_flag TRUE) (= init_parasite FALSE))
	  (begin

	    ;Generation default message
	    (if (= init_parasite FALSE)
	      (set! init_message 
		(string-append 
		init_message 
		"Инициализация пакета RSK...\n\n"
		(if (= rsk-core-dev-state TRUE)
		  "Примечание!\nДанная версия находится в разработке и может оказатся нестабильной.\n\n"
		  ""
		)
		)
	      )
	    )

	    ;Attaching parasite to GIMP
	    (if (defined? 'gimp-attach-parasite)
	      (gimp-attach-parasite (list "rsk-init" 1 (number->string rsk-init-release)))
	      (gimp-parasite-attach (list "rsk-init" 1 (number->string rsk-init-release)))
	    )

	    ;Selftest run
	    (set! init_message (string-append init_message "=================================\n" (rsk-selftest-return) "=================================\n"))

	    ;Collecting error information
	    (if (= rsk-core-init-error TRUE)
	      (set! init_message
		(string-append
		init_message
		"\nПри инициализации пакета были обнаружены ошибки.\nСвяжитесь с разработчиками для разрешения проблем."
		"\n\n" rsk-release "\n© RED STORM STUDIO 2011"
		)
	      )
	      (set! init_message
		(string-append
		init_message
		"\nОшибок не обнаружено.\nПакет готов к работе!\n"
		"\n" rsk-release "\n© RED STORM STUDIO 2011"
		)
	      )
	    )

	    ;Make message out
	    (gimp-message-set-handler 0)
	    (gimp-message init_message)
	  )
	)
  )
)

;Start initialization
(rsk-init)