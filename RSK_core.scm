;RSK core script v0.3

;OS directory separator
(define rsk-sys-sep (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))

(define rsk-release "RSK R1")

;Plugin activation area

(define rsk-gmic-def (if (defined? 'plug-in-gmic) TRUE FALSE))

(define rsk-fixca-def (if (defined? 'Fix-CA) TRUE FALSE))

(define rsk-greycstoration-def (if (defined? 'plug-in-greycstoration) TRUE FALSE))

;Plugin information global list
(define rsk-plugs-list
  (list

    ;G'MIC info entry
    (list "G'MIC" "http://registry.gimp.org/node/13469" rsk-gmic-def)

    ;Fix-CA info entry
    (list "Fix-CA" "http://registry.gimp.org/node/3726" rsk-fixca-def)

    ;greycstoration info entry
    (list "greycstoration" "http://registry.gimp.org/node/137" rsk-greycstoration-def)
  )
)

;Plugins message procedure
(define (rsk-plugs-handle)
  (let* (
	(finded " найден.")
	(not_finded " не найден.\nПожалуйста установите плагин используя ссылку:")
	(line "\n")
	(space " ")
	(temp_list)
	(temp_entry)
	(plug_name)
	(plug_url)
	(plug_var)
	(plug_message "")
	)
	(set! temp_list rsk-plugs-list)
	(while (not (null? temp_list))
	  (set! temp_entry (car temp_list))
	  (set! plug_name (car temp_entry))
	  (set! plug_url (cadr temp_entry))
	  (set! plug_var (caddr temp_entry))
	  (if (= plug_var TRUE)
	    (set! plug_message (string-append plug_message plug_name space finded))
	    (begin
	      (set! plug_message (string-append plug_message plug_name space not_finded line plug_url))
	    )
	  )
	  (set! plug_message (string-append plug_message line))
	  (set! temp_list (cdr temp_list))
	)
	(set! plug_message (string-append plug_message line rsk-release line "© RED STORM STUDIO 2010"))
	(gimp-message plug_message)
  )
)

(script-fu-register 
"rsk-plugs-handle"
_"<Image>/Filters/RSK R1/RSK Про_верить расширения"
"Проверить наличие необходимых для RSK расширений"
"Непочатов Станислав <spoilt_exile[at]mail.ru>"
"Непочатов Станислав"
"7 Октября 2010"
""
)

;Plugin warning procedure
(define (rsk-dep_warn-handle dep_id)
  (let* (
	(temp_list rsk-plugs-list)
	(temp_entry)
	(temp_id 0)
	(dep_name)
	(dep_url)
	(dep_var)
	)
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
	  (gimp-message 
	    (string-append "Выбранное вами действие требует наличия расширения " dep_name ", которое не установленно на данный момент." 
	    "\nВы можете самостоятельно установить необходимые расширения с помощью адресса: " dep_url ", или получить их у поставщика пакета RSK."
	    "\n\nВыполнение продолжится без дополнительных эффектов."
	    "\n\n" rsk-release "\n© RED STORM STUDIO 2010"
	    )
	  )
	  (gimp-message 
	    (string-append "Внутренняя ошибка RSK!\n\nВыявлена неправильная работа интеграции с бинарным расширением " dep_name
	    ".\nСвяжитесь с разработчиком RSK для устранения ошибки."
	    )
	  )
	)
  )
)

;API checking procedure
(define (rsk-api-check)
  (if (defined? 'gimp-item-to-selection)
    (begin
      (gimp-message "Данная версия GIMP не совместима с пакетом " rsk-release ".\nПолучите у поставщика более новую версию RSK.\n" rsk-release "\n© RED STORM STUDIO 2010")
      (quit)
    )
  )
)