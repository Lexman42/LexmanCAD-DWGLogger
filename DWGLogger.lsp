;; LOAD REACTOR
(setq LCAD_SaveReactor (vlr-dwg-reactor nil '((:vlr-saveComplete . LCAD_updateSaveRecord))))

;; ---------------------------------------
;; REACTOR FUNCTION TO UPDATE THE SAVELIST
(defun LCAD_updateSaveRecord (caller cmdSet / )

  ;; CREATE THE LMANDICTIONARY DICTIONARY IF IT DOES NOT ALREADY EXIST
  (if (not (setq LMANDICTIONARYEntity (cdr (assoc -1 (dictsearch (namedobjdict) "LMANDICTIONARY")))))
    (progn 
      (setq LMANDICTIONARYEntity (entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary")))) 
      (dictadd (namedobjdict) "LMANDICTIONARY" LMANDICTIONARYEntity)
    );_progn
  );_if

  ;; ADD THE NEW SAVE ITEM TO THE BEGINNING OF THE SAVE LIST
  (if (ssget "X")
    (setq numberOfEntities (itoa (sslength (ssget "X"))))
    (setq numberOfEntities "0")
  );_if
    
  (setq drawingPath
    (if (= "" (getvar "SAVENAME"))
      (getvar "SAVEFILE")
      (getvar "SAVENAME")
    );_if
  );_setq
                      
  (setq newRecord (list (strcat (LCAD_dateTimeString) " USER - " (getenv "UserName") " #ENTITIES - " numberOfEntities " PATH - " drawingPath)))
  (setq oldRecord (vlax-ldata-get (namedobjdict) "LMANDICTIONARY"))
  (setq saveRecords (cons newRecord oldRecord)) 
  (vlax-ldata-put (namedobjdict) "LMANDICTIONARY" saveRecords)

  (princ "\nDWGLogger has updated the save log\n")
  (princ)
    
);_LCAD_updateSaveRecord

;; --------------------------------------------------------------------------------------------
;; FUNCTION TO RETURN A STRING OF THE CURRENT DATE AND TIME IN THIS FORMAT: YYYY:MM:DD HH:MM:SS
(defun LCAD_dateTimeString ( / day year month hour minute second dateReal)

  (setq dateReal (rtos (getvar "CDATE") 2 6))
  
  (setq year   (substr dateReal 1 4)
        month  (substr dateReal 5 2)
        day    (substr dateReal 7 2)
        hour   (substr dateReal 10 2)
        minute (substr dateReal 12 2)
        second (substr dateReal 14 2))

  ;; ENSURE THAT THE SECOND STRING CONTAINS TWO DIGITS
  (while (< (strlen second) 2)
    (setq second (strcat second "0")))
    
  (strcat "DATE - " year ":" month ":" day " TIME - " hour ":" minute ":" second)
    
);defun

;; --------------------------------
;; FUNCTION TO DISPLAY SAVE RECORDS
(defun C:LCAD_DisplayLog ( / )

  ;; WRITE THE DCL
  (setq dclPath (strcat (getvar 'tempprefix) "DWGLogger.dcl"))
  (LCAD_writeDWGLoggerDCL dclPath)

  (setq dialogReturn 2)

  (while (= dialogReturn 2)
    
    ;; LOAD THE SUMMARY DIALOG BOX
    (setq dialogID (load_dialog dclPath))
    (new_dialog "summary" dialogID)

    ;; FILL LIST WITH SAVE HISTORY
    (setq saveRecords (reverse (vlax-ldata-get (namedobjdict) "LMANDICTIONARY")))
    (start_list "listBox")
    (foreach record saveRecords
      (add_list (car record))
    );_foreach
    (end_list)

    ;; DEFINE ACTION TILES
    (action_tile "accept" "(done_dialog 1)")
    (action_tile "export" "(done_dialog 2)")

    ;; START DIALOG
    (setq dialogReturn (start_dialog))
    (unload_dialog dialogID)

    (if (= dialogReturn 2) (exportCSV))

  );_while
     
);_c:DisplayLog

;; ----------------------------------------------------
;; FUNCTION TO WRITE A DCL IN THE USER'S TEMP DIRECTORY
(defun LCAD_writeDWGLoggerDCL ( path / )

  ;; DETERMINE THE LONGEST LINE
  (setq longestEntry 30)
  (setq saveRecords (reverse (vlax-ldata-get (namedobjdict) "LMANDICTIONARY")))
  (foreach record saveRecords
    (setq tmp (car record))
    (if (> (strlen tmp) longestEntry)
      (setq longestEntry (strlen tmp))
    );_if
  );_foreach
    
  (if (setq file (open path "w" ))
    (progn

      (write-line "//--------------------" file)
      (write-line "// DRAWING SUMMARY DCL" file)
      (write-line "//--------------------" file)
      (write-line "summary : dialog" file)
      (write-line "{" file)
      (write-line "  label = \"Save History\";" file)
      (write-line (strcat "  :list_box { key = \"listBox\"; width = " (itoa longestEntry) "; height = 30; is_tab_stop = true;}") file)
      (write-line (strcat "  :button   { key = \"export\"; label = \"EXPORT TO CSV\"; width = " (itoa longestEntry) "; is_tab_stop = true;}") file)
      (write-line (strcat "  :button   { key = \"accept\"; label = \"CLOSE\"; is_default = true; is_cancel = true; width = " (itoa longestEntry) "; is_tab_stop = true;}") file)
      (write-line "}" file)

      ;; CLOSE THE FILE
      (close file)

    );_progn
    (alert (strcat "DWGLogger was denied read/write access to: \"" path "\"."
                   "\n DWGLogger is accessing this directory to write a dcl menu file"))
  );_if
);_LCAD_writeDWGLoggerDCL


;; ---------------------------------
;; FUNCTION TO EXPORT TO A .CSV FILE
(defun exportCSV (/)

  ;; FETCH AN ORDERED LOG
  (if (setq saveRecords (reverse (vlax-ldata-get (namedobjdict) "LMANDICTIONARY")))
    (progn
      (if (setq logFilePath (getfiled "Export log to CSV file" "" "csv" 1))
        (progn
          (if (setq file (open logFilePath "w" ))
            (progn
  
              ;; WRITE HEADERS
              (write-line "Date YYYY:MM:DD,Time HH:MM:SS,USER,#ENTITIES,FILE PATH" file)

              (foreach record saveRecords
                (setq recordString (car record))

                ;; EXTRACT THE DATE STRING
                (setq dateString (substr recordString 8 10))

                ;; EXTRACT THE TIME STRING
                (setq timeString (substr recordString 26 8))

                ;; EXTRACT THE USER STRING
                (setq tmp (vl-string-search "#ENTITIES" recordString))
                (setq userString (substr recordString 42 (- tmp 42)))

                ;; EXTRACT THE NUMBER OF ENTITIES
                (setq tmp1 (vl-string-search "#ENTITIES" recordString))
                (setq tmp2 (vl-string-search "PATH" recordString))
                (setq numberOfEntitiesString (substr recordString (+ tmp1 13) (- tmp2 (+ tmp1 13))))

                ;; EXTRACT THE FILE PATH
                (setq filePathString (substr recordString (+ tmp2 8) (strlen recordString)))

                ;; WRITE TO FILE
                (write-line (strcat dateString "," timeString "," userString "," numberOfEntitiesString "," filepathString) file)
      
              );_foreach
            );_progn
            (alert (strcat "Unable to write to: " logFilePath))
          );_if
        );_progn
      );_if
    );_progn
    (alert "No records to export")
  );_if

  (princ)
    
);_exportCSV

(princ "\nDWGLogger v.1.0 loaded successfully\n")
(princ)