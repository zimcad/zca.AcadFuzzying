(defun zca:assoc (asnr daten)
; gibt den Assoziationswert von anr aus daten zurück
 (if (and asnr daten)
  (if (= asnr 2)
    (if (cdr (assoc asnr daten)) (strcase (cdr (assoc asnr daten)))) ; wenn <> nil Immer in Grossbuchstaben zurückgeben
    (cdr (assoc asnr daten))
  )
  NIL
 )
)
(defun zca:assoc_lst (asnr entlst / i lstele)
; holt alle Elemente mit der gleichen Assoziationsnummer aus einer Elementdatenliste und
; gibt diese als Liste ohne die Assoziationsnummer wieder zurück
; z.B. Punktliste eine LWPolylinie
  (setq i 0 erglst '())
  (repeat (length entlst)
    (setq lstele (nth i entlst))
    (if (= (car lstele) asnr) (setq erglst (cons (cdr lstele) erglst)))
    (setq i (1+ i))
  )
 erglst
)

(defun ZCA:RANDOM-NUMBER (/)
  ;; Pseudo-random number genterator based on MSVC 4.1 function 'rand'.
  ;; Initialize seed for the RNG
  (if (not SEEDRAND) (setq SEEDRAND 1)) ;_ end of if
  (setq SEEDRAND (+ (* (fix SEEDRAND) 214013) 2531011))
  (boole 1 (/ SEEDRAND 65536) 32767)
) ;_ end of defun

(defun zca:editattr (att st obj /  sub subd)

;;  (*push-error-using-command*)
  
; aendert den Attributwert 'st' des Attributes 'att'=String von Objekt 'obj' 
; Attributbezeichnung beim Aufruf muss in Grossbuchstaben eingegeben werden
; steigt mit fehler aus der while - schleife bei entnext=NIL oder entget=NIL aus, wenn das Attribut nicht existiert  
  (setq sub (entnext obj))   ;sucht das erste Subelement = Attribut
  (while (/= (zca:assoc 2 (entget sub)) att) (setq sub (entnext sub))) ; alle anderen
  (setq subd (entget sub)) ; bis Position gefunden - dann Daten ermitteln
  (setq subd (subst (cons 1 st) (assoc 1 subd) subd)) ; und aendern
  (entmod subd) ; Block modifizieren
  (entupd sub)  ; und Hauptelement regenerieren

;;  (*pop-error-mode*)
)

(defun zca:editattr_font (att st obj / sub subd) 
; aendert den Font 'st' des Attributes 'att'=String von Objekt 'obj' 
; Attributbezeichnung beim Aufruf muss in Grossbuchstaben eingegeben werden
; steigt mit fehler aus der while - schleife bei entnext=NIL oder entget=NIL aus, wenn das Attribut nicht existiert  
  (setq sub (entnext obj))   ;sucht das erste Subelement = Attribut
  (while (/= (zca:assoc 7 (entget sub)) att) (setq sub (entnext sub))) ; alle anderen
  (setq subd (entget sub)) ; bis Position gefunden - dann Daten ermitteln
  (setq subd (subst (cons 1 st) (assoc 1 subd) subd)) ; und aendern
  (entmod subd) ; Block modifizieren
  (entupd sub)  ; und Hauptelement regenerieren

;;  (*pop-error-mode*)
)

(defun zca:holattr (att obj / sub subd) ; Rueckgabe = String des Attributwertes
; holt den Attributwert eines Attributes im Objekt
  (setq sub (entnext obj))   ;sucht das erste Subelement = Attribut
  (while (/= (zca:assoc 2 (entget sub)) att) (setq sub (entnext sub))) ; alle anderen
  (setq subd (entget sub)) ; bis Position gefunden - dann Daten ermitteln
  (cdr (assoc 1 subd))
)

(defun zca:attributliste (element / attlst daten elename blname sub subd raushier)
; gibt eine Liste zurueck mit der Attributbezeichnung und dem Attributwert
; saemtlicher im Block definierten Attribute
  (setq attlst '())
  (setq daten (entget (car element)))                      ; Daten Hauptelement
  (setq elename (cdr (assoc 0 daten)))                       ; Elementdaten Hauptelement
  (if (= elename "INSERT")                                 ; wenn Block                     
    (progn
     (if (= (cdr (assoc 66 daten)) 1)                        ; wenn Attwert 66 = 1 dann sind Attribute da
       (progn
        (setq sub (entnext (car element)))                 ; erstes Subelement
        (while (/= (cdr (assoc 0 (entget sub))) "SEQEND")    ; solange kein Blockende in Sicht
          (setq subd (entget sub))                         ; Attributdaten 
          (setq attlst (cons (cons (cdr (assoc 2 subd))(cdr (assoc 1 subd))) attlst)) 
          (setq sub (entnext sub))                         ; naechstes Subelement finden
        ) 
       );ende progn
         (setq attlst NIL) ; keine Attribute
     );ende if
   );ende progn
     (setq attlst NIL) ; kein Block
   )
  attlst ; liste zurueckgeben
)
