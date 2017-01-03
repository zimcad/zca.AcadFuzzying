(load "zca-biblio-tools")

(defun fzwert(); gibt eine zufällige realzahl unter 1 zurück
  (setq x (/ (zca:random-number) 99.85654))
  (/ (- x (fix x)) 100.0)
)

(defun fz:edit_line (ed)
  (setq k1 (zca:assoc 10 ed)); AP 
  (setq k2 (zca:assoc 11 ed)); EP
  (setq k1n (list (+ (car k1) (fzwert)) (+ (cadr k1) (fzwert)) (+ (caddr k1) (fzwert)) ))
  (setq k2n (list (+ (car k2) (fzwert)) (+ (cadr k2) (fzwert)) (+ (caddr k1) (fzwert)) ))
  (setq ed (subst (cons 10 k1n) (cons 10 k1) ed))
  (entmod ed)
  (setq ed (subst (cons 11 k2n) (cons 11 k2) ed))
  (entmod ed)
)

(defun fz:edit_insert (ed)
  (setq k1 (zca:assoc 10 ed)); EP 
  (setq k1n (list (+ (car k1) (fzwert)) (+ (cadr k1) (fzwert)) (+ (caddr k1) (fzwert)) ))
  (setq ed (subst (cons 10 k1n) (cons 10 k1) ed))
  (entmod ed)
)

(defun c:fuzzy()

; Vorgehensweise beim Weitergeben von DWG Zeichnungen
; Layout als Modell exportieren
; erstellte DWG öffnen
; FUZZY aufrufen
; Alle Elemente wählen, Farbe und Linienstärke auf weiss bzw. 0.00 ändern

  (setq as '() index 0);  fuzzyx 0.16 fuzzyy 0.9 fuzzyz 0.35) 
  (setq as (ssget "X"))
  (repeat (sslength as)
    (setq e (ssname as index))
    (setq ed (entget e))
    (setq elen (zca:assoc 0 ed))
;    (prompt (strcat "\nElement : " elen " im "))
    (if (= (zca:assoc 67 ed) 0); wenn 1 dann Papierbereich, 0 = Modellbereich
     (Progn
      (cond
	((= elen "LINE") (fz:edit_line ed)); AP und EP schieben
;	((= elen "PLINE") (fz:edit_pline ed)); alle Segmentpunkte verschieben
;	((= elen "TEXT") (fz:edit_text ed)); Drehwinkel 
;	((= elen "CIRCLE") (fz:edit_circle ed)); Radius ändern
	((= elen "INSERT") (progn
	     (fz:edit_insert ed); Einfügepunkt ändern, dann auflösen
             (command "._EXPLODE" e)
	     (prompt "x")
	))
      );c
      (entupd e)
      (prompt ".")
     );p
    );if
    (setq index (1+ index))
  );r
  (prompt "\nElemente gefuzzyt :") (princ index)(prompt "-> sieh zu dasse klarkommss :-)")(princ)
)
