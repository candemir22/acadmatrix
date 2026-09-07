;;; ============================================================
;;; DIKME-KES.lsp  -- "Dikme Kes" araci (v3.0 - KUSURSUZ KESIM)
;;; ============================================================

(setq *OFSET-ARALIK* 50.0) 
(setq *DIKME-KOD-YUKSEKLIK* 40.0)

;; ============================================================
;; ENTEGRE YARDIMCI FONKSIYONLAR
;; ============================================================

(defun kesim-katmani-hazirla ( / )
  (if (not (tblsearch "LAYER" "KESIM-SINIRI"))
    (command "_.-LAYER" "_N" "KESIM-SINIRI" "_C" "1" "KESIM-SINIRI" "")
  )
)

(defun poligon-ciz (pts / p)
  (command "_.PLINE")
  (foreach p pts (command "_NONE" p))
  (command "_C")
  (entlast)
)

(defun poligon-koseler (ent / ed pts)
  (if ent
    (progn
      (setq ed (entget ent))
      (foreach item ed
        (if (= (car item) 10)
          (setq pts (cons (cdr item) pts))
        )
      )
      (reverse pts)
    )
  )
)

(defun poligon-nokta-topla ( / p pts)
  (setq pts nil)
  (while (setq p (getpoint "\nPoligon noktasi secin (Bitirmek icin Enter): "))
    (setq pts (cons p pts))
  )
  (reverse pts)
)

(defun benzersiz-ad (prefix / )
  (strcat prefix "_" (rtos (* (getvar "MILLISECS") 1.0) 2 0))
)

;; GARANTILI DIS OFFSET MOTORU (Iceri kaymayi onler)
(defun poligon-dis-offset (ent dist / vla-obj ret-obj obj1 obj2 area0 area1 area2 final-ent)
  (vl-load-com)
  (setq vla-obj (vlax-ename->vla-object ent))
  (setq area0 (vlax-get-property vla-obj 'Area))
  
  (setq ret-obj (vl-catch-all-apply 'vla-offset (list vla-obj dist)))
  (if (and (not (vl-catch-all-error-p ret-obj)) ret-obj)
    (setq obj1 (vlax-safearray-get-element (vlax-variant-value ret-obj) 0))
  )
  
  (setq ret-obj (vl-catch-all-apply 'vla-offset (list vla-obj (- dist))))
  (if (and (not (vl-catch-all-error-p ret-obj)) ret-obj)
    (setq obj2 (vlax-safearray-get-element (vlax-variant-value ret-obj) 0))
  )
  
  (setq area1 (if obj1 (vlax-get-property obj1 'Area) 0.0))
  (setq area2 (if obj2 (vlax-get-property obj2 'Area) 0.0))
  
  (if (> area1 area2)
    (progn
      (if obj2 (vla-delete obj2))
      (setq final-ent (vlax-vla-object->ename obj1))
    )
    (progn
      (if obj1 (vla-delete obj1))
      (setq final-ent (vlax-vla-object->ename obj2))
    )
  )
  final-ent
)

(defun bolgeyi-garanti-patlat (pts / ss qa i)
  (setq qa (getvar "QAFLAGS"))
  (setvar "QAFLAGS" 1)
  (setq i 0)
  (while (< i 2)
    (setq ss (ssget "_CP" pts '((0 . "INSERT"))))
    (if ss
      (vl-catch-all-apply '(lambda () (command "_.EXPLODE" ss "")))
      (setq i 2)
    )
    (setq i (1+ i))
  )
  (setvar "QAFLAGS" qa)
  nil
)

(defun bandi-trimle-genel (ic-poly oc-poly / fpts old-trim edge-mode)
  (setq fpts (poligon-koseler oc-poly))
  (if (and fpts (> (length fpts) 1))
    (progn
      (setq edge-mode (getvar "EDGEMODE"))
      (setvar "EDGEMODE" 0)
      (if (getvar "TRIMEXTENDMODE")
        (progn
          (setq old-trim (getvar "TRIMEXTENDMODE"))
          (setvar "TRIMEXTENDMODE" 0)
        )
      )
      (vl-catch-all-apply
        '(lambda ()
           (command "_.TRIM" ic-poly "")
           (command "_F")
           (foreach pt fpts (command "_NONE" pt))
           (command "_NONE" (car fpts))
           (command "")
           (command "")
         )
      )
      (if old-trim (setvar "TRIMEXTENDMODE" old-trim))
      (setvar "EDGEMODE" edge-mode)
    )
  )
)

(defun dikme-ensure-style ( / )
  (if (not (tblsearch "STYLE" "DIKME-ARIAL"))
    (entmake
      (list
        (cons 0 "STYLE") (cons 100 "AcDbSymbolTableRecord") (cons 100 "AcDbTextStyleTableRecord")
        (cons 2 "DIKME-ARIAL") (cons 70 0)
        (cons 40 0.0) (cons 41 1.0) (cons 50 0.0) (cons 71 0) (cons 42 2.5)
        (cons 3 "Arial.ttf") (cons 4 "")
      )
    )
  )
)

(defun dikme-kod-yaz (pt kod / eskirenk eskistil)
  (dikme-ensure-style)
  (setq eskirenk (getvar "CECOLOR"))
  (setq eskistil (getvar "TEXTSTYLE"))
  (setvar "TEXTSTYLE" "DIKME-ARIAL")
  (setvar "CECOLOR" "5") 
  (vl-catch-all-apply
    '(lambda () (command "_.-TEXT" "_J" "TR" "_NONE" pt *DIKME-KOD-YUKSEKLIK* "0" kod ""))
  )
  (setvar "CECOLOR" eskirenk)
  (setvar "TEXTSTYLE" eskistil)
)

(defun dikme-kod-no-al (txt / rest i c tamam sayi)
  (setq sayi nil)
  (if (and txt (> (strlen txt) 3) (= (substr txt 1 3) "DK-"))
    (progn
      (setq rest (substr txt 4)) (setq tamam T) (setq i 1)
      (while (and tamam (<= i (strlen rest)))
        (setq c (substr rest i 1))
        (if (not (and (>= (ascii c) 48) (<= (ascii c) 57))) (setq tamam nil))
        (setq i (1+ i))
      )
      (if (and tamam (> (strlen rest) 0)) (setq sayi (atoi rest)))
    )
  )
  sayi
)

(defun dikme-sonraki-kod ( / ss n i ent txt no maxno)
  (setq maxno 0)
  (setq ss (ssget "_X" (list (cons 0 "TEXT") (cons 1 "DK-*"))))
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq txt (cdr (assoc 1 (entget ent))))
        (setq no (dikme-kod-no-al txt))
        (if (and no (> no maxno)) (setq maxno no))
        (setq i (1+ i))
      )
    )
  )
  (strcat "DK-" (itoa (1+ maxno)))
)

(defun dikme-kalinti-temizle (basla-ent koru-listesi / e)
  (setq e (if basla-ent (entnext basla-ent) (entnext)))
  (while e
    (if (and (entget e) (not (member e koru-listesi)))
      (entdel e)
    )
    (setq e (entnext e))
  )
)

;; ============================================================
;; ANA DIKME KES MOTORU
;; ============================================================

(defun dikme-kes-yap ( / mod-kw p1 p2 noktalar ix1 iy1 ix2 iy2 np
                        ic-nokta hedef dx dy z1 z2
                        orig-ic-poly noktalar-c ic-nokta-c ic-poly-c oc-poly-c
                        eskikatman ss-content basla-ent ss-i ss-e
                        ssblok kod blokadi kod-ent son-ent
                        old-osmode old-cmdecho)

  (initget "Dikdortgen Poligon PL")
  (setq mod-kw (getkword "\nAlan secim yontemi [Dikdortgen/Poligon(PL)] <Dikdortgen>: "))
  (if (not mod-kw) (setq mod-kw "Dikdortgen"))
  (if (= mod-kw "PL") (setq mod-kw "Poligon"))

  (if (= mod-kw "Poligon")
    (setq noktalar (poligon-nokta-topla))
    (progn
      (setq p1 (getpoint "\nKOPYALANACAK DIKME ALANININ ilk kosesini secin: "))
      (if (not p1) (progn (princ "\nIptal.") (quit)))
      (setq p2 (getcorner p1 "\nKARSI KOSEYI secin: "))
      (if (not p2) (progn (princ "\nIptal.") (quit)))
      (setq ix1 (min (car p1) (car p2))) (setq ix2 (max (car p1) (car p2)))
      (setq iy1 (min (cadr p1) (cadr p2))) (setq iy2 (max (cadr p1) (cadr p2)))
      (setq noktalar (list (list ix1 iy1 0.0) (list ix2 iy1 0.0) (list ix2 iy2 0.0) (list ix1 iy2 0.0)))
    )
  )

  (if (not noktalar) (progn (princ "\nNokta secilmedi. Iptal.") (quit)))

  (setq ix1 (car (car noktalar))) (setq ix2 (car (car noktalar)))
  (setq iy1 (cadr (car noktalar))) (setq iy2 (cadr (car noktalar)))
  (foreach np noktalar
    (setq ix1 (min ix1 (car np))) (setq ix2 (max ix2 (car np)))
    (setq iy1 (min iy1 (cadr np))) (setq iy2 (max iy2 (cadr np)))
  )
  (setq ic-nokta (list ix1 iy1 0.0))

  (setq hedef (getpoint ic-nokta "\nKopyayi nereye yerlestireyim? "))
  
  (if hedef
    (progn
      ;; HATA ONLEYICI KORUMALARI AKTIF ET
      (setq old-osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      (setq old-cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)

      (setq dx (- (car hedef) ix1)) (setq dy (- (cadr hedef) iy1))

      (kesim-katmani-hazirla)
      (setq eskikatman (getvar "CLAYER"))
      (setvar "CLAYER" "KESIM-SINIRI")
      (setq orig-ic-poly (poligon-ciz noktalar))
      (setvar "CLAYER" eskikatman)

      (setq ss-content (ssget "_CP" noktalar))
      (if ss-content
        (if (entget orig-ic-poly) (setq ss-content (ssdel orig-ic-poly ss-content)))
      )

      (if ss-content
        (progn
          (setq ss-i 0)
          (while (< ss-i (sslength ss-content))
            (setq ss-e (ssname ss-content ss-i))
            (if (and (entget ss-e) (= (cdr (assoc 8 (entget ss-e))) "KESIM-SINIRI"))
              (setq ss-content (ssdel ss-e ss-content))
              (setq ss-i (1+ ss-i))
            )
          )
        )
      )
      
      (if (or (not ss-content) (= (sslength ss-content) 0))
        (progn 
          (setvar "OSMODE" old-osmode)
          (princ "\nBu alanda kopyalanacak nesne yok. Iptal.") 
          (quit)
        )
      )

      (setq basla-ent (entlast))
      (command "_.COPY" ss-content "" "_NONE" ic-nokta "_NONE" hedef)

      (setq noktalar-c nil)
      (foreach np noktalar
        (setq noktalar-c (cons (list (+ (car np) dx) (+ (cadr np) dy) 0.0) noktalar-c))
      )
      (setq noktalar-c (reverse noktalar-c))
      (setq ic-nokta-c (list (+ ix1 dx) (+ iy1 dy) 0.0))

      (setvar "CLAYER" "KESIM-SINIRI")
      (setq ic-poly-c (poligon-ciz noktalar-c))
      (setvar "CLAYER" eskikatman)

      ;; KUSURSUZ DIS OFFSET
      (setq oc-poly-c (poligon-dis-offset ic-poly-c *OFSET-ARALIK*))
      
      ;; HATA ONLEYICI OTOMATIK ZOOM (Trim'in calismasi icin sart)
      (setq z1 (list (- (+ ix1 dx) (* *OFSET-ARALIK* 3.0)) (- (+ iy1 dy) (* *OFSET-ARALIK* 3.0)) 0.0))
      (setq z2 (list (+ (+ ix2 dx) (* *OFSET-ARALIK* 3.0)) (+ (+ iy2 dy) (* *OFSET-ARALIK* 3.0)) 0.0))
      (command "_.ZOOM" "_W" "_NONE" z1 "_NONE" z2)

      ;; MOTORLARI CALISTIR
      (bolgeyi-garanti-patlat (poligon-koseler oc-poly-c))
      (bandi-trimle-genel ic-poly-c oc-poly-c)

      (setq ssblok (ssget "_CP" (poligon-koseler ic-poly-c)))
      
      (setq kod (dikme-sonraki-kod))
      (setq blokadi (benzersiz-ad kod))

      (dikme-kod-yaz (list (+ ix2 dx) (+ iy2 dy) 0.0) kod)
      (setq kod-ent (entlast))
      (if kod-ent (setq ssblok (ssadd kod-ent ssblok)))

      (command "_.-BLOCK" blokadi "_NONE" ic-nokta-c ssblok "")
      (command "_.-INSERT" blokadi "_NONE" ic-nokta-c "1" "1" "0")
      (setq son-ent (entlast))

      ;; KALINTILARI SIL
      (dikme-kalinti-temizle basla-ent (list son-ent oc-poly-c))

      (dikme-kod-yaz (list ix2 iy2 0.0) kod)

      (if (entget oc-poly-c) (entdel oc-poly-c))
      
      ;; EKRANI ESKI HALINE GETIR
      (command "_.ZOOM" "_P")
      (setvar "OSMODE" old-osmode)
      (setvar "CMDECHO" old-cmdecho)
      
      (princ (strcat "\n\"" kod "\" kopyalandi -- orijinal HICBIR SEKILDE degismedi."))
    )
    (princ "\nIptal -- hicbir sey degismedi.")
  )
  (princ)
)

(defun c:DIKME-KES ( / )
  (dikme-kes-yap)
  (princ)
)

(princ "\nDIKME-KES v3.0 (Kusursuz Kesim) yuklendi. Komut: DIKME-KES")
(princ)