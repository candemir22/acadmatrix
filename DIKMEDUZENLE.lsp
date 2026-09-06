;;; ============================================================
;;; DIKME-DUZENLE.lsp  -- "Dikme Duzenle" araci (v3.0 - Uzanti Cizgileri ve Ust Metin)
;;;
;;; KOMUT: DIKME-DUZENLE
;;; ============================================================

(setq *DD-YARIM-GENISLIK-MM* 25.0)
(setq *DD-KORUMA-BANT-MM* 100.0)
(setq *DD-ACI-TOLERANS* 0.5)
(setq *DD-OLCU-TOLERANS* 1.0)
(setq *DD-DIKME-RENK* 5)
(setq *DD-YATAY-RENK* 1)
(setq *DD-MIN-DIKME-UZUNLUK-MM* 1000.0)
(setq *DD-MIN-YATAY-UZUNLUK-MM* 20.0)
(setq *DD-DIM-METIN-MM* 30.0)
(setq *DD-DIM-OK-MM* 15.0)
(setq *DD-DIM-UZANTI-MM* 10.0)
(setq *DD-DIM-BOSLUK-MM* 60.0)
(setq *DD-DIM-DIKEY-KOLON-MM* 40.0)
(setq *DD-DIM-TOPLAM-KOLON-MM* 140.0)

(defun dd-blogu-patlat-ve-topla (ent / basla-ent bitis-ent e sonuc)
  (setq sonuc nil)
  (setq basla-ent (entlast))
  (if (not (patlat-ve-attrib-duzelt ent))
    (if (and (entget ent) (manuel-patlat ent))
      (entdel ent)
    )
  )
  (setq bitis-ent (entlast))
  (if (not (equal basla-ent bitis-ent))
    (progn
      (setq e (entnext basla-ent))
      (while e
        (setq sonuc (cons e sonuc))
        (if (equal e bitis-ent) (setq e nil) (setq e (entnext e)))
      )
    )
  )
  sonuc
)

(defun dd-nesneleri-hazirla ( / ss n i ent edata etype calisma)
  (princ "\nDuzenlenecek nesneleri secin (dikme dikdortgeni + yatay cizgiler, vb.): ")
  (setq ss (ssget))
  (if (not ss) (progn (princ "\nSecim yok. Iptal.") (exit)))
  (setq calisma nil)
  (setq n (sslength ss)) (setq i 0)
  (while (< i n)
    (setq ent (ssname ss i))
    (if (entget ent)
      (progn
        (setq edata (entget ent))
        (setq etype (cdr (assoc 0 edata)))
        (if (= etype "INSERT")
          (setq calisma (append calisma (dd-blogu-patlat-ve-topla ent)))
          (setq calisma (cons ent calisma))
        )
      )
    )
    (setq i (1+ i))
  )
  (if (not calisma) (progn (princ "\nSecimde islenecek nesne yok. Iptal.") (exit)))
  calisma
)

(defun dd-olcek-katsayisi (olculen / )
  (cond
    ((<= (abs (- olculen 50.0)) *DD-OLCU-TOLERANS*) 1.0)
    ((<= (abs (- olculen 5.0)) *DD-OLCU-TOLERANS*) 0.1)
    (T nil)
  )
)

(defun dd-dikey-mi (adx ady) (and (<= adx *DD-ACI-TOLERANS*) (> ady *DD-ACI-TOLERANS*)))
(defun dd-yatay-mi (adx ady) (and (<= ady *DD-ACI-TOLERANS*) (> adx *DD-ACI-TOLERANS*)))

(defun dd-insert-sorted (x lst)
  (cond
    ((null lst) (list x))
    ((<= x (car lst)) (cons x lst))
    (T (cons (car lst) (dd-insert-sorted x (cdr lst))))
  )
)
(defun dd-sort-y (lst / res)
  (foreach n lst (setq res (dd-insert-sorted n res)))
  res
)

(defun dd-poly-sinir (ent / pts p xmin xmax ymin ymax)
  (setq pts (poligon-koseler ent))
  (setq p (car pts))
  (setq xmin (car p)) (setq xmax (car p)) (setq ymin (cadr p)) (setq ymax (cadr p))
  (foreach p pts
    (setq xmin (min xmin (car p))) (setq xmax (max xmax (car p)))
    (setq ymin (min ymin (cadr p))) (setq ymax (max ymax (cadr p)))
  )
  (list xmin ymin xmax ymax)
)

(defun dd-eksen-al (ent / edata etype bilgi genislik yukseklik ymid)
  (setq edata (entget ent))
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((= etype "LINE")
     (list (cdr (assoc 10 edata)) (cdr (assoc 11 edata)))
    )
    ((= etype "LWPOLYLINE")
     (setq bilgi (dd-poly-sinir ent))
     (setq genislik (- (nth 2 bilgi) (nth 0 bilgi)))
     (setq yukseklik (- (nth 3 bilgi) (nth 1 bilgi)))
     (if (> genislik yukseklik)
       (progn
         (setq ymid (/ (+ (nth 1 bilgi) (nth 3 bilgi)) 2.0))
         (list (list (nth 0 bilgi) ymid 0.0) (list (nth 2 bilgi) ymid 0.0))
       )
       nil
     )
    )
    (T nil)
  )
)

(defun dd-cizgi-analiz (ent / uclar p1 p2 dx dy)
  (setq uclar (dd-eksen-al ent))
  (if uclar
    (progn
      (setq p1 (nth 0 uclar)) (setq p2 (nth 1 uclar))
      (setq dx (- (car p2) (car p1))) (setq dy (- (cadr p2) (cadr p1)))
      (list p1 p2 (abs dx) (abs dy) (sqrt (+ (* dx dx) (* dy dy))))
    )
    nil
  )
)

(defun dd-yatay-gruplari-olustur (adaylar katsayi / kalan a b esles yeni-kalan fark ortusme sonuc y_lo y_hi gxmin gxmax hedef-bosluk yg)
  (setq sonuc nil)
  (setq hedef-bosluk (* 50.0 katsayi))
  (setq kalan adaylar)
  (while kalan
    (setq a (car kalan))
    (setq kalan (cdr kalan))
    (setq esles nil) (setq yeni-kalan nil)
    (foreach b kalan
      (if (not esles)
        (progn
          (setq fark (abs (- (abs (- (nth 1 a) (nth 1 b))) hedef-bosluk)))
          (setq ortusme (- (min (nth 3 a) (nth 3 b)) (max (nth 2 a) (nth 2 b))))
          (if (and (<= fark (* *DD-OLCU-TOLERANS* katsayi)) (> ortusme 0.0))
            (setq esles b)
            (setq yeni-kalan (cons b yeni-kalan))
          )
        )
        (setq yeni-kalan (cons b yeni-kalan))
      )
    )
    (setq kalan (reverse yeni-kalan))
    (if esles
      (progn
        (setq y_lo (min (nth 1 a) (nth 1 esles))) (setq y_hi (max (nth 1 a) (nth 1 esles)))
        (setq gxmin (max (nth 2 a) (nth 2 esles))) (setq gxmax (min (nth 3 a) (nth 3 esles)))
        (setq sonuc (cons (list y_lo y_hi gxmin gxmax (list (nth 0 a) (nth 0 esles))) sonuc))
      )
      (progn
        (setq yg (* *DD-YARIM-GENISLIK-MM* katsayi))
        (setq sonuc (cons (list (- (nth 1 a) yg) (+ (nth 1 a) yg) (nth 2 a) (nth 3 a) (list (nth 0 a))) sonuc))
      )
    )
  )
  (reverse sonuc)
)

(defun dd-yatay-parca-ciz (xa xb y1 y2 / ent)
  (if (> xb xa)
    (progn
      (setq ent (poligon-ciz (list (list xa y1 0.0) (list xb y1 0.0)
                                    (list xb y2 0.0) (list xa y2 0.0))))
      (if ent (entmod (append (entget ent) (list (cons 62 *DD-YATAY-RENK*)))))
      ent
    )
    nil
  )
)

(defun dd-yatay-katmani-hazirla ( / )
  (if (not (tblsearch "LAYER" "YATAY"))
    (command "_.-LAYER" "_N" "YATAY" "_C" "1" "YATAY" "")
  )
)

(defun dd-olcu-katmani-hazirla ( / )
  (if (not (tblsearch "LAYER" "DIKME-OLCU"))
    (command "_.-LAYER" "_N" "DIKME-OLCU" "_C" "3" "DIKME-OLCU" "")
  )
)

;; GUNCELLEME: x-obj (profile temas noktasi) eklendi, boylece yatay baglanti cizgileri cikacak
(defun dd-dikey-olcu-ciz (y-alt y-ust x-obj x-dim katsayi / pt1 pt2 ptd eskikatman)
  (dd-olcu-katmani-hazirla)
  (setq eskikatman (getvar "CLAYER"))
  (setvar "CLAYER" "DIKME-OLCU")
  (setq pt1 (list x-obj y-alt 0.0)) (setq pt2 (list x-obj y-ust 0.0))
  (setq ptd (list x-dim (/ (+ y-alt y-ust) 2.0) 0.0))
  
  (command "_.DIMLINEAR" "_non" pt1 "_non" pt2 "_V" "_non" ptd)
  (setvar "CLAYER" eskikatman)
)

(defun dikme-duzenle-yap ( / calisma dikme-ent dikme-sinir genislik yukseklik katsayi katsayi-aday
                          dikme-x1 dikme-y1 dikme-x2 dikme-y2 koruma koru-x1 koru-x2
                          korunacaklar cizgiler ent edata etype bilgi p1 p2 adx ady cy uzunluk
                          adaylar gruplar grup g-ylo g-yhi g-xmin g-xmax y-tol
                          sol-yrec sag-yrec yatay-sayisi sol-merkezler sag-merkezler
                          dikey-x-sol dikey-x2-sol dikey-x-sag eski-dimtxt eski-dimasz eski-dimexo eski-dimexe
                          rec-sayisi rec-en-uzun-genislik sirali-sol sirali-sag olcu-nok-sol olcu-nok-sag idx o-alt o-ust
                          L-val txt-L txt-Adet mid-x txt-pt1 txt-pt2 eskikatman)

  (setq calisma (dd-nesneleri-hazirla))

  (setq dikme-ent nil) (setq katsayi nil) (setq rec-sayisi 0) (setq rec-en-uzun-genislik 0.0)
  (foreach ent calisma
    (if (entget ent)
      (progn
        (setq edata (entget ent)) (setq etype (cdr (assoc 0 edata)))
        (if (and (= etype "LWPOLYLINE") (/= (cdr (assoc 8 edata)) "KESIM-SINIRI"))
          (progn
            (setq rec-sayisi (1+ rec-sayisi))
            (setq bilgi (dd-poly-sinir ent))
            (setq genislik (- (nth 2 bilgi) (nth 0 bilgi)))
            (setq yukseklik (- (nth 3 bilgi) (nth 1 bilgi)))
            (setq rec-en-uzun-genislik (max rec-en-uzun-genislik genislik))
            (setq katsayi-aday (dd-olcek-katsayisi genislik))
            (if (and (not dikme-ent) (> yukseklik genislik) katsayi-aday
                     (>= yukseklik (* *DD-MIN-DIKME-UZUNLUK-MM* katsayi-aday)))
              (progn
                (setq dikme-ent ent) (setq dikme-sinir bilgi)
                (setq katsayi katsayi-aday)
              )
            )
          )
        )
      )
    )
  )

  (if (not dikme-ent)
    (progn
      (princ "\nDikey duran, eni 50mm veya 5cm (+-1) olan, en az 100cm uzunlugunda kapali bir dikdortgen bulunamadi.")
      (exit)
    )
  )

  (setq dikme-x1 (nth 0 dikme-sinir)) (setq dikme-y1 (nth 1 dikme-sinir))
  (setq dikme-x2 (nth 2 dikme-sinir)) (setq dikme-y2 (nth 3 dikme-sinir))
  (setq koruma (* *DD-KORUMA-BANT-MM* katsayi))
  (setq koru-x1 (- dikme-x1 koruma)) (setq koru-x2 (+ dikme-x2 koruma))

  (entmod (append (entget dikme-ent) (list (cons 62 *DD-DIKME-RENK*))))
  (setq korunacaklar (list dikme-ent))

  (setq cizgiler nil)
  (foreach ent calisma
    (if (and (entget ent) (not (equal ent dikme-ent)))
      (progn
        (setq edata (entget ent)) (setq etype (cdr (assoc 0 edata)))
        (cond
          ((= etype "TEXT") (setq korunacaklar (cons ent korunacaklar))) 
          ((= etype "LINE") (setq cizgiler (cons ent cizgiler)))
          ((and (= etype "LWPOLYLINE") (/= (cdr (assoc 8 edata)) "KESIM-SINIRI"))
           (setq cizgiler (cons ent cizgiler))
          )
        )
      )
    )
  )

  (setq adaylar nil)
  (setq y-tol (* 10.0 katsayi)) 
  (foreach ent cizgiler
    (setq bilgi (dd-cizgi-analiz ent))
    (if bilgi
      (progn
        (setq p1 (nth 0 bilgi)) (setq p2 (nth 1 bilgi))
        (setq adx (nth 2 bilgi)) (setq ady (nth 3 bilgi)) (setq uzunluk (nth 4 bilgi))
        (setq cy (/ (+ (cadr p1) (cadr p2)) 2.0))
        (if (and (not (dd-dikey-mi adx ady)) 
                 (>= cy (- dikme-y1 y-tol)) 
                 (<= cy (+ dikme-y2 y-tol))
                 (>= uzunluk (* *DD-MIN-YATAY-UZUNLUK-MM* katsayi)))
          (setq adaylar (cons (list ent cy (min (car p1) (car p2)) (max (car p1) (car p2))) adaylar))
        )
      )
    )
  )

  (setq gruplar (dd-yatay-gruplari-olustur adaylar katsayi))

  (dd-yatay-katmani-hazirla)
  (setq eski-dimtxt (getvar "DIMTXT")) (setq eski-dimasz (getvar "DIMASZ"))
  (setq eski-dimexo (getvar "DIMEXO")) (setq eski-dimexe (getvar "DIMEXE"))
  (setvar "DIMTXT" (* *DD-DIM-METIN-MM* katsayi))
  (setvar "DIMASZ" (* *DD-DIM-OK-MM* katsayi))
  (setvar "DIMEXO" (* *DD-DIM-UZANTI-MM* katsayi))
  (setvar "DIMEXE" (* *DD-DIM-UZANTI-MM* katsayi))
  
  (setq dikey-x-sol (- koru-x1 (* *DD-DIM-DIKEY-KOLON-MM* katsayi)))
  (setq dikey-x2-sol (- koru-x1 (* *DD-DIM-TOPLAM-KOLON-MM* katsayi)))
  (setq dikey-x-sag (+ koru-x2 (* *DD-DIM-DIKEY-KOLON-MM* katsayi)))

  (setq yatay-sayisi 0) (setq sol-merkezler nil) (setq sag-merkezler nil)
  (foreach grup gruplar
    (setq g-ylo (nth 0 grup)) (setq g-yhi (nth 1 grup))
    (setq g-xmin (nth 2 grup)) (setq g-xmax (nth 3 grup))
    (setq sol-yrec (dd-yatay-parca-ciz (max g-xmin koru-x1) (min g-xmax dikme-x1) g-ylo g-yhi))
    (setq sag-yrec (dd-yatay-parca-ciz (max g-xmin dikme-x2) (min g-xmax koru-x2) g-ylo g-yhi))
    (if (or sol-yrec sag-yrec)
      (progn
        (setq yatay-sayisi (1+ yatay-sayisi))
        (setq cy (/ (+ g-ylo g-yhi) 2.0))
        (if sol-yrec (progn 
          (setq korunacaklar (cons sol-yrec korunacaklar))
          (setq sol-merkezler (cons cy sol-merkezler))
        ))
        (if sag-yrec (progn
          (setq korunacaklar (cons sag-yrec korunacaklar))
          (setq sag-merkezler (cons cy sag-merkezler))
        ))
      )
    )
  )

  ;; SOL AKS OLCULERI (dikme-x1 referans alindi)
  (if sol-merkezler
    (progn
      (setq sirali-sol (dd-sort-y sol-merkezler))
      (setq olcu-nok-sol (append (list dikme-y1) sirali-sol (list dikme-y2)))
      (setq idx 0)
      (while (< idx (1- (length olcu-nok-sol)))
        (setq o-alt (nth idx olcu-nok-sol))
        (setq o-ust (nth (1+ idx) olcu-nok-sol))
        (dd-dikey-olcu-ciz o-alt o-ust dikme-x1 dikey-x-sol katsayi)
        (setq idx (1+ idx))
      )
    )
    (dd-dikey-olcu-ciz dikme-y1 dikme-y2 dikme-x1 dikey-x-sol katsayi)
  )

  ;; SAG AKS OLCULERI (dikme-x2 referans alindi)
  (if sag-merkezler
    (progn
      (setq sirali-sag (dd-sort-y sag-merkezler))
      (setq olcu-nok-sag (append (list dikme-y1) sirali-sag (list dikme-y2)))
      (setq idx 0)
      (while (< idx (1- (length olcu-nok-sag)))
        (setq o-alt (nth idx olcu-nok-sag))
        (setq o-ust (nth (1+ idx) olcu-nok-sag))
        (dd-dikey-olcu-ciz o-alt o-ust dikme-x2 dikey-x-sag katsayi)
        (setq idx (1+ idx))
      )
    )
    (dd-dikey-olcu-ciz dikme-y1 dikme-y2 dikme-x2 dikey-x-sag katsayi)
  )

  ;; TOPLAM BOY OLCUSU (Sol Dis, dikme-x1 referans alindi)
  (dd-dikey-olcu-ciz dikme-y1 dikme-y2 dikme-x1 dikey-x2-sol katsayi)

  ;; GUNCELLEME: ALT METINLERI YUKARIYA TASIDIK (L=X ve 1 AD.)
  (setq L-val (- dikme-y2 dikme-y1))
  (setq txt-L (strcat "L=" (rtos L-val 2 0)))
  (setq txt-Adet "1 AD.")
  (setq mid-x (/ (+ dikme-x1 dikme-x2) 2.0))
  
  ;; Y2'nin uzerine (sistemin ustune) yerlesecek sekilde offset verildi
  (setq txt-pt1 (list mid-x (+ dikme-y2 (* 100.0 katsayi)) 0.0))
  (setq txt-pt2 (list mid-x (+ dikme-y2 (* 50.0 katsayi)) 0.0))

  (setq eskikatman (getvar "CLAYER"))
  (dd-olcu-katmani-hazirla)
  (setvar "CLAYER" "DIKME-OLCU")

  (entmake
    (list
      '(0 . "TEXT")
      (cons 10 txt-pt1)
      (cons 11 txt-pt1)
      (cons 40 (* *DD-DIM-METIN-MM* katsayi 1.2))
      (cons 1 txt-L)
      '(72 . 1)
    )
  )
  (entmake
    (list
      '(0 . "TEXT")
      (cons 10 txt-pt2)
      (cons 11 txt-pt2)
      (cons 40 (* *DD-DIM-METIN-MM* katsayi 1.2))
      (cons 1 txt-Adet)
      '(72 . 1)
    )
  )

  (setvar "CLAYER" eskikatman)
  (setvar "DIMTXT" eski-dimtxt) (setvar "DIMASZ" eski-dimasz)
  (setvar "DIMEXO" eski-dimexo) (setvar "DIMEXE" eski-dimexe)

  (foreach ent cizgiler (if (entget ent) (entdel ent)))
  (foreach ent calisma
    (if (and (entget ent) (not (member ent korunacaklar)))
      (entdel ent)
    )
  )

  (command "_.REGEN")
  (princ (strcat "\nIslem tamam. Uzanti cizgileri eklendi, metinler uste tasindi."))
  (princ)
)

(defun c:DIKME-DUZENLE ( / )
  (dikme-duzenle-yap)
  (princ)
)

(princ "\nDIKME-DUZENLE yuklendi (v3.0). Komut: DIKME-DUZENLE")
(princ)