;;; ============================================================
;;; DOSYA   : detay20_v3.lsp
;;; VERSIYON: v3
;;; EMIR    : CW2   (v1/v2'de DYS idi, artik CW2)
;;;
;;; v2'DEN FARKI:
;;;  - BUG DUZELTILDI: WIPEOUT komutunda "_p" (Polyline) secenegi
;;;    "nokta ile CIZ" degil, "VAR OLAN bir kapali polyline'i SEC
;;;    ve wipeout'a cevir" anlamina geliyormus. v2'de bu yuzden
;;;    AutoCAD "Select a closed polyline:" diye sorup kalmis,
;;;    ardindan STRETCH gibi baska komutlara sicramisti.
;;;    v3: once gecici kapali bir LWPOLYLINE ciziliyor, sonra
;;;    WIPEOUT "_p" ile o polyline SECILIP wipeout'a cevriliyor
;;;    (kaynak polyline "Yes" ile otomatik siliniyor).
;;;  - GROUP ismi uretimi duzeltildi (eski hali CDATE gibi cok
;;;    buyuk bir sayidan (itoa) ile isim uretiyordu, tasma/hata
;;;    riski vardi). Artik MILLISECS + sira no kullaniliyor.
;;;  - KAPSAM GENISLETILDI: Artik secim TEXT + MTEXT + DIMENSION
;;;    hepsini kapsiyor:
;;;      * TEXT/MTEXT  -> stil=DIKME-KODU, h=100, WIPEOUT cerceve
;;;                        (arkaplan renginde), yazi+cerceve GROUP
;;;                        ile tek parca.
;;;      * DIMENSION   -> DIMOVERRIDE ile o olcunun yazi stili ve
;;;                        yuksekligi degistirilir (DIMTXSTY,
;;;                        DIMTXT=100). Cizgi/ok/konum degismez.
;;;                        ONEMLI SINIR: dimension yazisina cerceve/
;;;                        wipeout cizilmiyor (dimension text'in
;;;                        gercek sinirlarini LISP'ten guvenilir
;;;                        hesaplamak mumkun degil). Bunu istersen
;;;                        ayri konusuruz.
;;;
;;; KULLANIM:
;;;   CW2 yaz, Enter, alani sec (pencere/crossing ile), Enter.
;;;
;;; VERSIYON GECMISI:
;;;   v1 - Ilk surum, beyaz SOLID + 3 ayri parca (sorunlu).
;;;   v2 - WIPEOUT (arkaplan rengi) + GROUP (tek parca) TEXT/MTEXT.
;;;   v3 - WIPEOUT bug fix, GROUP isim fix, DIMENSION destegi
;;;        (stil+yukseklik), komut adi CW2.
;;; ============================================================

(vl-load-com)

(defun DYS:ensure-style ()
  (if (not (tblsearch "STYLE" "DIKME-KODU"))
    (entmake
      (list
        '(0 . "STYLE")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbTextStyleTableRecord")
        '(2 . "DIKME-KODU")
        '(70 . 0)
        '(40 . 0.0)
        '(41 . 1.0)
        '(50 . 0.0)
        '(71 . 0)
        '(42 . 100.0)
        '(3 . "arial.ttf")
        '(4 . "")
      )
    )
  )
)

(defun DYS:ensure-layer ()
  (if (not (tblsearch "LAYER" "CW-YAZI-CERCEVE"))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        '(2 . "CW-YAZI-CERCEVE")
        '(70 . 0)
        '(62 . 7)
        '(6 . "Continuous")
      )
    )
  )
)

;; Once kapali bir LWPOLYLINE cizer, sonra onu WIPEOUT'a cevirir.
;; WIPEOUT arkaplan rengini (ekranda mevcut arkaplan, plotta kagit
;; rengini) otomatik kullanir; cercevesi WIPEOUTFRAME ile gorunur.
(defun DYS:make-wipeout (minpt maxpt / marg p1 p2 p3 p4 oldclayer plEn wpEn)
  (setq marg 20.0)
  (setq p1 (list (- (car minpt) marg) (- (cadr minpt) marg)))
  (setq p2 (list (+ (car maxpt) marg) (- (cadr minpt) marg)))
  (setq p3 (list (+ (car maxpt) marg) (+ (cadr maxpt) marg)))
  (setq p4 (list (- (car minpt) marg) (+ (cadr maxpt) marg)))

  (setq oldclayer (getvar "CLAYER"))
  (setvar "CLAYER" "CW-YAZI-CERCEVE")

  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "CW-YAZI-CERCEVE")
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)          ; kapali
      (cons 10 p1)
      (cons 10 p2)
      (cons 10 p3)
      (cons 10 p4)
    )
  )
  (setq plEn (entlast))

  (command "_.wipeout" "_p" plEn "_y")
  (setq wpEn (entlast))

  (setvar "CLAYER" oldclayer)
  wpEn
)

;; TEXT / MTEXT icin: stil+yukseklik+wipeout cerceve+group
(defun DYS:process-text (en basenum idx / ed vlaobj minSA maxSA minpt maxpt wpEn gname)
  (setq ed (entget en))

  (if (assoc 7 ed)
    (setq ed (subst (cons 7 "DIKME-KODU") (assoc 7 ed) ed))
    (setq ed (append ed (list (cons 7 "DIKME-KODU"))))
  )
  (if (assoc 40 ed)
    (setq ed (subst (cons 40 100.0) (assoc 40 ed) ed))
    (setq ed (append ed (list (cons 40 100.0))))
  )
  (entmod ed)
  (entupd en)

  (setq vlaobj (vlax-ename->vla-object en))
  (vla-getboundingbox vlaobj 'minSA 'maxSA)
  (setq minpt (vlax-safearray->list minSA))
  (setq maxpt (vlax-safearray->list maxSA))

  (setq wpEn (DYS:make-wipeout minpt maxpt))

  (command "_draworder" wpEn "" "_front")
  (command "_draworder" en "" "_front")

  (setq gname (strcat "DYSGRP" (itoa basenum) (itoa idx)))
  (command "_.-group" "_create" gname "" en wpEn "")
)

;; DIMENSION icin: sadece stil+yukseklik (cerceve yok, cizgi/ok/konum sabit)
(defun DYS:process-dim (en)
  (command "_.dimoverride" "DIMTXT" 100 "DIMTXSTY" "DIKME-KODU" "" en "")
)

(defun c:CW2 ( / ss n i en etype basenum)
  (setvar "cmdecho" 0)
  (DYS:ensure-style)
  (DYS:ensure-layer)
  (setvar "WIPEOUTFRAME" 1)
  (command "_undo" "_begin")

  (princ "\n[CW2 v3] Alani sec (TEXT + MTEXT + DIMENSION hepsi alinir): ")
  (setq ss (ssget '((-4 . "<OR")
                     (0 . "TEXT")
                     (0 . "MTEXT")
                     (0 . "DIMENSION")
                     (-4 . "OR>"))))

  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (setq basenum (getvar "MILLISECS"))
      (repeat n
        (setq en (ssname ss i))
        (setq etype (cdr (assoc 0 (entget en))))
        (cond
          ((or (= etype "TEXT") (= etype "MTEXT"))
           (DYS:process-text en basenum i)
          )
          ((= etype "DIMENSION")
           (DYS:process-dim en)
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n[CW2 v3] " (itoa n) " nesne guncellendi (TEXT/MTEXT: cerceveli+tek parca, DIMENSION: stil+yukseklik)."))
    )
    (princ "\n[CW2 v3] Secim yapilmadi, islem iptal.")
  )

  (command "_undo" "_end")
  (princ)
)

(princ "\n[CW2 v3] yuklendi. Calistirmak icin: CW2")
(princ)
