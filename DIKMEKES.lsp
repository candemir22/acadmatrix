;;; ============================================================
;;; DIKME-KES.lsp  -- "Dikme Kes" araci (v1.7)
;;;
;;; KOMUT: DIKME-KES
;;;
;;; ONKOSUL: SAIT-CIZIM-YARDIMCILARI.lsp de yuklenmis olmali (bu
;;; dosya onun fonksiyonlarini kullanir: poligon-ciz, bolgeyi-
;;; garanti-patlat, bandi-trimle-genel, kesim-katmani-hazirla,
;;; benzersiz-ad, vb.)
;;;
;;; NE YAPAR (ARA-KES'in kanitlanmis "Kes" fonksiyonundan --
;;; ara-kes-yap -- birebir uyarlandi):
;;;   1) Alan secimi: Dikdortgen VEYA Poligon (PL, nokta nokta).
;;;   2) Once "Kopyayi nereye yerlestireyim?" diye ADRES sorar.
;;;      Iptal edersen cizimde hicbir iz kalmaz.
;;;   3) Orijinal HICBIR SEKILDE degismez/silinmez -- once bir
;;;      KOPYASI cikarilir, o kopya hedefe tasinir.
;;;   4) Isim/etiket SORULMAZ -- KOD otomatik uretilir: DK-1, DK-2...
;;;   5) AYNI KOD, hem ORIJINALIN hem KOPYANIN sinir kutusunun
;;;      SAG-UST kosesine, Arial fontla, MAVI renkte yazilir.
;;;   6) Hedefte GARANTILI SADECE BLOK kalir -- -BLOCK'un icine
;;;      girmeyen hicbir kalinti/tasan parca birakilmaz
;;;      (dikme-kalinti-temizle).
;;;   7) KESIM-SINIRI katmanindaki (onceki kesimlerden kalma sinir
;;;      dikdortgenleri) ARTIK HICBIR ZAMAN yeni bir kesimde "icerik"
;;;      olarak secilmez/kopyalanmaz -- gurultu/yanlis dikme algisi
;;;      onlendi.
;;;   XDATA damgalama YOK (ileride eklenecek).
;;;
;;; VERSIYON GECMISI:
;;;   v1 -> v1.5: bkz. eski SAIT.lsp gecmisi -- TEST EDILDI, CALISTI.
;;;   v1.6 -- mimari degisikligi: SAIT.lsp'den ayri dosyaya tasindi.
;;;     Ayrica: ss-content secimine "~KESIM-SINIRI" katman FILTRESI
;;;     eklendi (ssget filtre listesiyle) -- ama bu "_CP" modunda
;;;     GUVENILMEZ CALISTI, secim BOS donup "Bu alanda kopyalanacak
;;;     nesne yok" hatasina yol acti (TEST EDILDI, CALISMADI).
;;;   v1.7 (BU DOSYA) -- DUZELTME: filtre listesi yaklasimi terk
;;;     edildi. Bunun yerine ONCE normal "_CP" ile secim yapilir,
;;;     SONRA secim icinde dolasilip KESIM-SINIRI katmanindakiler
;;;     tek tek cikarilir (ssdel dongusu) -- kanitlanmis, guvenilir
;;;     yontem.
;;; ============================================================

(setq *DIKME-KOD-YUKSEKLIK* 40.0)

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
  (setvar "CECOLOR" "5") ;; 5 = mavi (ACI)
  (vl-catch-all-apply
    '(lambda () (command "_.-TEXT" "_J" "TR" pt *DIKME-KOD-YUKSEKLIK* "0" kod ""))
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
  ;; basla-ent'ten (COPY'den HEMEN ONCEKI son nesne) itibaren olusan
  ;; HER SEYI dolasir; koru-listesi'nde OLMAYAN (yani -BLOCK'un icine
  ;; girmemis, TRIM'in tam temizleyemedigi kalinti/tasan parca) her
  ;; nesneyi KESIN olarak siler. TRIM'in tam olarak NEDEN eksik
  ;; biraktigini bilmeye gerek yok -- sonucta sadece blok (ve
  ;; koru-listesindekiler) hayatta kalir, gerisi temizlenir.
  (setq e (if basla-ent (entnext basla-ent) (entnext)))
  (while e
    (if (and (entget e) (not (member e koru-listesi)))
      (entdel e)
    )
    (setq e (entnext e))
  )
)

(defun dikme-kes-yap ( / mod-kw p1 p2 noktalar ix1 iy1 ix2 iy2 np
                        ic-nokta hedef dx dy
                        orig-ic-poly
                        noktalar-c ic-nokta-c ic-poly-c oc-poly-c
                        eskikatman ss-content basla-ent ss-i ss-e
                        ssblok kod blokadi kod-ent
                        son-ent atlanan-liste)

  (initget "Dikdortgen Poligon PL")
  (setq mod-kw
    (getkword "\nAlan secim yontemi [Dikdortgen/Poligon(PL)] <Dikdortgen>: ")
  )
  (if (not mod-kw) (setq mod-kw "Dikdortgen"))
  (if (= mod-kw "PL") (setq mod-kw "Poligon"))

  (if (= mod-kw "Poligon")
    ;; ---- POLIGON (PL) MODU: duzensiz/coklu koseli alan ----
    (setq noktalar (poligon-nokta-topla))
    ;; ---- DIKDORTGEN MODU (eski davranis) ----
    (progn
      (setq p1 (getpoint "\nKOPYALANACAK DIKME ALANININ ilk kosesini secin: "))
      (if (not p1) (progn (princ "\nIptal.") (exit)))
      (setq p2 (getcorner p1 "\nKARSI KOSEYI secin: "))
      (if (not p2) (progn (princ "\nIptal.") (exit)))
      (setq ix1 (min (car p1) (car p2))) (setq ix2 (max (car p1) (car p2)))
      (setq iy1 (min (cadr p1) (cadr p2))) (setq iy2 (max (cadr p1) (cadr p2)))
      (setq noktalar
        (list (list ix1 iy1 0.0) (list ix2 iy1 0.0) (list ix2 iy2 0.0) (list ix1 iy2 0.0))
      )
    )
  )

  ;; sinirlayici kutu (adres/kod konumu icin -- her iki modda da gecerli)
  (setq ix1 (car (car noktalar))) (setq ix2 (car (car noktalar)))
  (setq iy1 (cadr (car noktalar))) (setq iy2 (cadr (car noktalar)))
  (foreach np noktalar
    (setq ix1 (min ix1 (car np))) (setq ix2 (max ix2 (car np)))
    (setq iy1 (min iy1 (cadr np))) (setq iy2 (max iy2 (cadr np)))
  )
  (setq ic-nokta (list ix1 iy1 0.0)) ;; sol-alt referans (COPY/BLOCK basepoint)

  ;; DEGISIKLIK 1 (istegin uzerine): ONCE ADRES SORULUR -- iptal edersen
  ;; cizimde HICBIR IZ kalmaz (orijinal siniri bile cizilmez)
  (setq hedef (getpoint ic-nokta "\nKopyayi nereye yerlestireyim? "))
  (if (not hedef) (progn (princ "\nIptal -- hicbir sey degismedi.") (exit)))
  (setq dx (- (car hedef) ix1)) (setq dy (- (cadr hedef) iy1))

  ;; ORIJINAL KONUMDA IC SINIR -- KALICI, orijinal ASLA silinmez/tasinmaz
  ;; (ara-kes-yap'ta bu sinir SONUNDA bloga girip orijinal konumdan
  ;; KAYBOLURDU -- DIKME-KES'te asla bir selection'a eklenmiyor, bu
  ;; yuzden hep orijinal yerinde kalir)
  (kesim-katmani-hazirla)
  (setq eskikatman (getvar "CLAYER"))
  (setvar "CLAYER" "KESIM-SINIRI")
  (setq orig-ic-poly (poligon-ciz noktalar))
  (setvar "CLAYER" eskikatman)

  ;; KOPYALANACAK ICERIK: sinirin cevresindeki her sey (disariya tasan
  ;; parcalar da dahil -- kopya tarafinda ayni sekilde trim edilecek),
  ;; MINUS az once cizdigimiz sinirin kendisi.
  (setq ss-content (ssget "_CP" noktalar))
  (if ss-content
    (if (entget orig-ic-poly) (setq ss-content (ssdel orig-ic-poly ss-content)))
  )
  ;; KESIM-SINIRI katmanindaki (onceki kesimlerden kalma sinir
  ;; dikdortgenleri) HICBIR ZAMAN icerik olarak secilmez -- yoksa
  ;; gurultu olarak kopyalanip dikme sanilabilir. (NOT: bunu ssget'in
  ;; kendi filtre listesiyle degil, secim SONRASI eleyerek yapiyoruz --
  ;; "_CP" modunda filtre listesi guvenilir calismiyordu.)
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
    (progn (princ "\nBu alanda kopyalanacak nesne yok. Iptal.") (exit))
  )

  ;; DEGISIKLIK 2 (istegin uzerine): ara-kes-yap burada secileni
  ;; DOGRUDAN bloga alip orijinal yerden SILERDI (-BLOCK komutu kaynak
  ;; nesneleri tuketir). DIKME-KES'te once bir KOPYASI cikarilip o
  ;; kopya HEDEF noktaya tasiniyor -- orijinal ss-content'e hic
  ;; dokunulmuyor.
  (setq basla-ent (entlast)) ;; kalinti temizligi icin baslangic noktasi
  (command "_.COPY" ss-content "" "_D" (list dx dy 0.0))

  ;; kopyanin sinir noktalari (ayni sekil, dx/dy kadar kaymis)
  (setq noktalar-c nil)
  (foreach np noktalar
    (setq noktalar-c (cons (list (+ (car np) dx) (+ (cadr np) dy) 0.0) noktalar-c))
  )
  (setq noktalar-c (reverse noktalar-c))
  (setq ic-nokta-c (list (+ ix1 dx) (+ iy1 dy) 0.0))

  (setvar "CLAYER" "KESIM-SINIRI")
  (setq ic-poly-c (poligon-ciz noktalar-c))
  (setvar "CLAYER" eskikatman)

  (setq oc-poly-c (poligon-yonlu-offset ic-poly-c *OFSET-ARALIK* T))
  (if (not oc-poly-c)
    (progn
      (if (entget ic-poly-c) (entdel ic-poly-c))
      (princ "\nKesim siniri disari genisletilemedi (kendini kesen/karmasik bir sekil olabilir). Iptal.")
      (exit)
    )
  )

  ;; ara-kes-yap ile AYNEN AYNI patlatma + trim mekanizmasi -- sadece
  ;; artik kopyanin ustunde calisiyor, orijinalin degil
  (setq atlanan-liste (bolgeyi-garanti-patlat (poligon-koseler oc-poly-c)))
  (bandi-trimle-genel ic-poly-c oc-poly-c)
  (command "_.REGEN")

  (setq ssblok (ssget "_CP" (poligon-koseler ic-poly-c)))
  (if (not ssblok)
    (progn (princ "\nHazirlik sirasinda beklenmeyen bir sorun olustu. Iptal.") (exit))
  )

  ;; DEGISIKLIK 3 (istegin uzerine): isim SORULMAZ -- KOD otomatik
  ;; uretilir (DK-1, DK-2...)
  (setq kod (dikme-sonraki-kod))
  (setq blokadi (benzersiz-ad kod))

  ;; DEGISIKLIK 4 (istegin uzerine): ara-kes-yap'taki tek, sol-ust,
  ;; siyah "etiket-yaz" yerine -- KOPYANIN sag-ust kosesine Arial,
  ;; mavi KOD yazilir (bloga gomulur)
  (dikme-kod-yaz (list (+ ix2 dx) (+ iy2 dy) 0.0) kod)
  (setq kod-ent (entlast))
  (if kod-ent (setq ssblok (ssadd kod-ent ssblok)))

  ;; basepoint = kopyanin sinirlayici kutusunun sol-alt kosesi
  (command "_.-BLOCK" blokadi ic-nokta-c ssblok "")
  ;; ayni noktaya geri yerlestir -- kopya zaten dogru yerde, grid YOK
  (command "_.-INSERT" blokadi ic-nokta-c "1" "1" "0")
  (setq son-ent (entlast))

  ;; KESIN KALINTI TEMIZLIGI: -BLOCK'un icine girmeyen (yani TRIM'in
  ;; tam temizleyemedigi, sinirdan tasan) HER SEY burada silinir.
  ;; oc-poly-c ve yeni blok (son-ent) haric -- COPY'den bu yana olusan
  ;; baska HICBIR SEY hayatta kalmaz.
  (dikme-kalinti-temizle basla-ent (list son-ent oc-poly-c))

  ;; DEGISIKLIK 5 (istegin uzerine): AYNI KOD, ORIJINALIN de sag-ust
  ;; kosesine (Arial, mavi) -- boylece kopya ile orijinal kod
  ;; eslesmesiyle gorsel olarak baglanir. XDATA damgalama YOK (ileride).
  (dikme-kod-yaz (list ix2 iy2 0.0) kod)

  (if (entget oc-poly-c) (entdel oc-poly-c))
  (command "_.REGEN")
  (princ (strcat "\n\"" kod "\" kopyalandi -- orijinal HICBIR SEKILDE degismedi."))
  (if atlanan-liste
    (princ (strcat "\n(Not: " (itoa (length atlanan-liste)) " blok otomatik parcalanamadi, "
                   "oldugu gibi kopyaya dahil edildi.)"))
  )
  (princ)
)

(defun c:DIKME-KES ( / )
  (dikme-kes-yap)
  (princ)
)

(princ "\nDIKME-KES yuklendi. Komut: DIKME-KES")
(princ)
