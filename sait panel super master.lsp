;;; ============================================================
;;; SAT-PANEL.lsp  -- Kendi Komut Panelin (v5.0)
;;;
;;; KOMUT: SAT
;;;
;;; NE ISE YARAR:
;;;   "SAT" yazinca 5 SAYFALI bir pencere acilir. Ustte 5 sayfa
;;;   butonu vardir (SAYFA1..SAYFA5 -- istedigin ada degistirebilirsin),
;;;   birine tiklayinca o sayfanin 5 komut butonu gorunur. Bir komut
;;;   butonuna tiklayinca, o butona baglanmis KOMUTU (senin yazdigin
;;;   herhangi bir .lsp dosyasindaki komut) CALISTIRIR.
;;;
;;;   BAGIMSIZ bir dosyadir -- SAIT.lsp, CW, CW2, IMALAT, SATCW1 vb.
;;;   hicbir dosyaya dokunmaz/baglanmaz. Sadece komut ADINI bilmesi
;;;   yeterlidir (o komutun .lsp dosyasi APPLOAD ile ayrica yuklu
;;;   olmali).
;;;
;;; ============================================================
;;; ASAGISI SENIN DUZENLEYECEGIN TEK YER -- BASKA HICBIR YERE
;;; DOKUNMANA GEREK YOK. Bu dosyayi Not Defteri (veya VS Code) ile
;;; ac, asagidaki listeleri degistir, kaydet, AutoCAD'de tekrar
;;; APPLOAD ile yukle (veya AutoCAD'i kapat-ac). Hepsi bu kadar.
;;;
;;; NASIL DOLDURULUR (COK BASIT, adim adim):
;;;   1) *SAT-SAYFA-ADLARI* listesinde 5 tane sayfa ismi var --
;;;      istedigin isimle degistir (orn: "Dikme" "Yatay" "Cephe" ...).
;;;      Tirnak isaretlerinin ICINI degistir, disina dokunma.
;;;
;;;   2) *SAT-BUTONLAR* listesinde 5 SAYFA var (asagida ";; SAYFA 1"
;;;      gibi yorumlarla ayrilmis). Her sayfanin icinde 5 tane
;;;      (list "Buton Yazisi" "KOMUT-ADI") satiri var:
;;;        - "Buton Yazisi"  -> butonun uzerinde GORUNECEK yazi.
;;;        - "KOMUT-ADI"     -> AutoCAD komut satirina yazinca
;;;                             calisan GERCEK komut adi (orn:
;;;                             "SATCW1", "DIKME-KES", "CW2").
;;;                             c: ONEKI YAZMA -- sadece komut adi.
;;;      Bos birakmak icin ikisini de "" yap: (list "" "")
;;;
;;;   3) Kaydet, APPLOAD ile bu dosyayi yukle, "SAT" yaz -- hazir.
;;;
;;; ORNEK 1 (TEK komut): Buton 3, Sayfa 1'e "Cephe Hesapla" yazisiyla
;;; "CEPHEHESAP" komutunu baglamak istersen, Sayfa 1'in 3. satirini
;;; soyle yaparsin:
;;;   (list "Cephe Hesapla" "CEPHEHESAP")
;;;
;;; ORNEK 2 (SIRALI komutlar, .scr gibi): KOMUT-ADI yerine bir
;;; LISTE yazarsan, o butona basinca listedeki HER ELEMAN sirayla
;;; komut satirina yazilir -- tipki bir .scr (script) dosyasi gibi.
;;; Bos "" = ENTER tusuna basmak demektir. Ornek: once ZOOM->Extents
;;; yapip sonra SATCW1'i calistiran bir buton:
;;;   (list "Zoom+CW" (list "_.ZOOM" "_E" "SATCW1"))
;;; Ornek: bir cizgi cizip sonra kaydeden buton:
;;;   (list "Cizgi+Kaydet" (list "_.LINE" "0,0" "100,100" "" "_.QSAVE"))
;;; ============================================================
(vl-load-com)

;; 8 SAYFA, her sayfada 8 BUTON (asagidaki *SAT-SAYFA-SAYISI* ve
;; *SAT-BUTON-SAYISI* bu sayilari kod tarafinda da belirler --
;; ikisini de degistirmeden sadece bu iki sayiyi degistirerek
;; sayfa/buton adedini ayarlayabilirsin).
(setq *SAT-SAYFA-SAYISI* 8)
(setq *SAT-BUTON-SAYISI* 8)

(setq *SAT-SAYFA-ADLARI*
  (list
    "SAYFA1" "SAYFA2" "SAYFA3" "SAYFA4"
    "SAYFA5" "SAYFA6" "SAYFA7" "SAYFA8"
  )
)

(setq *SAT-BUTONLAR*
  (list
    ;; ------------------ SAYFA 1 (TEST/DEMO sayfasi) ------------------
    (list
      (list "Daire Ciz"  "DAIRECIZ")
      (list "Cizgi Ciz"  "LINECIZ")
      (list "Kare Ciz"   "KARECIZ")
      (list "Ucgen Ciz"  "UCGENCIZ")
      (list "SATCW1"     "SATCW1")
      (list "" "")
      (list "" "")
      (list "" "")
    )
    ;; ------------------ SAYFA 2 ------------------
    (list
      (list "Dikme Kes"     "DIKME-KES")
      (list "Dikme Duzenle" "DIKME-DUZENLE")
      (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "")
    )
    ;; ------------------ SAYFA 3 ------------------
    (list
      (list "" "") (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "") (list "" "")
    )
    ;; ------------------ SAYFA 4 ------------------
    (list
      (list "" "") (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "") (list "" "")
    )
    ;; ------------------ SAYFA 5 ------------------
    (list
      (list "" "") (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "") (list "" "")
    )
    ;; ------------------ SAYFA 6 ------------------
    (list
      (list "" "") (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "") (list "" "")
    )
    ;; ------------------ SAYFA 7 ------------------
    (list
      (list "" "") (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "") (list "" "")
    )
    ;; ------------------ SAYFA 8 ------------------
    (list
      (list "" "") (list "" "") (list "" "") (list "" "")
      (list "" "") (list "" "") (list "" "") (list "" "")
    )
  )
)

;;; ============================================================
;;; ASAGISI KOD -- DOKUNMANA GEREK YOK (istersen okuyabilirsin,
;;; her satirin ne yaptigi yorumlarda anlatiliyor).
;;; ============================================================

;; Aktif sayfa numarasini (0-4) hafizada tutar, panel her acildiginda
;; en son bakilan sayfada acilsin diye.
(setq *SAT-AKTIF-SAYFA* 0)

;; ------------------------------------------------------------
;; "SISTEMI YUKLE" -- Lisplerinin durdugu klasoru bulur ve icindeki
;; TUM .lsp dosyalarini tek seferde yukler.
;;
;; KLASOR SADECE 1 KERE SORULUR: gercek bir KLASOR SECME penceresi
;; acilir (dosya tek tek secmek YOK, direkt klasoru isaretleyip
;; "Klasor Sec" diyeceksin). Sectigin klasor %APPDATA% icine KALICI
;; olarak kaydedilir -- AutoCAD'i kapatip actiginda, hatta baska gun
;; tekrar SAT yazdiginda BIR DAHA SORMAZ, direkt hatirlar. Klasoru
;; degistirmek istersen asagidaki *SAT-KLASOR* satirini nil yap.
;; ------------------------------------------------------------
(setq *SAT-KLASOR* nil)

;; Kalici kayit dosyasinin yolu (%APPDATA%\SAT-KLASOR.lsp)
(defun sat-klasor-dosyasi () (strcat (getenv "APPDATA") "\\SAT-KLASOR.lsp"))

;; Sectigin klasoru kalici dosyaya yazar.
(defun sat-klasor-kaydet (yol / f)
  (setq f (open (sat-klasor-dosyasi) "w"))
  (write-line ";;; SAT-PANEL tarafindan otomatik kaydedildi." f)
  (write-line (strcat "(setq *SAT-KLASOR* \"" yol "\")") f)
  (close f)
)

;; GERCEK bir Windows klasor secme penceresi acar (dosya degil,
;; direkt klasor secilir). Iptal edilirse nil doner.
(defun sat-klasor-sec ( / shell klasor secilen yol)
  (setq shell (vlax-create-object "Shell.Application"))
  (setq klasor (vlax-invoke-method shell 'BrowseForFolder 0
                 "SAT: Lisp dosyalarinin bulundugu KLASORU sec" 0))
  (setq yol nil)
  (if klasor
    (progn
      (setq secilen (vlax-get-property klasor 'Self))
      (setq yol (vlax-get-property secilen 'Path))
      ;; ters slash \ -> duz slash / (AutoLISP'te ikisi de calisir,
      ;; ama duz slash kayit/okumada sorun cikarmaz)
      (setq yol (vl-string-translate "\\" "/" yol))
    )
  )
  (vlax-release-object shell)
  yol
)

;; Klasoru bulur -- SIRASI: 1) hafizada varsa onu kullan, 2) daha
;; once kaydedilmis dosya varsa oradan oku, 3) hicbiri yoksa VEYA
;; kayitli klasor ARTIK YOKSA (silinmis/tasinmis) klasor secme
;; penceresi ac ve KALICI kaydet.
(defun sat-klasor-bul ( / yol)
  (if (not *SAT-KLASOR*)
    (if (findfile (sat-klasor-dosyasi))
      (load (sat-klasor-dosyasi))
    )
  )
  ;; Kayitli klasor VARSA ama artik DISKTE YOKSA (dosyayi baska
  ;; yere attiysan) -- unut, yeniden sor.
  (if (and *SAT-KLASOR* (not (vl-file-directory-p *SAT-KLASOR*)))
    (progn
      (princ (strcat "\n[SAT] Kayitli klasor artik bulunamiyor (" *SAT-KLASOR* ") -- yeniden soruluyor."))
      (setq *SAT-KLASOR* nil)
    )
  )
  (if (not *SAT-KLASOR*)
    (progn
      (princ "\n[SAT] Lisp dosyalarini nereye attin? Klasor secme penceresi aciliyor...")
      (setq yol (sat-klasor-sec))
      (if yol
        (progn
          (setq *SAT-KLASOR* yol)
          (sat-klasor-kaydet yol)
          (princ (strcat "\n[SAT] Klasor kaydedildi: " yol))
        )
        (princ "\n[SAT] Klasor secilmedi, iptal edildi.")
      )
    )
  )
  *SAT-KLASOR*
)

;; ------------------------------------------------------------
;; KALICI VERI KAYDI: sayfa/buton tablon (*SAT-SAYFA-ADLARI*,
;; *SAT-BUTONLAR*, sayilar) %APPDATA%\SAT-VERILER.lsp dosyasina
;; yazilir. Boylece "Sayfa Ekle"/"Buton Ekle" ile ekledigin her sey
;; dosyayi elle ACMADAN, KALICI olarak saklanir -- AutoCAD'i
;; kapatip acsan bile kaybolmaz.
;; ------------------------------------------------------------
(defun sat-veri-dosyasi () (strcat (getenv "APPDATA") "\\SAT-VERILER.lsp"))

(defun sat-veri-kaydet ( / f)
  (setq f (open (sat-veri-dosyasi) "w"))
  (write-line (vl-prin1-to-string *SAT-SAYFA-SAYISI*) f)
  (write-line (vl-prin1-to-string *SAT-BUTON-SAYISI*) f)
  (write-line (vl-prin1-to-string *SAT-SAYFA-ADLARI*) f)
  (write-line (vl-prin1-to-string *SAT-BUTONLAR*) f)
  (close f)
)

;; Kayitli veri varsa okur ve dosyadaki VARSAYILAN tablonun UZERINE
;; yazar. Boylece SAT-PANEL.lsp'yi yeniden APPLOAD etsen bile senin
;; eklediklerin kaybolmaz.
(defun sat-veri-oku ( / f line)
  (if (findfile (sat-veri-dosyasi))
    (progn
      (setq f (open (sat-veri-dosyasi) "r"))
      (setq line (read-line f)) (if line (setq *SAT-SAYFA-SAYISI* (read line)))
      (setq line (read-line f)) (if line (setq *SAT-BUTON-SAYISI* (read line)))
      (setq line (read-line f)) (if line (setq *SAT-SAYFA-ADLARI* (read line)))
      (setq line (read-line f)) (if line (setq *SAT-BUTONLAR* (read line)))
      (close f)
      T
    )
    nil
  )
)

;; N elemanli bos bir sayfa (hepsi (list "" "")) uretir.
(defun sat-bos-sayfa ( / lst)
  (setq lst nil)
  (repeat *SAT-BUTON-SAYISI* (setq lst (append lst (list (list "" "")))))
  lst
)

;; Bir listenin idx.'inci elemanini val ile degistirir, yeni listeyi
;; dondurur (orijinal liste degismez).
(defun sat-liste-degistir (lst idx val / i out)
  (setq i 0 out nil)
  (foreach a lst
    (setq out (append out (list (if (= i idx) val a))))
    (setq i (1+ i))
  )
  out
)

;; "SAYFA EKLE" butonu -> komut satirinda yeni sayfa adini sorar,
;; ekler, kalici kaydeder, yeni sayfaya gecer.
(defun sat-sayfa-ekle ( / isim)
  (setq isim (getstring T "\nYeni sayfanin adi (bosluksuz, orn: CEPHE): "))
  (if (and isim (/= isim ""))
    (progn
      (setq *SAT-SAYFA-ADLARI* (append *SAT-SAYFA-ADLARI* (list isim)))
      (setq *SAT-BUTONLAR* (append *SAT-BUTONLAR* (list (sat-bos-sayfa))))
      (setq *SAT-SAYFA-SAYISI* (1+ *SAT-SAYFA-SAYISI*))
      (setq *SAT-AKTIF-SAYFA* (1- *SAT-SAYFA-SAYISI*))
      (sat-veri-kaydet)
      (princ (strcat "\n[SAT] '" isim "' sayfasi eklendi ve kalici kaydedildi."))
    )
    (princ "\n[SAT] Iptal edildi.")
  )
)

;; "BUTON EKLE" butonu -> AKTIF sayfadaki ILK BOS butonu bulur,
;; komut satirinda yazi + komut adini sorar, doldurur, kalici
;; kaydeder.
;; "a;b;c" gibi ";" ile ayrilmis bir yaziyi ("a" "b" "c") listesine
;; ceviri. Bos parcalar (";;" gibi) "" (ENTER) olarak kalir.
(defun sat-split-pipe (s / i ch tok out)
  (setq i 1 tok "" out nil)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (= ch ";")
      (progn (setq out (append out (list tok))) (setq tok ""))
      (setq tok (strcat tok ch))
    )
    (setq i (1+ i))
  )
  (setq out (append out (list tok)))
  out
)

(defun sat-buton-ekle ( / sayfa bos-idx i slot yazi komut komut-bos-mu)
  (setq sayfa (nth *SAT-AKTIF-SAYFA* *SAT-BUTONLAR*))
  (setq bos-idx nil i 0)
  (foreach slot sayfa
    (if (and (not bos-idx) (or (not (car slot)) (= (car slot) "")))
      (setq bos-idx i)
    )
    (setq i (1+ i))
  )
  (cond
    ((not bos-idx)
     (princ (strcat "\n[SAT] '" (nth *SAT-AKTIF-SAYFA* *SAT-SAYFA-ADLARI*)
                     "' sayfasinda bos buton kalmadi -- once 'Sayfa Ekle' ile yeni sayfa ac."))
    )
    (T
     (princ (strcat "\n[SAT] '" (nth *SAT-AKTIF-SAYFA* *SAT-SAYFA-ADLARI*)
                     "' sayfasinin " (itoa (1+ bos-idx)) ". (bos) butonunu dolduruyorsun."))
     (setq yazi (getstring T "\nButon uzerinde gorunecek YAZI: "))
     (princ "\nCalisacak KOMUT adi (c: onekisiz, orn: SATCW1).")
     (princ "\nBirden fazla adim icin ';' ile ayir (orn: _.ERASE;_ALL;): ")
     (setq komut (getstring T ""))
     (setq komut-bos-mu (or (not komut) (= komut "")))
     ;; ";" varsa SIRALI KOMUT (liste) yap, yoksa TEK KOMUT (duz yazi)
     (if (and (not komut-bos-mu) (vl-string-search ";" komut))
       (setq komut (sat-split-pipe komut))
     )
     (if (and yazi (/= yazi "") (not komut-bos-mu))
       (progn
         (setq sayfa (sat-liste-degistir sayfa bos-idx (list yazi komut)))
         (setq *SAT-BUTONLAR* (sat-liste-degistir *SAT-BUTONLAR* *SAT-AKTIF-SAYFA* sayfa))
         (sat-veri-kaydet)
         (princ (strcat "\n[SAT] Buton eklendi ve kalici kaydedildi: " yazi " -> " (vl-prin1-to-string komut)))
       )
       (princ "\n[SAT] Iptal edildi.")
     )
    )
  )
)

;; Bulunan klasordeki TUM .lsp dosyalarini yukler.
;; NOT: hatalar artik GIZLENMIYOR -- bir dosya yuklenirken hata
;; olursa "[HATA] dosyaadi -- mesaj" seklinde acikca yazilir, o
;; dosya sayilmaz ama digerlerinin yuklenmesine devam edilir.
(defun sat-load-all ( / klasor files sayac sonuc)
  (setq klasor (sat-klasor-bul))
  (cond
    ((not klasor)
     (princ "\n[SAT] Klasor bulunamadi, yukleme iptal edildi."))
    (T
     (setq files (vl-directory-files klasor "*.lsp" 1))
     (if (not files)
       (princ (strcat "\n[SAT] '" klasor "' klasorunde .lsp dosyasi bulunamadi."))
       (progn
         (setq sayac 0)
         (foreach fl files
           (setq sonuc
             (vl-catch-all-apply
               (function (lambda () (load (strcat klasor "\\" fl))))
             )
           )
           (if (vl-catch-all-error-p sonuc)
             (princ (strcat "\n  [HATA] " fl " -- " (vl-catch-all-error-message sonuc)))
             (progn
               (setq sayac (1+ sayac))
               (princ (strcat "\n  [OK] " fl))
             )
           )
         )
         (princ (strcat "\n[SAT] " (itoa sayac) "/" (itoa (length files)) " dosya yuklendi (" klasor ")."))
       )
     )
    )
  )
)

;; ------------------------------------------------------------
;; DCL dosyasini gecici bir dosyaya YAZAR. sayfa-no = 0..4,
;; hangi sayfanin butonlarinin gosterilecegini belirler.
;; ------------------------------------------------------------
(defun sat-dcl-hazirla (sayfa-no / dosya f i sayfa-adi slot lbl)
  (setq dosya (vl-filename-mktemp "SATPANEL" nil ".dcl"))
  (setq f (open dosya "w"))

  (write-line "sat_panel : dialog {" f)
  (write-line (strcat "  label = \"SAT PANEL -- " (nth sayfa-no *SAT-SAYFA-ADLARI*) "\";") f)
  (write-line "  : column {" f)

  ;; --- EN UST: sabit "Sistemi Yukle" / "Sayfa Ekle" / "Buton Ekle" ---
  (write-line "    : boxed_row {" f)
  (write-line "      label = \"\";" f)
  (write-line "      : button { key = \"yukle_btn\"; label = \">> SISTEMI YUKLE <<\"; width = 22; fixed_width = true; }" f)
  (write-line "      : button { key = \"sayfa_ekle_btn\"; label = \"+ Sayfa Ekle\"; width = 14; fixed_width = true; }" f)
  (write-line "      : button { key = \"buton_ekle_btn\"; label = \"+ Buton Ekle\"; width = 14; fixed_width = true; }" f)
  (write-line "    }" f)
  (write-line "    : spacer { height = 0.2; }" f)

  ;; --- Ust sira: N SAYFA butonu (sekme gibi calisir) ---
  (write-line "    : boxed_row {" f)
  (write-line "      label = \"Sayfalar\";" f)
  (setq i 0)
  (while (< i *SAT-SAYFA-SAYISI*)
    (setq sayfa-adi (nth i *SAT-SAYFA-ADLARI*))
    (write-line
      (strcat "      : button { key = \"sayfa_btn" (itoa (1+ i))
              "\"; label = \"" sayfa-adi "\"; width = 10; fixed_width = true; }")
      f
    )
    (setq i (1+ i))
  )
  (write-line "    }" f)

  (write-line "    : spacer { height = 0.3; }" f)

  ;; --- Alt sira: SECILI SAYFANIN N komut butonu ---
  (write-line "    : boxed_column {" f)
  (write-line (strcat "      label = \"" (nth sayfa-no *SAT-SAYFA-ADLARI*) " Komutlari\";") f)
  (setq i 0)
  (while (< i *SAT-BUTON-SAYISI*)
    (setq slot (nth i (nth sayfa-no *SAT-BUTONLAR*)))
    (setq lbl (car slot))
    (if (or (not lbl) (= lbl "")) (setq lbl (strcat "Bos " (itoa (1+ i)))))
    (write-line
      (strcat "      : button { key = \"cmd_btn" (itoa (1+ i))
              "\"; label = \"" lbl "\"; width = 28; fixed_width = true; }")
      f
    )
    (setq i (1+ i))
  )
  (write-line "    }" f)

  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  : button { key = \"kapat_btn\"; label = \"Kapat\"; is_cancel = true; }" f)
  (write-line "}" f)
  (close f)
  dosya
)

;; ------------------------------------------------------------
;; ANA KOMUT: SAT
;; ------------------------------------------------------------
(defun c:SAT ( / dcl-dosya dcl_id i kod slot fnstr devam csym)
  (setq devam T)
  (while devam
    (setq devam nil)
    (setq dcl-dosya (sat-dcl-hazirla *SAT-AKTIF-SAYFA*))
    (setq dcl_id (load_dialog dcl-dosya))

    (if (not (new_dialog "sat_panel" dcl_id))
      (princ "\nSAT paneli acilamadi.")
      (progn
        ;; SAYFA butonlari: tiklaninca o sayfaya geçip PANELI
        ;; YENIDEN ACAR (done_dialog kodu 100+sayfa-no).
        (setq i 0)
        (while (< i *SAT-SAYFA-SAYISI*)
          (action_tile
            (strcat "sayfa_btn" (itoa (1+ i)))
            (strcat "(done_dialog " (itoa (+ 100 i)) ")")
          )
          (setq i (1+ i))
        )
        ;; KOMUT butonlari: tiklaninca o butonun sirasini dondurur
        ;; (done_dialog kodu 0..*SAT-BUTON-SAYISI*-1).
        (setq i 0)
        (while (< i *SAT-BUTON-SAYISI*)
          (action_tile
            (strcat "cmd_btn" (itoa (1+ i)))
            (strcat "(done_dialog " (itoa i) ")")
          )
          (setq i (1+ i))
        )
        (action_tile "kapat_btn" "(done_dialog -1)")
        ;; SISTEMI YUKLE / SAYFA EKLE / BUTON EKLE butonlari
        (action_tile "yukle_btn" "(done_dialog 200)")
        (action_tile "sayfa_ekle_btn" "(done_dialog 300)")
        (action_tile "buton_ekle_btn" "(done_dialog 301)")

        (setq kod (start_dialog))
        (unload_dialog dcl_id)
        (vl-catch-all-apply 'vl-file-delete (list dcl-dosya))

        (cond
          ;; Bir SAYFA butonuna basildi -> o sayfaya gec, paneli
          ;; TEKRAR AC (devam=T dongu basa sarar).
          ((and (>= kod 100) (< kod (+ 100 *SAT-SAYFA-SAYISI*)))
           (setq *SAT-AKTIF-SAYFA* (- kod 100))
           (setq devam T)
          )
          ;; Bir KOMUT butonuna basildi -> o komutu calistir.
          ((and (>= kod 0) (< kod *SAT-BUTON-SAYISI*))
           (setq slot (nth kod (nth *SAT-AKTIF-SAYFA* *SAT-BUTONLAR*)))
           (setq fnstr (if slot (cadr slot) ""))
           (cond
             ((or (not fnstr) (= fnstr ""))
              (princ "\nBu butona henuz bir komut yazilmadi. (SAT-PANEL.lsp dosyasini ac, yukaridaki tabloyu doldur.)")
             )
             ;; TEK KOMUT: "KOMUT-ADI" duz yazi ise. ONCE bunun bir
             ;; AutoLISP fonksiyonu olarak (c:KOMUTADI) TANIMLI olup
             ;; olmadigina bakar ve OYLEYSE DOGRUDAN CAGIRIR -- bu,
             ;; "Sistemi Yukle" ile AZ ONCE yuklenmis bir komutun,
             ;; AutoCAD'in komut tablosunu henuz tanimadigi icin
             ;; "Unknown command" hatasi vermesini ONLER (komut
             ;; tablosunu hic beklemez, fonksiyonu direkt calistirir).
             ;; Boyle bir fonksiyon YOKSA (native AutoCAD komutu ise,
             ;; orn. "CO", "_.LINE") eskisi gibi komut satirina yazar.
             ((= (type fnstr) 'STR)
              (setq csym (vl-catch-all-apply 'read (list (strcat "c:" fnstr))))
              (if (and (not (vl-catch-all-error-p csym)) (boundp csym))
                (eval (list csym))
                (command fnstr)
              )
             )
             ;; SIRALI KOMUT (SCR gibi): KOMUT-ADI yerine bir LISTE
             ;; yazilmissa, listedeki HER ELEMANI sirayla komut
             ;; satirina yazar -- tipki bir .scr dosyasi gibi. Bos
             ;; string "" = ENTER tusuna basmak demektir.
             ((= (type fnstr) 'LIST)
              (apply 'command fnstr)
             )
           )
          )
          ;; SISTEMI YUKLE butonu -> klasordeki TUM .lsp'leri yukle.
          ;; Bu dosyanin KENDISI de yeniden yuklendigi icin (fonksiyon
          ;; tanimlari degisebilir), guvenlik amacli PANELI TEKRAR
          ;; ACMIYORUZ -- kullaniciya "SAT'i tekrar yaz" diyoruz.
          ((= kod 200)
           (sat-load-all)
           (princ "\n[SAT] Yukleme bitti. Degisiklikleri gormek icin SAT'i tekrar yaz.")
          )
          ;; SAYFA EKLE -> komut satirinda sorar, ekler, PANELI
          ;; TEKRAR ACAR (yeni sayfa uzerinde).
          ((= kod 300)
           (sat-sayfa-ekle)
           (setq devam T)
          )
          ;; BUTON EKLE -> komut satirinda sorar, doldurur, PANELI
          ;; TEKRAR ACAR.
          ((= kod 301)
           (sat-buton-ekle)
           (setq devam T)
          )
          ;; Kapat / ESC -> hicbir sey yapma.
          (T nil)
        )
      )
    )
  )
  (princ)
)

;; Daha once "Sayfa Ekle"/"Buton Ekle" ile kaydedilmis ozel bir
;; tablo varsa, asagidaki VARSAYILAN tablonun UZERINE yazar --
;; boylece eklediklerin dosyayi tekrar APPLOAD etsen bile kaybolmaz.
(if (sat-veri-oku)
  (princ "\n(Kayitli SAT-VERILER.lsp bulundu ve yuklendi -- sayfa/buton tablon ozellestirilmis.)")
)

(princ "\nSAT-PANEL yuklendi. Komut: SAT")
(princ "\n(Yeni sayfa/buton eklemek icin panelin USTUNDEKI")
(princ "\n '+ Sayfa Ekle' / '+ Buton Ekle' butonlarini kullan -- dosyayi")
(princ "\n acmana GEREK YOK, kalici olarak kendiliginden kaydedilir.)")
(princ)
