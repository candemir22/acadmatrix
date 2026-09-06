 (vl-load-com)

;; ==================================================
;; 1) BULUT DOSYA INDIRME MOTORU (V2 - KUSURSUZ)
;; ==================================================
(defun SaitBulutIndir (url yerel-yol / webObj streamObj)
  (setq webObj (vlax-create-object "MSXML2.XMLHTTP"))
  (if webObj
    (progn
      (vlax-invoke-method webObj 'Open "GET" url :vlax-false)
      (vlax-invoke-method webObj 'Send)
      (if (= (vlax-get-property webObj 'Status) 200)
        (progn
          (setq streamObj (vlax-create-object "ADODB.Stream"))
          (vlax-put-property streamObj 'Type 1)
          (vlax-invoke-method streamObj 'Open)
          (vlax-invoke-method streamObj 'Write (vlax-get-property webObj 'ResponseBody))
          (vlax-invoke-method streamObj 'SaveToFile yerel-yol 2)
          (vlax-invoke-method streamObj 'Close)
          (vlax-release-object streamObj)
        )
      )
      (vlax-release-object webObj)
    )
  )
)

;; ==================================================
;; 2) LISP (.LSP) CEKME VE YUKLEME
;; ==================================================
(defun SaitScriptCek (url / dosya-adi temp-dir yerel-yol)
  (setq dosya-adi (vl-filename-base url))
  (setq temp-dir (getenv "TEMP"))
  (setq yerel-yol (strcat temp-dir "\\" dosya-adi ".lsp"))
  
  (if (findfile yerel-yol) (vl-file-delete yerel-yol))
  (SaitBulutIndir url yerel-yol)
  
  (if (findfile yerel-yol)
    (progn
      (load yerel-yol)
      (princ (strcat "\n[" (strcase dosya-adi) "] yuklendi. Komut: " (strcase dosya-adi)))
    )
  )
  (princ)
)

;; ==================================================
;; 3) DWG BLOK CEKME VE YERLESTIRME (V2 DUZELTMESI BURADA)
;; ==================================================
(defun SaitBlokCek (url / dosya-adi temp-dir yerel-yol)
  (setq dosya-adi (vl-filename-base url))
  (setq temp-dir (getenv "TEMP"))
  (setq yerel-yol (strcat temp-dir "\\" dosya-adi ".dwg"))
  
  (princ (strcat "\n[SAT Bulut]: '" dosya-adi "' indiriliyor..."))
  
  (if (findfile yerel-yol) (vl-file-delete yerel-yol))
  (SaitBulutIndir url yerel-yol)
  
  (if (findfile yerel-yol)
    (progn
      (princ "\n[SAT Bulut]: Blok indirildi, ekrana tiklayarak yerlestirin...")
      ;; HATA BURADA COZULDU: _.-insert ve eksik parametreler (pause 1 1 0) eklendi!
      (command "_.-insert" yerel-yol pause 1 1 0)
    )
    (princ (strcat "\n[SAT Bulut HATA]: Dosya indirilemedi! Lutfen URL'yi kontrol edin."))
  )
  (princ)
)

;; ==================================================
;; 4) KOMUTLARI DWG'YE BAGLAMA
;; ==================================================
(defun SaitBlokBagla (komut-adi url)
  (eval (list 'defun (read (strcat "c:" komut-adi)) '() (list 'SaitBlokCek url)))
)

;; ==================================================
;; 5) YUKLEME ALANI (SISTEMIN KALBI)
;; ==================================================
(princ "\n[SaitAI]: Bilesenler GitHub'dan cekiliyor. Lutfen bekleyin...")

(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/satjsonyazveoku.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/DAIRECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/KARECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/LINECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/UCGENCIZ.lsp")

;; ALT SATIRA DIKKAT: ARTIK .DWG UZANTILI VE 'blocks' KLASORU EKLENDI
(SaitBlokBagla "GENELCEPHE1" "https://raw.githubusercontent.com/candemir22/acadmatrix/main/blocks/genel_cephe1.dwg")

(princ "\n[SaitAI]: Sistem Hazir (DWG Modu)!")
(princ "\n[SaitAI]: Sistem GitHub uzerinden basariyla ateslendi! Degisiklikleri gormek icin SAT komutunu tekrar girin.")
(princ)
