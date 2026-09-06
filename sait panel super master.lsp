 (vl-load-com)

;; ==================================================
;; BULUT LISP VE DWG YUKLEME MOTORU
;; ==================================================

(defun SaitBulutIndir (url yerel-yol / xmlhttp stream status)
  (setq xmlhttp (vlax-create-object "MSXML2.XMLHTTP"))
  (if xmlhttp
    (progn
      (vlax-invoke-method xmlhttp 'open "GET" url :vlax-false)
      (vlax-invoke-method xmlhttp 'send)
      (setq status (vlax-get-property xmlhttp 'status))
      (if (= status 200)
        (progn
          (setq stream (vlax-create-object "ADODB.Stream"))
          (vlax-put-property stream 'Type 1)
          (vlax-invoke-method stream 'open)
          (vlax-invoke-method stream 'write (vlax-get-property xmlhttp 'responseBody))
          (vlax-invoke-method stream 'saveToFile yerel-yol 2)
          (vlax-invoke-method stream 'close)
          (vlax-release-object stream)
        )
      )
      (vlax-release-object xmlhttp)
    )
  )
)

(defun SaitScriptCek (url / dosya-adi temp-dir yerel-yol)
  (setq dosya-adi (vl-filename-base url))
  (setq temp-dir (getenv "TEMP"))
  (setq yerel-yol (strcat temp-dir "\\" dosya-adi ".lsp"))
  (if (not (findfile yerel-yol))
    (SaitBulutIndir url yerel-yol)
  )
  (if (findfile yerel-yol)
    (load yerel-yol)
  )
)

(defun SaitBlokCek (url / dosya-adi temp-dir yerel-yol)
  (setq dosya-adi (vl-filename-base url))
  (setq temp-dir (getenv "TEMP"))
  (setq yerel-yol (strcat temp-dir "\\" dosya-adi ".dwg"))
  (princ (strcat "\n[SAT Bulut]: '" dosya-adi "' indiriliyor..."))
  (if (not (findfile yerel-yol))
    (SaitBulutIndir url yerel-yol)
  )
  (if (findfile yerel-yol)
    (progn
      (princ "\n[SAT Bulut]: Blok indirildi, yerlestiriniz...")
      (command "_.INSERT" yerel-yol pause "1" "1" "0")
    )
    (princ (strcat "\n[SAT Bulut HATA]: Dosya indirilemedi! URL: " url))
  )
  (princ)
)

(defun SaitBlokBagla (komut-adi url)
  (eval (list 'defun (read (strcat "c:" komut-adi)) '() (list 'SaitBlokCek url)))
)

;; ==================================================
;; LISP VE BLOK LISTESI
;; ==================================================
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/satjsonyazveoku.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/DAIRECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/KARECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/LINECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/UCGENCIZ.lsp")

;; BLOK BAGLANTISI
(SaitBlokBagla "GENELCEPHE1" "https://raw.githubusercontent.com/candemir22/acadmatrix/main/blocks/genel_cephe1.dwg")

(princ "\n[SaitAI]: Sistem Hazir (DWG Modu)!")
(princ)
