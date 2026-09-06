 ;;; ============================================================
;;; SAIT PANEL SUPER MASTER -- BULUT YUKLEYICI (KESIN COZUM)
;;; ============================================================
(vl-load-com)

;; 1. GUVENLI LISP CEKME FONKSIYONU
(defun SaitScriptCek (url / h tmp f code res)
  (setq h (vlax-create-object "MSXML2.XMLHTTP"))
  (if h
    (progn
      (setq res (vl-catch-all-apply
                  '(lambda ()
                     (vlax-invoke-method h 'open "GET" url :vlax-false)
                     (vlax-invoke-method h 'send)
                     (vlax-get-property h 'responseText)
                   )
                )
      )
      (vlax-release-object h) 

      (if (and (not (vl-catch-all-error-p res))
               (= (type res) 'STR)
               (> (strlen res) 0))
        (progn
          (setq tmp (vl-filename-mktemp "sait" nil ".lsp"))
          (setq f (open tmp "w"))
          (write-line res f)
          (close f)
          (vl-catch-all-apply 'load (list tmp))
          (vl-catch-all-apply 'vl-file-delete (list tmp))
        )
      )
    )
  )
  (princ)
)

;; 2. BULUTTAN DOSYAYI DISKE INDIREN SISTEM FONKSIYONU
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
          (vlax-put-property stream 'Type 1) ; Binary Mode
          (vlax-invoke-method stream 'open)
          (vlax-invoke-method stream 'write (vlax-get-property xmlhttp 'responseBody))
          (vlax-invoke-method stream 'saveToFile yerel-yol 2) ; Save & Overwrite
          (vlax-invoke-method stream 'close)
          (vlax-release-object stream)
        )
      )
      (vlax-release-object xmlhttp)
    )
  )
  (findfile yerel-yol)
)

;; 3. DXF EKLEME MOTORU
(defun SaitDxfCek (url / blok-adi temp-yol)
  (setq blok-adi (vl-filename-base url))
  (if (tblsearch "BLOCK" blok-adi)
    (progn
      (princ (strcat "\n[SAT Bulut]: '" blok-adi "' cizimde var, ekleniyor..."))
      (command "._-INSERT" blok-adi pause 1 1 0)
    )
    (progn
      (princ (strcat "\n[SAT Bulut]: '" blok-adi "' indiriliyor..."))
      (setq temp-yol (strcat (getenv "TEMP") "\\" blok-adi ".dxf"))
      (if (SaitBulutIndir url temp-yol)
        (progn
          (princ "\n[SAT Bulut]: Indirme tamamlandi, ekleniyor...")
          (command "._-INSERT" temp-yol pause 1 1 0)
          (princ (strcat "\n[SAT Bulut]: '" blok-adi "' eklendi!"))
        )
        (princ (strcat "\n[SAT Bulut HATA]: Dosya indirilemedi! URL: " url))
      )
    )
  )
  (princ)
)

;; 4. DXF BAGLANTI FONKSIYONU
(defun SaitDxfBagla (komut-adi url)
  (eval (list 'defun (read (strcat "c:" komut-adi)) '() (list 'SaitDxfCek url)))
)

;; ============================================================
;; >>> A. LISP LISTESI
;; ============================================================
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/satjsonyazveoku.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/DAIRECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/KARECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/LINECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/UCGENCIZ.lsp")

;; ============================================================
;; >>> B. DXF BLOK LISTESI
;; ============================================================
(SaitDxfBagla "GENELCEPHE1" "https://raw.githubusercontent.com/candemir22/acadmatrix/main/bloklar/genel_cephe1.dxf")
(SaitDxfBagla "GENELCEPHE1" "https://raw.githubusercontent.com/candemir22/acadmatrix/main/blocks/genel_cephe1.dxf")
(princ "\n[SaitAI]: Sistem Hazir!")
(princ)
