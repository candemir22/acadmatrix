 ;;; ============================================================
;;; SAIT PANEL SUPER MASTER -- BULUT YUKLEYICI (PROD FIX)
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
        (princ (strcat "\n[SAT HATA]: LISP cekilemedi -> " url))
      )
    )
  )
  (princ)
)

;; 2. GUVENLI DXF CEKME VE DOGRUDAN EKLEME FONKSIYONU
(defun SaitDxfCek (url / h tmp f blok-adi res)
  (setq blok-adi (vl-filename-base url))
  (if (tblsearch "BLOCK" blok-adi)
    (progn
      (princ (strcat "\n[SAT Bulut]: '" blok-adi "' cizimde mevcut, ekleniyor..."))
      (command "._-INSERT" blok-adi pause 1 1 0)
    )
    (progn
      (princ (strcat "\n[SAT Bulut]: '" blok-adi "' indiriliyor... Lutfen bekleyin."))
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
              (setq tmp (strcat (getenv "TEMP") "\\" blok-adi ".dxf"))
              (vl-catch-all-apply 'vl-file-delete (list tmp))
              
              (setq f (open tmp "w"))
              (write-line res f)
              (close f)
              
              ;; HATA DUZELTILDI: command fonksiyonu dogrudan cagiriliyor
              (command "._-INSERT" tmp pause 1 1 0)
              
              (vl-catch-all-apply 'vl-file-delete (list tmp))
              (princ (strcat "\n[SAT Bulut]: '" blok-adi "' basariyla eklendi!"))
            )
            (princ (strcat "\n[SAT Bulut Hata]: DXF cekilemedi! URL kontrol et: " url))
          )
        )
      )
    )
  )
  (princ)
)

;; 3. DXF URL'SINI KOMUTA BAGLAYAN FONKSIYON
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

(princ "\n[SaitAI]: PROD Surum Bulut Sistemi Guncellendi!")
(princ)
