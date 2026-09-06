 ;;; ============================================================
;;; SAIT PANEL SUPER MASTER -- BULUT YUKLEYICI (PROD SURUMU)
;;; ============================================================
(vl-load-com)

;; 1. GUVENLI LISP CEKME FONKSIYONU (Hata Yakalamali & Bellek Korumali)
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
      ;; ONEMLI: COM Nesnesini RAM'den kaldir (Memory leak onleyici)
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
          (vl-catch-all-apply 'vl-file-delete (list tmp)) ; Temizlik
        )
        (princ (strcat "\n[SAT HATA]: LISP cekilemedi (Baglanti koptu veya URL hatali) -> " url))
      )
    )
  )
  (princ)
)

;; 2. GUVENLI DXF CEKME VE EKLEME FONKSIYONU
(defun SaitDxfCek (url / h tmp f blok-adi res)
  (setq blok-adi (vl-filename-base url))
  ;; Blok zaten cizimdeyse hic internete baglanma, direkt koy
  (if (tblsearch "BLOCK" blok-adi)
    (progn
      (princ (strcat "\n[SAT Bulut]: '" blok-adi "' cizimde mevcut, hizli ekleniyor..."))
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
          (vlax-release-object h) ;; RAM Temizligi

          (if (and (not (vl-catch-all-error-p res))
                   (= (type res) 'STR)
                   (> (strlen res) 0))
            (progn
              (setq tmp (strcat (getenv "TEMP") "\\" blok-adi ".dxf"))
              
              ;; Eger onceden kalma ayni isimli kilitli dosya varsa diye once silmeyi dene
              (vl-catch-all-apply 'vl-file-delete (list tmp))
              
              (setq f (open tmp "w"))
              (write-line res f)
              (close f)
              
              ;; Gorsel ekleme (AutoCAD hata verirse script durmasin diye catch icinde)
              (vl-catch-all-apply 'command (list "._-INSERT" tmp pause 1 1 0))
              
              ;; Hayalet Mod: Arkada hicbir cöp dosya birakma
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
;; >>> A. LISP LISTESI (Yeni LISP ekledikce buraya alt alta yaz)
;; ============================================================
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/satjsonyazveoku.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/DAIRECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/KARECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/LINECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/UCGENCIZ.lsp")

;; ============================================================
;; >>> B. DXF BLOK LISTESI (Yeni DXF ekledikce buraya alt alta yaz)
;; ============================================================
(SaitDxfBagla "GENELCEPHE1" "https://raw.githubusercontent.com/candemir22/acadmatrix/main/bloklar/genel_cephe1.dxf")

(princ "\n[SaitAI]: PROD Surum Bulut Sistemi (LISP + DXF Zirhli) Yuklendi!")


(princ)
