 ;; HTML'deki <script src="..."> mantığıyla buluttan LISP çeken fonksiyon
(defun SaitScriptCek (url / h tmp f code)
  (vl-load-com)
  (setq h (vlax-create-object "MSXML2.XMLHTTP"))
  (vlax-invoke-method h 'open "GET" url :vlax-false)
  (vlax-invoke-method h 'send)
  (setq code (vlax-get-property h 'responseText))
  (if (> (strlen code) 0)
    (progn
      ;; Gelen saf metin kodunu geçici dosyaya yaz ve yükle
      (setq f (open (setq tmp (vl-filename-mktemp "sait" nil ".lsp")) "w"))
      (write-line code f)
      (close f)
      (load tmp)
      (vl-file-delete tmp) ; İş bitince iz bırakmadan sil (Hayalet mod)
    )
  )
)

;; HTML'deki <script src> etiketleri gibi alt lisp'leri merkeze bağlıyoruz:
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/satjsonyazveoku.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/DAIRECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/KARECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/LINECIZ.lsp")
(SaitScriptCek "https://raw.githubusercontent.com/candemir22/acadmatrix/refs/heads/main/UCGENCIZ.lsp")






