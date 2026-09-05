;;; ============================================================
;;; LINECIZ.lsp -- Test amacli basit cizim komutu (v1.0)
;;; KOMUT: LINECIZ
;;; NE YAPAR: Yan yana 10 dikey cizgi cizer (boy 200, aralik 100).
;;; ============================================================
(defun c:LINECIZ ( / x)
  (setq x 0.0)
  (repeat 10
    (entmake
      (list
        '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine")
        (cons 10 (list x 0.0 0.0))
        (cons 11 (list x 200.0 0.0))
      )
    )
    (setq x (+ x 100.0))
  )
  (command "_.ZOOM" "_E")
  (princ "\n[LINECIZ] 10 cizgi cizildi.")
  (princ)
)
(princ "\n[LINECIZ] yuklendi. Komut: LINECIZ")
(princ)
