;;; ============================================================
;;; UCGENCIZ.lsp -- Test amacli basit cizim komutu (v1.0)
;;; KOMUT: UCGENCIZ
;;; NE YAPAR: Yan yana 10 ucgen cizer (taban 150, aralik 50).
;;; ============================================================
(defun c:UCGENCIZ ( / x s)
  (setq x 0.0 s 150.0)
  (repeat 10
    (entmake
      (list
        '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
        '(90 . 3) '(70 . 1)
        (cons 10 (list x 0.0))
        (cons 10 (list (+ x s) 0.0))
        (cons 10 (list (+ x (/ s 2.0)) s))
      )
    )
    (setq x (+ x s 50.0))
  )
  (princ "\n[UCGENCIZ] 10 ucgen cizildi.")
  (princ)
)
(princ "\n[UCGENCIZ] yuklendi. Komut: UCGENCIZ")
(princ)
