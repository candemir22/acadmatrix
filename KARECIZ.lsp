;;; ============================================================
;;; KARECIZ.lsp -- Test amacli basit cizim komutu (v1.0)
;;; KOMUT: KARECIZ
;;; NE YAPAR: Yan yana 10 kare cizer (kenar 150, aralik 50).
;;; ============================================================
(defun c:KARECIZ ( / x s)
  (setq x 0.0 s 150.0)
  (repeat 10
    (entmake
      (list
        '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
        '(90 . 4) '(70 . 1)
        (cons 10 (list x 0.0))
        (cons 10 (list (+ x s) 0.0))
        (cons 10 (list (+ x s) s))
        (cons 10 (list x s))
      )
    )
    (setq x (+ x s 50.0))
  )
  (command "_.ZOOM" "_E")
  (princ "\n[KARECIZ] 10 kare cizildi.")
  (princ)
)
(princ "\n[KARECIZ] yuklendi. Komut: KARECIZ")
(princ)
