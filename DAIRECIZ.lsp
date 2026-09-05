;;; ============================================================
;;; DAIRECIZ.lsp -- Test amacli basit cizim komutu (v1.0)
;;; KOMUT: DAIRECIZ
;;; NE YAPAR: Yan yana 10 daire cizer (yaricap 100, aralik 300).
;;; ============================================================
(defun c:DAIRECIZ ( / x)
  (setq x 0.0)
  (repeat 10
    (entmake
      (list
        '(0 . "CIRCLE") '(100 . "AcDbEntity") '(100 . "AcDbCircle")
        (cons 10 (list x 0.0 0.0))
        '(40 . 100.0)
      )
    )
    (setq x (+ x 300.0))
  )
  (command "_.ZOOM" "_E")
  (princ "\n[DAIRECIZ] 10 daire cizildi.")
  (princ)
)
(princ "\n[DAIRECIZ] yuklendi. Komut: DAIRECIZ")
(princ)
