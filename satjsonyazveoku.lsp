(defun c:sat-jsonyaz ( / dosya musteri malzeme miktar siparis imalat jsonVerisi secim yeniAdi hedefYol )
  (vl-load-com)
  
  (setq musteri (getstring T "\nMusteri Adini Girin: "))
  (setq malzeme (getstring T "\nCephe Malzeme Secimini Girin: "))
  (setq miktar  (getstring T "\nMalzeme Miktarlarini Girin: "))
  (setq siparis (getstring T "\nSiparis Durumu / Detayi: "))
  (setq imalat  (getstring T "\nImalat Notlari: "))
  
  (initget "Varolan Yeni")
  (setq secim (getkword "\nJSON dosyasi nasil olusturulsun? [Varolan/Yeni]: "))
  
  (if (= secim "Yeni")
    (progn
      (setq yeniAdi (getstring T "\nYeni JSON dosya adini girin (uzantisz): "))
      (setq hedefYol (strcat (getvar "DWGPREFIX") yeniAdi ".json"))
    )
    (progn
      (setq hedefYol (getfiled "JSON Dosyasi Sec veya Olustur" (getvar "DWGPREFIX") "json" 1))
    )
  )
  
  (setq jsonVerisi 
    (strcat
      "{\n"
      "  \"MusteriAdi\": \"" musteri "\",\n"
      "  \"MalzemeSecimi\": \"" malzeme "\",\n"
      "  \"MalzemeMiktari\": \"" miktar "\",\n"
      "  \"Siparis\": \"" siparis "\",\n"
      "  \"Imalat\": \"" imalat "\",\n"
      "  \"ZamanDamgasi\": \"" (menucmd "m=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)") "\"\n"
      "}"
    )
  )
  
  (setq dosya (open hedefYol "w"))
  (if dosya
    (progn
      (write-line jsonVerisi dosya)
      (close dosya)
      (princ (strcat "\n[SaitAI]: Veriler basariyla JSON formatinda kaydedildi --> " hedefYol))
    )
    (princ "\n[HATA]: Dosya olusturulamadi veya yazilamadi!")
  )
  (princ)
)

(defun c:sat-jsonoku ( / hedefYol dosya satir tumVeri pt )
  (vl-load-com)
  
  (setq hedefYol (getfiled "Okunacak JSON Dosyasini Sec" (getvar "DWGPREFIX") "json" 0))
  
  (if (and hedefYol (findfile hedefYol))
    (progn
      (setq dosya (open hedefYol "r"))
      (setq tumVeri "")
      (while (setq satir (read-line dosya))
        (setq tumVeri (strcat tumVeri "\\P" satir))
      )
      (close dosya)
      
      (setq pt (getpoint "\nMetnin yazilacagi ekrandan bir nokta secin: "))
      
      (if pt
        (progn
          (entmake
            (list
              '(0 . "MTEXT")
              '(100 . "AcDbEntity")
              '(100 . "AcDbMText")
              (cons 10 pt)
              (cons 40 2.5)
              (cons 1 tumVeri)
            )
          )
          (princ "\n[SaitAI]: JSON verileri basariyla ekrana MTEXT olarak basildi!")
        )
      )
    )
    (princ "\n[HATA]: Dosya secilemedi veya bulunamadi!")
  )
  (princ)
)
