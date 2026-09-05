(defun c:SaitAIKayit ( / dosya yol musteri malzeme miktar siparis imalat jsonVerisi secim yeniAdi hedefYol )
  (vl-load-com)
  
  ;; Kullanicidan inputlar aliniyor
  (setq musteri (getstring T "\nMusteri Adini Girin: "))
  (setq malzeme (getstring T "\nCephe Malzeme Secimini Girin: "))
  (setq miktar  (getstring T "\nMalzeme Miktarlarini Girin: "))
  (setq siparis (getstring T "\nSiparis Durumu / Detayi: "))
  (setq imalat  (getstring T "\nImalat Notlari: "))
  
  ;; Dosya adi secimi
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
  
  ;; JSON veri formatini olustur
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
  
  ;; Dosyaya yazma islemi
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
