;;;=========================================================================
;;; CURTAIN WALL v9.6s - STABLE BRANCH
;;; DCL pencere (blok secimi + >> buton) + CFG hafiza + gelismis cizim
;;; Dilatasyon destegi + kademeli olcu duzeni
;;; Komutlar: CW, CWSET
;;;=========================================================================
(vl-load-com)

;;; ==================== UYUMLULUK YARDIMCILARI ====================
(defun CW-STRINGP (x) (= (type x) 'STR))
(defun CW-LISTP   (x) (= (type x) 'LIST))
(defun CW-SAFE-STR (x /)
  (if (CW-STRINGP x) x ""))
(defun CW-BLOCKNAME-OR-NIL (x /)
  (if (and (CW-STRINGP x) (/= x "") (tblsearch "BLOCK" x)) x nil))

;;; ==================== PARAMETRELER ====================
(defun CW-SYNC-TEXT-GROUPS ()
  (if (not (numberp *TH-GPK*))    (setq *TH-GPK* 40.0))
  (if (not (numberp *TH-DAP*))    (setq *TH-DAP* 25.0))
  (if (not (numberp *TH-YAP*))    (setq *TH-YAP* 25.0))
  (if (not (numberp *TH-CPOZ*))   (setq *TH-CPOZ* 30.0))
  (if (not (numberp *TH-COLCU*))  (setq *TH-COLCU* 20.0))
  (if (not (numberp *TH-DIM*))    (setq *TH-DIM* 20.0))
  (if (not (numberp *TH-IMALAT*)) (setq *TH-IMALAT* 25.0))
  (if (not (numberp *TH-OPT*))    (setq *TH-OPT* 20.0))
  (if (not (numberp *TH-CTAB*))   (setq *TH-CTAB* 20.0))
  (setq *TH-BASLIK* *TH-GPK*)
  (setq *TH-ETIKET* *TH-IMALAT*)
  (setq *TH-OLCU*   *TH-DIM*)
  (setq *TH-POZ*    *TH-CPOZ*)
  (setq *TH-TABLO*  *TH-CTAB*)
  T)

(defun CW-RESET-DEFAULTS ()
  (setq *PG* 50.0 *PD* 100.0 *CB* 21.0)
  (setq *UZU* 100.0 *UZA* 245.0 *KRT* 19.0)
  (setq *AUTO-UZ* "1" *IM-SP* 450.0 *DA-PGAP* 420.0)
  (setq *YA-DIM-L1* 80.0 *YA-DIM-R1* 80.0 *YA-DIM-R2* 160.0 *YA-DIM-R3* 240.0)
  (setq *CPX* "C" *DPX* "DA" *YPX* "YA")
  (setq *TH-GPK* 40.0 *TH-DAP* 25.0 *TH-YAP* 25.0)
  (setq *TH-CPOZ* 30.0 *TH-COLCU* 20.0 *TH-DIM* 20.0)
  (setq *TH-IMALAT* 25.0 *TH-OPT* 20.0 *TH-CTAB* 20.0)
  (setq *TH-BASLIK* 40.0 *TH-ETIKET* 25.0 *TH-OLCU* 20.0)
  (setq *TH-POZ* 30.0 *TH-TABLO* 20.0)
  (setq *DO-PLAN* "1" *DO-KESIT* "1" *DO-CAM* "1")
  (setq *DO-DA* "1" *DO-YA* "1" *DO-CSV* "0")
  (setq *DO-OPT* "1" *OPT-SAME* "0" *OPT-KERF* 8.0 *OPT-MINBARS* "0")
  (setq *OPT-DA-STOCK* "6000" *OPT-YA-STOCK* "6000")
  (setq *GK-KASA-OFS* 60.0 *GK-KANAT-OFS* 85.0 *GK-ICAM-OFS* 18.0 *GK-DCAM-OFS* 18.0)
  (setq *CW-XL* nil *CW-YDIST* nil *CW-CL* nil *CW-MSEGS* nil)
  (setq *LAST-PB1* nil *LAST-PB2* nil *LAST-PB3* nil)
  (setq *LAST-KB1* nil *LAST-KB2* nil *LAST-KB3* nil)
  (setq *CW-BASE-XY* nil *CW-HS* nil)
  (CW-SYNC-TEXT-GROUPS)
)

(defun CW-NORMALIZE-YA-DIMS ()
  (if (not (numberp *YA-DIM-L1*)) (setq *YA-DIM-L1* 80.0))
  (if (not (numberp *YA-DIM-R1*)) (setq *YA-DIM-R1* 80.0))
  (if (not (numberp *YA-DIM-R2*)) (setq *YA-DIM-R2* 160.0))
  (if (not (numberp *YA-DIM-R3*)) (setq *YA-DIM-R3* 240.0))
  (if (< *YA-DIM-L1* 20.0) (setq *YA-DIM-L1* 20.0))
  (if (< *YA-DIM-R1* 20.0) (setq *YA-DIM-R1* 20.0))
  (if (<= *YA-DIM-R2* *YA-DIM-R1*) (setq *YA-DIM-R2* (+ *YA-DIM-R1* 80.0)))
  (if (<= *YA-DIM-R3* *YA-DIM-R2*) (setq *YA-DIM-R3* (+ *YA-DIM-R2* 80.0)))
  T)

(defun CW-VALIDATE-CFG ()
  (if (not (numberp *PG*)) (setq *PG* 50.0))
  (if (not (numberp *PD*)) (setq *PD* 100.0))
  (if (not (numberp *CB*)) (setq *CB* 21.0))
  (if (not (numberp *UZU*)) (setq *UZU* 100.0))
  (if (not (numberp *UZA*)) (setq *UZA* 245.0))
  (if (not (numberp *KRT*)) (setq *KRT* 19.0))
  (if (not (numberp *IM-SP*)) (setq *IM-SP* 450.0))
  (if (not (numberp *DA-PGAP*)) (setq *DA-PGAP* 420.0))
  (if (not (numberp *TH-GPK*)) (setq *TH-GPK* 40.0))
  (if (not (numberp *TH-DAP*)) (setq *TH-DAP* 25.0))
  (if (not (numberp *TH-YAP*)) (setq *TH-YAP* 25.0))
  (if (not (numberp *TH-CPOZ*)) (setq *TH-CPOZ* 30.0))
  (if (not (numberp *TH-COLCU*)) (setq *TH-COLCU* 20.0))
  (if (not (numberp *TH-DIM*)) (setq *TH-DIM* 20.0))
  (if (not (numberp *TH-IMALAT*)) (setq *TH-IMALAT* 25.0))
  (if (not (numberp *TH-OPT*)) (setq *TH-OPT* 20.0))
  (if (not (numberp *TH-CTAB*)) (setq *TH-CTAB* 20.0))
  (if (not (numberp *TH-BASLIK*)) (setq *TH-BASLIK* 40.0))
  (if (not (numberp *TH-ETIKET*)) (setq *TH-ETIKET* 25.0))
  (if (not (numberp *TH-OLCU*)) (setq *TH-OLCU* 20.0))
  (if (not (numberp *TH-POZ*)) (setq *TH-POZ* 30.0))
  (if (not (numberp *TH-TABLO*)) (setq *TH-TABLO* 20.0))
  (if (not (numberp *OPT-KERF*)) (setq *OPT-KERF* 8.0))
  (if (not (numberp *GK-KASA-OFS*)) (setq *GK-KASA-OFS* 60.0))
  (if (not (numberp *GK-KANAT-OFS*)) (setq *GK-KANAT-OFS* 85.0))
  (if (not (numberp *GK-ICAM-OFS*)) (setq *GK-ICAM-OFS* 18.0))
  (if (not (numberp *GK-DCAM-OFS*)) (setq *GK-DCAM-OFS* 18.0))

  (if (not (CW-STRINGP *AUTO-UZ*)) (setq *AUTO-UZ* "1"))
  (if (not (member *AUTO-UZ* '("0" "1"))) (setq *AUTO-UZ* "1"))
  (if (not (CW-STRINGP *CPX*)) (setq *CPX* "C"))
  (if (not (CW-STRINGP *DPX*)) (setq *DPX* "DA"))
  (if (not (CW-STRINGP *YPX*)) (setq *YPX* "YA"))
  (if (not (CW-STRINGP *OPT-DA-STOCK*)) (setq *OPT-DA-STOCK* "6000"))
  (if (not (CW-STRINGP *OPT-YA-STOCK*)) (setq *OPT-YA-STOCK* "6000"))

  (if (not (member *DO-PLAN* '("0" "1"))) (setq *DO-PLAN* "1"))
  (if (not (member *DO-KESIT* '("0" "1"))) (setq *DO-KESIT* "1"))
  (if (not (member *DO-CAM* '("0" "1"))) (setq *DO-CAM* "1"))
  (if (not (member *DO-DA* '("0" "1"))) (setq *DO-DA* "1"))
  (if (not (member *DO-YA* '("0" "1"))) (setq *DO-YA* "1"))
  (if (not (member *DO-CSV* '("0" "1"))) (setq *DO-CSV* "0"))
  (if (not (member *DO-OPT* '("0" "1"))) (setq *DO-OPT* "1"))
  (if (not (member *OPT-SAME* '("0" "1"))) (setq *OPT-SAME* "0"))
  (if (not (member *OPT-MINBARS* '("0" "1"))) (setq *OPT-MINBARS* "0"))

  (if (and *LAST-PB1* (not (CW-STRINGP *LAST-PB1*))) (setq *LAST-PB1* nil))
  (if (and *LAST-PB2* (not (CW-STRINGP *LAST-PB2*))) (setq *LAST-PB2* nil))
  (if (and *LAST-PB3* (not (CW-STRINGP *LAST-PB3*))) (setq *LAST-PB3* nil))
  (if (and *LAST-KB1* (not (CW-STRINGP *LAST-KB1*))) (setq *LAST-KB1* nil))
  (if (and *LAST-KB2* (not (CW-STRINGP *LAST-KB2*))) (setq *LAST-KB2* nil))
  (if (and *LAST-KB3* (not (CW-STRINGP *LAST-KB3*))) (setq *LAST-KB3* nil))
  (CW-NORMALIZE-YA-DIMS)
  (CW-SYNC-TEXT-GROUPS)
  T)

(CW-RESET-DEFAULTS)

;;; ==================== CFG OKUMA/YAZMA ====================
(defun CW-SAVEPATH (/ tmp)
  (cond
    ((setq tmp (getvar 'roamablerootprefix))
     (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" tmp)) "\\Support"))
    ((setq tmp (findfile "acad.pat"))
     (vl-string-right-trim "\\" (vl-string-translate "/" "\\" (vl-filename-directory tmp))))
    ((vl-string-right-trim "\\" (vl-string-translate "/" "\\" (vl-filename-directory (vl-filename-mktemp)))))))

(defun CW-WRITECFG (/ fp fn)
  (setq fn (strcat (CW-SAVEPATH) "\\CW_v10.cfg"))
  (if (setq fp (open fn "w"))
    (progn
      (foreach v '(*PG* *PD* *CB* *KRT* *UZA* *UZU* *AUTO-UZ* *IM-SP* *DA-PGAP* *DPX* *YPX* *CPX*
        *TH-BASLIK* *TH-ETIKET* *TH-OLCU* *TH-POZ* *TH-TABLO*
        *TH-GPK* *TH-DAP* *TH-YAP* *TH-CPOZ* *TH-COLCU* *TH-DIM* *TH-IMALAT* *TH-OPT* *TH-CTAB*
        *DO-PLAN* *DO-KESIT* *DO-CAM* *DO-DA* *DO-YA* *DO-CSV*
        *LAST-PB1* *LAST-PB2* *LAST-PB3*
        *LAST-KB1* *LAST-KB2* *LAST-KB3*
        *YA-DIM-L1* *YA-DIM-R1* *YA-DIM-R2* *YA-DIM-R3* *DO-OPT* *OPT-SAME* *OPT-KERF* *OPT-MINBARS* *OPT-DA-STOCK* *OPT-YA-STOCK* *GK-KASA-OFS* *GK-KANAT-OFS* *GK-ICAM-OFS* *GK-DCAM-OFS*)
        (write-line (vl-prin1-to-string (eval v)) fp))
      (close fp))))

(defun CW-READCFG (/ fp fn line val)
  (setq fn (strcat (CW-SAVEPATH) "\\CW_v10.cfg"))
  (if (and (findfile fn)(setq fp (open fn "r")))
    (progn
      (foreach v '(*PG* *PD* *CB* *KRT* *UZA* *UZU* *AUTO-UZ* *IM-SP* *DA-PGAP* *DPX* *YPX* *CPX*
        *TH-BASLIK* *TH-ETIKET* *TH-OLCU* *TH-POZ* *TH-TABLO*
        *TH-GPK* *TH-DAP* *TH-YAP* *TH-CPOZ* *TH-COLCU* *TH-DIM* *TH-IMALAT* *TH-OPT* *TH-CTAB*
        *DO-PLAN* *DO-KESIT* *DO-CAM* *DO-DA* *DO-YA* *DO-CSV*
        *LAST-PB1* *LAST-PB2* *LAST-PB3*
        *LAST-KB1* *LAST-KB2* *LAST-KB3*
        *YA-DIM-L1* *YA-DIM-R1* *YA-DIM-R2* *YA-DIM-R3* *DO-OPT* *OPT-SAME* *OPT-KERF* *OPT-MINBARS* *OPT-DA-STOCK* *OPT-YA-STOCK* *GK-KASA-OFS* *GK-KANAT-OFS* *GK-ICAM-OFS* *GK-DCAM-OFS*)
        (if (setq line (read-line fp))
          (progn
            (setq val (vl-catch-all-apply 'read (list line)))
            (if (not (vl-catch-all-error-p val))
              (set v val)))))
      (close fp)))
  (CW-VALIDATE-CFG))

;;; ==================== DCL YAZMA ====================
(defun CW-WRITEDCL (/ fn fp)
  (setq fn (strcat (CW-SAVEPATH) "\\CW_v10.dcl"))
  (if (setq fp (open fn "w"))
    (progn
      (foreach line '(
        "cwmain : dialog {"
        "  label = \"Giydirme Cephe v10.5b - Ayarlar\";"
        "  : column {"
        "    : boxed_row {"
        "      label = \"Genel Geometri\";"
        "      : column {"
        "        : row { : edit_box { key = \"pg\"; label = \"Profil Gen.\"; edit_width = 8; } : edit_box { key = \"cb\"; label = \"Cam Boslugu\"; edit_width = 8; } : edit_box { key = \"krt\"; label = \"Kertme\"; edit_width = 8; } }"
        "        : row { : edit_box { key = \"pd\"; label = \"Kesit Araligi\"; edit_width = 8; } : edit_box { key = \"im_sp\"; label = \"Imalat Bosluk\"; edit_width = 8; } : edit_box { key = \"da_gap\"; label = \"DA Parca Bosluk\"; edit_width = 8; } }"
        "      }"
        "      : column {"
        "        : row { : toggle { key = \"auto_uz\"; label = \"Uzanimlar otomatik\"; } : edit_box { key = \"uza\"; label = \"Alt Uzanim\"; edit_width = 8; } : edit_box { key = \"uzu\"; label = \"Ust Uzanim\"; edit_width = 8; } }"
        "        : row { : edit_box { key = \"dpx\"; label = \"Dusey Kod\"; edit_width = 8; } : edit_box { key = \"ypx\"; label = \"Yatay Kod\"; edit_width = 8; } : edit_box { key = \"cpx\"; label = \"Cam Kod\"; edit_width = 8; } }"
        "      }"
        "    }"
        "    : spacer { height = 0.2; }"
        "    : row {"
        "      : boxed_column {"
        "        label = \"Olcu Ofsetleri\";"
        "        : text { label = \"Yatay imalatta olcu cizgilerinin konumunu belirler.\"; }"
        "        : row { : edit_box { key = \"ya_l\"; label = \"YA Toplam Olcu Ofs.\"; edit_width = 8; } : edit_box { key = \"ya_r1\"; label = \"YA Ic Olcu Ofs.\"; edit_width = 8; } }"
        "        : row { : edit_box { key = \"ya_r2\"; label = \"YA Ust Kertme Ofs.\"; edit_width = 8; } : edit_box { key = \"ya_r3\"; label = \"YA Alt Kertme Ofs.\"; edit_width = 8; } }"
        "        : spacer { height = 0.2; }"
        "        : row { : toggle { key = \"do_plan\"; label = \"Plan\"; } : toggle { key = \"do_kesit\"; label = \"Kesit\"; } : toggle { key = \"do_cam\"; label = \"Cam\"; } }"
        "        : row { : toggle { key = \"do_da\"; label = \"DA Imalat\"; } : toggle { key = \"do_ya\"; label = \"YA Imalat\"; } : toggle { key = \"do_csv\"; label = \"CSV\"; } }"
        "      }"
        "      : boxed_column {"
        "        label = \"Optimizasyon\";"
        "        : row { : toggle { key = \"do_opt\"; label = \"Acik\"; } : toggle { key = \"opt_same\"; label = \"DA+YA ortak stok\"; } }"
        "        : row { : edit_box { key = \"opt_kerf\"; label = \"Kesim Payi\"; edit_width = 8; } : toggle { key = \"opt_minbars\"; label = \"Az boy / uzun stok tercihi\"; } }"
        "        : row { : edit_box { key = \"opt_da\"; label = \"DA Stoklar\"; edit_width = 18; } }"
        "        : row { : edit_box { key = \"opt_ya\"; label = \"YA Stoklar\"; edit_width = 18; } }"
        "        : text { label = \"Ornek giris: 6000,6500,7000\"; }"
        "      }"
        "      : boxed_column {"
        "        label = \"Gizli Kanat\";"
        "        : row { : edit_box { key = \"gk_kasa\"; label = \"Kasa Aks Ofs.\"; edit_width = 8; } : edit_box { key = \"gk_kanat\"; label = \"Kanat Aks Ofs.\"; edit_width = 8; } }"
        "        : row { : edit_box { key = \"gk_icam\"; label = \"Ic Cam Ofs.\"; edit_width = 8; } : edit_box { key = \"gk_dcam\"; label = \"Dis Cam Ofs.\"; edit_width = 8; } }"
        "      }"
        "    }"
        "    : spacer { height = 0.2; }"
        "    : row {"
        "      : boxed_column {"
        "        label = \"Plan Bloklari\";"
        "        : row { : edit_box { key = \"pb1\"; label = \"Bas\"; edit_width = 16; } : button { key = \"pb1_sel\"; label = \">>\"; width = 4; fixed_width = true; } }"
        "        : row { : edit_box { key = \"pb2\"; label = \"Orta\"; edit_width = 16; } : button { key = \"pb2_sel\"; label = \">>\"; width = 4; fixed_width = true; } }"
        "        : row { : edit_box { key = \"pb3\"; label = \"Son\"; edit_width = 16; } : button { key = \"pb3_sel\"; label = \">>\"; width = 4; fixed_width = true; } }"
        "      }"
        "      : boxed_column {"
        "        label = \"Kesit Bloklari\";"
        "        : row { : edit_box { key = \"kb1\"; label = \"Alt\"; edit_width = 16; } : button { key = \"kb1_sel\"; label = \">>\"; width = 4; fixed_width = true; } }"
        "        : row { : edit_box { key = \"kb2\"; label = \"Orta\"; edit_width = 16; } : button { key = \"kb2_sel\"; label = \">>\"; width = 4; fixed_width = true; } }"
        "        : row { : edit_box { key = \"kb3\"; label = \"Ust\"; edit_width = 16; } : button { key = \"kb3_sel\"; label = \">>\"; width = 4; fixed_width = true; } }"
        "      }"
        "      : boxed_column {"
        "        label = \"Yazi Gruplari\";"
        "        : row { : edit_box { key = \"th_gpk\"; label = \"Cam/Plan/Kesit\"; edit_width = 6; } : edit_box { key = \"th_dap\"; label = \"DA Poz\"; edit_width = 6; } : edit_box { key = \"th_yap\"; label = \"YA Poz\"; edit_width = 6; } }"
        "        : row { : edit_box { key = \"th_cpoz\"; label = \"Cam Poz\"; edit_width = 6; } : edit_box { key = \"th_colcu\"; label = \"Cam Olcu\"; edit_width = 6; } : edit_box { key = \"th_dim\"; label = \"Genel Olcu\"; edit_width = 6; } }"
        "        : row { : edit_box { key = \"th_imalat\"; label = \"DA-YA Imalat\"; edit_width = 6; } : edit_box { key = \"th_opt\"; label = \"Optimizasyon\"; edit_width = 6; } : edit_box { key = \"th_ctab\"; label = \"Cam Poz Tablosu\"; edit_width = 6; } }"
        "      }"
        "    }"
        "  }"
        "  spacer;"
        "  ok_cancel;"
        "}"
      ) (write-line line fp))
      (close fp)
      (while (not (findfile fn)))
      fn)))

;;; ==================== BLOK SEC (cizimden) ====================
;;; ==================== BLOK SEC (cizimden) ====================
(defun CW-INSERT-ENAME-P (e / ed)
  (and e (= (type e) 'ENAME)
       (setq ed (entget e))
       (= (cdr (assoc 0 ed)) "INSERT")))

(defun CW-FIND-OWNER-INSERT (e / ed own)
  (while (and e (not (CW-INSERT-ENAME-P e)))
    (setq ed (entget e)
          own (cdr (assoc 330 ed))
          e own))
  (if (CW-INSERT-ENAME-P e) e nil))

(defun CW-EFFECTIVE-BLOCKNAME (ent / obj raw eff bn)
  (if (CW-INSERT-ENAME-P ent)
    (progn
      (setq raw (cdr (assoc 2 (entget ent)))
            eff nil)
      (setq obj (vlax-ename->vla-object ent))
      (if (and obj (vlax-property-available-p obj 'EffectiveName))
        (setq eff (vlax-get-property obj 'EffectiveName)))
      (setq bn
        (cond
          ((and (CW-STRINGP eff) (/= eff "")) eff)
          ((and (CW-STRINGP raw) (/= raw "")) raw)
          (T nil)))
      bn)
    nil))

(defun CW-PICK-BLOCK (/ sel ent path bn)
  (princ "
Blok secin (referans / attribut / blok ici oge) <ENTER=iptal>: ")
  (setq sel (nentselp))
  (if sel
    (progn
      (setq ent nil bn nil)
      ;; Dogrudan block reference secildi ise
      (if (CW-INSERT-ENAME-P (car sel))
        (setq ent (car sel)))
      ;; Nested secimde container path icindeki INSERT'i yakala
      (setq path (nth 3 sel))
      (if (and (null ent) (CW-LISTP path))
        (foreach e path
          (if (and (null ent) (CW-INSERT-ENAME-P e))
            (setq ent e))))
      ;; Attribute vb. owner zincirinden INSERT bul
      (if (null ent)
        (setq ent (CW-FIND-OWNER-INSERT (car sel))))
      (setq bn (CW-EFFECTIVE-BLOCKNAME ent))
      (if bn
        (progn
          (princ (strcat "\nSecilen blok: " bn))
          bn)
        (progn
          (princ "\nBlok adi okunamadi. Lutfen blok referansini tekrar secin.")
          nil)))
    nil))

;;; ==================== DCL GOSTER (loop ile >> destegi) ====================
(defun CW-DCL-SHOW (/ dcl-id ok fn pick-mode result)
  (setq fn (CW-WRITEDCL))
  (if (not fn)(progn (princ "\nDCL olusturulamadi!")(exit)))
  (setq pick-mode nil result nil)
  (while (not result)
    (setq dcl-id (load_dialog fn))
    (if (not (new_dialog "cwmain" dcl-id))(progn (princ "\nDCL yuklenemedi!")(exit)))
    (CW-VALIDATE-CFG)
    (set_tile "pg" (rtos *PG* 2 0))
    (set_tile "pd" (rtos *PD* 2 0))
    (set_tile "cb" (rtos *CB* 2 0))
    (set_tile "krt" (rtos *KRT* 2 0))
    (set_tile "im_sp" (rtos *IM-SP* 2 0))
    (set_tile "da_gap" (rtos *DA-PGAP* 2 0))
    (set_tile "ya_l" (rtos *YA-DIM-L1* 2 0))
    (set_tile "ya_r1" (rtos *YA-DIM-R1* 2 0))
    (set_tile "ya_r2" (rtos *YA-DIM-R2* 2 0))
    (set_tile "ya_r3" (rtos *YA-DIM-R3* 2 0))
    (set_tile "auto_uz" *AUTO-UZ*)
    (set_tile "uza" (rtos *UZA* 2 0))
    (set_tile "uzu" (rtos *UZU* 2 0))
    (set_tile "dpx" *DPX*)(set_tile "ypx" *YPX*)(set_tile "cpx" *CPX*)
    (set_tile "pb1" (if *LAST-PB1* *LAST-PB1* ""))
    (set_tile "pb2" (if *LAST-PB2* *LAST-PB2* ""))
    (set_tile "pb3" (if *LAST-PB3* *LAST-PB3* ""))
    (set_tile "kb1" (if *LAST-KB1* *LAST-KB1* ""))
    (set_tile "kb2" (if *LAST-KB2* *LAST-KB2* ""))
    (set_tile "kb3" (if *LAST-KB3* *LAST-KB3* ""))
    (set_tile "th_gpk" (rtos *TH-GPK* 2 0))
    (set_tile "th_dap" (rtos *TH-DAP* 2 0))
    (set_tile "th_yap" (rtos *TH-YAP* 2 0))
    (set_tile "th_cpoz" (rtos *TH-CPOZ* 2 0))
    (set_tile "th_colcu" (rtos *TH-COLCU* 2 0))
    (set_tile "th_dim" (rtos *TH-DIM* 2 0))
    (set_tile "th_imalat" (rtos *TH-IMALAT* 2 0))
    (set_tile "th_opt" (rtos *TH-OPT* 2 0))
    (set_tile "th_ctab" (rtos *TH-CTAB* 2 0))
    (set_tile "do_plan" *DO-PLAN*)(set_tile "do_kesit" *DO-KESIT*)
    (set_tile "do_cam" *DO-CAM*)(set_tile "do_da" *DO-DA*)
    (set_tile "do_ya" *DO-YA*)(set_tile "do_csv" *DO-CSV*)
    (set_tile "do_opt" *DO-OPT*)(set_tile "opt_same" *OPT-SAME*)
    (set_tile "opt_kerf" (rtos *OPT-KERF* 2 0))
    (set_tile "opt_minbars" *OPT-MINBARS*)
    (set_tile "opt_da" *OPT-DA-STOCK*)
    (set_tile "opt_ya" *OPT-YA-STOCK*)
    (set_tile "gk_kasa" (rtos *GK-KASA-OFS* 2 0))
    (set_tile "gk_kanat" (rtos *GK-KANAT-OFS* 2 0))
    (set_tile "gk_icam" (rtos *GK-ICAM-OFS* 2 0))
    (set_tile "gk_dcam" (rtos *GK-DCAM-OFS* 2 0))

    (action_tile "pg" "(setq *PG* (atof $value))")
    (action_tile "pd" "(setq *PD* (atof $value))")
    (action_tile "cb" "(setq *CB* (atof $value))")
    (action_tile "krt" "(setq *KRT* (atof $value))")
    (action_tile "im_sp" "(setq *IM-SP* (atof $value))")
    (action_tile "da_gap" "(setq *DA-PGAP* (atof $value))")
    (action_tile "ya_l" "(setq *YA-DIM-L1* (atof $value))")
    (action_tile "ya_r1" "(setq *YA-DIM-R1* (atof $value))")
    (action_tile "ya_r2" "(setq *YA-DIM-R2* (atof $value))")
    (action_tile "ya_r3" "(setq *YA-DIM-R3* (atof $value))")
    (action_tile "auto_uz" "(setq *AUTO-UZ* $value)")
    (action_tile "uza" "(setq *UZA* (atof $value))")
    (action_tile "uzu" "(setq *UZU* (atof $value))")
    (action_tile "dpx" "(setq *DPX* $value)")
    (action_tile "ypx" "(setq *YPX* $value)")
    (action_tile "cpx" "(setq *CPX* $value)")
    (action_tile "pb1" "(setq *LAST-PB1* (if (= $value \"\" ) nil $value))")
    (action_tile "pb2" "(setq *LAST-PB2* (if (= $value \"\" ) nil $value))")
    (action_tile "pb3" "(setq *LAST-PB3* (if (= $value \"\" ) nil $value))")
    (action_tile "kb1" "(setq *LAST-KB1* (if (= $value \"\" ) nil $value))")
    (action_tile "kb2" "(setq *LAST-KB2* (if (= $value \"\" ) nil $value))")
    (action_tile "kb3" "(setq *LAST-KB3* (if (= $value \"\" ) nil $value))")
    (action_tile "pb1_sel" "(setq *LAST-PB1* (get_tile \"pb1\"))(setq *LAST-PB2* (get_tile \"pb2\"))(setq *LAST-PB3* (get_tile \"pb3\"))(setq *LAST-KB1* (get_tile \"kb1\"))(setq *LAST-KB2* (get_tile \"kb2\"))(setq *LAST-KB3* (get_tile \"kb3\"))(done_dialog 10)")
    (action_tile "pb2_sel" "(setq *LAST-PB1* (get_tile \"pb1\"))(setq *LAST-PB2* (get_tile \"pb2\"))(setq *LAST-PB3* (get_tile \"pb3\"))(setq *LAST-KB1* (get_tile \"kb1\"))(setq *LAST-KB2* (get_tile \"kb2\"))(setq *LAST-KB3* (get_tile \"kb3\"))(done_dialog 11)")
    (action_tile "pb3_sel" "(setq *LAST-PB1* (get_tile \"pb1\"))(setq *LAST-PB2* (get_tile \"pb2\"))(setq *LAST-PB3* (get_tile \"pb3\"))(setq *LAST-KB1* (get_tile \"kb1\"))(setq *LAST-KB2* (get_tile \"kb2\"))(setq *LAST-KB3* (get_tile \"kb3\"))(done_dialog 12)")
    (action_tile "kb1_sel" "(setq *LAST-PB1* (get_tile \"pb1\"))(setq *LAST-PB2* (get_tile \"pb2\"))(setq *LAST-PB3* (get_tile \"pb3\"))(setq *LAST-KB1* (get_tile \"kb1\"))(setq *LAST-KB2* (get_tile \"kb2\"))(setq *LAST-KB3* (get_tile \"kb3\"))(done_dialog 13)")
    (action_tile "kb2_sel" "(setq *LAST-PB1* (get_tile \"pb1\"))(setq *LAST-PB2* (get_tile \"pb2\"))(setq *LAST-PB3* (get_tile \"pb3\"))(setq *LAST-KB1* (get_tile \"kb1\"))(setq *LAST-KB2* (get_tile \"kb2\"))(setq *LAST-KB3* (get_tile \"kb3\"))(done_dialog 14)")
    (action_tile "kb3_sel" "(setq *LAST-PB1* (get_tile \"pb1\"))(setq *LAST-PB2* (get_tile \"pb2\"))(setq *LAST-PB3* (get_tile \"pb3\"))(setq *LAST-KB1* (get_tile \"kb1\"))(setq *LAST-KB2* (get_tile \"kb2\"))(setq *LAST-KB3* (get_tile \"kb3\"))(done_dialog 15)")
    (action_tile "th_gpk" "(setq *TH-GPK* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_dap" "(setq *TH-DAP* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_yap" "(setq *TH-YAP* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_cpoz" "(setq *TH-CPOZ* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_colcu" "(setq *TH-COLCU* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_dim" "(setq *TH-DIM* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_imalat" "(setq *TH-IMALAT* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_opt" "(setq *TH-OPT* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "th_ctab" "(setq *TH-CTAB* (atof $value))(CW-SYNC-TEXT-GROUPS)")
    (action_tile "do_plan" "(setq *DO-PLAN* $value)")
    (action_tile "do_kesit" "(setq *DO-KESIT* $value)")
    (action_tile "do_cam" "(setq *DO-CAM* $value)")
    (action_tile "do_da" "(setq *DO-DA* $value)")
    (action_tile "do_ya" "(setq *DO-YA* $value)")
    (action_tile "do_csv" "(setq *DO-CSV* $value)")
    (action_tile "do_opt" "(setq *DO-OPT* $value)")
    (action_tile "opt_same" "(setq *OPT-SAME* $value)")
    (action_tile "opt_kerf" "(setq *OPT-KERF* (atof $value))")
    (action_tile "opt_minbars" "(setq *OPT-MINBARS* $value)")
    (action_tile "opt_da" "(setq *OPT-DA-STOCK* $value)")
    (action_tile "opt_ya" "(setq *OPT-YA-STOCK* $value)")
    (action_tile "gk_kasa" "(setq *GK-KASA-OFS* (atof $value))")
    (action_tile "gk_kanat" "(setq *GK-KANAT-OFS* (atof $value))")
    (action_tile "gk_icam" "(setq *GK-ICAM-OFS* (atof $value))")
    (action_tile "gk_dcam" "(setq *GK-DCAM-OFS* (atof $value))")

    (setq ok (start_dialog))
    (unload_dialog dcl-id)
    (cond
      ((= ok 1)
       (CW-VALIDATE-CFG)
       (CW-WRITECFG)
       (setq result T))
      ((= ok 0)
       (setq result 'cancel))
      ((= ok 10)(progn (setq pick-mode (CW-PICK-BLOCK)) (if pick-mode (setq *LAST-PB1* pick-mode))))
      ((= ok 11)(progn (setq pick-mode (CW-PICK-BLOCK)) (if pick-mode (setq *LAST-PB2* pick-mode))))
      ((= ok 12)(progn (setq pick-mode (CW-PICK-BLOCK)) (if pick-mode (setq *LAST-PB3* pick-mode))))
      ((= ok 13)(progn (setq pick-mode (CW-PICK-BLOCK)) (if pick-mode (setq *LAST-KB1* pick-mode))))
      ((= ok 14)(progn (setq pick-mode (CW-PICK-BLOCK)) (if pick-mode (setq *LAST-KB2* pick-mode))))
      ((= ok 15)(progn (setq pick-mode (CW-PICK-BLOCK)) (if pick-mode (setq *LAST-KB3* pick-mode))))
      (T (setq result 'cancel))))
  (if (eq result T) T nil))

;;; ==================== LAYERLAR ====================
;;; ==================== LAYERLAR ====================
(defun CW-LAY ()
  (foreach L '(("CW-PROFIL" 7 "Continuous" 30)("CW-CAM" 132 "Continuous" -3)
    ("CW-EKSEN" 1 "CENTER" -3)("CW-OLCU" 2 "Continuous" 15)
    ("CW-YAZI" 7 "Continuous" 15)("CW-IMALAT" 5 "Continuous" 30)
    ("CW-BAG" 8 "Continuous" 20)("CW-TABLO" 7 "Continuous" 15))
    (if (not (tblsearch "LAYER" (car L)))
      (entmake (list '(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")(cons 2 (car L))(cons 62 (cadr L))
        (cons 6 (caddr L))(cons 370 (cadddr L))'(70 . 0))))))

;;; ==================== CIZIM ARACLARI ====================
(defun CW-RND (x)(atoi (rtos x 2 0)))
(defun CW-R (px py w h lay)
  (if (and (numberp px)(numberp py)(numberp w)(numberp h)(> (abs w) 0.01)(> (abs h) 0.01))
    (entmake (list '(0 . "LWPOLYLINE")'(100 . "AcDbEntity")(cons 8 lay)
      '(100 . "AcDbPolyline")'(90 . 4)'(70 . 1)
      (cons 10 (list px py 0.0))(cons 10 (list (+ px w) py 0.0))
      (cons 10 (list (+ px w)(+ py h) 0.0))(cons 10 (list px (+ py h) 0.0))))))
(defun CW-L (x1 y1 x2 y2 lay)
  (if (and (numberp x1)(numberp y1)(numberp x2)(numberp y2))
    (entmake (list '(0 . "LINE")'(100 . "AcDbEntity")(cons 8 lay)
      '(100 . "AcDbLine")(cons 10 (list x1 y1 0.0))(cons 11 (list x2 y2 0.0))))))
(defun CW-TM (px py txt h lay)
  (if (and (numberp px)(numberp py) txt (numberp h)(> h 0))
    (entmake (list '(0 . "TEXT")'(100 . "AcDbEntity")(cons 8 lay)'(100 . "AcDbText")
      (cons 10 (list px py 0.0))(cons 40 h)(cons 1 txt)'(50 . 0.0)'(72 . 1)'(73 . 2)
      (cons 11 (list px py 0.0))'(100 . "AcDbText")'(73 . 2)))))
;; Dusey yonlu yazi (90 derece)
(defun CW-TL (px py txt h lay)
  (if (and (numberp px)(numberp py) txt (numberp h)(> h 0))
    (entmake (list '(0 . "TEXT")'(100 . "AcDbEntity")(cons 8 lay)'(100 . "AcDbText")
      (cons 10 (list px py 0.0))(cons 40 h)(cons 1 txt)'(50 . 0.0)'(72 . 0)'(73 . 2)
      (cons 11 (list px py 0.0))'(100 . "AcDbText")'(73 . 2)))))
(defun CW-TV (px py txt h lay)
  (if (and (numberp px)(numberp py) txt (numberp h)(> h 0))
    (entmake (list '(0 . "TEXT")'(100 . "AcDbEntity")(cons 8 lay)'(100 . "AcDbText")
      (cons 10 (list px py 0.0))(cons 40 h)(cons 1 txt)(cons 50 (/ pi 2.0))'(72 . 1)'(73 . 2)
      (cons 11 (list px py 0.0))'(100 . "AcDbText")'(73 . 2)))))
(defun CW-INS (bn px py sx sy rot lay)
  (setq bn (CW-BLOCKNAME-OR-NIL bn))
  (if (and bn (numberp px)(numberp py))
    (entmake (list '(0 . "INSERT")'(100 . "AcDbEntity")(cons 8 lay)
      '(100 . "AcDbBlockReference")(cons 2 bn)(cons 10 (list px py 0.0))
      (cons 41 sx)(cons 42 sy)(cons 43 1.0)(cons 50 (* rot (/ pi 180.0)))))))
(defun CW-DIMVARS-PUSH (/ vars out)
  (setq vars '("DIMTXT" "DIMASZ" "DIMGAP" "DIMEXO" "DIMEXE" "DIMTIX" "DIMTIH" "DIMTOH" "DIMTMOVE" "DIMTAD" "DIMJUST"))
  (setq out '())
  (foreach v vars (setq out (cons (cons v (getvar v)) out)))
  (setvar "DIMTXT" (max 2.5 *TH-OLCU*))
  (setvar "DIMASZ" (max 2.0 (* *TH-OLCU* 0.75)))
  (setvar "DIMGAP" (max 0.8 (* *TH-OLCU* 0.20)))
  (setvar "DIMEXO" 0.0)
  (setvar "DIMEXE" (max 1.0 (* *TH-OLCU* 0.40)))
  (setvar "DIMTIX" 1)
  (setvar "DIMTIH" 0)
  (setvar "DIMTOH" 0)
  (setvar "DIMTMOVE" 0)
  (setvar "DIMTAD" 1)
  (setvar "DIMJUST" 0)
  out)
(defun CW-DIMVARS-POP (lst)
  (foreach p lst (if p (setvar (car p) (cdr p))))
  T)
(defun CW-DH (x1 x2 y dy)
  (if (and (numberp x1)(numberp x2)(numberp y)(numberp dy)(> (abs (- x2 x1)) 0.1))
    (progn (setvar "CLAYER" "CW-OLCU")
      (command "_.DIMLINEAR" (list x1 y 0)(list x2 y 0)(list (/ (+ x1 x2) 2.0) dy 0)))))
(defun CW-DV (y1 y2 x dx)
  (if (and (numberp y1)(numberp y2)(numberp x)(numberp dx)(> (abs (- y2 y1)) 0.1))
    (progn (setvar "CLAYER" "CW-OLCU")
      (command "_.DIMLINEAR" (list x y1 0)(list x y2 0)(list dx (/ (+ y1 y2) 2.0) 0)))))
(defun CW-SECBLK (msg / ent)
  (setq ent (car (entsel (strcat "\n" msg " (ENTER=atla): "))))
  (if ent (CW-EFFECTIVE-BLOCKNAME (or (CW-FIND-OWNER-INSERT ent) ent)) nil))

;;; ==================== BBOX + OKUMA ====================
(defun CW-ENT-BBOX (ent / typ pts xs ys e10 e11)
  (setq typ (cdr (assoc 0 (entget ent))))
  (cond
    ((= typ "LWPOLYLINE")
     (setq pts (mapcar 'cdr (vl-remove-if-not (function (lambda (p)(= (car p) 10))) (entget ent))))
     (setq xs (mapcar 'car pts) ys (mapcar 'cadr pts))
     (list (apply 'min xs)(apply 'min ys)(apply 'max xs)(apply 'max ys)))
    ((= typ "LINE")
     (setq e10 (cdr (assoc 10 (entget ent))) e11 (cdr (assoc 11 (entget ent))))
     (list (min (car e10)(car e11))(min (cadr e10)(cadr e11))
           (max (car e10)(car e11))(max (cadr e10)(cadr e11))))
    (T nil)))

;;; ==================== DUSEY PROFIL OKUMA ====================
;;; Segmentleri prof-bot'a gore normalize eder
(defun CW-READ-MULLIONS (/ ss i ent bbox cx bboxes xlist prof-bot prof-top
  mullion-segs seg-list)
  (princ "\n>>> DUSEY profilleri secin: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss)(progn (princ "\nSecim yok!")(exit)))
  (setq bboxes '() i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i) bbox (CW-ENT-BBOX ent))
    (if bbox (if (< (- (nth 2 bbox)(nth 0 bbox))(- (nth 3 bbox)(nth 1 bbox)))
      (setq bboxes (append bboxes (list bbox)))))
    (setq i (1+ i)))
  ;; X merkezlerini cikar
  (setq xlist '())
  (foreach bb bboxes
    (setq cx (/ (+ (nth 0 bb)(nth 2 bb)) 2.0))
    (if (not (member (CW-RND cx) (mapcar 'CW-RND xlist)))
      (setq xlist (append xlist (list cx)))))
  (setq xlist (vl-sort xlist '<))
  ;; Genel alt/ust sinir
  (setq prof-bot (apply 'min (mapcar (function (lambda (bb)(nth 1 bb))) bboxes)))
  (setq prof-top (apply 'max (mapcar (function (lambda (bb)(nth 3 bb))) bboxes)))
  ;; Her X icin profil segmentlerini topla ve NORMALIZE et (prof-bot'a gore)
  (setq mullion-segs '())
  (foreach cx xlist
    (setq seg-list '())
    (foreach bb bboxes
      (if (= (CW-RND cx)(CW-RND (/ (+ (nth 0 bb)(nth 2 bb)) 2.0)))
        (setq seg-list (append seg-list
          (list (list (- (nth 1 bb) prof-bot)(- (nth 3 bb) prof-bot)))))))
    ;; Y'ye gore sirala
    (setq seg-list (vl-sort seg-list (function (lambda (a b)(< (car a)(car b))))))
    (setq mullion-segs (append mullion-segs (list (cons cx seg-list)))))
  ;; Ust uste profil raporu
  (foreach ms mullion-segs
    (if (> (length (cdr ms)) 1)
      (princ (strcat "\n  X=" (rtos (car ms) 2 0) " : " (itoa (length (cdr ms))) " ayri profil (dilatasyon)"))))
  (princ (strcat "\n  " (itoa (length xlist)) " dusey aks"))
  (list xlist prof-bot prof-top mullion-segs))

;;; ==================== YATAY PROFIL OKUMA ====================
(defun CW-READ-TRANSOMS (xlist / ss i ent bbox cy bboxes pg2 result xi x1 x2
  ys-for-span all-ys y-base result-norm ydist-raw ys found ydist-final xi-val
  merged ydist-clean x-base xlist-norm)
  (princ "\n>>> YATAY profilleri secin: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss)(progn (princ "\nSecim yok!")(exit)))
  (setq bboxes '() pg2 (/ *PG* 2.0) i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i) bbox (CW-ENT-BBOX ent))
    (if bbox (if (> (- (nth 2 bbox)(nth 0 bbox))(- (nth 3 bbox)(nth 1 bbox)))
      (setq bboxes (append bboxes (list bbox)))))
    (setq i (1+ i)))
  (princ (strcat "\n  " (itoa (length bboxes)) " yatay profil"))
  (setq result '() xi 0)
  (repeat (1- (length xlist))
    (setq x1 (nth xi xlist) x2 (nth (1+ xi) xlist) ys-for-span '())
    (foreach bb bboxes
      (if (and (> (nth 2 bb)(+ x1 pg2 -5))(< (nth 0 bb)(- x2 pg2 -5)))
        (progn (setq cy (/ (+ (nth 1 bb)(nth 3 bb)) 2.0))
          (if (not (member (CW-RND cy)(mapcar 'CW-RND ys-for-span)))
            (setq ys-for-span (append ys-for-span (list cy)))))))
    (setq ys-for-span (vl-sort ys-for-span '<))
    (setq result (append result (list (cons xi ys-for-span))))
    (setq xi (1+ xi)))
  (setq all-ys '())
  (foreach sp result (foreach y (cdr sp)(if (not (member y all-ys))(setq all-ys (append all-ys (list y))))))
  (setq all-ys (vl-sort all-ys '<))
  (if (not all-ys)(progn (princ "\nY aks yok!")(exit)))
  (setq y-base (car all-ys) result-norm '())
  (foreach sp result
    (setq result-norm (append result-norm
      (list (cons (car sp)(mapcar (function (lambda (y)(- y y-base)))(cdr sp)))))))
  ;; yDist birlestirme
  (setq ydist-final '())
  (foreach sp result-norm
    (setq ys (cdr sp) xi-val (car sp) merged nil)
    (setq ydist-final
      (mapcar (function (lambda (d)
        (if (and (equal (nth 0 d) ys)(not merged))
          (if (and (<= (nth 1 d) xi-val)(<= xi-val (nth 2 d)))
            (progn (setq merged T)(list ys (min (nth 1 d) xi-val)(max (nth 2 d)(1+ xi-val)))) d)
          d))) ydist-final))
    (if (not merged)(setq ydist-final (append ydist-final (list (list ys xi-val (1+ xi-val)))))))
  (setq ydist-clean '())
  (foreach d ydist-final (setq found nil)
    (foreach dc ydist-clean
      (if (and (equal (nth 0 d)(nth 0 dc))(= (nth 1 d)(nth 1 dc))(= (nth 2 d)(nth 2 dc)))(setq found T)))
    (if (not found)(setq ydist-clean (append ydist-clean (list d)))))
  (princ (strcat "\n  " (itoa (length ydist-clean)) " dagilim"))
  (setq x-base (car xlist) xlist-norm (mapcar (function (lambda (x)(- x x-base))) xlist))
  (list xlist-norm ydist-clean (list x-base y-base)))

;;; ==================== SPAN/FINGERPRINT ====================
(defun CW-SPAN-YLIST (xi yDist / res)
  (setq res '())
  (foreach dist yDist
    (if (and (CW-LISTP dist)(>= xi (nth 1 dist))(< xi (nth 2 dist)))
      (foreach y (nth 0 dist)(if (not (member y res))(setq res (append res (list y)))))))
  (vl-sort res '<))
(defun CW-ALL-Y (yDist / res)
  (setq res '())(foreach dist yDist (foreach y (nth 0 dist)
    (if (not (member y res))(setq res (append res (list y))))))(vl-sort res '<))

;;; Segment bilgisi iceren DA fingerprint
;;; Segmentleri de parmak izine ekler (dilatasyon farki algisi)
(defun CW-DAFP (idx xL yDist / sol sag yl yr ys seg-info segs s)
  (setq sol (if (> idx 0)(CW-RND (- (nth idx xL)(nth (1- idx) xL))) 0))
  (setq sag (if (< idx (1- (length xL)))(CW-RND (- (nth (1+ idx) xL)(nth idx xL))) 0))
  (setq yl (if (> idx 0)(CW-SPAN-YLIST (1- idx) yDist) '()))
  (setq yr (if (< idx (1- (length xL)))(CW-SPAN-YLIST idx yDist) '()))
  (setq ys (strcat "L" (itoa (length yl)) "R" (itoa (length yr))))
  (foreach y yl (setq ys (strcat ys "l" (itoa (CW-RND y)))))
  (foreach y yr (setq ys (strcat ys "r" (itoa (CW-RND y)))))
  ;; Segment bilgisini ekle
  (setq seg-info "" segs (CW-GET-SEGS-FOR-IDX idx))
  (if (and segs (> (length segs) 1))
    (progn (setq seg-info (strcat "S" (itoa (length segs))))
      (foreach s segs
        (setq seg-info (strcat seg-info "h" (itoa (CW-RND (- (cadr s)(car s)))))))))
  (strcat (itoa sol) "|" (itoa sag) "|" ys "|" seg-info))

;;; Aks indeksine gore segment verisi getir (normalize edilmis)
(defun CW-GET-SEGS-FOR-IDX (idx / ms x-abs x-base result)
  (setq result nil)
  (if *CW-MSEGS*
    (progn
      (setq x-base (if *CW-BASE-XY* (car *CW-BASE-XY*) 0))
      (setq x-abs (+ x-base (nth idx *CW-XL*)))
      (foreach ms *CW-MSEGS*
        (if (and (not result)(= (CW-RND (car ms))(CW-RND x-abs)))
          (setq result (cdr ms))))))
  result)

(defun CW-DAID (idx xL yDist / fps ufps fp i result)
  (setq fps '() i 0)
  (foreach xi xL (setq fps (append fps (list (CW-DAFP i xL yDist))) i (1+ i)))
  (setq ufps '())(foreach fp fps (if (not (member fp ufps))(setq ufps (append ufps (list fp)))))
  (setq fp (CW-DAFP idx xL yDist) result 1 i 1)
  (foreach uf ufps (if (= uf fp)(setq result i))(setq i (1+ i))) result)



;; ARIAL yazi stilini olustur
(if (not (tblsearch "STYLE" "ARIAL"))
  (entmake '((0 . "STYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbTextStyleTableRecord") (2 . "ARIAL") (70 . 0) (40 . 0.0) (3 . "arial.ttf") (4 . "")))
)

;; ==================== KODLAMA (gorunus uzerine) ====================
(defun CW-KODLA (bx by xL yDist / pg pg2 i xi xi2 allY all-y ya-idx ya-done ya-boy px py txt)
  (setq pg *PG* pg2 (/ pg 2.0) allY (CW-ALL-Y yDist))
  ;; DA kodlama - parca bazli
  (CW-DA-INSTANCE-LABELS bx by xL yDist)
  ;; YA kodlama
  (setq ya-idx 0 ya-done '() i 0)
  (repeat (1- (length xL))
    (setq xi (nth i xL) xi2 (nth (1+ i) xL))
    (setq all-y (CW-SPAN-YLIST i yDist))
    (foreach yj all-y
      (setq ya-boy (CW-RND (- xi2 xi pg)))
      (if (not (member ya-boy ya-done))
        (progn (setq ya-idx (1+ ya-idx))
               (setq ya-done (append ya-done (list ya-boy)))))
      
      ;; ARIAL, H=100, Kırmızı (Renk 1) metin ekleme
      (setq px (+ bx (/ (+ xi xi2) 2.0)))
      (setq py (+ by yj 50.0))
      (setq txt (strcat *YPX* "-" (itoa (1+ (vl-position ya-boy ya-done)))))
      
      (entmake (list '(0 . "TEXT")
                     '(100 . "AcDbEntity")
                     '(8 . "CW-YAZI")
                     '(62 . 1)
                     '(100 . "AcDbText")
                     (cons 10 (list px py 0.0))
                     '(40 . 100.0)
                     (cons 1 txt)
                     '(7 . "ARIAL")
                     '(50 . 0.0)
                     '(72 . 1)
                     '(73 . 2)
                     (cons 11 (list px py 0.0))
                     '(100 . "AcDbText")
                     '(73 . 2)))
    )
    (setq i (1+ i)))
  (foreach hs *CW-HS* (CW-DRAW-HS-MARK bx by xL hs))
  (princ " OK"))
  
  
  

;;; ==================== PLAN ==================== 
;;; ==================== PLAN ====================
(defun CW-PLAN (bx by xL blk1 blk2 blk3 / i xi n blk sx)
  (setq n (length xL))
  (CW-TM (+ bx (/ (+ (car xL)(CW-LASTVAL xL)) 2.0))(- by *PD* (* *TH-BASLIK* 2))
    "PLAN" *TH-BASLIK* "CW-YAZI")
  (setq i 0)
  (foreach xi xL
    (setq blk (cond ((= i 0) blk1)((= i (1- n)) blk3)(T blk2)))
    (setq sx (if (= i (1- n)) -1.0 1.0))
    (if blk (CW-INS blk (+ bx xi) by sx 1.0 0 "CW-PROFIL"))
    (CW-L (+ bx xi)(- by 80)(+ bx xi)(+ by *PD* 80) "CW-EKSEN")
    (setq i (1+ i)))
  (setq i 0)
  (repeat (1- n)
    (CW-DH (+ bx (nth i xL))(+ bx (nth (1+ i) xL))(+ by *PD* 30)(+ by *PD* 150))
    (setq i (1+ i)))
  (princ " OK"))

;;; ==================== KESIT ====================
(defun CW-KESIT (bx by yDist blk1 blk2 blk3 / pg2 di allY boy j yj n sp cx
  unique-ys drawn-ys ys-key)
  (setq pg2 (/ *PG* 2.0) sp (+ *PD* 650) di 0)
  (setq unique-ys '() drawn-ys '())
  (foreach dist yDist
    (setq ys-key (apply 'strcat (mapcar (function (lambda (y)(strcat (rtos y 2 0) ","))) (nth 0 dist))))
    (if (not (member ys-key drawn-ys))
      (progn (setq drawn-ys (append drawn-ys (list ys-key)))
             (setq unique-ys (append unique-ys (list dist))))))
  (foreach dist unique-ys
    (setq allY (nth 0 dist) n (length allY))
    (if (> n 1) (progn
      (setq cx (+ bx (* di sp)))
      (setq boy (+ (- (CW-LASTVAL allY)(car allY)) *PG* *UZU* *UZA*))
      (CW-TM cx (- (+ by (car allY)) pg2 *UZA* (* *TH-BASLIK* 3.5))
        (strcat "KESIT " (chr (+ 65 di))) *TH-BASLIK* "CW-YAZI")
      (CW-R (+ cx 10)(- (+ by (car allY)) pg2 *UZA*)(- *PD* 20) boy "CW-PROFIL")
      (CW-L (- cx 40)(- (+ by (car allY)) pg2 *UZA*)(+ cx *PD* 40)(- (+ by (car allY)) pg2 *UZA*) "CW-PROFIL")
      (CW-L (- cx 40)(+ (+ by (CW-LASTVAL allY)) pg2 *UZU*)(+ cx *PD* 40)(+ (+ by (CW-LASTVAL allY)) pg2 *UZU*) "CW-PROFIL")
      (setq j 0)
      (foreach yj allY
        (cond ((= j 0)(if blk1 (CW-INS blk1 cx (+ by yj) 1.0 1.0 0 "CW-PROFIL")))
              ((= j (1- n))(if blk3 (CW-INS blk3 cx (+ by yj) 1.0 1.0 0 "CW-PROFIL")))
              (T (if blk2 (CW-INS blk2 cx (+ by yj) 1.0 1.0 0 "CW-PROFIL"))))
        (CW-L (- cx 80)(+ by yj)(+ cx *PD* 80)(+ by yj) "CW-EKSEN")
        (setq j (1+ j)))
      ;; Aks arasi olculer
      (setq j 0)
      (repeat (1- n)
        (CW-DV (+ by (nth j allY))(+ by (nth (1+ j) allY))(- cx 30)(- cx 140))
        (setq j (1+ j)))
      (if (> n 2)(CW-DV (+ by (car allY))(+ by (CW-LASTVAL allY))(- cx 30)(- cx 260)))
      ;; Uzanim olculeri
      (CW-DV (- (+ by (car allY)) pg2 *UZA*)(+ by (car allY))(+ cx *PD* 30)(+ cx *PD* 140))
      (CW-DV (+ by (CW-LASTVAL allY))(+ (+ by (CW-LASTVAL allY)) pg2 *UZU*)(+ cx *PD* 30)(+ cx *PD* 140))
      (CW-DV (- (+ by (car allY)) pg2 *UZA*)(+ (+ by (CW-LASTVAL allY)) pg2 *UZU*)(+ cx *PD* 30)(+ cx *PD* 260))
      (setq di (1+ di)))))
  (princ " OK"))

;;; ==================== CAM CIZIMI ====================
(defun CW-CAM (bx by xL yDist prefix / pg cb i j xi xi2 yj yj2
  cw ch all-y allY pn ct pk pl cl hs-item)
  (setq pg *PG* cb (/ *CB* 2.0) pn 0 ct '() cl '() allY (CW-ALL-Y yDist))
  (CW-TM (+ bx (/ (+ (car xL)(CW-LASTVAL xL)) 2.0)) (- (+ by (car allY)) (* *TH-BASLIK* 4))
    "CAM CIZIMI" *TH-BASLIK* "CW-YAZI")
  ;; CAM CIZIMINDE PROFIL GOSTERME: sadece camlar ve yazilar cizilir
  (setq i 0)
  (repeat (1- (length xL))
    (setq xi (nth i xL) xi2 (nth (1+ i) xL) all-y (CW-SPAN-YLIST i yDist) j 0)
    (repeat (1- (length all-y))
      (setq yj (nth j all-y) yj2 (nth (1+ j) all-y) cw (- xi2 xi *CB*) ch (- yj2 yj *CB*))
      (if (and (> cw 0) (> ch 0))
        (progn
          (setq hs-item (CW-FIND-HS i j))
          (if hs-item
            (CW-DRAW-HS-IN-CAM bx by xi xi2 yj yj2 hs-item)
            (progn
              (setq pk (strcat (rtos cw 2 0) "x" (rtos ch 2 0)))
              (if (not (assoc pk ct))
                (progn
                  (setq pn (1+ pn) pl (strcat prefix "-" (if (< pn 10) "0" "") (itoa pn)))
                  (setq ct (append ct (list (cons pk pl)))))
                (setq pl (cdr (assoc pk ct))))
              (CW-R (+ bx xi cb) (+ by yj cb) cw ch "CW-CAM")
              (CW-TM (+ bx xi cb (/ cw 2.0)) (+ by yj cb (/ ch 2.0) (* *TH-POZ* 1.5)) pl *TH-POZ* "CW-YAZI")
              (CW-TM (+ bx xi cb (/ cw 2.0)) (+ by yj cb (/ ch 2.0) (* *TH-POZ* -1.5)) pk *TH-OLCU* "CW-YAZI")
              (setq cl (append cl (list (list pl cw ch pk))))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (princ " OK") cl)

(defun CW-LASTVAL (lst)
  (if lst (car (reverse lst)) nil))


(defun CW-PL (pts lay closed / data)
  (if (and (CW-LISTP pts) (> (length pts) 1))
    (progn
      (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                       '(100 . "AcDbPolyline") (cons 90 (length pts))
                       (cons 70 (if closed 1 0))))
      (foreach p pts
        (setq data (append data (list (cons 10 (list (car p) (cadr p) 0.0))))))
      (entmake data))))

(defun CW-INTERSECT-LEN (a1 a2 b1 b2 / lo hi)
  (setq lo (max (min a1 a2) (min b1 b2))
        hi (min (max a1 a2) (max b1 b2)))
  (max 0.0 (- hi lo)))

(defun CW-BEST-SPAN-INDEX (a1 a2 vals / i besti bestlen cur)
  (setq i 0 besti nil bestlen -1.0)
  (while (< (1+ i) (length vals))
    (setq cur (CW-INTERSECT-LEN a1 a2 (nth i vals) (nth (1+ i) vals)))
    (if (> cur bestlen) (setq bestlen cur besti i))
    (setq i (1+ i)))
  (if (and besti (> bestlen 0.0)) besti nil))

(defun CW-FIND-HS (idx rowi / hit it)
  (setq hit nil)
  (foreach it *CW-HS*
    (if (and (= idx (nth 0 it)) (= rowi (nth 1 it)))
      (setq hit it)))
  hit)


(defun CW-HS-SIGNATURE (xi yi raw-xlist yDist / x1 x2 y1 y2 kasaW kasaH kanatW kanatH icW icH dcW dcH)
  (setq x1 (nth xi raw-xlist)
        x2 (nth (1+ xi) raw-xlist)
        y1 (nth yi yDist)
        y2 (nth (1+ yi) yDist)
        kasaW (max 0 (CW-RND (- x2 x1 (* 2.0 *GK-KASA-OFS*))))
        kasaH (max 0 (CW-RND (- y2 y1 (* 2.0 *GK-KASA-OFS*))))
        kanatW (max 0 (CW-RND (- x2 x1 (* 2.0 *GK-KANAT-OFS*))))
        kanatH (max 0 (CW-RND (- y2 y1 (* 2.0 *GK-KANAT-OFS*))))
        icW (max 0 (CW-RND (- kanatW (* 2.0 *GK-ICAM-OFS*))))
        icH (max 0 (CW-RND (- kanatH (* 2.0 *GK-ICAM-OFS*))))
        dcW (max 0 (CW-RND (- kanatW (* 2.0 *GK-DCAM-OFS*))))
        dcH (max 0 (CW-RND (- kanatH (* 2.0 *GK-DCAM-OFS*)))) )
  (strcat
    (itoa kasaW) "x" (itoa kasaH) "|"
    (itoa kanatW) "x" (itoa kanatH) "|"
    (itoa icW) "x" (itoa icH) "|"
    (itoa dcW) "x" (itoa dcH)))

(defun CW-READ-HIDDEN-SASHES (raw-xlist yDist base-xy / ss i ent bb xi ysabs yi out code ynorm y1 y2 sig sigmap hit)
  (princ "\n>>> GIZLI KANAT V polylinelari secin (ENTER=atla): ")
  ;; Format: (aksIdx rowIdx y1 y2 kod bbox sig)
  (setq ss (ssget '((0 . "LWPOLYLINE"))) out '() code 1 sigmap '())
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) bb (CW-ENT-BBOX ent))
        (if bb
          (progn
            (setq xi (CW-BEST-SPAN-INDEX (nth 0 bb) (nth 2 bb) raw-xlist))
            (if xi
              (progn
                (setq ynorm (CW-SPAN-YLIST xi yDist)
                      ysabs (mapcar '(lambda (y) (+ (cadr base-xy) y)) ynorm)
                      yi (CW-BEST-SPAN-INDEX (nth 1 bb) (nth 3 bb) ysabs))
                (if (and yi (< (1+ yi) (length ynorm)))
                  (progn
                    (setq y1 (nth yi ynorm)
                          y2 (nth (1+ yi) ynorm)
                          sig (CW-HS-SIGNATURE xi yi raw-xlist ynorm)
                          hit (assoc sig sigmap))
                    (if (not hit)
                      (progn
                        (setq hit (cons sig (strcat "GK-" (if (< code 10) "0" "") (itoa code))))
                        (setq sigmap (append sigmap (list hit)))
                        (setq code (1+ code))))
                    (setq out (append out (list (list xi yi y1 y2 (cdr hit) bb sig))))))))))
        (setq i (1+ i)))))
  (princ (strcat "\n  " (itoa (length out)) " gizli kanat paneli"))
  out)

(defun CW-DRAW-HS-MARK (bx by xL item / idx y1 y2 xi xi2 x1 x2 yb yt xm)
  (setq idx (nth 0 item) y1 (nth 2 item) y2 (nth 3 item)
        xi (nth idx xL) xi2 (nth (1+ idx) xL)
        x1 (+ bx xi *GK-KASA-OFS*)
        x2 (+ bx xi2 (- *GK-KASA-OFS*))
        yb (+ by y1 *GK-KASA-OFS*)
        yt (+ by y2 (- *GK-KASA-OFS*))
        xm (/ (+ x1 x2) 2.0))
  (if (and (> x2 x1) (> yt yb))
    (progn
      (CW-PL (list (list x1 yt) (list xm yb) (list x2 yt)) "CW-PROFIL" nil)
      (CW-TM xm (+ yb (* *TH-CPOZ* 1.0)) (nth 4 item) *TH-CPOZ* "CW-YAZI"))))

(defun CW-DRAW-HS-IN-CAM (bx by xi xi2 yj yj2 item / xk1 xk2 yk1 yk2 xw1 xw2 yw1 yw2
                         xig1 xig2 yig1 yig2 xdg1 xdg2 ydg1 ydg2 xm ym
                         kasaW kasaH kanatW kanatH icW icH dcW dcH code lineStep)
  (setq code (nth 4 item)
        xk1 (+ bx xi *GK-KASA-OFS*) xk2 (+ bx xi2 (- *GK-KASA-OFS*))
        yk1 (+ by yj *GK-KASA-OFS*) yk2 (+ by yj2 (- *GK-KASA-OFS*)))
  (if (and (> xk2 xk1) (> yk2 yk1))
    (progn
      ;; CAM CIZIMINDE PROFIL/KASA/KANAT CIZILMEZ; sadece camlar ve bilgiler yazilir
      (setq xw1 (+ bx xi *GK-KANAT-OFS*) xw2 (+ bx xi2 (- *GK-KANAT-OFS*))
            yw1 (+ by yj *GK-KANAT-OFS*) yw2 (+ by yj2 (- *GK-KANAT-OFS*)))
      (if (and (> xw2 xw1) (> yw2 yw1))
        (progn
          (setq xm (/ (+ xw1 xw2) 2.0) ym (/ (+ yw1 yw2) 2.0))
          (setq xig1 (+ xw1 *GK-ICAM-OFS*) xig2 (- xw2 *GK-ICAM-OFS*)
                yig1 (+ yw1 *GK-ICAM-OFS*) yig2 (- yw2 *GK-ICAM-OFS*)
                xdg1 (+ xw1 *GK-DCAM-OFS*) xdg2 (- xw2 *GK-DCAM-OFS*)
                ydg1 (+ yw1 *GK-DCAM-OFS*) ydg2 (- yw2 *GK-DCAM-OFS*))
          (if (and (> xig2 xig1) (> yig2 yig1)) (CW-R xig1 yig1 (- xig2 xig1) (- yig2 yig1) "CW-CAM"))
          (if (and (> xdg2 xdg1) (> ydg2 ydg1)
                   (or (/= (CW-RND xdg1) (CW-RND xig1)) (/= (CW-RND ydg1) (CW-RND yig1))
                       (/= (CW-RND xdg2) (CW-RND xig2)) (/= (CW-RND ydg2) (CW-RND yig2))))
            (CW-R xdg1 ydg1 (- xdg2 xdg1) (- ydg2 ydg1) "CW-CAM"))
          (setq kasaW (CW-RND (- xk2 xk1)) kasaH (CW-RND (- yk2 yk1))
                kanatW (CW-RND (- xw2 xw1)) kanatH (CW-RND (- yw2 yw1))
                icW (max 0 (CW-RND (- xig2 xig1))) icH (max 0 (CW-RND (- yig2 yig1)))
                dcW (max 0 (CW-RND (- xdg2 xdg1))) dcH (max 0 (CW-RND (- ydg2 ydg1)))
                lineStep (* *TH-COLCU* 1.15))
          (CW-TM xm (+ ym (* *TH-CPOZ* 2.35)) code *TH-CPOZ* "CW-YAZI")
          (CW-TM xm (+ ym (* lineStep 1.35)) (strcat "KASA " (itoa kasaW) "x" (itoa kasaH)) *TH-COLCU* "CW-YAZI")
          (CW-TM xm (+ ym (* lineStep 0.15)) (strcat "KANAT " (itoa kanatW) "x" (itoa kanatH)) *TH-COLCU* "CW-YAZI")
          (CW-TM xm (- ym (* lineStep 1.05)) (strcat "IC CAM " (itoa icW) "x" (itoa icH)) *TH-COLCU* "CW-YAZI")
          (CW-TM xm (- ym (* lineStep 2.25)) (strcat "DIS CAM " (itoa dcW) "x" (itoa dcH)) *TH-COLCU* "CW-YAZI"))))))

(defun CW-TRANSOM-CENTER-ABS (y allY / baseY)
  (setq baseY (if allY (car allY) 0.0))
  (+ *UZA* (- y baseY) (/ *PG* 2.0)))

(defun CW-SEG-CENTERS (ylist seg allY / res cy)
  (setq res '())
  (foreach y ylist
    (setq cy (CW-TRANSOM-CENTER-ABS y allY))
    (if (and (>= cy (- (car seg) 0.5))(<= cy (+ (cadr seg) 0.5)))
      (setq res (append res (list (- cy (car seg)))))))
  (vl-sort res '<))

(defun CW-GROUP-CAM (cl / grp pk)
  (setq grp '())
  (foreach c cl
    (setq pk (car c))
    (if (assoc pk grp)
      (setq grp (subst (cons pk (1+ (cdr (assoc pk grp))))(assoc pk grp) grp))
      (setq grp (append grp (list (cons pk 1))))))
  grp)

(defun CW-LISTE-LEGACY (bx by cl / grp pk cnt rh c1 c2 c3 px py tot olcu th)
  (setq grp (CW-GROUP-CAM cl) th *TH-TABLO* rh (* th 3.0)
        c1 (max 160.0 (* th 8.0))
        c2 (max 260.0 (* th 14.0))
        c3 (max 90.0 (* th 5.5)))
  (setq px bx py by)
  (CW-TM (+ px (/ (+ c1 c2 c3) 2.0))(+ py rh (* th 1.5)) "CAM LISTESI" *TH-BASLIK* "CW-TABLO")
  (CW-R px py c1 rh "CW-TABLO")(CW-TM (+ px (/ c1 2.0))(+ py (/ rh 2.0)) "POZ" th "CW-TABLO")
  (CW-R (+ px c1) py c2 rh "CW-TABLO")(CW-TM (+ px c1 (/ c2 2.0))(+ py (/ rh 2.0)) "OLCU" th "CW-TABLO")
  (CW-R (+ px c1 c2) py c3 rh "CW-TABLO")(CW-TM (+ px c1 c2 (/ c3 2.0))(+ py (/ rh 2.0)) "ADET" th "CW-TABLO")
  (setq py (- py rh))
  (foreach g grp (setq pk (car g) cnt (cdr g) olcu "")
    (foreach c cl (if (and (= (car c) pk)(= olcu ""))(setq olcu (nth 3 c))))
    (CW-R px py c1 rh "CW-TABLO")(CW-TM (+ px (/ c1 2.0))(+ py (/ rh 2.0)) pk th "CW-TABLO")
    (CW-R (+ px c1) py c2 rh "CW-TABLO")(CW-TM (+ px c1 (/ c2 2.0))(+ py (/ rh 2.0)) olcu th "CW-TABLO")
    (CW-R (+ px c1 c2) py c3 rh "CW-TABLO")(CW-TM (+ px c1 c2 (/ c3 2.0))(+ py (/ rh 2.0))(itoa cnt) th "CW-TABLO")
    (setq py (- py rh)))
  (setq tot 0)(foreach g grp (setq tot (+ tot (cdr g))))
  (CW-R px py (+ c1 c2) rh "CW-TABLO")(CW-TM (+ px (/ (+ c1 c2) 2.0))(+ py (/ rh 2.0)) "TOPLAM" th "CW-TABLO")
  (CW-R (+ px c1 c2) py c3 rh "CW-TABLO")(CW-TM (+ px c1 c2 (/ c3 2.0))(+ py (/ rh 2.0))(itoa tot) th "CW-TABLO"))

(defun CW-LISTE (bx by cl / grp rows cols rowh space doc ms tbl r pk cnt olcu tot)
  (setq grp (CW-GROUP-CAM cl))
  (if (null grp)
    nil
    (progn
      (setq rows (+ (length grp) 3)
            cols 3
            rowh (* *TH-TABLO* 3.0)
            space (* *TH-TABLO* 8.0))
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq ms (vla-get-ModelSpace doc))
      (setq tbl
        (vl-catch-all-apply
          'vla-AddTable
          (list ms (vlax-3d-point (list bx by 0.0)) rows cols rowh (max 140.0 (* *TH-TABLO* 8.0)))))
      (if (vl-catch-all-error-p tbl)
        (CW-LISTE-LEGACY bx by cl)
        (progn
          (vla-put-Layer tbl "CW-TABLO")
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 (max 160.0 (* *TH-TABLO* 8.0))))
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 (max 260.0 (* *TH-TABLO* 14.0))))
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 2 (max 90.0 (* *TH-TABLO* 5.5))))
          (vl-catch-all-apply 'vla-MergeCells (list tbl 0 0 0 2))
          (vla-SetText tbl 0 0 "CAM LISTESI")
          (vla-SetText tbl 1 0 "POZ")
          (vla-SetText tbl 1 1 "OLCU")
          (vla-SetText tbl 1 2 "ADET")
          (setq r 2)
          (foreach g grp
            (setq pk (car g) cnt (cdr g) olcu "")
            (foreach c cl (if (and (= (car c) pk)(= olcu ""))(setq olcu (nth 3 c))))
            (vla-SetText tbl r 0 pk)
            (vla-SetText tbl r 1 olcu)
            (vla-SetText tbl r 2 (itoa cnt))
            (setq r (1+ r)))
          (setq tot 0)(foreach g grp (setq tot (+ tot (cdr g))))
          (vla-SetText tbl r 0 "TOPLAM")
          (vla-SetText tbl r 2 (itoa tot))
          (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 0 0 5))
          (setq r 0)
          (repeat rows
            (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl r 0 *TH-TABLO*))
            (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl r 1 *TH-TABLO*))
            (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl r 2 *TH-TABLO*))
            (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r 0 5))
            (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r 1 5))
            (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r 2 5))
            (setq r (1+ r)))
          tbl)))))

;;; ==================== CAM LISTESI ====================
;;; ==================== CSV ====================
(defun CW-CSV (cl / grp pk cnt fn fp item)
  (setq grp '())(foreach c cl (setq pk (car c))
    (if (assoc pk grp)(setq grp (subst (cons pk (1+ (cdr (assoc pk grp))))(assoc pk grp) grp))
      (setq grp (append grp (list (cons pk 1))))))
  (setq fn (getfiled "Cam Listesi CSV" "" "csv" 1))
  (if fn (progn (setq fp (open fn "w"))
    (write-line "POZ;GENISLIK;YUKSEKLIK;OLCU;ADET" fp)
    (foreach g grp (setq pk (car g) cnt (cdr g))
      (setq item (car (vl-remove-if-not (function (lambda (c)(= (car c) pk))) cl)))
      (if item (write-line (strcat pk ";" (rtos (cadr item) 2 0) ";" (rtos (caddr item) 2 0)
        ";" (nth 3 item) ";" (itoa cnt)) fp)))
    (close fp)(princ (strcat "\n  CSV: " fn)))))

;;; ==================== DA IMALAT v9 ====================
;;; Dilatasyon: ayni X'te birden fazla profil AYRI cizilir
;;; Kademeli olcu duzeni: ust uste binme yok
;;; Uzanim olculeri transom olcu zincirinin devami olarak
(defun CW-DA-PIECE-FP (piece-h lpts rpts / s)
  (setq s (strcat "H" (itoa (CW-RND piece-h)) "|L" (itoa (length lpts))))
  (foreach v lpts (setq s (strcat s "_" (itoa (CW-RND v)))))
  (setq s (strcat s "|R" (itoa (length rpts))))
  (foreach v rpts (setq s (strcat s "_" (itoa (CW-RND v)))))
  s)

(defun CW-DA-COLLECT-ITEMS (xL yDist / pg allY items idx x-base ms-data ms segs y-sol y-sag
  seg piece-h lpts rpts fp rec item-start)
  (setq pg *PG* allY (CW-ALL-Y yDist))
  (setq items '()
        idx 0
        x-base (if *CW-BASE-XY* (car *CW-BASE-XY*) 0.0)
        ms-data (if *CW-MSEGS* *CW-MSEGS* nil))
  (repeat (length xL)
    (setq segs nil)
    (if ms-data
      (foreach ms ms-data
        (if (= (CW-RND (car ms))(CW-RND (+ x-base (nth idx xL))))
          (setq segs (cdr ms)))))
    (if (or (not segs)(= (length segs) 0))
      (setq segs (list (list 0 (+ (- (CW-LASTVAL allY)(car allY)) pg *UZU* *UZA*)))))

    (setq y-sol (if (> idx 0) (CW-SPAN-YLIST (1- idx) yDist) '()))
    (setq y-sag (if (< idx (1- (length xL))) (CW-SPAN-YLIST idx yDist) '()))

    (foreach seg segs
      (setq piece-h (- (cadr seg)(car seg)))
      (setq lpts (CW-SEG-CENTERS y-sol seg allY))
      (setq rpts (CW-SEG-CENTERS y-sag seg allY))
      (setq fp (CW-DA-PIECE-FP piece-h lpts rpts))
      (setq item-start (car seg))
      (setq rec (assoc fp items))
      (if rec
        (setq items
          (subst
            (list fp piece-h lpts rpts (1+ (nth 4 rec)) (min (nth 5 rec) idx) (min (nth 6 rec) item-start))
            rec items))
        (setq items
          (append items (list (list fp piece-h lpts rpts 1 idx item-start))))))
    (setq idx (1+ idx)))

  ;; Once uzun / alt seri, sonra kisa / ust seri
  (vl-sort items
    (function
      (lambda (a b)
        (cond
          ((/= (CW-RND (nth 1 a))(CW-RND (nth 1 b))) (> (nth 1 a)(nth 1 b)))
          ((/= (CW-RND (nth 6 a))(CW-RND (nth 6 b))) (< (nth 6 a)(nth 6 b)))
          (T (< (nth 5 a)(nth 5 b))))))))


(defun CW-DA-CODEMAP (xL yDist / items code fp out)
  (setq items (CW-DA-COLLECT-ITEMS xL yDist)
        code 1
        out '())
  (foreach item items
    (setq fp (car item))
    (setq out (append out (list (cons fp code))))
    (setq code (1+ code)))
  out)

(defun CW-DA-ITEMS-WITH-CODES (xL yDist / items code out)
  (setq items (CW-DA-COLLECT-ITEMS xL yDist)
        code 1
        out '())
  (foreach item items
    (setq out (append out (list (append item (list code)))))
    (setq code (1+ code)))
  out)

(defun CW-DA-INSTANCE-LABELS (bx by xL yDist / allY idx x-base ms-data ms segs seg
  y-sol y-sag piece-h lpts rpts fp cmap code y0 x0)
  (setq allY (CW-ALL-Y yDist)
        cmap (CW-DA-CODEMAP xL yDist)
        idx 0
        x-base (if *CW-BASE-XY* (car *CW-BASE-XY*) 0.0)
        ms-data *CW-MSEGS*)
  (repeat (length xL)
    (setq segs nil)
    (if ms-data
      (foreach ms ms-data
        (if (= (CW-RND (car ms))(CW-RND (+ x-base (nth idx xL))))
          (setq segs (cdr ms)))))
    (if (or (not segs)(= (length segs) 0))
      (setq segs (list (list 0 (+ (- (CW-LASTVAL allY)(car allY)) *PG* *UZU* *UZA*)))))
    (setq y-sol (if (> idx 0) (CW-SPAN-YLIST (1- idx) yDist) '()))
    (setq y-sag (if (< idx (1- (length xL))) (CW-SPAN-YLIST idx yDist) '()))
    (foreach seg segs
      (setq piece-h (- (cadr seg)(car seg)))
      (setq lpts (CW-SEG-CENTERS y-sol seg allY))
      (setq rpts (CW-SEG-CENTERS y-sag seg allY))
      (setq fp (CW-DA-PIECE-FP piece-h lpts rpts))
      (setq code (cdr (assoc fp cmap)))
      (if code
        (progn
          (setq x0 (+ bx (nth idx xL)))
          (setq y0 (+ by (- (+ (car allY) (/ *PG* 2.0)) *UZA*) (car seg) (/ piece-h 2.0)))
          (CW-TV x0 y0 (strcat *DPX* "-" (itoa code)) *TH-DAP* "CW-YAZI"))))
    (setq idx (1+ idx))))


(defun CW-IMALAT-DA (bx by xL yDist / pg items item cursor-x py
  piece-h lpts rpts da-adet pxx item-max-top
  total-left left1 left2 right1 right2
  i label-w etk-h etk-gap item-w da-no box-x olddims)

  (setq pg *PG*)
  (CW-TM (+ bx 400)(- by (* *TH-BASLIK* 2.5))
    (strcat *DPX* " IMALAT") *TH-IMALAT* "CW-YAZI")

  (setq olddims (CW-DIMVARS-PUSH))
  (setq items (CW-DA-COLLECT-ITEMS xL yDist))
  (setq cursor-x bx da-no 1)

  (foreach item items
    (setq py by
          pxx cursor-x
          piece-h (nth 1 item)
          lpts (nth 2 item)
          rpts (nth 3 item)
          da-adet (nth 4 item)
          item-max-top (nth 1 item))

    (CW-R pxx py pg piece-h "CW-IMALAT")
    (foreach cp lpts (CW-R (- pxx 62)(+ py cp -25) 60 50 "CW-BAG"))
    (foreach cp rpts (CW-R (+ pxx pg 2)(+ py cp -25) 60 50 "CW-BAG"))

    (setq total-left 260.0 left1 160.0 left2 90.0 right1 160.0 right2 90.0)

    (CW-DV py (+ py piece-h)(- pxx 10)(- pxx total-left))
    (if (> (length lpts) 1)
      (progn
        (setq i 1)
        (repeat (1- (length lpts))
          (CW-DV (+ py (nth (1- i) lpts))(+ py (nth i lpts))(- pxx 10)(- pxx left1))
          (setq i (1+ i)))))
    (if lpts
      (progn
        (CW-DV py (+ py (car lpts))(- pxx 10)(- pxx left2))
        (CW-DV (+ py (CW-LASTVAL lpts))(+ py piece-h)(- pxx 10)(- pxx left2))))

    (if (> (length rpts) 1)
      (progn
        (setq i 1)
        (repeat (1- (length rpts))
          (CW-DV (+ py (nth (1- i) rpts))(+ py (nth i rpts))(+ pxx pg 10)(+ pxx pg right1))
          (setq i (1+ i)))))
    (if rpts
      (progn
        (CW-DV py (+ py (car rpts))(+ pxx pg 10)(+ pxx pg right2))
        (CW-DV (+ py (CW-LASTVAL rpts))(+ py piece-h)(+ pxx pg 10)(+ pxx pg right2))))

    (setq etk-h (* *TH-ETIKET* 2.5)
          etk-gap (* *TH-ETIKET* 0.5)
          label-w (max (* *TH-ETIKET* 10) 320.0)
          item-w 380.0)

    (setq box-x (- (+ pxx (/ pg 2.0)) (/ label-w 2.0)))
    (CW-R box-x (+ py item-max-top (* *TH-ETIKET* 2)) label-w etk-h "CW-IMALAT")
    (CW-TM (+ box-x (/ label-w 2.0))(+ py item-max-top (* *TH-ETIKET* 2)(/ etk-h 2.0))
      (strcat (itoa da-adet) " ADET") *TH-ETIKET* "CW-YAZI")

    (CW-R box-x (+ py item-max-top (* *TH-ETIKET* 2) etk-h etk-gap) label-w etk-h "CW-IMALAT")
    (CW-TM (+ box-x (/ label-w 2.0))(+ py item-max-top (* *TH-ETIKET* 2) etk-h etk-gap (/ etk-h 2.0))
      (strcat *DPX* "-" (itoa da-no)) *TH-ETIKET* "CW-YAZI")

    (setq cursor-x (+ cursor-x item-w *IM-SP*))
    (setq da-no (1+ da-no)))

  (CW-DIMVARS-POP olddims)
  (princ " OK")
  (- cursor-x bx))

;;; ==================== YA IMALAT v9 ====================
;;; Kertme olculeri ayni tarafta (sag), toplam boy sol, ic boy sag
(defun CW-IMALAT-YA (bx by xL yDist da-ofs / pg sp krt i j ya-boy ic-boy
  ya-done ya-cnt ya-adet px py kw etk-h etk-w etk-gap
  dim-lv1-l dim-lv1-r dim-lv2-r dim-lv3-r olddims box-x ya-red)
  (setq olddims (CW-DIMVARS-PUSH))
  (setq pg *PG* sp (+ 350 *IM-SP*) krt *KRT* ya-red 6.0)
  (CW-TM (+ bx 300)(- by (* *TH-BASLIK* 2.5))
    (strcat *YPX* " IMALAT") *TH-IMALAT* "CW-YAZI")
  ;; Olcu ofsetleri (ayarlar penceresinden gelir)
  (setq dim-lv1-l *YA-DIM-L1*
        dim-lv1-r *YA-DIM-R1*
        dim-lv2-r *YA-DIM-R2*
        dim-lv3-r *YA-DIM-R3*)
  (setq ya-done '() ya-cnt 0 i 0)
  (repeat (1- (length xL))
    (setq ya-boy (max 0.0 (- (+ (- (nth (1+ i) xL) (nth i xL) pg) (* krt 2)) ya-red))
          ic-boy (max 0.0 (- ya-boy (* krt 2))))
    (if (not (member (CW-RND ya-boy) ya-done))
      (progn
        ;; Adet hesapla
        (setq ya-adet 0
              j 0)
        (repeat (1- (length xL))
          (if (= (CW-RND (max 0.0 (- (+ (- (nth (1+ j) xL) (nth j xL) pg) (* krt 2)) ya-red)))
                 (CW-RND ya-boy))
            (setq ya-adet (+ ya-adet (length (CW-SPAN-YLIST j yDist)))))
          (setq j (1+ j)))
        (if (> ya-adet 0)
          (progn
            (setq px (+ bx (* ya-cnt sp))
                  py by
                  kw (- pg 10))
            ;; Kertmeli profil cizimi
            (entmake
              (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 "CW-IMALAT")
                    '(100 . "AcDbPolyline") '(90 . 8) '(70 . 1)
                    (cons 10 (list (+ px 5) py 0.0))
                    (cons 10 (list (+ px 5 kw) py 0.0))
                    (cons 10 (list (+ px 5 kw) (+ py krt) 0.0))
                    (cons 10 (list (+ px pg) (+ py krt) 0.0))
                    (cons 10 (list (+ px pg) (+ py krt ic-boy) 0.0))
                    (cons 10 (list (+ px 5 kw) (+ py krt ic-boy) 0.0))
                    (cons 10 (list (+ px 5 kw) (+ py ya-boy) 0.0))
                    (cons 10 (list (+ px 5) (+ py ya-boy) 0.0))))
            ;; ============ SOL TARAF: Toplam boy ============
            (CW-DV py (+ py ya-boy) (- px 10) (- px dim-lv1-l))
            ;; ============ SAG TARAF: Ic boy + kertmeler ============
            (CW-DV (+ py krt) (+ py krt ic-boy) (+ px pg 10) (+ px pg dim-lv1-r))
            (CW-DV py (+ py krt) (+ px pg dim-lv1-r) (+ px pg dim-lv2-r))
            (CW-DV (+ py ya-boy (- krt)) (+ py ya-boy) (+ px pg dim-lv1-r) (+ px pg dim-lv3-r))
            ;; Etiket
            (setq etk-h (* *TH-ETIKET* 2.5)
                  etk-w (* *TH-ETIKET* 10)
                  etk-gap (* *TH-ETIKET* 0.5))
            (setq box-x (- (+ px (/ pg 2.0)) (/ etk-w 2.0)))
            (CW-R box-x (+ py ya-boy (* *TH-ETIKET* 2)) etk-w etk-h "CW-IMALAT")
            (CW-TM (+ box-x (/ etk-w 2.0))
                   (+ py ya-boy (* *TH-ETIKET* 2) (/ etk-h 2.0))
                   (strcat (itoa ya-adet) " ADT.") *TH-ETIKET* "CW-YAZI")
            (CW-R box-x (+ py ya-boy (* *TH-ETIKET* 2) etk-h etk-gap) etk-w etk-h "CW-IMALAT")
            (CW-TM (+ box-x (/ etk-w 2.0))
                   (+ py ya-boy (* *TH-ETIKET* 2) etk-h etk-gap (/ etk-h 2.0))
                   (strcat *YPX* "-" (itoa (1+ ya-cnt))) *TH-ETIKET* "CW-YAZI")
            (setq ya-cnt (1+ ya-cnt))))
        (setq ya-done (append ya-done (list (CW-RND ya-boy))))))
    (setq i (1+ i)))
  (CW-DIMVARS-POP olddims)
  (princ " OK")
  (* ya-cnt sp))





;;; ==================== OPTIMIZASYON ====================
(defun CW-REPLACE-NTH (lst idx val / i out)
  (setq i 0 out '())
  (foreach a lst
    (if (= i idx)
      (setq out (append out (list val)))
      (setq out (append out (list a))))
    (setq i (1+ i)))
  out)

(defun CW-SUM (lst / total)
  (setq total 0.0)
  (foreach a lst (setq total (+ total a)))
  total)

(defun CW-PARSE-NUM-LIST (s / i ch tok out v)
  (setq i 1 tok "" out '())
  (while (<= i (strlen (if s s "")))
    (setq ch (substr s i 1))
    (if (wcmatch ch "[0-9.]")
      (setq tok (strcat tok ch))
      (if (/= tok "")
        (progn
          (setq v (atof tok))
          (if (> v 0.0) (setq out (append out (list v))))
          (setq tok ""))))
    (setq i (1+ i)))
  (if (/= tok "")
    (progn
      (setq v (atof tok))
      (if (> v 0.0) (setq out (append out (list v))))))
  (if out
    (vl-sort out '(lambda (a b) (< a b)))
    (list 6000.0)))

(defun CW-UNIQ-NUMS (lst / out)
  (setq out '())
  (foreach a (vl-sort lst '(lambda (x y) (< x y)))
    (if (or (null out) (/= (CW-RND a) (CW-RND (CW-LASTVAL out))))
      (setq out (append out (list a)))))
  out)

(defun CW-MAX-STOCK (vals / v mx)
  (setq mx 0.0)
  (cond
    ((numberp vals) (setq mx vals))
    ((CW-LISTP vals)
      (foreach v vals
        (if (and (numberp v) (> v mx)) (setq mx v)))))
  (if (> mx 0.0) mx 7000.0))

(defun CW-DA-PARTS (xL yDist / items out n item)
  (setq items (CW-DA-COLLECT-ITEMS xL yDist) out '() n 1)
  (foreach item items
    (setq out (append out (list (list "DA" (strcat *DPX* "-" (itoa n)) (nth 1 item) (nth 4 item)))))
    (setq n (1+ n)))
  out)

(defun CW-YA-PARTS (xL yDist / pg krt done out i n ya-boy qty j ya-red)
  (setq pg *PG* krt *KRT* done '() out '() i 0 n 1 ya-red 6.0)
  (repeat (1- (length xL))
    (setq ya-boy (max 0.0 (- (+ (- (nth (1+ i) xL) (nth i xL) pg) (* krt 2)) ya-red)))
    (if (not (member (CW-RND ya-boy) done))
      (progn
        (setq qty 0 j 0)
        (repeat (1- (length xL))
          (if (= (CW-RND (max 0.0 (- (+ (- (nth (1+ j) xL) (nth j xL) pg) (* krt 2)) ya-red))) (CW-RND ya-boy))
            (setq qty (+ qty (length (CW-SPAN-YLIST j yDist)))))
          (setq j (1+ j)))
        (if (> qty 0)
          (progn
            (setq out (append out (list (list "YA" (strcat *YPX* "-" (itoa n)) ya-boy qty))))
            (setq n (1+ n))))
        (setq done (append done (list (CW-RND ya-boy))))))
    (setq i (1+ i)))
  out)


(defun CW-YA-WIDTH (xL yDist / cnt)
  (setq cnt (length (CW-YA-PARTS xL yDist)))
  (* cnt (+ 350.0 *IM-SP*)))

(defun CW-EXPAND-PARTS (parts / out p i)
  (setq out '())
  (foreach p parts
    (setq i 0)
    (repeat (nth 3 p)
      (setq out (append out (list (list (car p) (nth 1 p) (nth 2 p)))))
      (setq i (1+ i))))
  out)

(defun CW-PART-SORT-DESC (parts)
  (vl-sort parts
    (function
      (lambda (a b)
        (if (/= (CW-RND (nth 2 a)) (CW-RND (nth 2 b)))
          (> (nth 2 a) (nth 2 b))
          (< (nth 1 a) (nth 1 b)))))))

(defun CW-BAR-FP (bar / s p)
  (setq s (strcat "S" (itoa (CW-RND (car bar)))))
  (foreach p (nth 2 bar)
    (setq s (strcat s "|" (car p) ":" (nth 1 p) ":" (itoa (CW-RND (nth 2 p))))))
  s)

(defun CW-GROUP-BARS (bars / out bar fp rec)
  (setq out '())
  (foreach bar bars
    (setq fp (CW-BAR-FP bar)
          rec (assoc fp out))
    (if rec
      (setq out (subst (list fp (nth 1 rec) (nth 2 rec) (1+ (nth 3 rec))) rec out))
      (setq out (append out (list (list fp bar (nth 2 bar) 1))))))
  out)

(defun CW-STOCK-SUMMARY (bars / out bar key rec)
  (setq out '())
  (foreach bar bars
    (setq key (CW-RND (car bar))
          rec (assoc key out))
    (if rec
      (setq out (subst (cons key (1+ (cdr rec))) rec out))
      (setq out (append out (list (cons key 1))))))
  (vl-sort out '(lambda (a b) (< (car a) (car b)))))

(defun CW-REMOVE-FIRST-EQUAL (lst itm / out dropped)
  (setq out '() dropped nil)
  (foreach a lst
    (if (and (not dropped) (equal a itm 1e-8))
      (setq dropped T)
      (setq out (append out (list a)))))
  out)

(defun CW-SIMULATE-BAR (seed remaining stock kerf / left pieces used need p)
  (setq left (CW-REMOVE-FIRST-EQUAL remaining seed)
        pieces (list seed)
        used (nth 2 seed))
  (foreach p left
    (setq need (+ (nth 2 p) kerf))
    (if (<= (+ used need) stock)
      (progn
        (setq used (+ used need))
        (setq pieces (append pieces (list p))))))
  ;; remove assigned pieces from remaining
  (setq left remaining)
  (foreach p pieces
    (setq left (CW-REMOVE-FIRST-EQUAL left p)))
  (list stock used pieces left))

(defun CW-BAR-SCORE (sim / stock used pieces waste)
  (setq stock (nth 0 sim)
        used  (nth 1 sim)
        pieces (nth 2 sim)
        waste (- stock used))
  ;; *OPT-MINBARS*=1 => onceki cubukta daha cok parca / daha cok kullanim / daha kucuk fire
  ;; aksi halde min fire => daha az fire / daha cok kullanim / daha kisa stok
  (if (= *OPT-MINBARS* "1")
    (list (- (length pieces)) (- used) waste stock)
    (list waste (- used) stock (- (length pieces)))))

(defun CW-BETTER-SCORE-P (a b)
  (cond
    ((null b) T)
    ((< (car a) (car b)) T)
    ((> (car a) (car b)) nil)
    ((< (cadr a) (cadr b)) T)
    ((> (cadr a) (cadr b)) nil)
    ((< (nth 2 a) (nth 2 b)) T)
    ((> (nth 2 a) (nth 2 b)) nil)
    ((< (nth 3 a) (nth 3 b)) T)
    (T nil)))

(defun CW-OPTIMIZE-PARTS (parts stock-options kerf / remaining opts seed best-sim best-score sim score stock bars)
  (setq bars '()
        opts (CW-UNIQ-NUMS stock-options)
        remaining (CW-PART-SORT-DESC (CW-EXPAND-PARTS parts)))
  (if (null opts) (setq opts (list 6000.0)))
  (while remaining
    (setq seed (car remaining)
          best-sim nil
          best-score nil)
    (foreach stock opts
      (if (>= stock (nth 2 seed))
        (progn
          (setq sim (CW-SIMULATE-BAR seed remaining stock kerf)
                score (CW-BAR-SCORE sim))
          (if (CW-BETTER-SCORE-P score best-score)
            (setq best-sim sim best-score score)))))
    (if best-sim
      (progn
        (setq bars (append bars (list (list (nth 0 best-sim) (nth 1 best-sim) (nth 2 best-sim)))))
        (setq remaining (CW-PART-SORT-DESC (nth 3 best-sim))))
      ;; hicbir stok sigmadiysa parcayi kendi boyunda tek cubuk kabul et
      (progn
        (setq bars (append bars (list (list (nth 2 seed) (nth 2 seed) (list seed)))))
        (setq remaining (cdr remaining)))))
  bars)

(defun CW-BARS-TOTAL-PART-LEN (bars / total bar p)
  (setq total 0.0)
  (foreach bar bars
    (foreach p (nth 2 bar) (setq total (+ total (nth 2 p)))))
  total)

(defun CW-BARS-TOTAL-STOCK-LEN (bars / total bar)
  (setq total 0.0)
  (foreach bar bars (setq total (+ total (car bar))))
  total)

(defun CW-BARS-TOTAL-FIRE (bars / total bar)
  (setq total 0.0)
  (foreach bar bars (setq total (+ total (- (car bar) (cadr bar)))))
  total)

(defun CW-BOX-TABLE (bx by title headers widths rows / th rowh titleh totalw px py i x r y)
  (setq th *TH-TABLO*
        rowh (max (* th 2.8) 45.0)
        titleh rowh
        totalw (CW-SUM widths))
  (setq py (- by titleh))
  (CW-R bx py totalw titleh "CW-TABLO")
  (CW-TM (+ bx (/ totalw 2.0)) (+ py (/ titleh 2.0)) title th "CW-TABLO")
  (setq py (- py rowh) x bx i 0)
  (foreach w widths
    (CW-R x py w rowh "CW-TABLO")
    (CW-TM (+ x (/ w 2.0)) (+ py (/ rowh 2.0)) (nth i headers) th "CW-TABLO")
    (setq x (+ x w) i (1+ i)))
  (setq y py r 0)
  (foreach row rows
    (setq y (- y rowh) x bx i 0)
    (foreach w widths
      (CW-R x y w rowh "CW-TABLO")
      (CW-TM (+ x (/ w 2.0)) (+ y (/ rowh 2.0)) (if (nth i row) (nth i row) "") th "CW-TABLO")
      (setq x (+ x w) i (1+ i)))
    (setq r (1+ r)))
  (* rowh (+ 2 (length rows))))

(defun CW-OPT-TABLES (bx by title parts bars / part-rows stock-rows grp map-rows sum-rows stock-summ total-bars total-parts total-stock total-fire fire-pct h1 h2 h3 h4 y gap)
  (setq part-rows '())
  (foreach p parts
    (setq part-rows (append part-rows (list (list (car p) (nth 1 p) (rtos (nth 2 p) 2 0) (itoa (nth 3 p)))))))
  (setq stock-summ (CW-STOCK-SUMMARY bars) stock-rows '())
  (foreach s stock-summ
    (setq stock-rows (append stock-rows (list (list (itoa (car s)) (itoa (cdr s)) (rtos (/ (* (car s) (cdr s)) 1000.0) 2 2))))))
  (setq grp (CW-GROUP-BARS bars) map-rows '())
  (foreach g grp
    (setq map-rows
      (append map-rows
        (list
          (list (strcat "MAP-" (itoa (1+ (length map-rows))))
                (rtos (car (nth 1 g)) 2 0)
                (itoa (nth 3 g))
                (itoa (length (nth 2 g)))
                (rtos (- (car (nth 1 g)) (cadr (nth 1 g))) 2 0))))))
  (setq total-bars (length bars)
        total-parts 0)
  (foreach p parts (setq total-parts (+ total-parts (nth 3 p))))
  (setq total-stock (CW-BARS-TOTAL-STOCK-LEN bars)
        total-fire (CW-BARS-TOTAL-FIRE bars)
        fire-pct (if (> total-stock 0.0) (* 100.0 (/ total-fire total-stock)) 0.0)
        sum-rows (list
          (list "Toplam Parca" (itoa total-parts))
          (list "Kullanilan Boy" (itoa total-bars))
          (list "Toplam Boy (m)" (rtos (/ total-stock 1000.0) 2 2))
          (list "Toplam Fire (m)" (rtos (/ total-fire 1000.0) 2 2))
          (list "Fire Orani" (strcat (rtos fire-pct 2 2) " %"))))
  (CW-TM (+ bx 700.0) (+ by (* *TH-OPT* 1.2)) title *TH-OPT* "CW-YAZI")
  (setq gap (max 60.0 (* *TH-TABLO* 3.0))
        y by)
  (setq h1 (CW-BOX-TABLE bx y "KESIM LISTESI" '("TIP" "KOD" "OLCU" "ADET")
            (list (max 90.0 (* *TH-TABLO* 5.0)) (max 160.0 (* *TH-TABLO* 8.0)) (max 140.0 (* *TH-TABLO* 7.0)) (max 90.0 (* *TH-TABLO* 5.0))) part-rows))
  (setq y (- y h1 gap))
  (setq h2 (CW-BOX-TABLE bx y "GEREKLI BOYLAR" '("STOK" "ADET" "TOPLAM m")
            (list (max 120.0 (* *TH-TABLO* 6.0)) (max 90.0 (* *TH-TABLO* 5.0)) (max 120.0 (* *TH-TABLO* 6.0))) stock-rows))
  (setq y (- y h2 gap))
  (setq h3 (CW-BOX-TABLE bx y "KESIM HARITALARI" '("HARITA" "STOK" "ORNEK" "PARCA" "FIRE")
            (list (max 120.0 (* *TH-TABLO* 6.0)) (max 120.0 (* *TH-TABLO* 6.0)) (max 100.0 (* *TH-TABLO* 5.0)) (max 100.0 (* *TH-TABLO* 5.0)) (max 110.0 (* *TH-TABLO* 5.5))) map-rows))
  (setq y (- y h3 gap))
  (setq h4 (CW-BOX-TABLE bx y "OZET" '("KRITER" "DEGER")
            (list (max 180.0 (* *TH-TABLO* 9.0)) (max 140.0 (* *TH-TABLO* 7.0))) sum-rows))
  (+ h1 h2 h3 h4 (* gap 4.0)))

(defun CW-MOD (a b)
  (- a (* b (fix (/ a b)))))

(defun CW-DRAW-OPT-MAPS (bx by title bars / grp idx total scale barh gap maxstock yy g bar count stock fire pieces px cur endx p hdrtxt eff hdrw pw cx shortn txty lastdimx lvl dimgap mapText dimText fireText topTxtOfs extBase extStep basey titleh hdrh minInside minCodeOnly outerText minBothText)
  (setq grp (CW-GROUP-BARS bars)
        total (length grp)
        maxstock (cond
                   ((and (boundp '*CW-OPT-DRAW-MAXSTOCK*) (numberp *CW-OPT-DRAW-MAXSTOCK*) (> *CW-OPT-DRAW-MAXSTOCK* 0.0))
                    *CW-OPT-DRAW-MAXSTOCK*)
                   (T 0.0)))
  (if (<= maxstock 0.0)
    (foreach g grp (if (> (car (nth 1 g)) maxstock) (setq maxstock (car (nth 1 g))))))
  (if (<= maxstock 0.0) (setq maxstock 7000.0))
  (setq scale 1.0
        barh 100.0
        gap 195.0
        yy by
        idx 1
        titleh (max (* *TH-OPT* 1.4) 34.0)
        hdrh (max (* *TH-OPT* 1.4) 34.0)
        mapText (max 9.0 (* *TH-TABLO* 0.50))
        dimText (max 8.0 (* *TH-TABLO* 0.45))
        fireText (max 9.0 (* *TH-TABLO* 0.50))
        dimgap 220.0
        topTxtOfs 50.0
        extBase 18.0
        extStep 14.0
        minInside 260.0
        minBothText 420.0
        minCodeOnly 120.0
        outerText 70.0)
  (CW-TL bx (+ by (* *TH-OPT* 1.1)) title *TH-OPT* "CW-YAZI")
  (foreach g grp
    (setq bar (nth 1 g)
          pieces (nth 2 g)
          count (nth 3 g)
          stock (car bar)
          fire (- (car bar) (cadr bar))
          px bx
          shortn 0
          lastdimx -1e99
          basey (- yy hdrh))
    (setq eff (if (> stock 0.0) (* 100.0 (/ (- stock fire) stock)) 0.0)
          hdrtxt (strcat (itoa idx) "/" (itoa total) "   STOK " (rtos stock 2 0) " mm"
                         (if (> count 1) (strcat "   |   " (itoa count) " ORNEK") "   |   TEK ORNEK")))
    (setq hdrw (* stock scale))
    (CW-R bx basey hdrw hdrh "CW-TABLO")
    (CW-TM (+ bx (/ hdrw 2.0)) (+ basey (/ hdrh 2.0)) hdrtxt *TH-OPT* "CW-YAZI")
    (setq yy (- basey 12.0))
    (CW-R px (- yy barh) (* stock scale) barh "CW-IMALAT")
    (setq cur 0.0)
    (foreach p pieces
      (setq endx (+ cur (nth 2 p))
            pw (* (nth 2 p) scale)
            cx (+ px (* (+ cur (/ (nth 2 p) 2.0)) scale)))
      (CW-R (+ px (* cur scale)) (- yy barh) pw barh "CW-BAG")
      (CW-L (+ px (* endx scale)) (- yy barh) (+ px (* endx scale)) yy "CW-OLCU")
      (cond
        ((>= pw minBothText)
          (CW-TM cx (- yy 32.0) (nth 1 p) mapText "CW-YAZI")
          (CW-TM cx (- yy 62.0) (rtos (nth 2 p) 2 0) dimText "CW-OLCU"))
        ((>= pw minInside)
          (CW-TM cx (- yy 48.0) (strcat (nth 1 p) " / " (rtos (nth 2 p) 2 0)) mapText "CW-YAZI"))
        ((>= pw minCodeOnly)
          (CW-TM cx (- yy 48.0) (nth 1 p) mapText "CW-YAZI")
          (setq lvl (CW-MOD shortn 3)
                txty (+ yy extBase (* lvl extStep)))
          (CW-L cx yy cx (+ yy 8.0) "CW-OLCU")
          (CW-TM cx txty (rtos (nth 2 p) 2 0) dimText "CW-OLCU")
          (setq shortn (1+ shortn)))
        (T
          (setq lvl (CW-MOD shortn 4)
                txty (+ yy extBase (* lvl extStep)))
          (CW-L cx yy cx (+ yy 8.0) "CW-OLCU")
          (CW-TM cx txty (nth 1 p) mapText "CW-YAZI")
          (if (> pw outerText)
            (CW-TM cx (- yy 48.0) (rtos (nth 2 p) 2 0) dimText "CW-OLCU")
            (progn
              (CW-L cx (- yy barh) cx (- yy barh 10.0) "CW-OLCU")
              (CW-TM cx (- yy barh 18.0) (rtos (nth 2 p) 2 0) dimText "CW-OLCU")))
          (setq shortn (1+ shortn))))
      (if (> (- (+ px (* endx scale)) lastdimx) dimgap)
        (progn
          (CW-TM (+ px (* endx scale)) (+ yy topTxtOfs) (rtos endx 2 0) dimText "CW-OLCU")
          (setq lastdimx (+ px (* endx scale)))))
      (setq cur (+ endx *OPT-KERF*)))
    (if (> fire 0.0)
      (progn
        (CW-R (+ px (* (- stock fire) scale)) (- yy barh) (* fire scale) barh "CW-TABLO")
        (if (> (* fire scale) 130.0)
          (CW-TM (+ px (* (- stock (/ fire 2.0)) scale)) (- yy 48.0) (strcat "FIRE " (rtos fire 2 0)) fireText "CW-YAZI"))))
    (CW-TM (+ bx (/ hdrw 2.0)) (- yy (+ barh 28.0))
           (strcat "TOPLAM FIRE: " (rtos fire 2 0) " mm   |   VERIM: " (rtos eff 2 2) " %")
           *TH-OPT* "CW-YAZI")
    (setq yy (- yy (+ barh gap 38.0))
          idx (1+ idx)))
  (- by yy))


(defun CW-OPT-SECTION (bx by title parts stockstr / stocks bars usedh maph hdr)
  (setq stocks (CW-UNIQ-NUMS (CW-PARSE-NUM-LIST stockstr)))
  (setq hdr (strcat title (if (= *OPT-MINBARS* "1") " [MOD: AZ BOY]" " [MOD: MIN FIRE]")))
  (setq bars (CW-OPTIMIZE-PARTS parts stocks *OPT-KERF*))
  (setq usedh (CW-OPT-TABLES bx by hdr parts bars))
  (setq maph (CW-DRAW-OPT-MAPS bx (- by usedh 120.0) (strcat hdr " - KESIM HARITALARI") bars))
  (+ usedh maph 250.0))

(defun CW-OPTIMIZATION (bx by xL yDist / da-parts ya-parts h all-parts all-stocks)
  (setq da-parts (CW-DA-PARTS xL yDist)
        ya-parts (CW-YA-PARTS xL yDist))
  ;; Tum optimizasyon alaninda ayni cizim olcegi: 6000 ve 7000 cubuklar farkli gorunsun
  (setq all-stocks (CW-UNIQ-NUMS (append (CW-PARSE-NUM-LIST *OPT-DA-STOCK*) (CW-PARSE-NUM-LIST *OPT-YA-STOCK*))))
  (setq *CW-OPT-DRAW-MAXSTOCK* (CW-MAX-STOCK all-stocks))
  (if (= *OPT-SAME* "1")
    (progn
      (setq all-parts (append da-parts ya-parts))
      (CW-OPT-SECTION bx by "DA + YA OPTIMIZASYONU" all-parts
        (vl-string-translate ";/|" ",,," (strcat *OPT-DA-STOCK* "," *OPT-YA-STOCK*))))
    (progn
      (setq h (CW-OPT-SECTION bx by "DA OPTIMIZASYONU" da-parts *OPT-DA-STOCK*))
      (CW-OPT-SECTION bx (- by h 150.0) "YA OPTIMIZASYONU" ya-parts *OPT-YA-STOCK*)))
  (setq *CW-OPT-DRAW-MAXSTOCK* nil))

(defun CW-DA-TABLE (bx by xL yDist / parts rows)
  (setq parts (CW-DA-PARTS xL yDist) rows '())
  (foreach p parts
    (setq rows (append rows (list (list (nth 1 p) (rtos (nth 2 p) 2 0) "-" "-" (itoa (nth 3 p)))))))
  (CW-BOX-TABLE bx by "DA OZETI" '("KOD" "BOY" "SOL" "SAG" "ADET")
    (list (max 140.0 (* *TH-TABLO* 7.0)) (max 120.0 (* *TH-TABLO* 6.0)) (max 90.0 (* *TH-TABLO* 4.5)) (max 90.0 (* *TH-TABLO* 4.5)) (max 90.0 (* *TH-TABLO* 4.5)))
    rows))
(defun CW-LAYOUT-PTS (base-xy xL yDist / bx by allY viewW coreH viewH gapX gapY secW secH planDrop camDrop imX imY optY)
  (setq bx (car base-xy)
        by (cadr base-xy)
        allY (CW-ALL-Y yDist)
        viewW (+ (CW-LASTVAL xL) *PG*)
        coreH (- (CW-LASTVAL allY) (car allY))
        viewH (+ coreH *PG* *UZA* *UZU*)
        gapX (max 1500.0 (* *TH-GPK* 22.0))
        gapY (max 1300.0 (* *TH-GPK* 18.0))
        secW (+ 260.0 (* (max 1 (length yDist)) (+ *PD* 70.0)))
        secH (+ viewH (max 400.0 (* *TH-GPK* 8.0)))
        planDrop (max (+ *PD* 420.0) (* *TH-GPK* 20.0))
        camDrop (+ viewH gapY)
        imX (+ bx viewW gapX)
        imY (- by (max (* viewH 0.48) 950.0))
        optY (- by (+ viewH (* gapY 2.0) (max 3400.0 (* *TH-OPT* 78.0)))))
  (list
    (list bx (- by planDrop))
    (list (- bx secW gapX) (+ by (* *TH-GPK* 5.0)))
    (list bx (- by camDrop))
    (list imX imY)
    (list bx optY)))

;;; ==================== ANA KOMUT: CW ====================
(defun C:CW ( / mullion-data raw-xlist prof-bot prof-top read-result
  xL yDist base-xy bx by osm allY cl da-n ya-n cam-bp im-bp y-base-raw
  pb1 pb2 pb3 kb1 kb2 kb3 msegs layout-pts sec-pt plan-pt auto-cam auto-im auto-opt opt-bp ya-w table-x)

  (setq *error* (lambda (m)(if osm (setvar "OSMODE" osm))(setvar "CMDECHO" 1)
    (if m (princ (strcat "\nHATA: " m)))(princ)))

  (princ "\n================================================")
  (princ "\n  GIYDIRME CEPHE v10.5h")
  (princ "\n================================================")

  (CW-READCFG)
  (if (not (CW-DCL-SHOW))(progn (princ "\n  Iptal.")(exit)))

  (setq pb1 (CW-BLOCKNAME-OR-NIL *LAST-PB1*)
        pb2 (CW-BLOCKNAME-OR-NIL *LAST-PB2*)
        pb3 (CW-BLOCKNAME-OR-NIL *LAST-PB3*))
  (setq kb1 (CW-BLOCKNAME-OR-NIL *LAST-KB1*)
        kb2 (CW-BLOCKNAME-OR-NIL *LAST-KB2*)
        kb3 (CW-BLOCKNAME-OR-NIL *LAST-KB3*))
  (princ (strcat "\n  Plan bloklari: "
    (if pb1 pb1 "-") " / " (if pb2 pb2 "-") " / " (if pb3 pb3 "-")))
  (princ (strcat "\n  Kesit bloklari: "
    (if kb1 kb1 "-") " / " (if kb2 kb2 "-") " / " (if kb3 kb3 "-")))

  (princ "\n\n--- GORUNUSTEN OKUMA ---")
  (setq mullion-data (CW-READ-MULLIONS))
  (setq raw-xlist (nth 0 mullion-data))
  (setq prof-bot (nth 1 mullion-data))
  (setq prof-top (nth 2 mullion-data))
  (if (nth 3 mullion-data)(setq *CW-MSEGS* (nth 3 mullion-data)))
  (setq read-result (CW-READ-TRANSOMS raw-xlist))
  (setq xL (nth 0 read-result))
  (setq yDist (nth 1 read-result))
  (setq base-xy (nth 2 read-result))
  (setq *CW-BASE-XY* base-xy)

  (setq *CW-HS* (CW-READ-HIDDEN-SASHES raw-xlist yDist base-xy))

  (setq allY (CW-ALL-Y yDist))
  (setq y-base-raw (cadr base-xy))
  (if (= *AUTO-UZ* "1")
    (progn
      (setq *UZA* (- (+ y-base-raw (car allY))(/ *PG* 2.0) prof-bot))
      (setq *UZU* (- prof-top (+ y-base-raw (CW-LASTVAL allY))(/ *PG* 2.0)))
      (if (< *UZA* 0)(setq *UZA* 0))
      (if (< *UZU* 0)(setq *UZU* 0))))
  (princ (strcat "\n  Alt uzanim: " (rtos *UZA* 2 1) " | Ust uzanim: " (rtos *UZU* 2 1)))

  (setq *CW-XL* xL *CW-YDIST* yDist)
  (setq bx (car base-xy) by (cadr base-xy))
  (setq layout-pts (CW-LAYOUT-PTS base-xy xL yDist)
        plan-pt (nth 0 layout-pts)
        sec-pt (nth 1 layout-pts)
        auto-cam (nth 2 layout-pts)
        auto-im (nth 3 layout-pts)
        auto-opt (nth 4 layout-pts))

  (command "_.UNDO" "_BE")
  (setq osm (getvar "OSMODE"))(setvar "OSMODE" 0)(setvar "CMDECHO" 0)
  (CW-LAY)

  (princ "\n  [1] Kodlama...")
  (CW-KODLA bx by xL yDist)

  (if (= "1" *DO-PLAN*)
    (progn
      (princ "\n  [2] Plan...")
      (CW-PLAN (car plan-pt) (cadr plan-pt) xL pb1 pb2 pb3)))

  (if (= "1" *DO-KESIT*)
    (progn
      (princ "\n  [3] Kesit...")
      (CW-KESIT (car sec-pt) (cadr sec-pt) yDist kb1 kb2 kb3)))

  (if (= "1" *DO-CAM*)
    (progn
      (setvar "OSMODE" osm)
      (setq cam-bp (getpoint "\nCam cizimi noktasi <ENTER=otomatik>: "))
      (setvar "OSMODE" 0)
      (if (null cam-bp) (setq cam-bp auto-cam))
      (if cam-bp
        (progn
          (princ "\n  [4] Cam...")
          (setq cl (CW-CAM (car cam-bp)(cadr cam-bp) xL yDist *CPX*))
          (CW-LISTE (+ (car cam-bp)(CW-LASTVAL xL) 400)(+ (cadr cam-bp)(CW-LASTVAL allY)) cl)
          (setq *CW-CL* cl)))))

  (if (or (= "1" *DO-DA*)(= "1" *DO-YA*))
    (progn
      (setvar "OSMODE" osm)
      (setq im-bp (getpoint "\nImalat noktasi <ENTER=otomatik>: "))
      (setvar "OSMODE" 0)
      (if (null im-bp) (setq im-bp auto-im))
      (if im-bp
        (progn
          (setq da-n 0 ya-n 0)
          (if (= "1" *DO-DA*)
            (progn
              (princ "\n  [5] DA...")
              (setq da-n (CW-IMALAT-DA (car im-bp)(cadr im-bp) xL yDist))))
          (if (= "1" *DO-YA*)
            (progn
              (princ "\n  [6] YA...")
              (setq ya-n (CW-IMALAT-YA (+ (car im-bp) (if da-n da-n 0) 300)(cadr im-bp) xL yDist (if da-n da-n 0)))))
          (if (= "1" *DO-DA*)
            (progn
              (setq ya-w (if (> ya-n 0) ya-n (CW-YA-WIDTH xL yDist))
                    table-x (+ (car im-bp) (if da-n da-n 0) 300 ya-w 450.0))
              (CW-DA-TABLE table-x (+ (cadr im-bp) (* *TH-TABLO* 18.0)) xL yDist)))))))

  (if (= "1" *DO-OPT*)
    (progn
      (setvar "OSMODE" osm)
      (setq opt-bp (getpoint "\nOptimizasyon noktasi <ENTER=otomatik>: "))
      (setvar "OSMODE" 0)
      (if (null opt-bp) (setq opt-bp auto-opt))
      (if opt-bp
        (progn
          (princ "\n  [7] OPTIMIZASYON...")
          (CW-OPTIMIZATION (car opt-bp) (cadr opt-bp) xL yDist)))))

  (if (and (= "1" *DO-CSV*) cl) (CW-CSV cl))

  (setvar "OSMODE" osm)(setvar "CMDECHO" 1)
  (command "_.UNDO" "_E")(command "_.ZOOM" "_E")
  (princ "\n  TAMAMLANDI!")
  (princ))

;;; CWSET
(defun C:CWSET ()
  (CW-READCFG)
  (if (CW-DCL-SHOW) (princ "\n  Ayarlar kaydedildi."))
  (princ))

;;; Yukleme
(CW-READCFG)
(princ "\n================================================")
(princ "\n  GIYDIRME CEPHE v10.5h")
(princ "\n  CW = Ana komut | CWSET = Ayarlar")
(princ "\n================================================")
(princ)
