;;;; gost_7805-70.lisp

(in-package #:ost)

;;; "ost" goes here. Hacks and glory await!

;;;; ГОСТ 7805-70

(defparameter *gost_7805-70_head*
  '(("Номинальный диаметр резьбы d" 	                                        1.6	2	2.5	3	-3.5	4	5	6	8	10	12	-14	16	-18	20	-22	24	-27	30	36	42	48)
    ("Шаг резьбы крупный"                                                       0.35	0.4	0.45	0.5	0.6	0.7	0.8	1	1.25	1.5	1.75	2	2	2.5	2.5	2.5	3	2.5	3.5	4	4.5	5)
    ("Шаг резьбы мелкий"                                                        nil 	nil	nil	nil	nil	nil	nil	nil	1	1.25	1.25	1.5	1.5	1.5	1.5	1.5	2	2	2	3	3	3)
    ("Диаметр стержня d1"     		                                        1.6	2	2.5	3	3.5	4	5	6	8	10	12	14	16	18	20	22	24	27	30	36	42	48)
    ("Размер «под ключ» S"   		                                        3.2	4	5	5.5	6	7	8	10	13	16	18	21	24	27	30	34	36	41	46	55	65	75)
    ("Высота головки k"        		                                        1.1	1.4	1.7	2.0	2.4	2.8	3.5	4.0	5.3	6.4	7.5	8.8	10.0	12.0	12.5	14.0	15.0	17.0	18.7	22.5	26.0	30.0)
    ("Диаметр описанной окружности е. не менее"		                        3.4	4.3	5.5	6.0	6.6	7.7	8.8	11.1	14.4	17.8	20.0	23.4	26.8	30.1	33.5	37.7	40.0	45.6	51.3	61.3	72.6	83.9)
    ("dw. не менее"                                  	                        2.3	3.1	4.1	4.6	5.1	5.9	6.9	8.9	11.6	14.6	16.6	19.6	22.5	25.3	28.2	31.7	33.6	38.0	42.7	51.1	61.0	70.5)
    ("hw	не менее"                                                       nil	nil	nil	0.15	0.15	0.15	0.15	0.15	0.15	0.15	0.15	0.15	0.20	0.20	0.20	0.20	0.20	0.20	0.20	0.20	0.25	0.25)
    ("hw	не более"                                                       nil	nil	nil	0.4	0.4	0.4	0.5	0.5	0.6	0.6	0.6	0.6	0.8	0.8	0.8	0.8	0.8	0.8	0.8	0.8	0.8	0.8)
    ("Диаметр отверстия в стержне d3"        		                        nil	nil	nil	nil	nil	1.0	1.2	1.6	2.0	2.5	3.2	3.2	4.0	4.0	4.0	5.0	4.0	4.0	6.3	6.3	8.0	8.0)
    ("Диаметр отверстия в головке d4 H15"      		                        nil	nil	nil	nil	nil	1.0	1.2	2.0	2.5	2.5	3.2	3.2	4.0	4.0	4.0	4.0	4.0	4.0	4.0	5.0	5.0	5.0)
    ("Расстояние от опорной поверхности до оси отверстия в головке l2 js15"	nil	nil	nil	nil	nil	1.4	1.8	2.0	2.8	3.5	4.0	4.5	5.0	6.0	6.5	7.0	7.5	8.5	9.5	11.5	13.0	15.0)))


;;;;Длина болта l	Длина резьбы b и расстояние от опорной поверхности головки до оси отверстия в стержне l1 при номинальном диаметре резьбы d (знаком × отмечены болты с резьбой на всей длине стержня)
(defparameter *gost_7805-70_length_d*
  '(    1.6	2	2.5	3	3.5	4	4	5	5	6	6	8	8	10	10	12	12	-14	-14	16	16	-18	-18	20	20	-22	-22	24	24	-27	-27	30	30	36	36	42	42	48	48))

(defparameter *gost_7805-70_length_b_l1_str*
  '(       "b"	"b"	"b"	"b"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"	"l1"	"b"))

(defparameter *gost_7805-70_length_d_b_l1*
  '((2    T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (3	  T	T	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (4	  T	T	T	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (5	  T	T	T	T	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (6	  T	T	T	T	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (8	  T	T	T	T	T	nil	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (10	  T	T	T	T	T	7.5	T	nil	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (12	  9	T	T	T	T	9.5	T	9.5	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (14	  9	10	11	12	13	11.5	T	11.5	T	10	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (16	  nil	10	11	12	13	13.5	14	13.5	T	12	T	12	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (-18  nil	10	11	12	13	15.5	14	15.5	16	14	T	14	T	14	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (20	  nil	nil	11	12	13	17.5	14	17.5	16	16	T	16	T	16	T	15	T	nil	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (-22  nil	nil	11	12	13	19.5	14	19.5	16	18	18	18	T	18	T	17	T	17	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (25	  nil	nil	11	12	13	22.5	14	22.5	16	21	18	21	T	21	T	20	T	20	T	19	T	nil	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (-28  nil	nil	nil	12	13	25.5	14	25.5	16	24	18	24	22	24	T	23	T	23	T	22	T	22	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (30	  nil	nil	nil	12	13	27.5	14	27.5	16	26	18	26	22	26	T	25	T	25	T	24	T	24	T	24	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (-32  nil	nil	nil	nil	nil	29.5	14	29.5	16	28	18	28	22	28	26	27	T	27	T	26	T	26	T	26	T	25	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil)
    (35	  nil	nil	nil	nil	nil	32.5	14	32.5	16	31	18	31	22	31	26	30	30	30	T	29	T	29	T	29	T	28	T	28	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil)
    (-38  nil	nil	nil	nil	nil	35.5	14	35.5	16	34	18	34	22	34	26	33	30	33	T	32	T	32	T	32	T	31	T	31	T	nil	T	nil	nil	nil	nil	nil	nil	nil	nil)
    (40	  nil	nil	nil	nil	nil	37.5	14	37.5	16	36	18	36	22	36	26	35	30	35	34	34	T	34	T	34	T	33	T	33	T	32	T	nil	T	nil	nil	nil	nil	nil	nil)
    (45	  nil	nil	nil	nil	nil	42.5	14	42.5	16	41	18	41	22	41	26	40	30	40	34	39	38	39	T	39	T	38	T	38	T	37	T	36	T	nil	nil	nil	nil	nil	nil)
    (50	  nil	nil	nil	nil	nil	47.5	14	47.5	16	46	18	46	22	46	26	45	30	45	34	44	38	44	42	44	T	43	T	43	T	42	T	41	T	40	T	nil	nil	nil	nil)
    (55	  nil	nil	nil	nil	nil	52.5	14	52.5	16	51	18	51	22	51	26	50	30	50	34	49	38	49	42	49	46	48	T	48	T	47	T	46	T	45	T	nil	T	nil	nil)
    (60	  nil	nil	nil	nil	nil	57.5	14	57.5	16	56	18	56	22	56	26	55	30	55	34	54	38	54	42	54	46	53	50	53	T	52	T	51	T	50	T	48	T	nil	nil)
    (65	  nil	nil	nil	nil	nil	nil	nil	62.5	16	61	18	61	22	61	26	60	30	60	34	59	38	59	42	59	46	58	50	58	54	57	T	56	T	55	T	53	T	nil	T)
    (70	  nil	nil	nil	nil	nil	nil	nil	67.5	16	66	18	66	22	66	26	65	30	65	34	64	38	64	42	64	46	63	50	63	54	62	60	61	T	60	T	58	T	58	T)
    (75	  nil	nil	nil	nil	nil	nil	nil	72.5	16	71	18	71	22	71	26	70	30	70	34	69	38	69	42	69	46	68	50	68	54	67	60	66	66	65	T	63	T	56	T)
    (80	  nil	nil	nil	nil	nil	nil	nil	77.5	16	76	18	76	22	76	26	75	30	75	34	74	38	74	42	74	46	73	50	73	54	72	60	71	66	70	T	68	T	68	T)
    (-85  nil	nil	nil	nil	nil	nil	nil	nil	nil	81	18	81	22	81	26	80	30	80	34	79	38	79	42	79	46	78	50	78	54	77	60	76	66	75	T	73	T	73	T)
    (90	  nil	nil	nil	nil	nil	nil	nil	nil	nil	86	18	86	22	86	26	85	30	85	34	84	38	84	42	84	46	83	50	83	54	82	60	81	66	80	78	78	T	78	T)
    (-95  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	91	22	91	26	90	30	90	34	89	38	89	42	89	46	88	50	88	54	87	60	86	66	85	78	83	T	83	T)
    (100  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	96	22	96	26	95	30	95	34	94	38	94	42	94	46	93	50	93	54	92	60	91	66	90	78	88	T	88	T)
    (-105 nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	101	26	100	30	100	34	99	38	99	42	99	46	98	50	98	54	97	60	96	66	95	78	93	90	93	T)
    (110  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	106	26	105	30	105	34	104	38	104	42	104	46	103	50	103	54	102	60	101	66	100	78	98	90	98	T)
    (-115 nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	111	26	110	30	110	34	109	38	109	42	109	46	108	50	108	54	107	60	106	66	105	78	103	90	103	102)
    (120  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	116	26	115	30	115	34	114	38	114	42	114	46	113	50	113	54	112	60	111	66	110	78	108	90	108	102)
    (-125 nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	121	26	120	30	120	34	119	38	119	42	119	46	118	50	118	54	117	60	116	66	115	78	113	90	113	102)
    (130  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	126	32	125	36	125	40	124	44	124	48	124	52	123	56	123	60	122	66	121	72	120	84	118	96	118	108)
    (140  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	136	32	135	36	135	40	134	44	134	48	134	52	133	56	133	60	132	66	131	72	130	84	128	96	128	108)
    (150  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	146	32	145	36	145	40	144	44	144	48	144	52	143	56	143	60	142	66	141	72	140	84	138	96	138	108)
    (160  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	156	32	155	36	155	40	154	44	154	48	154	52	153	56	153	60	152	66	151	72	150	84	148	96	148	108)
    (170  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	166	32	165	36	165	40	164	44	164	48	164	52	163	56	163	60	162	66	161	72	160	84	158	96	158	108)
    (180  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	176	32	175	36	175	40	174	44	174	48	174	52	173	56	173	60	172	66	171	72	170	84	168	96	168	108)
    (190  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	186	32	185	36	185	40	184	44	184	48	184	52	183	56	183	60	182	66	181	72	180	84	178	96	178	108)
    (200  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	196	32	195	36	195	40	194	44	194	48	194	52	193	56	193	60	192	66	191	72	190	84	188	96	188	108)
    (220  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	215	49	215	53	214	57	214	61	214	65	213	69	213	73	212	79	211	85	210	97	208	109	208	121)
    (240  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	235	49	235	53	234	57	234	61	234	65	233	69	233	73	232	79	231	85	230	97	228	109	228	121)
    (260  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	255	49	255	53	254	57	254	61	254	65	253	69	253	73	252	79	251	85	250	97	248	109	248	121)
    (280  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	275	53	274	57	274	61	274	65	273	69	273	73	272	79	271	85	270	97	268	109	268	121)
    (300  nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	nil	295	53	294	57	294	61	294	65	293	69	293	73	292	79	291	85	290	97	288	109	288	121)))


(defparameter *gost_7805-70_length*
  (mapcar #'car *gost_7805-70_length_d_b_l1*))

(defparameter *gost_7805-70_d_b_l1*
  (lst-arr:list-list-transponate (mapcar #'cdr *gost_7805-70_length_d_b_l1*)))

(defun get-from-gost_7805-70_length_d_b_l1 (key)
    (let ((rez nil))
    (mapcar #'(lambda (dia str b_l1)
		(mapcar #'(lambda (k len)
		     (cond
		       ((and k (equal key str))
			(setf rez (cons (list dia len k ) rez)))))
		 b_l1
		 *gost_7805-70_length*))
	    *gost_7805-70_length_d*
	    *gost_7805-70_length_b_l1_str*
	    *gost_7805-70_d_b_l1*)
	  (reverse rez)))

(defparameter *gost_7805-70_tbl-d-l-b-l1*
  (let ((seq (get-from-gost_7805-70_length_d_b_l1 "b")))
    (mapcar
     #'(lambda(el)
	 (setf seq
	       (substitute
		(append
		 (list (abs (first el))
		       (abs (second el))
		       (third el)
		      ) 
		 (list
		  (third
		   (find (list (first el) (second el))
			 (get-from-gost_7805-70_length_d_b_l1 "l1")
			 :test #'equal
			 :key #'(lambda (p) (list (first p) (second p)))))))
		el  seq :test #'equal)))
     (get-from-gost_7805-70_length_d_b_l1 "b"))
    seq))

(defun select-bolt-gost_7805-70 (l_gl l_rez &key (d_min 1) (d_max 48))
  "Позволяет подобрать длину болта по ГОСТ 7805-70
l_gl  - длина гладкой части свинчиваемого участка;
l_rez - длина резьбовой части свинчиваемого участка;
Примеры использования:
(select-bolt-gost_7805-70 15 15)
(select-bolt-gost_7805-70 15 15 :d_min 10 )
(select-bolt-gost_7805-70 15 15 :d_max 12 )
(select-bolt-gost_7805-70 15 15 :d_min 10 :d_max 10 )
"
  (remove-if-not
   #'(lambda (x)
       (let ((d (first x))
	     (l (second x))
	     (b (if (numberp (third x)) (third x) (second x))))
	 (and (< (- l b ) l_gl)
	      (> l (+ l_gl l_rez))
	      (<= d d_max)
	      (>= d d_min))))
   *gost_7805-70_tbl-d-l-b-l1*))
