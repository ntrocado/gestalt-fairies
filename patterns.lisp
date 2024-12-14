;;; (ql:quickload "cl-patterns/alsa-midi")

(in-package #:cl-patterns)

(backend-start 'alsa-midi)
(start-clock-loop)


;;; microfreak config

(defparameter *midi-channel* 0)
(defparameter *alsa-midi-pitchbend-range* 12)

(mapc (lambda (args) (apply #'alsa-midi-set-cc-mapping args))
      '((5   "Glide"              :glide          unipolar-1-to-midi)
	(9   "Oscillator Type"    :type           unipolar-1-to-midi)
	(10  "Oscillator Wave"    :wave           unipolar-1-to-midi)
	(12  "Oscillator Timbre"  :timbre         unipolar-1-to-midi)
	(13  "Oscillator Shape"   :shape          unipolar-1-to-midi)
	(23  "Filter Cutoff"      :cutoff         unipolar-1-to-midi)
	(83  "Filter Resonance"   :resonance      unipolar-1-to-midi)
	(26  "Filter Amount"      :filter-amount  unipolar-1-to-midi)
	(102 "Cycling Env Rise"   :cycling-rise   unipolar-1-to-midi)
	(103 "Cycling Env Fall"   :cycling-fall   unipolar-1-to-midi)
	(28  "Cycling Env Hold"   :cycling-hold   unipolar-1-to-midi)
	(24  "Cycling Env Amount" :cycling-amount unipolar-1-to-midi)
	(105 "Envelope Attack"    :attack         unipolar-1-to-midi)
	(106 "Envelope Decay"     :decay          unipolar-1-to-midi)
	(29  "Envelope Sustain"   :sustain        unipolar-1-to-midi)))

(defun preset (n)
  (multiple-value-bind (bank program)
      (floor (1- n) 128)
    (format t "bank-msb: ~a; program: ~a" bank program)))

;;; 1

(defmacro longp (then else)
  `(pif (p> (pk :dur) 2) ,then ,else))

(defparameter *h1*
  '(0 1 4 10 12 15 18 24))

(defparameter *h2*
  '(0 2 7 11 12 16 21 25))

(pb :#x382C4B6E52092AD0
  :bank-msb 0
  :instrument 95
  :scale :chromatic
  :degree (prand (list
		  (pclump (prand *h1*) 4)
		  (pclump (prand *h2*) 4)))
  :root (pclump (pseq '(0 6)) 4)
  :octave (pclump (pwhite 1 4) 4)
  :cutoff (pwhite .1 .4)
  :resonance (pif (p< (pk :cutoff) .3)
		  .2 .5)
  :attack (pwhite .04 .1)
  :decay (pwhite .1 .6)
  :dur (pseq (list 6
		   (pseq '(1 1/5 2 1/4) 4)
		   4/3
		   (pseq '(1 1/5 2 1/4) 4)
		   1 1/6))
  :wave (pwhite)
  :timbre (pwhite)
  :shape (pwhite)
  :cycling-amount (longp .15 .001)
  :filter-amount (pwhite .55 .75)
  :amp (longp (pwhite .8 1) (pwhite .4 .75))
  :tempo 200
  :quant 0)

(play :#x382C4B6E52092AD0)
(stop :#x382C4B6E52092AD0)


;;; 2

(pb :#x382C4B6E99A53D8B
  :bank-msb 2
  :instrument 69
  :scale :chromatic
  :degree (pseq (list 0
		      (prand '(11 8) 1)
		      1 6 3 7))
  :octave 2
  :dur (pseq (list 1/2 1/7
		   (pr 1/5 4)
		   1/2
		   (pr 1/7 3)
		   1/5 2/7 1/2 1/7 1/2))
  :shape (pslide '(.18 .45 .16 .6 .14 .9))
  :wave (pwrand '(.363 .17 .7)
		'(.8   .15 .05))
  :tempo 75
  :quant 0)

(play :#x382C4B6E99A53D8B)
(stop :#x382C4B6E99A53D8B)


;;; 3

(pb :#x382C4B6E6F67D00D
  :bank-msb 1
  :instrument 19
  :midinote (pseq '(58 50))
  :wave (pseq '(1 .5 .2 .1))
  :timbre (pwhite 0.01 .2)
  :shape (pwhite .6 .78)
  :dur (p* (prand '(1 1/2 2/3 3/4 4/5))
	   (pseq '(1 1/2 1/4 1/2 1/4) 1))
  :decay (pwhite .2 .4)
  :cycling-fall (prun (pseq '(.15 .21 .12 .5))
		      (pseq '(15 4 10 3)))
  :filter-amount (prun (pseq '(.5 1 .4 0))
		       (pseq '(20 1.5 5 1.5)))
  :tempo 80
  :quant 0)

(play :#x382C4B6E6F67D00D)
(stop :#x382C4B6E6F67D00D)


;;; 4

(defmacro pconcat (&rest patterns)
  `(psym (pseq (list ,@patterns))))

(pb :inharm-series
  :bank-msb 2
  :instrument 70
  :freq (pconcat
	  (pgeom (midinote-freq 37)
		 1.44 5)
	  (pgeom (midinote-freq 37)
		 1.22 9)
	  (pgeom (midinote-freq 37)
		 1.33 11)
	  (pgeom (midinote-freq 31)
		 1.59 7)
	  (pgeom (midinote-freq 40)
		 1.27 13))
  :dur 1/10
  :amp (pseq '(1 .1 .3 .1))
  :glide (pconcat
	   (pr 0 90)
	   (pr .5 8))
  :cutoff (pconcat
	    (pr .3 60)
	    (pseries* .3 .7 20))
  :shape (pwhite .1 .2)
  :quant 0)


;;; 5

(defparameter *harmony*
  '((36 (60 63 66 67 71))
    (32 (59 63 66 70 73))
    (37 (61 64 66 68 72))))

(defun harm-arp (harm)
  (destructuring-bind (bass chord)
      harm
    (pseq (list chord bass) 5)))

(pb :fastarp-chords
  :bank-msb 2
  :instrument 71
  :midinote
  (pseq (mapcar #'harm-arp *harmony*))
  :timbre (pwrand '(.4 1)
		  '(.9 .1))
  :shape (pwhite .78 .85)
  :cutoff (pwrand '(.55 .7 .4)
		  '(.8  .1 .1))
  :resonance (pwhite .68 .73)
  :dur 1
  :quant 0)


;;; 6

(let ((kk '(13 123 124 14 36))
      (sn '(43 4 41 7))
      (others '(2 29 21 44 80)))
  (pb :#x344B7014E9448D1B
    :bank-msb 1
    :instrument
    (ppatlace (list (pwxrand '(13 123 124 14 36)
			     '(.2 .3  .2  .2 .1))
		    (pwxrand '(43  4  41 7)
			     '(.65 .2 .2 .05))
		    (pxrand (list 2 29 21 44 80
				  (pwhite 1 128 1)))))
    :midinote 38
    :dur (pseq '(1.5 1 .25 1 1 .5 1 1 2 .5 .5))
    :tempo 180
    :quant 0))

(let ((kk '((13 123 124 14 36)
	    (.2 .3 .2 .2 .1)))
      (sn '((43 4 41 7)
	    (.65 .2 .2 .05)))
      (others '(2 29 21 44 80)))
  
  (pdef :track
	(pt (:dur 1
	     :midinote 38
	     :bank-msb 1)
	    
	    (:instrument (apply #'pwxrand kk) :amp 1)
	    (:instrument (apply #'pwxrand sn) :amp 1)
	    (:instrument (pxrand others)
	     :dur 1/2 :amp .3)
	    (:instrument (pwhite 1 128)
	     :dur (pxrand (list 1 (/ pi 2)))
	     :amp .15))))

(play :track)
(stop :track)


;;; 7

(tempo 75)

(defparameter *row* '(0 1 9 11 8 10 4 5 6 2 3 7))

(pb :p1
  :bank-msb 2
  :instrument 73
  :scale :chromatic
  :degree (pseq *row*)
  :octave (pseq '(3 2 4 4))
  :attack (pseq '(.02 .1))
  :decay .2
  :wave (pwhite .6 .65)
  :shape .27
  :dur (pseq '(1/3 1 1/2))
  :delayed (pwhite .05 .1)
  :quant 1)

(pb :p2
  :bank-msb 2
  :instrument 73
  :scale :chromatic
  :degree (pseq *row*)
  :octave (pseq '(4 3 2 3))
  :decay .1
  :wave (pwhite .2 .3)
  :shape .54
  :dur (pseq '(1/4 1 1/5))
  :delayed (pwhite .05 .1)
  :quant 1)

(pb :p3
  :bank-msb 2
  :instrument 73
  :scale :chromatic
  :degree (pseq *row*)
  :octave 5
  :attack (pseq '(0 .01 .6))
  :decay .4
  :wave (pwhite .7 .75)
  :shape (pseq '(.8 .1))
  :dur 1
  :quant 1)

(mapcar #'play '(:p1 :p2 :p3))


;;; 8

(pb :foo
  :bank-msb 2
  :instrument 74
  :type :set
  :note 60
  )
