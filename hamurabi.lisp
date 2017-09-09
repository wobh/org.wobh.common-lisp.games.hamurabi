;;;; -*- Mode:lisp;coding:utf-8 -*-
;;;; Hamurabi

;;;; DESCRIPTION

;;;; Converted from the original Focal program and modified
;;;; for Edusystem 70 by David Ahl, Digital
;;;; Modified for 8K by Peter Turnbull
;;;; Modified for Common Lisp by William Clifford.

;;;; based on several sources
;;;; http://www.atariarchives.org/basicgames/showpage.php?page=78

;;;; AUTHORS

;;;; William H. Clifford <wobh@wobh.org>

;;;; NOTES

;;;; 

(defpackage "HAMURABI"
  (:nicknames "SUMER" "HAMMURABI")
  (:use "CL")
  (:export "MAIN")
  (:documentation "HAMURABI"))

(in-package "HAMURABI")

(defstruct (sumeria-env (:conc-name sumer-))
  (total-dead 0)           ; (d1 0)
  (pop-starved-avg 0)      ; (p1 0)
  (year 0)                 ; z
  (plague-year Nil)        ; (q 1)
  (populus 95)             ; p
  (populus-dead 0)         ; (d 0)
  (populus-new 5)          ; (i 5)
  (bushels-harvested 3000) ; (h 3000)
  (bushels-eaten 0)
  (bushels-spoiled 200)    ; (e (- h s))
  (bushels-stored 2800)    ; s 
  (land-price 3)           ; (y 3)
  (acres 1000)             ; (a (/ h y))
  (acres-planted 0)
  )

;;; FIXME: make a sumer-history as a list of sumer-year data points
;;; recording acres-bought, acres-sold, acres-planted, births, deaths,
;;; harvest-yeild, bushels-spoiled, etc. Sumeria could be a table of
;;; years, acres, populations, storage, prices, and the yearly record.


(defparameter *sumer-rnd* (make-random-state t))

(defun set-sumer-land-price (sumeria)
  (setf (sumer-land-price sumeria)
	(+ 17 (random 10 *sumer-rnd*))))

;; 310 C=INT(10*RND(1)):Y=C+17
;; ...

(defun sumer-harvest-year (sumeria)
  (with-accessors ((acres-planted sumer-acres-planted)
		   (bushels sumer-bushels-stored)
		   (harvest sumer-bushels-harvested)
		   (spoiled sumer-bushels-spoiled)) sumeria
    (setf harvest (* acres-planted (1+ (random 5 *sumer-rnd*))))
    (let ((c (1+ (random 5 *sumer-rnd*))))
      (cond ((eq (/ c 2) (floor c 2))
	     (setf spoiled (floor bushels c))
	     (decf bushels spoiled))
	    (t (setf spoiled 0))))
    (incf bushels harvest)))

;; 511 GOSUB 800
;; 512 REM *** A BOUNTYFULL HARVEST!!
;; 515 Y=C:H=D*Y:E=0
;; 521 GOSUB 800
;; 522 IF INT(C/2)<>C/2 THEN 530
;; 523 REM *** THE RATS ARE RUNNING WILD!!
;; 525 E=INT(S/C)
;; 530 S=S-E+H
;; ...
;; 800 C=INT(RND(1)*5)+1

(defun sumer-populus-year (sumeria)
  (with-accessors ((year sumer-year)
		   (plague-year sumer-plague-year)
		   (acres sumer-acres)
		   (acres-planted sumer-acres-planted)
		   (bushels sumer-bushels-stored)
		   (bushels-eaten sumer-bushels-eaten)
		   (populus sumer-populus)
		   (populus-new sumer-populus-new)
		   (dead sumer-populus-dead)
		   (total-dead sumer-total-dead)
		   (pop-starved-avg sumer-pop-starved-avg)) sumeria
    (let ((c (1+ (random 5 *sumer-rnd*))))
      (setf populus-new
	    (floor (1+ (/ (/ (* c 20.0 (+ acres bushels)) populus) 100))))
      (incf populus populus-new)
      ;; HORRORS, A 15% CHANCE OF PLAGUE
      ;; FIXME: I translated this directly. (< (1+ (random 20)) 3))
      (when (< (* 10 (- (* 2 (random 1.0 *sumer-rnd*)) 0.3)) 0)
	(setf plague-year t
	      populus (floor populus 2)))
      (let ((starved (floor bushels-eaten 20)))
	(when (< starved populus)
	  (setf dead (- populus starved))
	  (decf populus dead)
	  (cond ((< dead (* 0.45 populus))
		 (setf pop-starved-avg
		       (/ (/ (* (1- year) (+ pop-starved-avg dead) 100.0)
			     populus)
			  year))
		 (incf total-dead dead))
		(t
		 (print-sumer
		  (format Nil
			  "~2&You starved ~D people in one year!!!~%" dead))
		 (list 'end-game 'impeached))))))))

;; 227 IF Q>0 THEN 230
;; 228 P=INT(P/2)
;; 229 PRINT "A HORRIBLE PLAGUE STRUCK!  HALF THE PEOPLE DIED."
;; ...
;; 410 PRINT "HOW MANY BUSHELS DO YOU WISH TO FEED YOUR PEOPLE";
;; 411 INPUT Q
;; ...
;; 440 PRINT "HOW MANY ACRES DO YOU WISH TO PLANT WITH SEED";
;; 441 INPUT D:IF D=0 THEN 511
;; ...  
;; 531 GOSUB 800  
;; 532 REM *** LET'S HAVE SOME BABIES
;; 533 I=INT(C*(20*A+S)/P/100+1)
;; 539 REM *** HOW MANY PEOPLE HAD FULL TUMMIES?
;; 540 C=INT(Q/20)
;; 541 REM *** HORRORS, A 15% CHANCE OF PLAGUE
;; 542 Q=INT(10*(2*RND(1)-.3))
;; 550 IF P<C THEN 210
;; 551 REM *** STARVE ENOUGH FOR IMPEACHMENT?
;; 552 D=P-C:IF D>.45*P THEN 560
;; 553 P1=((Z-1)*P1+D*100/P)/Z
;; 555 P=C:D1=D1+D:GOTO 215
;; 560 PRINT:PRINT "YOU STARVED"D"PEOPLE IN ONE YEAR!!!"
;; ...
;; 800 C=INT(RND(1)*5)+1

;;; FIXME

(defun sumer-populace-disgruntled (sumeria)
  (random (* 0.8 (sumer-populus sumeria)) *sumer-rnd*))

;; 966 PRINT INT(P*.8*RND(1));"PEOPLE WOULD"
;; 970 PRINT "DEARLY LIKE TO SEE YOU ASSASSINATED BUT WE ALL HAVE OUR"
;; 975 PRINT "TRIVIAL PROBLEMS."


;;;; Output

(defparameter *sumer-out* *standard-output*)

(defparameter *sumer-fmt* "~:@(~@?~)"
  "Set to Nil to print in mixed case.")

(defun print-sumer (message &optional (stream *sumer-out*))
  "Print messages to Sumeria."
  (apply #'format stream 
	 (if *sumer-fmt* (list *sumer-fmt* message) (list message)))
  (force-output stream))

(defparameter *sumer-io* *query-io*)

(defun read-sumer (prompt &optional (stream *sumer-io*))
  "Read input frum Sumeria."
  (print-sumer prompt stream)
  (force-output stream)
  (clear-input stream)
  (parse-integer (read-line stream) :junk-allowed T))

(defmacro with-hamurabi-command ((var prompt &key intro) &body body)
    "Read input and do something with it or set it to Nil a different input is needed."
    (let* ((out (gensym "OUTCOME"))
	   (get-choice `(loop
			   with ,var = Nil
			   with ,out = Nil
			   do 
			     (setf ,var (read-sumer ,prompt))
			     (setf ,out ,@body)
			   until (not (null ,var))
			   finally (return ,out))))
      (if intro
	  `(progn
	     (print-sumer ,intro)
	     ,get-choice)
	  get-choice)))

;; FIXME this basically saves a few lines of code

(defun title-message ()
	 (format Nil "~&~32THamurabi~%~
                      ~&~15TCreative Computing  Morristown, New Jersey~%"))

(defun launch-message (&optional stream)
  (format stream "~2&Try your hand at governing ancient Sumeria~%~
                   ~&successfully for a 10-yr term of office~%"))

(defun lack-acres-message (sumeria &optional stream)
  (format stream "~2&Hamurabi:  think again. you own only~%~
                  ~&~D acres.  Now then," (sumer-acres sumeria)))

(defun lack-grain-message (sumeria &optional stream)
  (format stream "~2&Hamurabi:  think again. you have only~%~
                  ~&~D bushels of grain.  Now then,"
	  (sumer-bushels-stored sumeria)))

(defun lack-people-message (sumeria &optional stream)
  (format stream "~2&But you have only~
                  ~&~D people to tend the fields.  Now then,"
	  (sumer-populus sumeria)))

(defun bad-order-message (&optional stream)
  (format stream "~2&Hamurabi:  I cannot do what you wish.~
                  ~&Get yourself another steward!!!!!~%"))

(defun bad-hamurabi-message (&optional stream)
  (format stream "~2&Due to this extreme mismanagement you have not only~%~
                  ~&been impeached and thrown out of office but you have~%~
                  ~&also been declared 'NATIONAL FINK' !!~%"))

;; 565 PRINT "DUE TO THIS EXTREME MISMANAGEMENT YOU HAVE NOT ONLY"
;; 566 PRINT "BEEN IMPEACHED AND THROWN OUT OF OFFICE BUT YOU HAVE"
;; 567 PRINT "ALSO BEEN DECLARED 'NATIONAL FINK' !!":GOTO 990


(defun turn-message (sumeria &optional stream)
  "Make turn message."
  (with-accessors ((year sumer-year)
		   (acres sumer-acres)
		   (plague sumer-plague-year)
		   (populus sumer-populus)
		   (new-people sumer-populus-new)
		   (starved sumer-populus-dead)
		   (bushels sumer-bushels-stored)
		   (harvest sumer-bushels-harvested)
		   (spoiled sumer-bushels-spoiled)) sumeria
    (format
     stream
     (with-output-to-string (message)
       (format message "~2&Hamurabi:  I beg to report to you,~%~
                        ~&in year ~D, ~D people starved, ~D came to the city."
	       year starved new-people)
       (when plague
	 (format message
		 "~&A horrible plague struck!  Half the people died.~%"))
       (format message "~&Population is now ~D~%~
	                ~&The city now owns ~D acres.~%~
	                ~&You harvested ~D bushels per acre.~%~
	                ~&Rats ate ~D bushels.~%~
	                ~&You have ~D bushels in store.~%"
	       populus acres harvest spoiled bushels)))))

;; 95 D1=0:P1=0
;; 110 Z=0:P=95:S=2800:H=3000:E=H-S
;; 120 Y=3:A=H/Y:I=5:Q=1
;; 210 D=0
;; 215 PRINT:PRINT:PRINT "HAMURABI:  I BEG TO REPORT TO YOU,":Z=Z+1
;; 217 PRINT "IN YEAR"Z","D"PEOPLE STARVED,"I"CAME TO THE CITY."
;; 218 P=P+I
;; 227 IF Q>0 THEN 230
;; 228 P=INT(P/2)
;; 229 PRINT "A HORRIBLE PLAGUE STRUCK!  HALF THE PEOPLE DIED."
;; 230 PRINT "POPULATION IS NOW"P 
;; 232 PRINT "THE CITY NOW OWNS"A"ACRES."
;; 235 PRINT "YOU HARVESTED"Y"BUSHELS PER ACRE."
;; 250 PRINT "RATS ATE"E"BUSHELS."
;; 260 PRINT "YOU NOW HAVE"S"BUSHELS IN STORE.":PRINT
;; 270 IF Z=11 THEN 860


(defun summary-message (sumeria &optional stream)
  (with-accessors ((acres sumer-acres)
		   (populus sumer-populus)
		   (pop-starved-avg sumer-pop-starved-avg)
		   (total-dead sumer-total-dead)) sumeria
    (let ((acres-per-person (float (/ acres populus))))
      (format
       stream
       (with-output-to-string (message)
	 (format message
		 "~2&In your 10-year term of office, ~d percent of the~%~
                   ~&population starved per year on average, ~
                     i.e., a total of~%~
                   ~&~D people died!!~%~
                   ~&You started with 10 acres per person and ended with~%~
                   ~&~D acres per person.~%"
		 pop-starved-avg total-dead acres-per-person)
	 (cond ((or (< 33 pop-starved-avg) (< 7 acres-per-person))
		(bad-hamurabi-message message))
	       ((or (< 10 pop-starved-avg) (< 9 acres-per-person))
		(format message
			"~&Your heavy-handed performance smacks of Nero~
                           and Ivan IV.~%~
                         ~&The people (remaining) find you an~
                           unpleasant ruler, and,~%~
                         ~&frankly, hate your guts!~%"))
	       ((or (< 3 pop-starved-avg) (< 10 acres-per-person))
		(format message
			"~&Your performance could have been somewhat~
                           better, but~%~
                         ~&really wasn't too bad at all. ~D people would~%~
                         ~&dearly like to see you assassinated but we all~
                           have our~%~
                         ~&trivial problems.~%"
			(sumer-populace-disgruntled sumeria)))
	       (t
		(format message
			"~&A fantastic performance!!!  ~
                           Charlemange, Disraeli,~%~
                         ~&Jefferson combined could not have~
                           done better!~%"))))))))

(defun end-game-message (&optional stream)
  (format stream "~2&So long for now."))

;; 860 PRINT "IN YOUR 10-YEAR TERM OF OFFICE,"P1"PERCENT OF THE"
;; 862 PRINT "POPULATION STARVED PER YEAR ON AVERAGE, I.E., A TOTAL OF"
;; 865 PRINT D1"PEOPLE DIED!!":L=A/P
;; 870 PRINT "YOU STARTED WITH 10 ACRES PER PERSON AND ENDED WITH"
;; 875 PRINT L"ACRES PER PERSON.":PRINT
;; 880 IF P1>33 THEN 565
;; 885 IF L<7 THEN 565
;; 890 IF P1>10 THEN 940
;; 892 IF L<9 THEN 940
;; 895 IF P1>3 THEN 960
;; 896 IF L<10 THEN 960
;; 900 PRINT "A FANTASTIC PERFORMANCE!!!  CHARLEMANGE, DISRAELI, AND"
;; 905 PRINT "JEFFERSON COMBINED COULD NOT HAVE DONE BETTER!":GOTO 990
;; 940 PRINT "YOUR HEAVY-HANDED PERFORMANCE SMACKS OF NERO AND IVAN IV."
;; 945 PRINT "THE PEOPLE (REMAINING) FIND YOU AN UNPLEASANT RULER, AND,"
;; 950 PRINT "FRANKLY, HATE YOUR GUTS!":GOTO 990
;; 960 PRINT "YOUR PERFORMANCE COULD HAVE BEEN SOMEWHAT BETTER, BUT"
;; 965 PRINT "REALLY WASN'T TOO BAD AT ALL. ";
;; 966 PRINT INT(P*.8*RND(1));"PEOPLE WOULD"
;; 970 PRINT "DEARLY LIKE TO SEE YOU ASSASSINATED BUT WE ALL HAVE OUR"
;; 975 PRINT "TRIVIAL PROBLEMS."
;; 990 PRINT:FOR N=1 TO 10:PRINT CHR$(7);:NEXT N
;; 995 PRINT "SO LONG FOR NOW.":PRINT
;; 999 END


(defparameter *handle-negative-input* Nil)

(defparameter *handle-other-input* Nil)

;;; FIXME: learn to use the condition system for handling inputs



(defun sell-land (sumeria)
  "Sell land."
  (with-accessors ((acres sumer-acres)
		   (bushels sumer-bushels-stored)
		   (price sumer-land-price)) sumeria
    (with-hamurabi-command
	(acres-sold
	 (format Nil "~&How many acres do you wish to sell "))
      (cond ((< acres-sold 0)
	     (cond (*handle-negative-input*
		    (setf acres-sold Nil))
		   (t
		    (print-sumer (bad-order-message))
		    (list 'end-game 'sold-negative-acres acres-sold))))
	    ((<= acres-sold acres)
	     (decf acres acres-sold)
	     (incf bushels (* price acres-sold)))
	    ((< acres acres-sold)
	     (print-sumer (lack-acres-message sumeria))
	     (setf acres-sold Nil))
	    (t
	     (cond (*handle-other-input*
		    (setf acres-sold Nil))
		   (t
		    (error "Bad input selling land: ~A" acres-sold))))))))

(defun buy-land (sumeria)
  "Buy land."
  (with-accessors ((acres sumer-acres)
		   (bushels sumer-bushels-stored)
		   (price sumer-land-price)) sumeria
    (set-sumer-land-price sumeria)
    (with-hamurabi-command
	(acres-bought
	 (format Nil "~2&How many acres do you wish to buy ")
	 :intro (format Nil
			"~2&Land is trading at ~D bushels per acre."
			price))
      (cond ((< acres-bought 0)
	     (cond (*handle-negative-input*
		    (setf acres-bought Nil))
		   (t
		    (print-sumer (bad-order-message))
		    (list 'end-game 'bought-negative-acres acres-bought))))
	    ((zerop acres-bought)
	     (sell-land sumeria))
	    ((< bushels (* price acres-bought))
	     (print-sumer (lack-grain-message sumeria))
	     (setf acres-bought Nil))
	    ((<= (* price acres-bought) bushels)
	     (incf acres acres-bought)
	     (decf bushels (* price acres-bought)))
	    (t
	     (cond (*handle-other-input*
		    (setf acres-bought Nil))
		   (t
		    (error "Bad input buying land: ~A" acres-bought))))))))


;; 310 C=INT(10*RND(1)): Y=C+17
;; 312 PRINT "LAND IS TRADING AT";Y;"BUSHELS PER ACRE."
;; 320 PRINT "HOW MANY ACRES DO YOU WISH TO BUY";
;; 321 INPUT Q: IF Q<0 THEN 850
;; 322 IF Y*Q<=S THEN 330
;; 323 GOSUB 710
;; 324 GOTO 320
;; 330 IF Q=0 THEN 340
;; 331 A=A+Q: S=S-Y*Q: C=0
;; 334 GOTO 400
;; 340 PRINT "HOW MANY ACRES DO YOU WISH TO SELL";
;; 341 INPUT Q: IF Q<0 THEN 850
;; 342 IF Q<A THEN 350
;; 343 GOSUB 720
;; 344 GOTO 340
;; 350 A=A-Q: S=S+Y*Q: C=0

(defun feed-people (sumeria)
  "Feed thee people."
  (with-accessors ((acres sumer-acres)
		   (bushels sumer-bushels-stored)
		   (eaten sumer-bushels-eaten)
		   (price sumer-land-price)) sumeria 
    (with-hamurabi-command
	(feed
	 (format Nil "~2&How many bushels do you wish to feed your people "))
      (cond ((< feed 0)
	     (cond (*handle-negative-input*
		    (setf feed Nil))
		   (t
		    (print-sumer (bad-order-message))
		    (list 'end-game 'feed-negative-bushels feed))))
	    ((< bushels feed)
	     (print-sumer (lack-grain-message sumeria))
	     (setf feed Nil))
	    ((<= feed bushels)
	     (setf eaten feed))
	    (t
	     (cond (*handle-other-input*
		    (setf feed Nil))
		   (t
		    (error "Bad input feeding people: ~A" feed))))))))

;; 410 PRINT "HOW MANY BUSHELS DO YOU WISH TO FEED YOUR PEOPLE";
;; 411 INPUT Q
;; 412 IF Q<0 THEN 850
;; 418 REM *** TRYING TO USE MORE GRAIN THAN IS IN SILOS?
;; 420 IF Q<=S THEN 430
;; 421 GOSUB 710
;; 422 GOTO 410
;; 430 S=S-Q:C=1:PRINT

;;; This C=1 doesn't seem to have any effect. C isn't referenced again until 511 GOSUB 800 resets it.

(defun plant-seed (sumeria)
  "Plant acres with seed."
  (with-accessors ((acres sumer-acres)
		   (acres-planted sumer-acres-planted)
		   (bushels sumer-bushels-stored)
		   (spoiled sumer-bushels-spoiled)
		   (land-price sumer-land-price)
		   (populus sumer-populus)
		   (harvest sumer-bushels-harvested)) sumeria
    (with-hamurabi-command
	(seeded
	 (format Nil "~2&How many acres do you wish to plant with seed "))
      (cond ((zerop seeded)
	     (setf acres-planted seeded))
	    ((< seeded 0)
	     (cond (*handle-negative-input*
		    (setf seeded Nil))
		   (t
		    (print-sumer (bad-order-message))
		    (list 'end-game 'plant-negative-acres seeded))))
	    ((< acres seeded)
	     (setf seeded Nil)
	     (print-sumer (lack-acres-message sumeria)))
	    ((< bushels (floor seeded 2))
	     (setf seeded Nil)
	     (print-sumer (lack-grain-message sumeria)))
	    ((< (* 10 populus) seeded)
	     (setf seeded Nil)
	     (print-sumer (lack-people-message sumeria)))
	    ((<= seeded (* 10 populus))
	     (setf acres-planted seeded)
	     (decf bushels (floor seeded 2)))
	    (t
	     (cond (*handle-other-input*
		    (setf seeded Nil))
		   (t
		    (error "Bad input planting seeds: ~A" seeded))))))))

;; 440 PRINT "HOW MANY ACRES DO YOU WISH TO PLANT WITH SEED";
;; 441 INPUT D:IF D=0 THEN 511
;; 442 IF D<0 THEN 850
;; 444 REM *** TRYING TO PLANT MORE ACRES THAN YOU OWN?
;; 445 IF D<=A THEN 450
;; 446 GOSUB 720
;; 447 GOTO 440
;; 449 REM *** ENOUGH GRAIN FOR SEED?
;; 450 IF INT(D/2)<S THEN 455
;; 452 GOSUB 710
;; 453 GOTO 440 
;; 454 REM *** ENOUGH PEOPLE TO TEND THE CROPS?
;; 455 IF D<10*P THEN 510
;; 460 PRINT "BUT YOU HAVE ONLY"P"PEOPLE TO TEND THE FIELDS. NOW THEN,"
;; 470 GOTO 440
;; 510 S=S-INT(D/2)


(defun end-game-p (outcome)
  (when (and (listp outcome) (eq (first outcome) 'end-game))
    (rest outcome)))

(defun end-game-type-of (outcome)
  (first (end-game-p outcome)))

(defun turn-year (sumeria)
  (incf (sumer-year sumeria))
  (print-sumer (turn-message sumeria))
    (loop
     for fn in (list #'buy-land #'feed-people #'plant-seed
		     #'sumer-harvest-year #'sumer-populus-year)
     with outcome = Nil
     do
       (setf outcome (funcall fn sumeria))
     until
       (end-game-p outcome)
     finally
	 (return outcome)))

;;; FIXME: The original game does this weird thing where entering a
;;; negative number when buying selling feeding or planting quits the
;;; game.

(defun main (&optional sumeria)
  (print-sumer (title-message))
  (let ((sumeria (or sumeria (make-sumeria-env))))
    (print-sumer (launch-message))
    (loop
       repeat 10
       with outcome = Nil
       do
	 (setf outcome (turn-year sumeria))
       until
	 (end-game-p outcome)
       finally
	 (unless (find (end-game-type-of outcome)
		       '(impeached bought-negative-acres sold-negative-acres
			 feed-negative-bushels plant-negative-acres))
	   (print-sumer (summary-message sumeria))))
    (print-sumer (end-game-message))
    sumeria))
  

;;; TODO: write this whole thing in tagbody, mimicking the original control-flow of HMRABI.BAS.
