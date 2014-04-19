(defvar input nil)(defvar *inventory* nil)(defvar money 0)(defvar chance 0)(defvar is-defenced 0)
(defvar *charecters* (list (list 0 0 "@") (list 0 0 "G") (list 0 0 "%") (list 0 0 "$")))
(defvar player-level (list 1 0 0))(defvar expirience 0)(defvar hp (list 0 0))(defvar mp (list 0 0))(defvar lk (list 0 0))
(defvar ehp 0)(defvar emp 0)
(defvar see-far (list "" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro randomize(var num) "Make things shorter :)" `(setf ,var (random ,num)))

(defmacro putting-macro(x y) "Puts every char from list on the map"
  `(dolist (char *charecters*)
     (if(and (eq(car char) ,y) (eq (car (cdr char)) ,x))
	(progn(format t (car (cdr (cdr char)))) (return 1)))))

(defmacro smthng-on-my-way(what) "Well, this one helps a lot"
  `(let((that ,what))
    (cond
      ((string= that "food")
       (if(< (length *charecters*) 10)(progn (push "food" *inventory*)
					     (randomize (car (cdr (car (cdr (cdr *charecters*))))) 39)
					     (randomize (car (car (cdr (cdr *charecters*)))) 29))
	  (format t "Inventory is full!")))
      ((string= that "money")(progn (setf money (+ money (random 50))) (randomize (car (cdr (car (cdr (cdr (cdr *charecters*)))))) 39)
				    (randomize (car (car (cdr (cdr (cdr *charecters*))))) 29))))))

(defmacro use-smthng(what) "Is used in fight, but will also be used within it"
  `(let ((that ,what)) (cond ((and(not(eq *inventory* nil))(string= that "food"))(progn(pop *inventory*)(setf (car hp) (+ (car hp) (+ 15 (random 10)))))))))

(defun skillpoints()
  (if(> (car (cdr (cdr player-level))) 0)
     (progn
       (format t "~%What do you want to improve?~%(1)HP (2)MP (3)Luck~%")
       (setf input (read-line))
       (cond
	 ((string= input "1")(progn (decf (car (cdr (cdr player-level)))) (incf (car (cdr hp)))))
	 ((string= input "2")(progn (decf (car (cdr (cdr player-level)))) (incf (car (cdr mp)))))
	 ((string= input "3")(progn (decf (car (cdr (cdr player-level)))) (incf (car (cdr lk)))(setf (car lk) (* 5 (car (cdr lk))))))
	 (t (format t "Nothing always means nothing...~%"))))
     (format t "You don't have any of skill points :(~%")))

(defmacro create-enemy(fact) "Makes my life kinda easier" `(+ 30 (* ,fact 9) (random 10)))

(defun draw()
  (loop for y from 1 to 30 
     do(loop for x from 0 to 101 
	  do(if(or(eq x 0)(eq x 101))
	       (format t "|")
	       (if(not(eq (putting-macro x y) 1))
		  (format t " "))))
     do(format t "~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attack-player(defenced)
  (if(eq defenced 0)
     (progn
       (setf input (random 100))(if(> input 86)(setf (car hp) (- (car hp) 45))(setf (car hp) (- (car hp) (random 15)))))(format t "Clear defence!~%")))

(defun enemy-move()
  (let ((x (car (cdr (car (cdr *charecters*)))))(y (car (car (cdr *charecters*))))
	(px (car (cdr (car *charecters*))))(py (car (car *charecters*))))
    (if(or(not(eq px x))(not(eq py y)))
       (progn
	 (setf input (random 1000))
	 (cond
	   ((and(> input 456)(< input 478))
	    (progn(if(and(or(and(> y py)(< (- y py) 5))(and(< y py)(< (- py y) 5)))(or(and(> x px)(< (- x px) 5))(and(< x px)(< (- px x) 5))))
		     (progn(setf x px)(setf y py)(format t "~%Enemy has felt you!~%")))))
	   ((and(> input 193)(< input 254))(if(> px x)(incf x)(decf x)))
	   ((and(> input 932)(< input 999))(if(> py y)(incf y)(decf y)))
	   (t (if(or(eq (+ x 1) px)(eq (- x 1) px))(setf x px)
		 (if(or(eq (+ y 1) py)(eq (- y 1) py))(setf y py)
		    (progn (setf input (random 100))
			   (cond
			     ((and(> input 0)(< input 24)(> y 1))(decf y)) ((and(> input 24)(< input 49)(> x 1))(decf x))
			     ((and(> input 49)(< input 74)(< x 40))(incf x)) ((and(> input 74)(< input 100)(< y 30))(incf y)) (t (enemy-move))))))))))
    (setf (car (cdr (car (cdr *charecters*)))) x) (setf (car (car (cdr *charecters*))) y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attack-enemy() 
  (incf chance)
  (randomize input 100)
  (let ((player-level (car player-level)))
    (if(and (> (+ chance (car lk)) 15) (> input 90))
       (setf ehp (+ ehp (- (+ 10 (* 25 player-level) (* 15 player-level)))))
       (if(> input 30)(setf ehp (- (+ ehp player-level) (+ (random 30) (* (+ player-level 1) 2))))(format t "~%You've missed!~%")))))

(defun defence-self() (randomize input 100) (if(< input 39)(setf is-defenced 1)(setf is-defenced 0)))

(defun try-to-flee() (randomize input 100)(if(> input 85)(progn (format t "You have succsessfully got out of troubles!~%") 
								(setf (car (car *charecters*)) (- (car (car *charecters*)) 3))
								(setf (car (cdr (car *charecters*))) (- (car (cdr (car *charecters*))) 3)))
					     (progn (setf (car hp) (- (car hp) 5))(format t "Worsest try ever!~%"))))

(defun fight-stance()
  (setf is-defenced 0)
  (loop for i from 0 to 39 do(format t "_"))
  (format t "~%Your stats: HP ~D, MP ~D~%Enemy: HP ~D MP ~D~%You have ~D food~%" (car hp) (car mp) ehp emp (length *inventory*))
  (loop for i from 0 to 39 do(format t "_"))
  (format t "~%")
  (if(> (car hp) 0)(progn
;; Comment next 5 lines and uncomment the commented ones to make it automazed
		     (setf input (read-line))
		     (cond((string= input "flee")(try-to-flee))
			  ((string= input "att")(attack-enemy))
			  ((string= input "def")(defence-self))
			  ((string= (subseq input 0 1) "u")(use-smthng (subseq input 2))))
;		     (cond
;		       ((> (car hp) ehp)(attack-enemy))
;		       ((and(< (car hp) ehp)(not(eq *inventory* nil)))(use-smthng "food")(attack-enemy))
;		       ((and(< (car hp) ehp)(eq *inventory* nil))(if(> (random 100) 73)(attack-enemy)(defence-self))))
		     (if(> ehp 0)(progn(attack-player is-defenced)(fight-stance))
			(progn (setf (car (cdr player-level)) (+ (floor (/ (+ 30 (* (car (cdr player-level)) 9) (random 10))  5.0))))
			       (randomize (car (cdr (car (cdr *charecters*)))) 39) (randomize (car (car (cdr *charecters*))) 29))))
     (format t "You're defeated!~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-moves (direction)
  (let ((x (car (cdr (car *charecters*)))) (y (car (car *charecters*))))
    (cond
      ((and(not(eq (- y 1) 0))(string= direction "w"))(decf y)) ((and(not(eq (- x 1) 0))(string= direction "a"))(decf x))
      ((and(not(eq (+ y 1) 31))(string= direction "s"))(incf y)) ((and(not(eq (+ x 1) 100))(string= direction "d"))(incf x)))
    (setf (car (cdr (car *charecters*))) x) (setf (car (car *charecters*)) y)))

(defun check-positions()
  (dolist (f *charecters*) (if(or (eq (car f) 0)(eq (car (cdr f)) 0)) (progn(randomize (car f) 29)(randomize (car (cdr f)) 39))))
  (let ((px (car (cdr (car *charecters*))))(py (car (car *charecters*)))(ex (car (cdr (car (cdr *charecters*)))))(ey (car (car (cdr *charecters*))))
	(fx (car (cdr (car (cdr (cdr *charecters*))))))(fy (car (car (cdr (cdr *charecters*)))))(mx (car (cdr (car (cdr (cdr (cdr *charecters*)))))))
	(my (car (car (cdr (cdr (cdr *charecters*)))))))
    (cond 
      ((and(eq px ex)(eq py ey))
       (progn (let((lvl (car player-level)))
		(setf ehp (create-enemy (+ (random 10) lvl)))(setf emp (create-enemy (+ (random 5) lvl)))(fight-stance))))
      ((and(eq px fx)(eq py fy))(smthng-on-my-way "food")) ((and(eq px mx)(eq py my))(smthng-on-my-way "money")))))

(defun check-stats()
  (let((exp (car (cdr player-level))))(if(> exp (+ (- (* 13 (car player-level)) (* 3 (car player-level))) (* 2 (car player-level)))) 
					 (progn(setf (car (cdr player-level)) 0)(incf (car player-level))(incf (car (cdr (cdr player-level))))
					       (setf (car hp) (+ 100 (* (car (cdr hp)) 10)))(setf (car mp) (+ 80 (* (car (cdr mp)) 5))))))
  (if(and(< (car hp) (+ 100 (* 10 (car (cdr hp)))))(not(eq *inventory* nil)))
     (progn (pop *inventory*)(setf (car hp) (+ (car hp) 20))(if(> (car hp) (+ 100 (* 10 (car (cdr hp)))))(setf (car hp) (+ 100 (* 10 (car (cdr hp))))))))
  (if(> (car (cdr (cdr player-level))) 0)(skillpoints)))

(defun check-surroundings()
  (let ((px (car (cdr (car *charecters*))))(py (car (car *charecters*)))
	(ex (car (cdr (car (cdr *charecters*)))))(ey (car (car (cdr *charecters*))))) 
    (if(> (- px ex) 0)(setf (car (cdr see-far)) "left")(if(eq (- px ex) 0)(setf (car (cdr see-far)) "")(setf (car (cdr see-far)) "right")))
    (if(> (- py ey) 0)(setf (car see-far) "up")(if(eq (- py ey) 0)(setf (car see-far) "")(setf (car see-far) "down"))))
  (if(and(> (length *inventory*) 1)(> (car hp) 99))
     (progn 
       (if(string= (car (cdr see-far)) "left")(decf (car (cdr (car *charecters*))))
	  (if(not(string= (car (cdr see-far)) ""))(incf (car (cdr (car *charecters*))))))
       (if(string= (car see-far) "up")(decf (car (car *charecters*)))
	  (if(not(string= (car see-far) ""))(incf (car (car *charecters*))))))
     (progn
       (let ((px (car (cdr (car *charecters*)))) (py (car (car *charecters*))) (ex (car (cdr (car (cdr *charecters*))))) (ey (car (car (cdr *charecters*)))) 
	     (fx (car (cdr (car (cdr (cdr *charecters*))))))(fy (car (car (cdr (cdr *charecters*))))))
	 (if(and(< (car hp) (+ 100 (* 10 (car (cdr hp)))))(or (eq (+ px 1) ex)(eq (- px 1) ex)(eq (+ py 1) ey)(eq (- py 1) ey)))
	    (progn (setf input (random 100)) (if(and(> input 32)(< input 81))(progn (setf px (+ ex 1))(setf py (+ ey 1))))))   
	 (if(> (- px fx) 0)(setf (car (cdr see-far)) "left")(if(eq (- px fx) 0)(setf (car (cdr see-far)) "")(setf (car (cdr see-far)) "right")))
	 (if(> (- py fy) 0)(setf (car see-far) "up")(if(eq (- py fy) 0)(setf (car see-far) "")(setf (car see-far) "down"))))
       (if(string= (car (cdr see-far)) "left")(decf (car (cdr (car *charecters*))))
	  (if(not(string= (car (cdr see-far)) ""))(incf (car (cdr (car *charecters*))))))
       (if(string= (car see-far) "up")(decf (car (car *charecters*)))
	  (if(not(string= (car see-far) ""))(incf (car (car *charecters*))))))))

(defun main()
  (check-positions)
  (if(> (car hp) 0)
     (progn
       (check-stats)(draw)
       (let((exp (car (cdr player-level)))(n (+ (- (* 13 (car player-level)) (* 3 (car player-level))) (* 2 (car player-level))))(i *inventory*)(m money)
	    (sp (car (cdr (cdr player-level)))))
	 ((lambda(f s th fo fi si se)
	    (format t "~15tEXP:~D/~D~25tHP:~D~35tMP:~D~45t$:~D~55tYou have ~D Skill points~%~15tInventory: ~{~A~^, ~}.~%" f s th fo fi si se)) 
	  exp n (car hp) (car mp) m sp i))
;;Comment this two lines and uncomment the commented one to make the game automazed
       (format t "#") (setf input (read-line))
       (if(not(string= input "q")) (progn (if(string= input "sp")(skillpoints)) (if(eq (length input) 1) (check-moves input)) (enemy-move) (main))))
;       (check-surroundings)(enemy-move)(main))
     (format t "You're dead. Start the game again.")))

;; For better launch, just initialize this function
(defun init()
  (setf hp '(100 0))
  (setf mp '(80 0))
  (setf lk '(0 0))
  (setf player-level '(1 0 0))
  (setf money 0)
  (dolist (chara *charecters*)
   (randomize (car (cdr chara)) 39)
   (randomize (car chara) 29))
  (main))
