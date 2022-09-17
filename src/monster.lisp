(in-package :casket2022aki)


(defparameter *battle-monster-pos* '(0 ((180 280)) ((180 340) (180 180)) ((180 400) (180 280) (180 160))
                                      ((180 460) (180 340) (180 220) (180 100))
                                      ((180 100) (320 460) (320 340) (320 220) (320 100))
                                      ((180 220) (180 100) (320 460) (320 340) (320 220) (320 100))
                                      ((180 340) (180 220) (180 100) (320 460) (320 340) (320 220) (320 100))
                                      ((180 460) (180 340) (180 220) (180 100) (320 460) (320 340) (320 220) (320 100))
                                      ((40 100) (180 460) (180 340) (180 220) (180 100) (320 460) (320 340) (320 220) (320 100))
                                      ((40 220) (40 100) (180 460) (180 340) (180 220) (180 100) (320 460) (320 340) (320 220) (320 100))
                                      ((40 340) (40 220) (40 100) (180 460) (180 340) (180 220) (180 100) (320 460) (320 340) (320 220) (320 100))
                                     ((40 460) (40 340) (40 220) (40 100) (180 460) (180 340) (180 220) (180 100) (320 460) (320 340) (320 220) (320 100))))


;;モンスターシンボルの出現率
(defparameter *symbol1-10* '((:slime . 300) (:orc . 100) (:brigand . 70) (:bubble . 30)))
(defparameter *symbol11-20* '((:slime . 200) (:orc . 150) (:brigand . 100) (:bubble . 60) (:hydra . 50)))
(defparameter *symbol21-30* '((:slime . 100) (:orc . 100) (:brigand . 80) (:bubble . 70) (:hydra . 70) (:skelton . 50)))
(defparameter *symbol31-40* '((:slime . 50) (:orc . 70) (:brigand . 70) (:bubble . 70) (:hydra . 70) (:skelton . 70) (:dragon . 30)))
(defparameter *symbol41-50* '((:slime . 30) (:orc . 40) (:brigand . 50) (:bubble . 50) (:hydra . 70) (:skelton . 70) (:dragon . 50)))
  


(defun create-map-monster (x y kind)
  (case kind
    (:orc
     (make-instance 'orc :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			 :kind *orc* :w 32 :h 32 :w2 32 :h2 32))
    (:slime
     (make-instance 'slime :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			   :kind *slime* :w 32 :h 32 :w2 32 :h2 32))
    (:brigand
     (make-instance 'brigand :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			     :kind *brigand* :w 32 :h 32 :w2 32 :h2 32))
    (:bubble
     (make-instance 'bubble :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			    :kind *bubble* :w 32 :h 32 :w2 32 :h2 32))
    (:hydra
     (make-instance 'hydra :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			   :kind *hydra* :w 32 :h 32 :w2 32 :h2 32))
    (:skelton
     (make-instance 'skelton :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			     :kind *skelton* :w 32 :h 32 :w2 32 :h2 32))
    (:dragon
     (make-instance 'dragon :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			    :kind *dragon* :w 32 :h 32 :w2 32 :h2 32))))

;;マップ上のラスボス
(defun create-boss ()
  (make-instance 'mogemos :posx 12 :posy 4 :drawx (* 11 32) :drawy (* 2 32)
		 :kind *mogemos* :w 64 :h 64 :w2 96 :h2 96))

;;マップ上のモンスターシンボル作成
(defun create-monsters (donjon)
  (with-slots (yuka-list monsters floor-num) donjon
    (setf monsters nil)
    (cond
      ((= floor-num 50)
       (push (create-boss) monsters))
      (t
       (let ((rate (cond
		     ((>= 10 floor-num 1) *symbol1-10*)
		     ((>= 20 floor-num 11) *symbol11-20*)
		     ((>= 30 floor-num 21) *symbol21-30*)
		     ((>= 40 floor-num 31) *symbol31-40*)
		     ((>= 50 floor-num 41) *symbol41-50*))))
	 (loop :repeat (+ 3 (random (+ 2 (floor floor-num 2))))
               :do (let ((yuka (nth (random (length yuka-list)) yuka-list))
			 (kind (weightpick rate)))
                     (push (create-map-monster (car yuka) (cadr yuka) kind) monsters)
                     (setf yuka-list (remove yuka yuka-list :test #'equal)))))))))

;;battle---------------------------------------------------------------------------------------
;;バトルモンスターの出現率
(defun get-battle-monster-rate ()
  (with-slots (collide-monster) *p*
    (let ((kind (monster/kind collide-monster)))
      (cond
	((= kind *slime*) '((:slime . 100) (:orc . 70) (:brigand . 50) (:bubble . 20)))
	((= kind *orc*) '((:slime . 100) (:orc . 170) (:brigand . 80) (:bubble . 80)))
	((= kind *brigand*) '((:slime . 50) (:orc . 70) (:brigand . 150) (:bubble . 80) (:hydra . 20)))
	((= kind *bubble*) '((:slime . 30) (:orc . 50) (:brigand . 70) (:bubble . 120) (:hydra . 60) (:skelton . 30)))
	((= kind *hydra*) '((:orc . 20) (:brigand . 50) (:bubble . 70) (:hydra . 120) (:skelton . 60) (:dragon . 5)))
	((= kind *skelton*) '((:slime . 10) (:orc . 15) (:brigand . 30) (:bubble . 60) (:hydra . 70) (:skelton . 70) (:dragon . 25)))
	((= kind *dragon*) '((:slime . 10) (:orc . 15) (:brigand . 30) (:bubble . 40) (:hydra . 60) (:skelton . 60) (:dragon . 45)))
	((= kind *mogemos*) '((:mogemos . 10)))))))

;;dragon
(defun create-dragon (x y lv)
  (let ((hp (+ 35 (floor (random lv)))))
    (make-instance 'dragon :kind *dragon* :drawx x :drawy y :lv lv
			   :w 32 :h 32 :w2 64 :h2 64
			   :hp hp :maxhp hp :exp (+ 30 (randval lv))
			   :str (+ 1 (floor (random lv) 3))
			   :agi (+ 1 (floor (random lv) 4)))))
;;slime
(defun create-slime (x y lv)
  (let ((hp (+ 5 (floor (random lv) 3))))
    (make-instance 'slime :kind *slime* :drawx x :drawy y :lv lv
			  :w 32 :h 32 :w2 64 :h2 64
			  :hp hp :maxhp hp :exp (+ 3 (randval lv))
			  :str (+ 1 (floor (random lv) 3))
			  :agi (+ 1 (floor (random lv) 4)))))

;;orc
(defun create-orc (x y lv)
  (let ((hp (+ 10 (random lv))))
    (make-instance 'orc :kind *orc* :drawx x :drawy y :lv lv
			:w 32 :h 32 :w2 64 :h2 64
			:hp hp :maxhp hp :exp (+ 5 (randval lv))
			:str (+ 5 (floor (random lv) 2))
			:agi (+ 3 (floor (random lv) 5)))))
;;brigand
(defun create-brigand (x y lv)
  (let ((hp (+ 7 (floor (randval lv) 2))))
    (make-instance 'brigand :kind *brigand* :drawx x :drawy y :lv lv
			    :w 32 :h 32 :w2 64 :h2 64
			    :hp hp :maxhp hp :exp (+ 7 (randval lv))
			    :str (+ 4 (floor (random lv) 2))
			    :agi (+ 4 (floor (random lv) 2)))))
;;bubble
(defun create-bubble (x y lv)
  (let ((hp (+ 5 (floor (randval lv) 3))))
    (make-instance 'bubble :kind *bubble* :drawx x :drawy y :lv lv
			   :w 32 :h 32 :w2 64 :h2 64
			   :hp hp :maxhp hp :exp (+ 8 (randval lv))
			   :str (+ 4 (floor (random lv) 3))
			   :agi (+ 4 (floor (random lv) 3)))))
;;hydra
(defun create-hydra (x y lv)
  (let ((hp (+ 20 (floor (randval (* lv 2))))))
    (make-instance 'hydra :kind *hydra* :drawx x :drawy y :lv lv
			  :w 32 :h 32 :w2 64 :h2 64
			  :hp hp :maxhp hp :exp (+ 10 (randval lv))
			  :str (+ 9 (floor (random lv)))
			  :agi (+ 3 (floor (random lv) 4)))))
;;skelton
(defun create-skelton (x y lv)
  (let ((hp (+ 15 (floor (randval (* lv 1.5))))))
    (make-instance 'skelton :kind *skelton* :drawx x :drawy y :lv lv
			    :w 32 :h 32 :w2 64 :h2 64
			    :hp hp :maxhp hp :exp (+ 13 (randval lv))
			    :str (+ 10 (floor (random (* lv 1.1))))
			    :agi (+ 5 (floor (random lv))))))
;;mogemos
(defun create-mogemos ()
  (make-instance 'mogemos :kind *mogemos* :drawx 60 :drawy 30 :lv 50
			  :w 64 :h 64 :w2 512 :h2 512
			  :hp 1000 :maxhp 1000 :str 100 :agi 100))
;;
(defun create-battle-monster (kind x y floor-num)
  (with-slots (lv) *p*
    (let ((monster-lv (if (>= (random 3) 1)
			  (max 1 (- lv  (randval (floor floor-num 5))))
			  (+ lv (random 4) (floor floor-num 3)))))
      (case kind
	(:slime (create-slime x y monster-lv))
	(:orc (create-orc x y monster-lv))
	(:brigand (create-brigand x y monster-lv))
	(:bubble (create-bubble x y monster-lv))
	(:hydra (create-hydra x y monster-lv))
	(:skelton (create-skelton x y monster-lv))
	(:mogemos (create-mogemos))))))


;;バトル用のモンスター作成
(Defun create-battle-monsters (donjon)
  (with-slots (battle-monsters floor-num) donjon
    (setf battle-monsters nil)
    (let* ((monster-num 10);;(min 12 (+ (+ (1+ (random 3)) (floor floor-num 8)) (random (1+ (floor floor-num 4))))))
           (monsters-pos (nth monster-num *battle-monster-pos*))
	   (battle-monster-rate (get-battle-monster-rate)))
        (loop :repeat monster-num
              :for pos :in monsters-pos
              :do (let* ((monster-kind (weightpick battle-monster-rate))
			 (monster (create-battle-monster monster-kind (car pos) (cadr pos) floor-num)))
                    (push monster battle-monsters))))))
