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

(defun create-map-monster (x y kind)
  (case kind
    (:orc
     (make-instance 'orc :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			 :kind *orc*))
    (:slime
     (make-instance 'slime :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			   :kind *slime*))
    (:brigand
     (make-instance 'brigand :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			     :kind *brigand*))
    (:bubble
     (make-instance 'bubble :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
			    :kind *bubble*))))


;;マップ上のモンスターシンボル作成
(defun create-monsters (donjon)
  (with-slots (yuka-list monsters floor-num) donjon
    (setf monsters nil)
    (let ((rate (cond
		  ((>= 10 floor-num 1) *symbol1-10*))))
      (loop :repeat (+ 3 (random (+ 2 (floor floor-num 2))))
            :do (let ((yuka (nth (random (length yuka-list)) yuka-list))
		      (kind (weightpick rate)))
                  (push (create-map-monster (car yuka) (cadr yuka) kind) monsters)
                  (setf yuka-list (remove yuka yuka-list :test #'equal)))))))


;;バトルモンスターの出現率
(defun get-battle-monster-rate ()
  (with-slots (collide-monster) *p*
    (let ((kind (monster/kind collide-monster)))
      (cond
	((= kind *slime*) '((:slime . 100) (:orc . 70) (:brigand . 50) (:bubble . 20)))
	((= kind *orc*) '((:slime . 100) (:orc . 170) (:brigand . 80) (:bubble . 80)))
	((= kind *brigand*) '((:slime . 50) (:orc . 70) (:brigand . 150) (:bubble . 80) (:hydra . 20)))
	((= kind *bubble*) '((:slime . 30) (:orc . 50) (:brigand . 70) (:bubble . 120) (:hydra . 60) (:skelton . 30)))))))

;;slime
(defun create-slime (x y lv)
  (let ((hp (+ 5 (floor (random lv) 3))))
    (make-instance 'slime :kind *slime* :drawx x :drawy y :lv lv
			  :hp hp :maxhp hp
			  :str (+ 1 (floor (random lv) 3))
			  :agi (+ 1 (floor (random lv) 4)))))

;;orc
(defun create-orc (x y lv)
  (let ((hp (+ 10 (random lv))))
    (make-instance 'orc :kind *orc* :drawx x :drawy y :lv lv
			:hp hp :maxhp hp
			:str (+ 5 (floor (random lv) 2))
			:agi (+ 3 (floor (random lv) 5)))))
;;brigand
(defun create-brigand (x y lv)
  (let ((hp (+ 7 (floor (random lv) 2))))
    (make-instance 'brigand :kind *brigand* :drawx x :drawy y :lv lv
			    :hp hp :maxhp hp
			    :str (+ 4 (floor (random lv) 2))
			    :agi (+ 4 (floor (random lv) 2)))))
;;bubble
(defun create-bubble (x y lv)
  (let ((hp (+ 5 (floor (random lv) 3))))
    (make-instance 'bubble :kind *bubble* :drawx x :drawy y :lv lv
			   :hp hp :maxhp hp
			   :str (+ 4 (floor (random lv) 3))
			   :agi (+ 4 (floor (random lv) 3)))))
;;hydra
(defun create-hydra (x y lv)
  (let ((hp (+ 20 (floor (random (* lv 1.5))))))
    (make-instance 'hydra :kind *hydra* :drawx x :drawy y :lv lv
			  :hp hp :maxhp hp
			  :str (+ 9 (floor (random lv)))
			  :agi (+ 3 (floor (random lv) 4)))))
;;skelton
(defun create-skelton (x y lv)
  (let ((hp (+ 15 (floor (random (* lv 1.2))))))
    (make-instance 'skelton :kind *skelton* :drawx x :drawy y :lv lv
			    :hp hp :maxhp hp
			    :str (+ 10 (floor (random (* lv 1.1))))
			    :agi (+ 5 (floor (random lv))))))
;;
(defun create-battle-monster (kind x y floor-num)
  (with-slots (lv) *p*
    (let ((monster-lv (if (>= (random 3) 1)
			  (max 1 (- lv (random 4) (floor floor-num 5)))
			  (+ lv (random 4) (floor floor-num 4)))))
      (case kind
	(:slime (create-slime x y monster-lv))
	(:orc (create-orc x y monster-lv))
	(:brigand (create-brigand x y monster-lv))
	(:bubble (create-bubble x y monster-lv))
	(:hydra (create-hydra x y monster-lv))
	(:skelton (create-skelton x y monster-lv))))))


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
