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
(defun create-map-monster (x y)
  (make-instance 'monster :posx x :posy y :drawx (* x *cell-size*) :drawy (* y *cell-size*)
                 :kind *orc*))



(defun create-monsters (donjon)
  (with-slots (yuka-list monsters) donjon
    (loop :repeat 5
          :do (let ((yuka (nth (random (length yuka-list)) yuka-list)))
                (push (create-map-monster (car yuka) (cadr yuka)) monsters)
                (setf yuka-list (remove yuka yuka-list :test #'equal))))))

;;バトル用敵生成
(defun create-battle-monster (x y)
  (make-instance 'orc :drawx x :drawy y :hp 10 :maxhp 10 :lv 1 :str 5
                 :kind *orc*))

(Defun create-battle-monsters (donjon)
  (with-slots (battle-monsters) donjon
    (let* ((monster-num 12) ;;(1+ (random 12)))
           (monsters-pos (nth monster-num *battle-monster-pos*)))
        (loop :repeat monster-num
              :for pos :in monsters-pos
              :do (let ((monster (create-battle-monster (car pos) (cadr pos))))
                    (push monster battle-monsters))))))
