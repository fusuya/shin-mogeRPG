(in-package :casket2022aki)


(defparameter *edges* nil)
(defparameter *grid* (make-array (list *h-cell-num* *w-cell-num*)))

(defparameter *laststage*
  (make-array (list *h-cell-num* *w-cell-num*)
	      :initial-contents
	      '((3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
		(3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 3)
		(3 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 3)
		(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))))

(defun make-last-stage-cell ()
  (let ((arr (make-array (list *h-cell-num* *w-cell-num*))))
    (loop :for y :from 0 :below *h-cell-num*
          :do (loop :for x :from 0 :below *w-cell-num*
                    :do (let ((cell (aref *laststage* y x)))
                          (setf (aref arr y x) (make-instance 'cell :cell cell
								    :posx x :posy y
								    :drawx (* x *cell-size*)
								    :drawy (* y *cell-size*))))))
    arr))

(defun random-dir ()
  (case (random 4)
    (0 '(0 1))
    (1 '(0 -1))
    (2 '(1 0))
    (3 '(-1 0))))

(defun make-grid-array ()
  (let ((arr (make-array (list *h-cell-num* *w-cell-num*)))
        (num 0))
    (loop :for y :from 0 :below *h-cell-num*
          :do (loop :for x :from 0 :below *w-cell-num*
                    :do (let ((cell (cond
                                      ((or (= y 0) (= x 0)
                                           (= x (1- *w-cell-num*))
                                           (= y (1- *h-cell-num*)))
                                       +hard-block+)
                                      ((and (= (mod y 2) 1) (= (mod x 2) 1))
                                       +yuka+)
                                      (t
                                        +soft-block+))))
                          (setf (aref arr y x) (make-instance 'cell :num num :cell cell
								    :posx x :posy y
								    :drawx (* x *cell-size*)
								    :drawy (* y *cell-size*)))
                          (incf num))))
    arr))


(defun getnum (x y stage)
  (cell/num (aref stage y x)))

(defun getcell (x y stage)
  (cell/cell (aref stage y x)))

(defun setnum (x y newnum stage)
  (setf (cell/num (aref stage y x)) newnum))

(defun yuka-p (x y stage)
  (= (cell/cell (aref stage y x)) +yuka+))

(defun set-yuka (x y stage)
  (with-slots (cell) (aref stage y x)
    (when (and (> x 0)
               (> (1- *w-cell-num*) x)
               (> y 0)
               (> (1- *h-cell-num*) y))
      (setf cell +yuka+)
      (push (list x y) (donjon/yuka-list (game/donjon *game*))))))

(defun update-num (num1 num2 stage)
  (let ((big 0)
        (small 0))
    (if (> num1 num2)
        (setf big num1
              small num2)
        (setf big num2
              small num1))
    (loop :for y :from 0 :below *h-cell-num*
          :do (loop :for x :from 0 :below *w-cell-num*
                    :do (if (= (getnum x y stage) small)
                            (setnum x y big stage))))
    big))


;;迷路生成完了チェック
(defun end-mogeskal? (stage)
  (let ((num 99999))
    (loop :for y :from 0 :below *h-cell-num*
          :do (loop :for x :from 0 :below *w-cell-num*
                    :do (when (= (getcell x y stage) +yuka+)
                          (cond
                            ((= 99999 num)
                             (setf num (getnum x y stage)))
                            ((/= num (getnum x y stage))
                             (return-from end-mogeskal? nil))))))
    t))

;;四方向に壁がないかチェック
(defun four-dir-wall (x y stage)
  (let ((wall 0)
        (right (getcell (1+ x) y stage))
        (left (getcell (1- x) y stage))
        (up (getcell x (1- y) stage))
        (down (getcell x (1+ y) stage)))
    (when (or (= up +soft-block+)
              (= up +hard-block+))
      (incf wall))
    (when (or (= down +soft-block+)
              (= down +hard-block+))
      (incf wall))
    (when (or (= right +soft-block+)
              (= right +hard-block+))
      (incf wall))
    (when (or (= left +soft-block+)
              (= left +hard-block+))
      (incf wall))
    wall))

;;行き止まりの座標リスト返す
(defun find-dead-end (stage)
  (loop :for y :from 0 :below *h-cell-num*
        :append (loop :for x :from 0 :below *w-cell-num*
                      :when (and (= (getcell x y stage) +yuka+)
                                 (>= (four-dir-wall x y stage) 3))
                      :collect (list x y))))

      

;;ステージ生成
(defun mogeskal (stage)
  (let* ((randx (1+ (* (random (floor (/ *w-cell-num* 2))) 2)))
         (randy (1+ (* (random (floor (/ *h-cell-num* 2))) 2)))
         (dir (random-dir))
         (wallx (+ randx (car dir)))
         (wally (+ randy (cadr dir)))
         (nextx (+ wallx (car dir)))
         (nexty (+ wally (cadr dir))))
    (loop :while (or (> 0 nextx)
                     (> nextx (1- *w-cell-num*))
                     (> 0 nexty)
                     (> nexty (1- *h-cell-num*)))
          :do (setf randx (1+ (* (random (floor (/ *w-cell-num* 2))) 2))
                    randy (1+ (* (random (floor (/ *h-cell-num* 2))) 2))
                    dir (random-dir)
                    wallx (+ randx (car dir))
                    wally (+ randy (cadr dir))
                    nextx (+ wallx (car dir))
                    nexty (+ wally (cadr dir))))
    (let* ((num1 (getnum randx randy stage))
           (num2 (getnum nextx nexty stage)))
      (cond
        ((and (not (yuka-p wallx wally stage))
              (/= num1 num2)
          (let ((newnum (update-num num1 num2 stage)))
            (set-yuka wallx wally stage)
            (setnum wallx wally newnum stage))))
        (t
         (mogeskal stage))))))


;;プレイヤーの初期位置セット
(defun set-player-init-pos (pos)
  (with-slots (posx posy drawx drawy) *p*
    (setf posx (car pos)
          posy (cadr pos)
          drawx (* posx 32)
          drawy (* posy 32))))

;;cellになんか置く
(defun set-obj-cell (pos item-num stage)
  (with-slots (item) (aref stage (cadr pos) (car pos))
    (setf item item-num)))

;;オブジェクト設置
(defun set-object-donjon (stage)
  (let* ((pos-list (find-dead-end stage))
         (set-pos (nth (random (length pos-list)) pos-list)))
    (set-player-init-pos set-pos)
    (setf pos-list (remove set-pos pos-list :test #'equal))
    ;;階段設置
    (setf set-pos (nth (random (length pos-list)) pos-list))
    (set-obj-cell set-pos +kaidan+ stage)
    (setf pos-list (remove set-pos pos-list :test #'equal))
    ;;宝箱設置
    (loop :repeat (min 10 (+ 3 (random (- (length pos-list) 3))))
	  :do (setf set-pos (nth (random (length pos-list)) pos-list))
	      (set-obj-cell set-pos +chest+ stage)
	      (setf pos-list (remove set-pos pos-list :test #'equal)))))

;;ダンジョン生成
(defun create-maze (donjon)
  (with-slots (stage yuka-list floor-num) donjon
    (cond
      ((= floor-num 50)
       (setf stage (make-last-stage-cell)
	     (chara/dir *p*) *up*)
       (set-player-init-pos '(12 13)))
      (t
       (setf stage (make-grid-array)
	     yuka-list nil)
       (loop :until (end-mogeskal? stage)
             :do (mogeskal stage))
       (set-object-donjon stage)))))
