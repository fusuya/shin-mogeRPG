(in-package :casket2022aki)



(defun set-font ()
  (setf *font24* (gk:make-font :mplus 24)
        *font28* (gk:make-font :mplus 28)
        *font32* (gk:make-font :mplus 32)
        *font64* (gk:make-font :mplus 64)
        *font128* (gk:make-font :mplus 128)
        *font300* (gk:make-font :mplus 300)))


(gk:defgame shin-mogeRPG () ()
  (:viewport-width *window-w*)
  (:viewport-height *window-h*))


(defun init-keystate ()
  (with-slots (keya right left up down) *keystate*
    (setf keya nil right nil left nil up nil down nil)))

(defun input-bind ()
  (gk:bind-button :a :pressed
                  (lambda () (setf (keystate/keya *keystate*) t)))
  (gk:bind-button :a :released
                  (lambda () (setf (keystate/keya *keystate*) nil)))
  (gk:bind-button :right :pressed
                  (lambda () (setf (keystate/right *keystate*) t)))
  (gk:bind-button :right :released
                  (lambda () (setf (keystate/right *keystate*) nil)))
  (gk:bind-button :left :pressed
                  (lambda () (setf (keystate/left *keystate*) t)))
  (gk:bind-button :left :released
                  (lambda () (setf (keystate/left *keystate*) nil)))
  (gk:bind-button :up :pressed
                  (lambda () (setf (keystate/up *keystate*) t)))
  (gk:bind-button :up :released
                  (lambda () (setf (keystate/up *keystate*) nil)))
  (gk:bind-button :down :pressed
                  (lambda () (setf (keystate/down *keystate*) t)))
  (gk:bind-button :down :released
                  (lambda () (setf (keystate/down *keystate*) nil))))


(defun create-donjon-stage ()
  (let ((stage (make-array (list *donjon-h* *donjon-w*) :initial-element 6)))
    stage))

(defun create-donjon ()
  (setf *donjon* (make-instance 'donjon :stage (create-donjon-stage))))

(defun create-donjon-test ()
  (setf *grid* (make-grid-array)))

(defmethod gk:post-initialize ((app shin-mogeRPG))
  (setf *random-state* (make-random-state t)
        *player* (make-instance 'player :posx 1 :posy 1 :drawx 32 :drawy 32))
  (set-font)
  (create-donjon-test)
  (setf *keystate* (make-instance 'keystate))
  (input-bind))


;; y : up 64 down 96 left 32 right 0
(defun draw-player ()
  (with-slots (posx posy dir drawx drawy) *player*
    (let ((dir 0)
          (w 24) (h 32)
          (scale 1.3))
      (bg:draw-image (gk:vec2 drawx drawy) (* w scale) (* h scale) (gk::resource-by-id :ido-anime)
                                         :translate-x 0
                                         :translate-y (- (* dir scale))
                                         :scale-x scale :scale-y scale))))


(defun draw-donjon ()
  (with-slots (stage) *donjon*
    (loop :for y :from 0 :below *donjon-h*
          :do (loop :for x :from 0 :below *donjon-w*
                    :do
                    (let ((obj (aref stage y x))
                          (scale 1.2))
                      (bg:draw-image (gk:vec2 (* x *obj-w* scale) (* y *obj-h* scale))
                                     (* *obj-w* scale) (* *obj-h* scale) (gk::resource-by-id :objs)
                                     :translate-x (- (* obj *obj-w* scale))
                                     :scale-x scale :scale-y scale))))))

(defun draw-donjon-test ()
  (loop :for y :from 0 :below *donjon-h*
        :do (loop :for x :from 0 :below *donjon-w*
                  :do
                  (let ((obj (getf (aref *grid* y x) :cell))
                        (hoge (getf (aref *grid* y x) :test))
                        (scale 1.0))
                    (bg:draw-image (gk:vec2 (* x *obj-w* scale) (* y *obj-h* scale))
                                   (* *obj-w* scale) (* *obj-h* scale) (gk::resource-by-id :objs)
                                   :translate-x (- (* obj *obj-w* scale))
                                   :scale-x scale :scale-y scale)
                    (when (= hoge 1)
                      (bg:draw-image (gk:vec2 (* x *obj-w* scale) (* y *obj-h* scale))
                                     (* *obj-w* scale) (* *obj-h* scale) (gk::resource-by-id :red-stone)
                                     :translate-x (- (* obj *obj-w* scale))
                                     :scale-x scale :scale-y scale))))))

(defparameter *test* nil)


(defun walk-animation ()
  (with-slots (posx posy drawx drawy dir walk-num walk-flag) *player*
    (cond
      ((= dir *right*) (incf drawx 2))
      ((= dir *left*) (decf drawx 2))
      ((= dir *up*)   (incf drawy 2))
      ((= dir *down*) (decf drawy 2)))
    (incf walk-num)
    (print "hoge")
    (when (>= walk-num 16)
      (setf walk-flag nil
            walk-num 0))))



(defun update-player ()
  (with-slots (posx posy dir walk-flag) *player*
    (if walk-flag
        (walk-animation)
        (cond
          ((keystate/right *keystate*) (setf dir 0 walk-flag t))
          ((keystate/left *keystate*) (setf dir 32 walk-flag t))
          ((keystate/up *keystate*) (setf dir 64 walk-flag t))
          ((keystate/down *keystate*) (setf dir 96 walk-flag t))))))

(defmethod gk:draw ((app shin-mogeRPG))
  (if (keystate/right *keystate*)
      (gk:draw-text "右 key" (gk:vec2 300 300) :font *font64*)
      (gk:draw-text "nil" (gk:vec2 300 300)))
  (draw-donjon-test)
  (draw-player)
  (when *test*
    (gk:draw-text "オワリ" (gk:vec2 400 400) :font *font128*)))

(defmethod gk:act ((app shin-mogerpg))
  (update-player)
  (when (keystate/keya *keystate*)
    (mogeskal)
    (when (end-kraskal?)
      (find-dead-end)
      (setf *test* t)))
  (init-keystate))

(defun run ()
  (gk:start 'shin-mogeRPG :viewport-resizable t :autoscaled t))
