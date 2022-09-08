(in-package :casket2022aki)



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


(defun render-game (hwnd hdc)
  (render-donjon hwnd hdc))