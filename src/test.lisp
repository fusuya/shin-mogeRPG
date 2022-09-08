(defun hoge ()
  (let ((moge 0)
        (kome 2))
    (loop :for i :from 0 :to 8
          :do (incf moge)
              (print (+ moge kome i)))))

(defparameter hoge 2)

(defun hoge1 ())