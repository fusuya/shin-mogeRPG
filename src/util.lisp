(in-package :casket2022aki)


(defun randval (n)
  (1+ (random (max 1 n))))

;;’Š‘I-----------------------------------------------
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;lst = *copy-buki*
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))
;;------------------------------------------------------------



;;ŽžŠÔ•ÏŠ·
(defun get-hms (n)
  (multiple-value-bind (h m1) (floor n 3600000000)
    (multiple-value-bind (m s1) (floor m1 60000000)
      (multiple-value-bind (s ms1) (floor s1 1000000)
       (multiple-value-bind (ms) (floor ms1 10000)
          (values h m s ms))))))


;;class copy
(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))
