(in-package :casket2022aki)

(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
             collect `(defparameter ,name ,i))))


(defmacro with-double-buffering-2 ((var hwnd) &body body)
  "Evaluate body in a WITH-PAINT context where VAR is bound to an in-memory HDC
which is blitted onto the hwnd's DC as the final step. This prevents flickering
when drawing lots of small items on the screen."
  (alexandria:with-gensyms (gbm gold gwidth gheight ghdc gps)
  ;; (let ((gbm (gensym)) (gold (gensym)) (gwidth (gensym))
  ;; 	(gheight (gensym)) (ghdc (gensym)) (gps (gensym)))
    `(with-paint (,hwnd ,ghdc ,gps)
       (let ((,gwidth (rect-right *c-rect*))
           ;;(paintstruct-paint ,gps)))
             (,gheight (rect-bottom *c-rect*)))
      ;;(paintstruct-paint ,gps))))
         (with-compatible-dc (,var ,ghdc)
           (let* ((,gbm (create-compatible-bitmap ,ghdc ,gwidth ,gheight))
                  (,gold (select-object ,var ,gbm)))
             (unwind-protect (progn ,@body)
               (stretch-blt ,ghdc 0 0 ,var 0 0
                           :width-dest *change-window-w*
                           :height-dest *change-window-h*
                           :width-source (rect-right *c-rect*)
                           :height-source (rect-bottom *c-rect*)
                           :raster-op :srccopy)
               ;; (transparent-blt ,ghdc 0 0 ,var 0 0
               ;; 			:width-dest *change-screen-w*
               ;; 			:height-dest *change-screen-h*
               ;; 			:width-source (rect-right *c-rect*)
               ;; 			:height-source (rect-bottom *c-rect*)
               ;; 			:transparent-color (encode-rgb 0 255 0))

               (select-object ,var ,gold)
               (delete-object ,gbm))))))))

(defparameter *data-root* (asdf:system-source-directory 'casket2022aki))
(defparameter *img-root* (merge-pathnames "assets/img/" *data-root*))
(defparameter *sound-root* (merge-pathnames "assets/sound/" *data-root*))
(defparameter *save-root* (merge-pathnames "assets/save/" *data-root*))

(defparameter *objs-img* nil)
(defparameter *p-walk-img* nil)
(defparameter *p-atk-img* nil)
(defparameter *monsters-img* nil)
(defparameter *waku-img* nil)
(defparameter *waku-black* nil)
(defparameter *slash-img* nil)
(defparameter *ankoku-img* nil)

(defun load-img (path)
  (load-image (namestring (merge-pathnames path *img-root*))
              :type :bitmap
              :flags '(:load-from-file :create-dib-section)))


(defun load-images ()
  (setf *objs-img* (load-img "objs-img2.bmp")
	*waku-black* (load-img "waku-black.bmp")
        *p-walk-img* (load-img "p-ido-anime.bmp")
        *monsters-img* (load-img "monsters2.bmp")
        *waku-img* (load-img "waku.bmp")
        *p-atk-img* (load-img "p-atk-anime.bmp")
        *slash-img* (load-img "slash-img.bmp")
        *ankoku-img* (load-img "ankoku-img.bmp")))

(defun delete-imgs ()
  (delete-object *objs-img*)
  (delete-object *waku-img*)
  (delete-object *p-walk-img*)
  (delete-object *monsters-img*)
  (delete-object *waku-img*)
  (delete-object *p-atk-img*)
  (delete-object *slash-img*)
  (delete-object *ankoku-img*))


(defparameter *brush* nil)
;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 8 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 160))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 1 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))
                                (create-solid-brush (encode-rgb 255 255 255))))))


(my-enum +purple+ +red+ +green+ +blue+ +yellow+ +cyan+ +pink+ +white+)
(my-enum +slash+ +double-slash+ +swing+ +chopped+ +assalut+ +ankoku+ +potion+)

(defun delete-object-array (arr)
  (loop for i across arr
     do (delete-object i)))

(defun delete-brush ()
  (delete-object-array *brush*))
;;font----------------------------------------------------------
(defparameter *font140* nil)
(defparameter *font90* nil)
(defparameter *font70* nil)
(defparameter *font40* nil)
(defparameter *font30* nil)
(defparameter *font20* nil)
(defparameter *font2* nil)

;;(defparameter *font* (directory-namestring (merge-pathnames *font-root* "mplus-1mn-regular.ttf")))



(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
        *font90* (create-font "MSゴシック" :height 90)
        *font70* (create-font "MSゴシック" :height 70)
        *font40* (create-font "MSゴシック" :height 40)
        *font30* (create-font "MSゴシック" :height 30)
        *font20* (create-font "MSゴシック" :height 25)
        *font2* (create-font "MSゴシック" :height 15))) ;; :width 12 :weight (const +fw-bold+))))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font90*)
  (delete-object *font70*)
  (delete-object *font40*)
  (delete-object *font30*)
  (delete-object *font20*)
  (delete-object *font2*))
;;font----------------------------------------------------------


(defparameter *right* 96)
(defparameter *left* 64)
(defparameter *up*   32)
(defparameter *down* 0)
(defparameter *guard* 128)
(defparameter *heal* 160)

(defparameter *cell-size* 32)

(defparameter *w-cell-num* 25)
(defparameter *h-cell-num* 19)

;;ダンジョン領域
(defparameter *donjon-w* (* *w-cell-num* *cell-size*))
(defparameter *donjon-h* (* *h-cell-num* *cell-size*))
;;プレイヤーのステータス表示用領域サイズ
(defparameter *status-w* 180)
(defparameter *status-h* 160)


(defparameter *window-w* (+ *donjon-w* *status-w*))
(defparameter *window-h* (+ *donjon-h* *status-h*))

(defparameter *change-window-w* *window-w*)
(defparameter *change-window-h* *window-h*)

(defconstant +hard-block+ 3)
(defconstant +soft-block+ 6)
(defconstant +yuka+ 7)
(defconstant +chest+ 13)
(defconstant +kaidan+ 14)


(defparameter *c-rect* nil) ;;クライアント領域

(defparameter *keystate* nil)
(defparameter *donjon* nil)
(defparameter *game* nil)
(Defparameter *p* nil)

(defparameter *cell-scale* 1.0)
(defparameter *player-scale* 1.3)
(defparameter *battle-pos* '(800 280))

(defparameter *brigand* 0)
(defparameter *dragon* 32)
(defparameter *hydra* 64)
(defparameter *metal* 96)
(defparameter *orc* 128)
(defparameter *slime* 160)
(defparameter *buble* 192)
(defparameter *skelton* 224)

;;--------------------------------------------------------------
(defclass cell ()
  ((cell :initarg :cell :initform 0 :accessor cell/cell)
   (num :initarg :num :initform 0 :accessor cell/num)
   (item :initarg :item :initform nil :accessor cell/item)
   (dead-end :initarg :dead-end :initform nil :accessor cell/dead-end)))

(defclass keystate ()
  ((right :initarg :right :initform nil :accessor keystate/right)
   (left  :initarg :left  :initform nil :accessor keystate/left)
   (up    :initarg :up    :initform nil :accessor keystate/up)
   (down  :initarg :down  :initform nil :accessor keystate/down)
   (keya  :initarg :keya  :initform nil :accessor keystate/keya)
   (keyz  :initarg :keyz  :initform nil :accessor keystate/keyz)
   (keyx  :initarg :keyx  :initform nil :accessor keystate/keyx)
   (keyc  :initarg :keyc  :initform nil :accessor keystate/keyc)))

(defclass obj ()
  ((posx :initarg :posx :initform 0 :accessor obj/posx)
   (posy :initarg :posy :initform 0 :accessor obj/posy)
   (drawx :initarg :drawx :initform 0 :accessor obj/drawx)
   (drawy :initarg :drawy :initform 0 :accessor obj/drawy)
   (img :initarg :img :initform nil :accessor obj/img)))

(defclass damage (obj)
  ((num :initarg :num :initform 0 :accessor damage/num)
   (timer :initarg :timer :initform 0 :accessor damage/timer)
   (tempy :initarg :tempy :initform 0 :accessor damage/tempy)
   (topy :initarg :topy :initform 0 :accessor damage/topy)
   (fall :initarg :fall :initform 0 :accessor damage/temp)
   (color :initarg :color :initform nil :accessor damage/color)
   (dir  :initarg :dir :initform nil :accessor damage/dir)
   (dx :initarg :dx :initform 0 :accessor damage/dx)
   (dy :initarg :dy :initform 0 :accessor damage/dy)
   (topflag :initarg :topflag :initform nil :accessor damage/topflag)))

(defclass chara (obj)
  ((dir  :initarg :dir  :initform 0 :accessor chara/dir)
   (walk-flag :initarg :walk-flag :initform nil :accessor chara/walk-flag)
   (walk-img :initarg :walk-img :initform 0 :accessor chara/walk-img)
   (walk-num :initarg :walk-num :initform 0 :accessor chara/walk-num)
   (lv :initarg :lv :initform 0 :accessor :chara/lv)
   (damage :initarg :damage :initform nil :accessor chara/damage)
   (heal-data :initarg :heal-data :initform nil :accessor chara/heal-data)
   (hp :initarg :hp :initform 0 :accessor chara/hp)
   (maxhp :initarg :maxhp :initform 0 :accessor chara/maxhp)
   (str :initarg :str :initform 0 :accessor chara/str)
   (maxstr :initarg :maxstr :initform 0 :accessor chara/maxstr)
   (agi :initarg :agi :initform 0 :accessor chara/agi)
   (maxagi :initarg :maxagi :initform 0 :accessor chara/maxagi)))



(defclass player (chara)
  ((skill :initarg :skill :initform nil :accessor player/skill)
   (weapon :initarg :weapon :initform 0 :accessor weapon/weapon)
   (skill-num :initarg :skill-num :initform 1 :accessor player/skill-num)
   (selected-skill :initarg :selected-skill :initform 0 :accessor player/selected-skill)
   (selected-enemy :initarg :selected-enmey :initform 0 :accessor player/selected-enemy)
   (explore-state :initarg :explore-state :initform nil :accessor player/explore-state)
   (battle-state :initarg :battle-state :initform nil :accessor player/battle-state)
   (attack-num :initarg :attack-num :initform 0 :accessor player/attack-num)
   (chopped-num :initarg :chopped-num :initform 0 :accessor player/chopped-num)
   (d-atk :initarg :d-atk :initform 0 :accessor player/d-atk)
   (assalut-flag :initarg :assalut-flag :initform nil :accessor player/assalut-flag)
   (ankoku-img :initarg :ankoku-img :initform 0 :accessor player/ankoku-img)
   (collide-monster :initarg :collide-monster :initform nil :accessor player/collide-monster)
   (get-item :initarg :get-item :initform nil :accessor player/get-item)
   (cursor :initarg :cursor :initform 0 :accessor player/cursor)
   (potion :initarg :potion :initform 0 :accessor player/potion)
   (hammer :initarg :hammer :initform 0 :accessor player/potion)
   (end-animation :initarg :end-animation :initform nil :accessor player/end-animation)
   (tempdrawx :initarg :tempdrawx :initform 0 :accessor player/tempdrawx)
   (tempdrawy :initarg :tempdrawy :initform 0 :accessor player/tempdrawy)))

(defclass monster (chara)
  ((kind :initarg :kind :initform 0 :accessor monster/kind)))

(defclass orc (monster)
  ())

(defclass skill ()
  ((name :initarg :name :initform nil :accessor skill/name)
   (power :initarg :power :initform 0 :accessor skill/power)
   (accuracy :initarg :accuracy :initform 0 :accessor skill/accuracy)
   (learn-lv :initarg :learn-lv :initform 0 :accessor skill/learn-lv)))


(defclass donjon ()
  ((stage :initarg :stage :initform nil :accessor donjon/stage)
   (yuka-list :initarg :yuka-list :initform nil :accessor donjon/yuka-list)
   (monsters :initarg :monsters :initform nil :accessor donjon/monsters)
   (battle-monsters :initarg :battle-mosnters :initform nil :accessor donjon/battle-monsters)))

(defclass game ()
  ((donjon :initarg :donjon :initform nil :accessor game/donjon)
   (state :initarg :state :initform :title :accessor game/state)
   (item-list :initarg :item-list :initform nil :accessor game/item-list)
   (battle-state :initarg :battle-state :initform :player-turn :accessor game/battle-state)))

(defclass weapon ()
  ((power :initarg :power :initform 0 :accessor weapon/power)
   (incHP :initarg :incHP :initform 0 :accessor weapon/incHP)
   (incagi :initarg :incagi :initform 0 :accessor weapon/incagi)
   (name :initarg :name :initform 0 :accessor weapon/name)
   (diffhp :initarg :diffhp :initform nil :accessor weapon/diffhp)
   (diffagi :initarg :diffagi :initform nil :accessor weapon/diffagi)
   (diffpower :initarg :diffpower :initform nil :accessor weapon/diffpower)))


;;時間変換
(defun get-hms (n)
  (multiple-value-bind (h m1) (floor n 3600000)
    (multiple-value-bind (m s1) (floor m1 60000)
      (multiple-value-bind (s ms1) (floor s1 1000)
       (multiple-value-bind (ms) (floor ms1 10)
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