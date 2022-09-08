(in-package :casket2022aki)

(defun set-player-skill ()
  (list
   (make-instance 'skill :name "斬る" :power 10 :accuracy 90 :learn-lv 1)
   (make-instance 'skill :name "二段斬り" :power 8 :accuracy 85 :learn-lv 3)
   (make-instance 'skill :name "なぎ払い" :power 3 :accuracy 70 :learn-lv 5)
   (make-instance 'skill :name "乱れ切り" :power 6 :accuracy 80 :learn-lv 7)
   (make-instance 'skill :name "突撃" :power 20 :accuracy 50 :learn-lv 9)
   (make-instance 'skill :name "暗黒" :power 10 :accuracy 80 :learn-lv 11)))

(defun init-game ()
  (setf *game* (make-instance 'game :donjon (make-instance 'donjon) :state :explore
			      :item-list (copy-tree *buki-d*))
        *p* (make-instance 'player :drawx 800 :drawy 280 :posx 1 :posy 1 :hp 20 :maxhp 30
                                   :maxagi 30 :agi 30 :str 30 :tempdrawx 800 :tempdrawy 280
                                   :battle-state :action-select 
                                   :explore-state :player-move
                                   :potion 3
				   :weapon (make-instance 'weapon :name "素手" :power 0 :inchp 0 :incagi 0)
                                   :skill (set-player-skill))
        *keystate* (make-instance 'keystate))
  (load-images)
  (create-maze (game/donjon *game*))
  (create-monsters (game/donjon *game*)))
  ;;(create-battle-monsters (game/donjon *game*)))

(defun init-keystate ()
  (with-slots (keya right left up down keyz) *keystate*
    (setf keya nil right nil left nil up nil down nil keyz nil keya nil)))

;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (with-slots (left right down up keyz keyx keyc keya) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:left (setf left t))
        (:right (setf right t))
        (:down (setf down t))
        (:up (setf up t))
        (:keyz (setf keyz t))
        (:keyx (setf keyx t))
        (:keyc (setf keyc t))
        (:keya (setf keya t))
        (:keyq ;; quit
          (send-message hwnd (const +wm-close+) nil nil))))))

;;キー話したとき
(defun moge-keyup (wparam)
  (with-slots (left right down up keyz keyx keyc keya) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:left (setf left nil))
        (:right (setf right nil))
        (:down (setf down nil))
        (:up (setf up nil))
        (:keya (setf keya nil))
        (:keyx (setf keyx nil))
        (:keyc (setf keyc nil))
        (:keyz (setf keyz nil))))))



(defparameter *test* nil)

;;重み付け抽選-----------------------------------------------
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

;;----------------------explore--------------------------------------


;;接触した敵を消す
(defun delete-collide-monster (donjon)
  (with-slots (monsters) donjon
    (setf monsters (remove (player/collide-monster *p*) monsters :test #'equal))))



;;敵シンボルと接触判定 当たった敵を返す
(defun collide-monster? (p monster)
  (and (= (obj/posx p) (obj/posx monster))
       (= (obj/posy p) (obj/posy monster))))

;;敵全員と当たり判定
(defun collide-monsters? (donjon)
  (with-slots (monsters) donjon
    (loop :for monster :in monsters
          :do (when (collide-monster? *p* monster)
                (setf (player/collide-monster *p*) monster)
                      ;;monsters (remove monster monsters :test #'equal))
                (return)))))

;;歩行画像更新
(defun update-walk-animation (walker)
  (with-slots (walk-img walk-num walk-flag) walker
    (let ((tmp (if walk-flag 3 9)))
      (incf walk-num)
      (when (zerop (mod walk-num tmp))
        (incf walk-img)
        (when (>= walk-img 4)
          (setf walk-img 0
                walk-num 0))))))

;;次の移動先座標ゲット
(defun get-next-pos (next-dir mover)
  (with-slots (posx posy) mover
    (case next-dir
      (:wait  (list posx posy))
      (:right (list (1+ posx) posy))
      (:left  (list (1- posx) posy))
      (:up    (list posx (1- posy)))
      (:down  (list posx (1+ posy))))))

;;移動先の地形ゲット
(defun get-next-cell (next-pos)
  (with-slots (stage) (game/donjon *game*)
    (aref stage (cadr next-pos) (car next-pos))))


;;移動先が床だったらいける
(defun can-move? (next-dir mover)
  (let* ((next-pos (get-next-pos next-dir mover))
         (next-cell (get-next-cell next-pos)))
    (= (cell/cell next-cell) +yuka+)))


;;プレイヤーの方向更新
(defun update-player-dir ()
  (with-slots (right left up down) *keystate*
    (with-slots (posx posy dir walk-flag) *p*
      (cond
        (right
          (setf dir *right*)
          (when (can-move? :right *p*)
            (setf walk-flag t)
            (incf posx)))
        (left
          (setf dir *left*)
          (when (can-move? :left *p*)
            (setf walk-flag t)
            (decf posx)))
        (up
          (setf dir *up*)
          (when (can-move? :up *p*)
            (setf walk-flag t)
            (decf posy)))
        (down
          (setf dir *down*)
          (when (can-move? :down *p*)
            (setf walk-flag t)
            (incf posy)))))))

;;歩行関係初期化
(defun init-walk (mover)
  (with-slots (walk-flag walk-num drawx drawy posx posy) mover
    (setf walk-flag nil
          walk-num 0
          walk-img 0
          drawx (* posx *cell-size*)
          drawy (* posy *cell-size*))))


;;バトル入る前にpos記憶
(defun save-player-pos ()
  (with-slots (drawx drawy tempdrawy tempdrawx) *p*
    (setf tempdrawx drawx
          tempdrawy drawy)))

;;バトル用の描画座標セット
(defun set-battle-pos ()
  (with-slots (drawx drawy) *p*
    (setf drawx (car *battle-pos*)
          drawy (cadr *battle-pos*))))

;;次の移動先まで行ったか
(defun reach-next-pos? (mover)
  (with-slots (dir posx posy drawx drawy) mover
    (or (and (or (= dir *right*) (= dir *down*))
             (<= (* posx *cell-size*) drawx) 
             (<= (* posy *cell-size*) drawy))
        (and (or (= dir *left*) (= dir *up*))
             (>= (* posx *cell-size*) drawx)
             (>= (* posy *cell-size*) drawy)))))

;;プレイヤーの描画座標更新
(defun update-position (mover)
  (with-slots (drawx drawy dir) mover
    (let ((d 3))
      (cond
        ((= dir *right*)
         (incf drawx d))
        ((= dir *left*)
         (decf drawx d))
        ((= dir *up*)
         (decf drawy d))
        ((= dir *down*)
         (incf drawy d))))))

;;アイテムゲット
(defun get-item (p)
  (with-slots (weapon get-item explore-state) p
    (let* ((item (weightpick (game/item-list *game*)))
	   (name (car item))
	   (power (cadr item))
	   (sin-power (if (>= (random 3) 1)
			  (+ power (random (max 1 (floor power 3))))
			  (- power (random (max 1 (floor power 4))))))
	   (inchp (random power))
	   (incagi (random (max 1 (floor power 5))))
	   (diffpower (- sin-power (weapon/power weapon)))
	   (diffhp (- inchp (weapon/inchp weapon)))
	   (diffagi (- incagi (weapon/incagi weapon)))
	   (powercolor nil)
	   (hpcolor nil)
	   (agicolor nil))
      (if (>= diffpower 0)
	  (setf powercolor (encode-rgb 0 254 0)
		diffpower (format nil "+~3d" diffpower))
	  (setf powercolor (encode-rgb 255 0 0)
		diffpower (format nil "~4d" diffpower)))
      (if (>= diffhp 0)
	  (setf hpcolor (encode-rgb 0 254 0)
		diffhp (format nil "+~2d" diffhp))
	  (setf hpcolor (encode-rgb 255 0 0)
		diffhp (format nil "~3d" diffhp)))
      (if (>= diffagi 0)
	  (setf agicolor (encode-rgb 0 254 0)
		diffagi (format nil "+~2d" diffagi))
	  (setf agicolor (encode-rgb 255 0 0)
		diffagi (format nil "~3d" diffagi)))
      (setf get-item (make-instance 'weapon :name name :power sin-power
					    :inchp inchp :incagi incagi
					    :diffpower (cons diffpower powercolor)
					    :diffhp (cons diffhp hpcolor)
					    :diffagi (cons diffagi agicolor))
	    explore-state :get-item))))

;;次のステージへ
(defun go-to-next-stage ()
  )

;;アイテムの当たり判定
(defun player-pos-event (mover)
  (with-slots (posx posy) mover
    (let ((cell (aref (donjon/stage (game/donjon *game*)) posy posx)))
      (with-slots (item) cell
	(when item
	  (cond
	    ((= item +chest+)
	     (get-item mover))
	    ((= item +kaidan+)
	     (go-to-next-stage))))))))

;;移動終わったかチェック
(defmethod walk-end ((mover player))
  (when (reach-next-pos? mover)
    (player-pos-event mover)
    (init-walk mover)))

(defmethod walk-end ((mover monster))
  (when (reach-next-pos? mover)
    (init-walk mover)))


;;プレイヤー色々更新
(defun update-player ()
  (with-slots (dir walk-flag explore-state collide-monster) *p*
    (update-walk-animation *p*)
    (cond
      (walk-flag
        (update-position *p*)
        (walk-end *p*))
      ((null collide-monster)
       (update-player-dir)))))


;;移動できる方向ゲット
(defun get-can-move-dir (monster)
  (loop :for dir :in '(:up :down :right :left :wait)
        :for next-pos = (get-next-pos dir monster)
        :when (can-move? dir monster)
        :collect dir))

;;モンスターランダム移動
(defun monster-random-move (monster)
  (with-slots (posx posy dir walk-flag) monster
    (let* ((can-move-dir (get-can-move-dir monster))
           (next-dir (nth (random (length can-move-dir)) can-move-dir)))
      (case next-dir
        (:up (setf dir *up*
                   posy (1- posy)))
        (:down (setf dir *down*
                     posy (1+ posy)))
        (:right (setf dir *right*
                      posx (1+ posx)))
        (:left (setf dir *left*
                     posx (1- posx)))
        (:wait))
      (setf walk-flag t))))

;;モンスター更新
(defun update-monster (monster)
  (with-slots (right left up down) *keystate*
    (with-slots (walk-flag) monster
     (update-walk-animation monster)
     (cond
      (walk-flag
        (update-position monster)
        (walk-end monster))
      ((and (or right left up down)
            (chara/walk-flag *p*)
            (null (player/collide-monster *p*)))
       (monster-random-move monster))))))

;;モンスター全員動き終わった
(defun monsters-move-end? (donjon)
  (with-slots (monsters) donjon
    (every #'(lambda (monster) (null (chara/walk-flag monster))) monsters)))

;;モンスターの移動更新
(defun update-monsters (donjon)
  (with-slots (monsters) donjon
    (loop :for monster :in monsters
          :do (update-monster monster))))
    ;;(when (monsters-move-end? donjon)
    ;;  (setf (player/explore-state *p*) :player-move))))



;;探索更新
(defun update-explore (donjon)
  (with-slots (explore-state walk-flag collide-monster walk-num walk-img) *p*
    (update-player)
    (unless collide-monster
      (collide-monsters? donjon))
    (cond
      ((and collide-monster
            (null walk-flag)
            (eq explore-state :player-move))
       (save-player-pos)
       (set-battle-pos)
       (create-battle-monsters  donjon)
       (delete-collide-monster donjon)
       (setf (game/state *game*) :battle
             collide-monster nil
             explore-state :player-move
             walk-num 0
             walk-img 0))
      ((eq explore-state :collide-m)
       (update-monsters donjon)
       (when (monsters-move-end? donjon)
          (setf explore-state :player-move)))
      (t
        (update-monsters donjon)
        (unless collide-monster
          (collide-monsters? donjon)
          (when collide-monster
            (setf explore-state :collide-m)))))))


;;----------------------------battle-------------------------------------


(defun debug-battle-monsters (donjon)
  (with-slots (battle-monsters) donjon
    (setf battle-monsters nil)
    (create-battle-monsters donjon)))

;;斬るのダメージ計算
(defun slash-damage-calc ()
  (with-slots (str) *p*
    (+ 2 (random (ash str -1))))) ;;TODO

;;二段切りのダメージ計算
(defun double-slash-damage-calc ()
  (with-slots (str) *p*
    (1+ (random (floor str 5)))))

;;凪払いのダメージ
(defun swing-damage-calc ()
  (with-slots (str) *p*
    (1+ (random (floor str 10)))))

;;乱れ切りのダメージ
(defun chopped-damage-calc ()
  (with-slots (str) *p*
    (1+ (random (floor str 8)))))

;;突撃のダメージ
(defun assalut-damage-calc ()
  (with-slots (str agi) *p*
    (let ((n (random (+ 30 agi))))
      (cond
        ((>= n 30)
         (+ (floor str 3) (random (floor str 2))))
        (t "ミス")))))

;;暗黒ダメージ
(Defun ankoku-damage-calc ()
  (with-slots (str) *p*
    (1+ (random (floor str 5)))))

;;暗黒使えるか
(defun can-ankoku? ()
  (with-slots (hp maxhp) *p*
    (> hp (floor (* 3 maxhp) 10))))

;;暗黒被ダメージ
(defun ankoku-damaged-calc ()
  (with-slots (hp maxhp) *p*
    (decf hp (floor (* 3 maxhp) 10))))

;;プレイヤーへのダメージデータ生成
(defun create-player-damaged-data (num color)
  (with-slots (drawy damage drawx) *p*
    (let* ((tempy (+ drawy (random 60)))
           (topy  (- tempy (random 40)))
           (dir (if (= (random 2) 0) :right :left))
           (dx1 (+ (random 4) 2))
           (dx 0)
           (dmg-drawx 0))
      (if (eq dir :right)
          (setf dmg-drawx (+ drawx 35)
                dx dx1)
          (setf dmg-drawx (- drawx 10)
                dx (- dx1)))
      (make-instance 'damage :dx dx :tempy tempy :topy topy :drawy tempy
                             :dy 4 :color color
                             :dir dir :num num :drawx dmg-drawx))))


;;単体へのダメージデータ生成
(defun create-single-damage-data (target num)
  (with-slots (damage drawx drawy hp) target
    (when (numberp num)
      (decf hp (min hp num)))
    (setf damage (make-instance 'damage :num num :timer 0 :color (encode-rgb 255 255 255)
                                        :dy 5 :dx -2
                                        :drawx (- drawx 5)
                                        :drawy (+ drawy 52)
                                        :tempy (+ drawy 52)
                                        :topy  (+ drawy 30)))))
;;全体へのダメージデータ精製
(defun create-all-damage-date (battle-monsters func)
  (loop :for monster :in battle-monsters
        :do (create-single-damage-data monster (funcall func))))

;;ダメージのデータ生成
(defun create-damage-data (battle-monsters)
  (with-slots (selected-skill selected-enemy hp maxhp) *p*
    (cond
      ((= selected-skill +slash+)
       (create-single-damage-data (nth selected-enemy battle-monsters) (slash-damage-calc)))
      ((= selected-skill +double-slash+)
       (create-single-damage-data (nth selected-enemy battle-monsters) (double-slash-damage-calc)))
      ((= selected-skill +swing+)
       (create-all-damage-date battle-monsters #'swing-damage-calc))
      ((= selected-skill +assalut+)
       (create-single-damage-data (nth selected-enemy battle-monsters) (assalut-damage-calc)))
      ((= selected-skill +chopped+)
       (let ((target (nth (random (length battle-monsters)) battle-monsters)))
         (create-single-damage-data target (chopped-damage-calc))))
      ((= selected-skill +ankoku+)
       (create-all-damage-date battle-monsters #'ankoku-damage-calc)))))



;;ダメージフォントの位置更新
(defun update-damage-position (dmg)
  (with-slots (drawx drawy topflag timer tempy fall topy dy dx) dmg
    (if topflag
        (when (> tempy drawy)
          (incf drawy dy)
          (incf drawx dx))
        (progn (decf drawy dy)
               (incf drawx dx)))
    (when (> topy drawy)
      (setf topflag t))
    (when (>= drawy tempy)
      (incf timer))))

;;敵のダメージ表示更新
(defun update-enemy-damage-timer ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (loop :for monster :in battle-monsters
          :do (with-slots (damage) monster
                (when damage
                  (update-damage-position damage)
                  (when (> (damage/timer damage) 30)
                    (setf damage nil)))))))

;;ダメージ関係更新
(defun update-player-damage-timer ()
  (with-slots (damage dir) *p*
    (loop :for dmg :in damage
          :do (update-damage-position dmg)
              (when (> (damage/timer dmg) 30)
                (setf damage (remove dmg damage :test #'equal))))
    (when (null damage)
      (setf dir *left*))))



;;死んだ敵がいないか判定
;;敵のポジション更新
(defun update-monsters-position ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (let* ((num (length battle-monsters))
           (poss (reverse (nth num *battle-monster-pos*))))
      (loop :for monster :in battle-monsters
            :for pos :in poss
            :do (with-slots (drawx drawy) monster
                  (setf drawx (car pos)
                        drawy (cadr pos)))))))

;;HPが0になった敵を消す
(defun delete-dead-monsters ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (let ((dead? nil))
      (loop :for monster :in battle-monsters
            :do (with-slots (hp) monster
                  (when (>= 0 hp)
		    (format t "~d~%" (length battle-monsters))
                    (setf battle-monsters (remove monster battle-monsters :test #'equal)
                          dead? t))))
      (when dead?
        (sleep 0.1) ;;敵が死んだときちょっと止める
        (if battle-monsters
            (update-monsters-position)
            ;;敵が全滅した時
            (with-slots (attack-num battle-state d-atk drawx drawy tempdrawx tempdrawy) *p*
              (setf (game/state *game*) :explore
                    (game/battle-state *game*) :player-turn
                    attack-num 0
                    d-atk t
                    battle-state :action-select
                    drawx tempdrawx
                    drawy tempdrawy)))))))


;;攻撃終わったあとの処理
(defun update-player-state ()
  (with-slots (battle-state chopped-num attack-num agi selected-skill d-atk dir end-animation) *p*
    (setf end-animation nil)
    (cond
      ((and d-atk (= selected-skill +double-slash+))
       (setf d-atk nil
             battle-state :enemy-select))
      ;;プレイヤーのターン終了
      ((>= attack-num (1+ (truncate (/ (max 0 agi) 15))))
       (setf (game/battle-state *game*) :enemy-turn
             d-atk t
             attack-num 0))
      ((and (> chopped-num 0) (= selected-skill +chopped+))
       (decf chopped-num)
       (create-damage-data (donjon/battle-monsters (game/donjon *game*))))
      (t ;;プレイヤーの次の攻撃へ
       (incf attack-num)
       (setf battle-state :action-select
             d-atk t)))))


;;暗黒攻撃モーション
(defun update-ankoku-animation ()
  (with-slots (walk-img walk-num end-animation) *p*
    (let ((d (if (> walk-img 0) 14 18)))
      (incf walk-num)
      (when (zerop (mod walk-num d))
        (incf walk-img)
        (when (>= walk-img 4)
          (setf walk-img 0
                walk-num 0
                end-animation t))))))

;;突撃の攻撃モーション
(defun update-assalut-animation ()
  (with-slots (end-animation tempdrawy assalut-flag drawy walk-img walk-num) *p*
    (incf walk-num)
    (when (zerop (mod walk-num 3))
      (if assalut-flag
          (incf drawy 8)
          (decf drawy 10))
      (cond
        ((and (null assalut-flag)
              (>= (- tempdrawy 40) drawy))
         (setf assalut-flag t))
        ((and assalut-flag
              (>= drawy tempdrawy))
         (setf walk-img 0
               walk-num 0
               drawy tempdrawy
               assalut-flag nil
               end-animation t))
        ((and assalut-flag
              (< walk-img 3))
         (incf walk-img))))))

;;通常の攻撃モーション
(defun update-normal-attack-animation ()
  (with-slots (walk-img walk-num attack-num end-animation) *p*
    (incf walk-num)
    (when (zerop (mod walk-num 4))
      (incf walk-img)
      (when (>= walk-img 4)
        (setf walk-img 0
              walk-num 0
              end-animation t)))))

;;ポーションのモーション
(defun update-potion-animation ()
  (with-slots (hp maxhp damage walk-img walk-num end-animation) *p*
    (incf walk-num)
    (when (zerop (mod walk-num 8))
      (let ((heal-num (1+ (random 4))))
        (incf walk-img)
        (incf hp heal-num)
        (push (create-player-damaged-data heal-num (encode-rgb 0 254 0)) damage)
        (when (>= walk-img 4)
          (setf walk-img 0))
        (when (>= hp maxhp)
          (setf walk-num 0
                walk-img 0
                hp maxhp
                end-animation t))))))

;;攻撃アニメの更新
(defun update-attack-animation ()
  (with-slots (selected-skill walk-img walk-num attack-num end-animation) *p*
    (cond
      ((or (= selected-skill +slash+)
           (= selected-skill +double-slash+)
           (= selected-skill +chopped+)
           (= selected-skill +swing+))
       (update-normal-attack-animation))
      ((= selected-skill +ankoku+)
       (update-ankoku-animation))
      ((= selected-skill +potion+)
       (update-potion-animation))
      ((= selected-skill +assalut+)
       (update-assalut-animation)))
    (when end-animation
      (update-player-state)
      (delete-dead-monsters))))




;;ターゲット選ぶか選ばないか
(defun select-skill-action ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (with-slots (chopped-num cursor selected-skill battle-state str) *p*
      (cond
        ((or (= selected-skill +slash+)
             (= selected-skill +double-slash+)
             (= selected-skill +assalut+))
         (setf battle-state :enemy-select
               cursor 0))
        ((or (= selected-skill +swing+)
             (= selected-skill +ankoku+))
         (create-damage-data battle-monsters)
         (setf battle-state :attack-animation
               cursor 0))
        ((= selected-skill +chopped+)
         (create-damage-data battle-monsters)
         (setf chopped-num (1+ (random (floor str 3)))
               battle-state :attack-animation
               cursor 0))))))

;;ポーション使う
(defun use-potion ()
  (with-slots (damage selected-skill hp battle-state maxhp dir) *p*
    (setf selected-skill +potion+
          battle-state :attack-animation)))

;;バトル中の攻撃する敵を選択 4-3 3-2 2-1 1-0
(defun update-enemy-select ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (with-slots (cursor selected-skill selected-enemy battle-state) *p*
      (with-slots (right left up down keyz) *keystate*
        (let* ((num (length battle-monsters)))
          (cond
            (left
             (cond ((>= cursor 8)
                    (decf cursor 8))
                   ((and (>= 7 cursor 4) (>= 8 num 5))
                    (decf cursor 4))
                   ((>= 4 num)
                    nil)
                   (t
                    (incf cursor 4)
                    (when (> cursor (1- num))
                      (setf cursor (1- num))))))
            (right
             (cond ((and (> 4 cursor) (>= 8 num 5))
                    (incf cursor 4)
                    (when (> cursor (1- num))
                      (setf cursor (1- num))))
                   ((and (> 4 cursor) (>= num 9))
                    (incf cursor 8)
                    (when (> cursor (1- num))
                      (setf cursor (1- num))))
                   ((and (> 4 cursor) (>= 4 num))
                    nil)
                   (t
                    (decf cursor 4))))
            (up
             (cond ((and (>= num 2) (/= cursor 0) (/= cursor 4) (/= cursor 8))
                    (decf cursor 1))
                   ((or (= cursor 0) (= cursor 4) (= cursor 8))
                    (incf cursor 3)
                    (when (> cursor (1- num))
                      (setf cursor (1- num))))))
            (down
             (cond ((and (>= 4 num) (= cursor (1- num)))
                    (decf cursor (1- num)))
                   ((and (>= num 5) (= cursor 3))
                    (decf cursor 3))
                   ((and (>= 8 num 5) (= cursor (1- num)))
                    (decf cursor (1- (- num 4))))
                   ((and (>= num 9) (= cursor 7))
                    (decf cursor 3))
                   ((and (>= 12 num 9) (= cursor (1- num)))
                    (decf cursor (1- (- num 8))))
                   (t
                    (incf cursor 1))))
            (keyz
             (setf battle-state :attack-animation
                   selected-enemy cursor
                   cursor 0)
             (create-damage-data battle-monsters))))))))


;;バトル中の行動選択
(defun update-action-select ()
  (with-slots (potion cursor skill selected-skill) *p*
    (with-slots (right left up down keyz keya) *keystate*
      (let ((skill-num (length skill)))
        (cond
          ((and keya (> potion 0))
           (use-potion))
          ((and keya (= potion 0))
           nil) ;;TODO sound
          (right
            (when (>= skill-num 4)
              (if (< cursor 3)
                  (incf cursor 3)
                  (decf cursor 3))))
          (left
            (when (>= skill-num 4)
              (if (< cursor 3)
                  (incf cursor 3)
                  (decf cursor 3))))
          (up
            (cond
              ((and (= cursor 0) (>= skill-num 3))
               (incf cursor 2))
              ((and (= cursor 0) (= skill-num 2))
               (incf cursor 1))
              ((and (= cursor 3) (= skill-num 6))
               (incf cursor 2))
              ((and (= cursor 3) (= skill-num 5))
               (incf cursor 1))
              ((= skill-num 1)
               nil)
              (t
               (decf cursor))))
          (down
           (cond
             ((or (= cursor 2) (= cursor 5))
              (decf cursor 2))
             ((>= skill-num 2)
              (incf cursor 1))))
          ((and keyz (= cursor +ankoku+)
                (not (can-ankoku?)))
           nil) ;;TODO sound
          (keyz
           (setf selected-skill cursor)
           (select-skill-action)))))))

;;バトル時のプレイヤーターン更新
(defun update-battle-player-turn ()
  (with-slots (battle-state) *p*
    (case battle-state
      (:action-select
       (update-action-select))
      (:enemy-select
       (update-enemy-select))
      (:attack-animation
       (update-attack-animation)))))

;;バトル時の敵の行動更新
(defmethod update-battle-monster-action ((orc orc))
  (with-slots (str) orc
    (case (random 1) ;;TODO
      (0 (1+ (random str))))))

;;バトル時の敵のターン更新
(Defun update-battle-monsters-turn (donjon)
  (with-slots (drawy damage drawx battle-state dir) *p*
    (with-slots (battle-monsters) donjon
      (setf dir *guard*)
      (loop :for monster :in battle-monsters
            :do (let* ((num (update-battle-monster-action monster))
                       (dmg (create-player-damaged-data num (encode-rgb 255 255 255))))
                  (push dmg damage)))
      (setf (game/battle-state *game*) :player-turn
            battle-state :action-select))))


          

#|
(defun update-player-wait-img ()
  (with-slots (damage dir heal-data) *p*
    (cond
      (damage (setf dir *guard*))
      (heal-data (setf dir *heal*))
      (t (setf dir *left*)))))
|#
;;バトル関係更新
(defun update-battle ()
  (with-slots (battle-state donjon) *game*
    ;;(update-player-wait-img)
    (case battle-state
      (:player-turn
       (update-battle-player-turn))
      (:enemy-turn
       (update-battle-monsters-turn donjon)))
    (update-player-damage-timer)
    (update-enemy-damage-timer)))


;;----------------------------------------------------------

;;ゲームループ
(defun main-game-loop ()
  (with-slots (state donjon) *game*
    (case state
      (:battle
        (update-battle))
        ;;(debug-battle-monsters donjon))
      (:explore
        (update-explore donjon)))
    (init-keystate)))



(defun moge-paint (hwnd)
  (with-double-buffering-2 (hdc hwnd)
  ;;(with-paint (hwnd hdc)
    (with-compatible-dc (hmemdc hdc)
      (set-bk-mode hdc :transparent)
      (render-game hdc hmemdc))))

;;ウィンドウサイズ変更時に画像拡大縮小する
(defun change-screen-size (lp)
  (let* ((change-w (loword lp))
         (change-h (hiword lp)))
    (setf *change-window-w* change-w
          *change-window-h* change-h)))
          ;;*mouse-hosei-x* (/ *change-screen-w*  (rect-right *c-rect*))
          ;;*mse-hosei-y* (/ *change-screen-h*  (rect-bottom *c-rect*)))))


(defun set-window-top (hwnd)
  (set-window-pos hwnd :no-topmost 0 0 0 0 '(:no-move :no-size)))
  ;;(set-window-pos hwnd :no-topmost 0 0 0 0 '(:no-move :no-size :show-window)))


(defun moge-create (hwnd)
  ;;(set-timer :hwnd hwnd :elapse 20 :replace-timer 1)
  ;;(setf *name* nil)
  (set-brush)
  (set-font)
  ;;(init-bgm)
  ;;(load-images)
  (init-game)
  ;;(set-client-size hwnd)
  ;;(bgm-play (bgm-alias *game*))
  (setf *c-rect* (get-client-rect hwnd))
  ;;(set-foreground-window hwnd)
  ;;(set-active-window hwnd)
  (set-window-top hwnd))
  ;;(set-focus hwnd)
  


;;proc
(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (moge-create hwnd))
    ((const +wm-paint+)
     (moge-paint hwnd))
    ((const +wm-size+)
     (change-screen-size lparam))
    ((const +wm-close+)
     (destroy-window hwnd))
    ;;((const +wm-timer+)
    ;; (invalidate-rect hwnd nil nil))
    ((const +wm-keydown+)
     (moge-keydown hwnd wparam))
    ((const +wm-keyup+)
     (moge-keyup wparam))
    ((const +wm-destroy+)
     (delete-font)
     (delete-imgs)
     (delete-brush)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


;;メイン
(defun moge ()
  (setf *random-state* (make-random-state t))
  (let ((myclass "MOGE"))
    (register-class myclass
                    (cffi:callback moge-wndproc)
                    :styles '(:hredraw :vredraw)
                    :icon (load-icon :application)
                    :cursor (load-cursor :arrow)
                    :background (create-solid-brush (encode-rgb 0 255 0)))
    (let ((hwnd (create-window myclass
                              :window-name "真・もげRPG"
                              :ex-styles  '(:topmost) ;;:LAYERED :composited) ;;透明
                              :styles '(:overlapped-window :visible)
                              :x 1500 :y 100 :width *window-w* :height *window-h*))
          (msg (make-msg)))
      ;;(init-game)
      (show-window hwnd)
      (update-window hwnd)
      (unwind-protect
          (progn
            (do ((done nil))
                (done)
              (let ((m (ftw:peek-message msg :remove-msg :remove :error-p nil)))
                (cond
                  (m
                    ;;(let ((r (get-message msg)))
                    (cond
                      ((= (msg-message msg) (const +wm-quit+))
                       (setf done t))
                      (t
                       (translate-message msg)
                       (dispatch-message msg))))
                  (t
                    (sleep 0.01)
                    (main-game-loop)
                    (invalidate-rect hwnd nil nil)))))
            (msg-lparam msg))
        (progn
         (delete-font)
         (delete-imgs)
         (delete-brush)
         (unregister-class myclass))))))
