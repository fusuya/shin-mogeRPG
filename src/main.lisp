(in-package :casket2022aki)

(defparameter *skill-list*
  (list
   (make-instance 'skill :name "斬る" :power 10 :accuracy 90 :learn-lv 1)
   (make-instance 'skill :name "二段斬り" :power 8 :accuracy 85 :learn-lv 3)
   (make-instance 'skill :name "なぎ払い" :power 3 :accuracy 70 :learn-lv 5)
   (make-instance 'skill :name "乱れ切り" :power 6 :accuracy 80 :learn-lv 7)
   (make-instance 'skill :name "突撃" :power 20 :accuracy 50 :learn-lv 9)
   (make-instance 'skill :name "暗黒" :power 10 :accuracy 80 :learn-lv 11)))

(defun set-init-skill ()
  (list (car *skill-list*)))
;;ゲーム
(defun create-game ()
  (setf *game* (make-instance 'game :state :title)
	*p* (make-instance 'player :cursor 0)
	*keystate* (make-instance 'keystate)))

;;ゲーム初期化
(defun init-game ()
  (setf *game* (make-instance 'game :donjon (make-instance 'donjon :floor-num 1) :state :explore
			      :start-time (get-internal-real-time)
			      :item-list (copy-tree *buki-d*))
        *p* (make-instance 'player :drawx 800 :drawy 280 :posx 1 :posy 1 :hp 30 :maxhp 30 :maxstr 30
                                   :maxagi 30 :agi 30 :str 30 :tempdrawx 800 :tempdrawy 280
                                   :battle-state :action-select :hammer 10
                                   :explore-state :player-move
                                   :potion 3 :lv 1 :exp 0 :max-exp 10
				   :weapon (make-instance 'weapon :name "素手" :power 0 :inchp 0 :incagi 0)
                                   :skill (set-init-skill)))
  (create-maze (game/donjon *game*))
  (create-monsters (game/donjon *game*)))
  ;;(create-battle-monsters (game/donjon *game*)))

(defun init-keystate ()
  (with-slots (keyx keya right left up down keyz) *keystate*
    (setf keya nil right nil left nil up nil down nil keyz nil keya nil keyx nil)))

;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (with-slots (left right down up keyz keyx keyc keya shift) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:shift (setf shift t))
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
  (with-slots (shift left right down up keyz keyx keyc keya) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:shift (setf shift nil))
        (:left (setf left nil))
        (:right (setf right nil))
        (:down (setf down nil))
        (:up (setf up nil))
        (:keya (setf keya nil))
        (:keyx (setf keyx nil))
        (:keyc (setf keyc nil))
        (:keyz (setf keyz nil))))))



(defparameter *test* nil)


;;----------------------explore--------------------------------------

;;先に進むごとにアイテムのドロップレート変更
(defun change-item-rate (donjon)
  (with-slots (floor-num) donjon
    (let ((r 0) (decr 0))
      (cond
	((>= 10 floor-num 1)  (setf r 2000 decr 50))
	((>= 20 floor-num 11) (setf r 1000 decr 20))
	((>= 30 floor-num 21) (setf r 500 decr 10))
	((>= 40 floor-num 31)  (setf r 300 decr 5))
	((>= 50 floor-num 41)  (setf r 100 decr 3)))
      (setf (game/item-list *game*)
	    (loop :for b :in (game/item-list *game*)
		  :collect (let ((rate (cdr b)))
			     (if (>= rate r)
				 (cons (car b) (- rate decr))
				 b)))))))

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
                (return t)))))

;;歩行画像更新
(defun update-walk-animation (walker)
  (with-slots (walk-img walk-num walk-flag) walker
    (let ((tmp (if walk-flag 5 13)))
      (incf walk-num)
      (when (zerop (mod walk-num tmp))
        (incf walk-img)
        (when (>= walk-img 4)
          (setf walk-img 0
                walk-num 0))))))

;;次の移動先座標ゲット
(defun get-next-pos (mover)
  (with-slots (posx posy dir) mover
    (cond
      ((= dir *right*) (list (1+ posx) posy))
      ((= dir *left*)  (list (1- posx) posy))
      ((= dir *up*)    (list posx (1- posy)))
      ((= dir *down*)  (list posx (1+ posy)))
      (t (list posx posy)))))

;;移動先の地形ゲット
(defun get-next-cell (next-pos)
  (with-slots (stage) (game/donjon *game*)
    (aref stage (cadr next-pos) (car next-pos))))


;;移動先が床だったらいける
(defun search-next-cell (mover &key (nextcell +yuka+))
  (let* ((next-pos (get-next-pos mover))
         (next-cell (get-next-cell next-pos)))
    (= (cell/cell next-cell) nextcell)))

;;壁が壊れる時間更新
(defun update-break-block ()
  (loop :for blk :in (player/breakblock *p*)
	:do (with-slots (breaked break-img) blk
	      (when (> breaked 0)
		(decf breaked)
		(when (zerop (mod breaked 7))
		  (incf break-img)
		  (when (> break-img 3)
		    (setf break-img 0
			  breaked 0
			  (player/breakblock *P*) (remove blk (player/breakblock *p*) :test #'equal))))))))
	      
		

;;壁を壊す
(defun break-soft-wall ()
  (let* ((next-pos (get-next-pos *p*))
	 (next-cell (get-next-cell next-pos)))
    (when (eq (cell/cell next-cell) +magic-block+)
      (setf (cell/item next-cell) +magic-circle+))
    (setf (cell/cell next-cell) +yuka+
	  (cell/breaked next-cell) 200)
    (push next-cell (player/breakblock *p*))))


;;ポーション使う
(Defun use-potion-explore ()
  (with-slots (potion hp maxhp str maxstr agi maxagi) *p*
    (when (or (/= hp maxhp) (/= agi maxagi) (/= str maxstr))
      (sound-play *potion-wav*)
      (setf potion (1- potion)
	    hp maxhp
	    str maxstr
	    agi maxagi))))
    

;;プレイヤーの方向更新
(defun update-player-dir ()
  (with-slots (right left up down keyx shift keya) *keystate*
    (with-slots (potion posx posy dir walk-flag hammer dash) *p*
      (cond
	;;ダッシュ
	((and shift
	      (or right left up down))
	 (setf dash t)
	 (cond (right (setf dir *right*))
	       (left (setf dir *left*))
	       (up (setf dir *up*))
	       (down (setf dir *down*))))
	;;ハンマー
	((and keyx (> hammer 0)
	      (or (search-next-cell *p* :nextcell +soft-block+)
		  (search-next-cell *p* :nextcell +magic-block+)))
	 (break-soft-wall)
	 (sound-play *hammer-wav*)
	 (decf hammer))
	;;ポーション
	((and (> potion 0) keya)
	 (use-potion-explore))
        (right
          (setf dir *right*)
          (when (search-next-cell *p* :nextcell +yuka+)
            (setf walk-flag t)
            (incf posx)))
        (left
          (setf dir *left*)
          (when (search-next-cell *p* :nextcell +yuka+)
            (setf walk-flag t)
            (decf posx)))
        (up
          (setf dir *up*)
          (when (search-next-cell *p* :nextcell +yuka+)
            (setf walk-flag t)
            (decf posy)))
        (down
          (setf dir *down*)
          (when (search-next-cell *p* :nextcell +yuka+)
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
             (>= (* posy *cell-size*) drawy))
	(= dir *wait*))))

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


;;拾ったアイテム装備
(defun equip-get-weapon ()
  (with-slots (weapon get-item maxhp maxagi maxstr hp str agi) *p*
    (decf maxhp (weapon/inchp weapon))
    (decf maxagi (weapon/incagi weapon))
    (decf maxstr (weapon/power weapon))
    (incf maxhp (weapon/inchp get-item))
    (incf hp (weapon/inchp get-item))
    (incf maxstr (weapon/power get-item))
    (incf str (weapon/power get-item))
    (incf maxagi (weapon/incagi get-item))
    (incf agi (weapon/incagi get-item))
    (when (> hp maxhp)
      (setf hp maxhp))
    (when (> str maxstr)
      (setf str maxstr))
    (when (> agi maxagi)
      (setf agi maxagi))
    (setf weapon get-item
	  get-item nil)))
    
;;アイテムゲット
(defun get-weapon (p)
  (with-slots (weapon get-item explore-state) p
    (let* ((item (weightpick (game/item-list *game*)))
	   (name (car item))
	   (power (cadr item))
	   (sin-power (if (>= (random 3) 1)
			  (+ power (random (max 1 (floor power 3))))
			  (- power (random (max 1 (floor power 4))))))
	   (inchp (if (>= (random 3) 1)
		      (random (max 1 (floor power 2)))
		      (- (random (max 1 (floor power 2))))))
	   (incagi (if (>= (random 3) 1)
		       (random (max 1 (floor power 5)))
		       (- (random (max 1 (floor power 5))))))
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
	    explore-state :get-weapon))))

;;potion get
(defun get-potion (p)
  (with-slots (explore-state potion) p
    (incf potion)
    (setf explore-state :get-potion)))

;;hammer get
(defun get-hammer (p)
  (with-slots (explore-state hammer) p
    (incf hammer)
    (setf explore-state :get-hammer)))


;;アイテムゲット
(defun get-item (p)
  (let ((n (random 6)))
    (cond
      ((>= 3 n 0) (get-weapon p))
      ((= n 4) (get-hammer p))
      ((>= 6 n 5) (get-potion p)))))

;;もげぞーの剣イベント
(defun mogezou-sword-event (p)
  (with-slots (explore-state weapon hp maxhp str maxstr agi maxagi) p
    (when (string= (weapon/name weapon) "もげぞーの剣")
      (setf explore-state :mogezou-sword
	    weapon (make-instance 'weapon :name "ｼﾝもげぞうの光剣" :power 200 :inchp 100 :incagi 50)
	    maxhp (+ maxhp 100)
	    hp maxhp
	    maxstr (+ str 100)
	    str maxstr
	    maxagi (+ maxagi 50)
	    agi maxagi))))

;;アイテムの当たり判定
(defun player-pos-event (mover)
  (with-slots (posx posy explore-state) mover
    (let ((cell (aref (donjon/stage (game/donjon *game*)) posy posx)))
      (with-slots (item) cell
	(when item
	  (cond
	    ((= item +magic-circle+)
	     (mogezou-sword-event mover))
	    ((= item +chest+)
	     (sound-play *chest-wav*)
	     (get-item mover)
	     (setf item +empty-chest+))
	    ((= item +kaidan+)
	     (sound-play *kaidan-wav*)
	     (setf explore-state :next))))))))

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
  (loop :for dir :in (list *up* *down* *right* *left* *wait*)
	:do (setf (chara/dir monster) dir)
        :when (search-next-cell monster :nextcell +yuka+)
        :collect dir))

;;モンスターランダム移動
(defun monster-random-move (monster)
  (with-slots (posx posy dir walk-flag) monster
    (let* ((can-move-dir (get-can-move-dir monster))
           (next-dir (nth (random (length can-move-dir)) can-move-dir)))
      (cond
        ((= next-dir *up*)
	 (setf dir *up*
               posy (1- posy)))
        ((= next-dir *down*)
	 (setf dir *down*
               posy (1+ posy)))
        ((= next-dir *right*)
	 (setf dir *right*
               posx (1+ posx)))
        ((= next-dir *left*)
	 (setf dir *left*
               posx (1- posx)))
        ((= next-dir *wait*)
	 ))
      (setf walk-flag t))))

;;モンスター更新
(defun update-monster (monster)
  (with-slots (right left up down) *keystate*
    (with-slots (walk-flag kind) monster
     (update-walk-animation monster)
     (cond
      (walk-flag
        (update-position monster)
        (walk-end monster))
      ((and (or right left up down)
            (chara/walk-flag *p*)
            (null (player/collide-monster *p*))
	    (/= kind *mogemos*))
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

;;ダッシュ終わった後の描画位置をセット
(defun set-draw-position (donjon)
  (with-slots (posx posy drawx drawy) *p*
    (setf drawx (* posx *cell-size*)
	  drawy (* posy *cell-size*)))
  (loop :for monster :in (donjon/monsters donjon)
	:do (with-slots (posx posy drawx drawy kind) monster
	      (when (/= kind *mogemos*)
		(setf drawx (* posx *cell-size*)
		      drawy (* posy *cell-size*))))))

;;ダッシュ
(defun dash (donjon)
  (with-slots (dir posx posy drawx drawy dash collide-monster) *p*
    (loop :named moge
	  :while (search-next-cell *p* :nextcell +yuka+)
	  :do (cond
		((= dir *right*) (incf posx))
		((= dir *left*) (decf posx))
		((= dir *up*) (decf posy))
		((= dir *down*) (incf posy)))
	      (when (collide-monsters? donjon)
		(return-from moge))
	      (loop :for monster :in (donjon/monsters donjon)
		    :do (when (/= (monster/kind monster) *mogemos*)
			  (monster-random-move  monster)
			  (setf (chara/walk-flag monster) nil)
			  (when (collide-monster? *p* monster)
			    (setf collide-monster monster)
			    (return-from moge))))
	      (when (player-pos-event *p*)
		(return-from moge)))
    (set-draw-position donjon)
    (setf dash nil)))
    

;;探索更新
(defun update-explore (donjon)
  (with-slots (dash explore-state walk-flag collide-monster walk-num walk-img max-attack-num agi tempdir dir) *p*
    (update-break-block)
    (update-player)
    (cond
      ;;階段
      ((eq explore-state :next)
       (incf (donjon/floor-num donjon))
       (change-item-rate donjon)
       (create-maze donjon)
       (create-monsters donjon)
       (setf explore-state :player-move))
      (dash
       (dash donjon))
      (t
       (unless collide-monster
	 (collide-monsters? donjon))
       (cond
	 ;;敵とぶつかった
	 ((and collide-monster
               (null walk-flag)
               (eq explore-state :player-move))
	  (save-player-pos)
	  (set-battle-pos)
	  (create-battle-monsters  donjon)
	  (delete-collide-monster donjon)
	  (sound-play *collide-monster-wav*)
	  (setf (game/state *game*) :battle
		collide-monster nil
		explore-state :player-move
		walk-num 0
		walk-img 0
		tempdir dir
		dir *left*
		max-attack-num (1+ (truncate (/ (max 0 agi) 15)))))
	 ;;敵からぶつかってきた
	 ((eq explore-state :collide-m)
	  (update-monsters donjon)
	  (when (monsters-move-end? donjon) ;;歩き終わるの待つ
            (setf explore-state :player-move)))
	 (t
          (update-monsters donjon)
          (unless collide-monster
            (collide-monsters? donjon)
            (when collide-monster
              (setf explore-state :collide-m)))))))))


;;----------------------------battle-------------------------------------


(defun debug-battle-monsters (donjon)
  (with-slots (battle-monsters) donjon
    (setf battle-monsters nil)
    (create-battle-monsters donjon)))

;;斬るのダメージ計算
(defun slash-damage-calc (target)
  (with-slots (str) *p*
    (with-slots (def) target
      (+ 2 (random (max 1 (floor str 2))))))) ;;TODO

;;二段切りのダメージ計算
(defun double-slash-damage-calc (target)
  (with-slots (str) *p*
    (with-slots (def) target
      (1+ (floor (random str) 5)))))

;;凪払いのダメージ
(defun swing-damage-calc (target)
  (with-slots (str) *p*
    (with-slots (def) target
      (1+ (floor (random str) 10)))))

;;乱れ切りのダメージ
(defun chopped-damage-calc (target)
  (with-slots (str) *p*
    (with-slots (def) target
      (min 8 (1+ (floor (randval str) 8))))))

;;突撃のダメージ
(defun assault-damage-calc (target)
  (with-slots (str agi) *p*
    (with-slots (def) target 
      (let ((n (random (+ 30 agi))))
	(cond
          ((>= n 30)
           (+ (floor str 2) (floor (randval str) 2)))
          (t "ミス"))))))

;;暗黒ダメージ
(Defun ankoku-damage-calc (target)
  (with-slots (str) *p*
    (with-slots (def) target
    (1+ (floor (randval str) 5)))))

;;暗黒使えるか
(defun can-ankoku? ()
  (with-slots (hp maxhp) *p*
    (> hp (floor (* 3 maxhp) 10))))

;;暗黒被ダメージ
(defun ankoku-damaged-calc ()
  (with-slots (hp maxhp) *p*
    (decf hp (floor (* 3 maxhp) 10))))

;;プレイヤーへのダメージデータ生成
(defun create-player-damaged-data (num color region)
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
                             :dy 4 :color color :region region
                             :dir dir :num num :drawx dmg-drawx))))


;;単体へのダメージデータ生成
(defun create-single-damage-data (target func)
  (with-slots (damage drawx drawy hp w h w2 h2 kind) target
    (let ((num (funcall func target)))
      (when (= kind *yote1*)
	(if (>= (random 3) 1)
	    (setf num 1)
	    (setf num "ミス")))
      (when (numberp num)
	(decf hp (min hp num)))
      (setf damage (make-instance 'damage :num num :timer 0 :color (encode-rgb 255 255 255)
                                          :dy 5 :dx -2
                                          :drawx (- drawx 5)
                                          :drawy (+ drawy (- h2 12))
                                          :tempy (+ drawy (- h2 12))
                                          :topy  (+ drawy (- h2 34)))))))
;;全体へのダメージデータ精製
(defun create-all-damage-date (battle-monsters func)
  (loop :for monster :in battle-monsters
        :do (create-single-damage-data monster func)))

;;HPが0以上敵を取り出す
(defun get-alive-monsters ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (remove-if #'(lambda (m) (<= (chara/hp m) 0)) battle-monsters)))

;;乱れ切りのターゲット決定
(defun get-chopped-target ()
  (let ((targets (get-alive-monsters)))
    (when targets
       (nth (random (length targets)) targets))))

;;ダメージのデータ生成
(defun create-damage-data (battle-monsters)
  (with-slots (selected-skill selected-enemy hp maxhp chopped-num) *p*
    (cond
      ((= selected-skill +slash+)
       (create-single-damage-data (nth selected-enemy battle-monsters) #'slash-damage-calc))
      ((= selected-skill +double-slash+)
       (create-single-damage-data (nth selected-enemy battle-monsters) #'double-slash-damage-calc))
      ((= selected-skill +swing+)
       (create-all-damage-date battle-monsters #'swing-damage-calc))
      ((= selected-skill +assault+)
       (create-single-damage-data (nth selected-enemy battle-monsters) #'assault-damage-calc))
      ((= selected-skill +chopped+)
       (let* ((target (get-chopped-target)))
	 (if target
             (progn (sound-play *slash-wav*)
		    (create-single-damage-data target #'chopped-damage-calc))
	     (setf chopped-num 0))))
      ((= selected-skill +ankoku+)
       (ankoku-damaged-calc)
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
  (with-slots (damage dir damage-flag) *p*
    (loop :for dmg :in damage
          :do (update-damage-position dmg)
              (when (> (damage/timer dmg) 30)
                (setf damage (remove dmg damage :test #'equal))))
    (when (null damage)
      (setf damage-flag nil))))



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

;;バトルに勝利
(defun battle-end? (donjon)
  (with-slots (battle-monsters floor-num) donjon
    (with-slots (attack-num damage exp dir tempdir battle-state d-atk drawx drawy tempdrawx tempdrawy max-exp explore-state) *p*
      (when (eq battle-state :battle-end)
	;;敵が全滅した時
	(setf (game/state *game*) :explore
              (game/battle-state *game*) :player-turn
              attack-num 0
              d-atk t
	      damage nil
	      dir tempdir
              battle-state :action-select
              drawx tempdrawx
              drawy tempdrawy)
	(when (>= exp max-exp)
	  (sound-play *levelup-wav*)
	  (setf explore-state :level-up))
	(when (= floor-num 50)
	  (setf (game/state *game*) :gameclear
		(game/end-time *game*) (get-internal-real-time)))))))

;;HPが0になった敵を消す
(defun delete-dead-monsters ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (let ((dead? nil))
      (loop :for monster :in battle-monsters
            :do (with-slots (hp exp damage) monster
                  (when (>= 0 hp)
		    (incf (chara/exp *p*) exp) ;;プレイヤー経験値獲得
                    (setf battle-monsters (remove monster battle-monsters :test #'equal)
                          dead? t))))
      (when dead?
	;;(sound-play *monster-dead-wav* )
        (sleep 0.1) ;;敵が死んだときちょっと止める
        (if battle-monsters
            (update-monsters-position))))))
            


;;攻撃終わったあとの処理
(defun update-player-state (donjon)
  (with-slots (battle-state chopped-num max-attack-num attack-num agi selected-skill d-atk dir end-animation) *p*
    (setf end-animation nil)
    (cond
      ((null (donjon/battle-monsters donjon))
       (setf battle-state :battle-end))
      ;;二段切り
      ((and d-atk (= selected-skill +double-slash+))
       (setf d-atk nil
             battle-state :enemy-select))
      ;;プレイヤーのターン終了
      ((and (>= attack-num max-attack-num)
	    (= chopped-num 0))
       (setf (game/battle-state *game*) :enemy-turn
             d-atk t
             attack-num 0))
      ;;乱れ切り
      ((and (> chopped-num 0) (= selected-skill +chopped+)
	    (eq battle-state :attack-animation))
       (decf chopped-num)
       (create-damage-data (donjon/battle-monsters (game/donjon *game*))))
      (t ;;プレイヤーの次の攻撃へ
       (setf battle-state :action-select
             d-atk t)))))


;;暗黒攻撃モーション
(defun update-ankoku-animation ()
  (with-slots (walk-img walk-num end-animation) *p*
    (let ((d (if (> walk-img 0) 14 18)))
      (incf walk-num)
      (when (= walk-num 30)
	(create-damage-data (donjon/battle-monsters (game/donjon *game*))))
      (when (zerop (mod walk-num d))
        (incf walk-img)
        (when (>= walk-img 4)
          (setf walk-img 0
                walk-num 0
                end-animation t))))))

;;突撃の攻撃モーション
(defun update-assault-animation ()
  (with-slots (end-animation tempdrawy assault-flag assault-topy drawy walk-img walk-num) *p*
    (incf walk-num)
    (when (zerop (mod walk-num 3))
      (if assault-flag
          (incf drawy 8)
          (decf drawy 10))
      (cond
        ((and (null assault-flag)
              (>= assault-topy drawy))
         (setf assault-flag t))
        ((and assault-flag
              (>= drawy (cadr *battle-pos*)))
         (setf walk-img 0
               walk-num 0
               drawy (cadr *battle-pos*)
               assault-flag nil
               end-animation t))
        ((and assault-flag
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
  (with-slots (hp maxhp damage walk-img walk-num end-animation str maxstr agi maxagi) *p*
    (incf walk-num)
    (when (zerop (mod walk-num 8))
      (let ((heal-num (randval (floor maxhp 7))))
	(incf walk-img)
        (incf hp heal-num)
        (push (create-player-damaged-data heal-num (encode-rgb 0 254 0) :hp) damage)
        (when (>= walk-img 4)
          (setf walk-img 0))
        (when (>= hp maxhp)
          (setf walk-num 0
                walk-img 0
                hp maxhp
		agi maxagi
		str maxstr
                end-animation t))))))



;;攻撃アニメの更新
(defun update-attack-animation (donjon)
  (with-slots (battle-state selected-skill walk-img walk-num attack-num end-animation) *p*
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
      ((= selected-skill +assault+)
       (update-assault-animation)))
    (when end-animation
      (delete-dead-monsters)
      (update-player-state donjon)
      )))


;;スキルの効果音
(defun play-skill-sound (select)
  (cond
    ((or (= select +slash+)
	 (= select +double-slash+)
	 (= select +swing+)
	 (= select +chopped+))
     (sound-play *slash-wav*))
    ((= select +ankoku+)
     (sound-play *ankoku-wav*))
    ((= select +assault+)
     (sound-play *assult-wav*))))
    

;;ターゲット選ぶか選ばないか
(defun select-skill-action ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (with-slots (chopped-num cursor selected-skill agi battle-state str attack-num) *p*
      (cond
        ((or (= selected-skill +slash+)
             (= selected-skill +double-slash+)
             (= selected-skill +assault+))
         (setf battle-state :enemy-select
               cursor 0))
	((= selected-skill +swing+)
	 (create-damage-data battle-monsters)
	 (play-skill-sound selected-skill)
         (setf battle-state :attack-animation
               cursor 0
	       attack-num (1+ attack-num)))
        ((= selected-skill +ankoku+)
	 (play-skill-sound selected-skill)
         (setf battle-state :attack-animation
               cursor 0
	       attack-num (1+ attack-num)))
        ((= selected-skill +chopped+)
         (create-damage-data battle-monsters)
	 (play-skill-sound selected-skill)
         (setf chopped-num (randval (floor agi 5))
               battle-state :attack-animation
               cursor 0
	       attack-num (1+ attack-num)))))))

;;ポーション使う
(defun use-potion-battle ()
  (with-slots (damage selected-skill hp battle-state maxhp dir potion) *p*
    (sound-play *potion-wav*)
    (decf potion)
    (setf selected-skill +potion+
          battle-state :attack-animation)))

;;バトル中の攻撃する敵を選択 4-3 3-2 2-1 1-0
(defun update-enemy-select ()
  (with-slots (battle-monsters) (game/donjon *game*)
    (with-slots (cursor selected-skill selected-enemy battle-state attack-num) *p*
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
                   cursor 0
		   attack-num (1+ attack-num))
	     (play-skill-sound selected-skill)
             (create-damage-data battle-monsters))))))))


;;バトル中のスキル選択
(defun update-action-select ()
  (with-slots (potion cursor skill selected-skill attack-num) *p*
    (with-slots (right left up down keyz keya) *keystate*
      (let ((skill-num (length skill)))
        (cond
          ((and keya (> potion 0))
	   (incf attack-num)
           (use-potion-battle))
          ((and keya (= potion 0))
           nil) ;;TODO sound
          (right
           (cond
	     ((and (= skill-num 6)
		   (<= cursor 2))
	      (incf cursor 3))
	     ((and (= skill-num 4)
		   (= cursor 0))
	      (incf cursor 3))
	     ((and (= skill-num 5)
		   (or (= cursor 0)
		       (= cursor 1)))
	      (incf cursor 3))
	     ((>= cursor 3)
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
              ((or (= skill-num 1)
		   (and (= skill-num 4) (= cursor 3)))
               nil)
              (t
               (decf cursor))))
          (down
           (cond
              ((or (= cursor 2) (= cursor 5))
               (decf cursor 2))
              ((and (= cursor 1) (= skill-num 2))
               (decf cursor 1))
              ((and (= cursor 4) (= skill-num 5))
               (decf cursor 1))
              ((or (and (= cursor 3) (= skill-num 4))
		   (= skill-num 1))
               nil)
              (t
               (incf cursor))))
          ((and keyz (= cursor +ankoku+)
                (not (can-ankoku?)))
           nil) ;;TODO sound
          (keyz
           (setf selected-skill cursor)
           (select-skill-action)))))))

;;レベルアップ画面更新
(defun update-level-up ()
  (with-slots (up down keyz) *keystate*
    (with-slots (cursor explore-state hp maxhp str maxstr agi maxagi exp max-exp lv skill) *p*
      (cond
	(up
	 (if (= cursor 0)
	     (incf cursor 2)
	     (decf cursor)))
	(down
	 (if (= cursor 2)
	     (decf cursor 2)
	     (incf cursor)))
	(keyz
	 (cond
	   ((= cursor 0)
	    (setf maxhp (+ maxhp 3)
		  hp maxhp))
	   ((= cursor 1)
	    (setf maxstr (1+ maxstr)
		  str maxstr))
	   ((= cursor 2)
	    (setf maxagi (1+ maxagi)
		  agi maxagi)))
	 (setf exp (- exp max-exp)
	       max-exp (+ max-exp (floor max-exp 10)))
	 (incf lv)
	 (cond
	   ((= lv 3)
	    (setf skill (append skill (list (nth 1 *skill-list*)))))
	   ((= lv 5)
	    (setf skill (append skill (list (nth 2 *skill-list*)))))
	   ((= lv 7)
	    (setf skill (append skill (list (nth 3 *skill-list*)))))
	   ((= lv 9)
	    (setf skill (append skill (list (nth 4 *skill-list*)))))
	   ((= lv 10)
	    (setf skill (append skill (list (nth 5 *skill-list*))))))
	 (when (> max-exp exp)
	   (setf explore-state :player-move
		 cursor 0)))))))

;;ゲームオーバー画面
(defun update-gameover (hwnd)
  (with-slots (up down keyz) *keystate*
    (with-slots (cursor) *p*
      (cond
	(up
	 (if (= cursor 0)
	     (incf cursor 2)
	     (decf cursor)))
	(down
	 (if (= cursor 2)
	     (decf cursor 2)
	     (incf cursor)))
	(keyz
	 (if (= cursor 0)
	     (init-game)
	     (send-message hwnd (const +wm-close+) nil nil)))))))
	     

;;アイテムゲット画面操作
(Defun update-get-item ()
  (with-slots (keyz keyx) *keystate*
    (with-slots (explore-state) *p*
    (when (or keyx keyz)
      (setf explore-state :player-move)))))


;;武器取得画面操作更新
(defun update-get-weapon ()
  (with-slots (up down keyz keyx) *keystate*
    (with-slots (cursor explore-state) *p*
      (cond
	((or up down)
	 (if (= cursor 0)
	     (incf cursor)
	     (decf cursor)))
	(keyx
	 (setf explore-state :player-move
	       cursor 0))
	(keyz
	 (when (= cursor 0)
	   (equip-get-weapon))
	 (setf explore-state :player-move
	       cursor 0))))))

;;敵が消えるの松

;;バトル時のプレイヤーターン更新
(defun update-battle-player-turn (donjon)
  (with-slots (battle-state) *p*
    (case battle-state
      (:action-select
       (update-action-select))
      (:enemy-select
       (update-enemy-select))
      (:attack-animation
       (update-attack-animation donjon)))))
      ;;(:no-monsters
      ;; (unless (donjon/battle-monsters donjon)
	;; (setf battle-state :battle-end))))))

;;バトル時の敵の行動更新------------------------------------------------------------------------
;;スケルトン
(defmethod update-battle-monster-action ((skelton skelton))
  (with-slots (str) skelton
    (create-player-damaged-data (randval str) (encode-rgb 255 255 255) :hp)))

;;ドラゴン
(defmethod update-battle-monster-action ((dragon dragon))
  (with-slots (str agi hp) dragon
    (case (random 3)
      (0 (create-player-damaged-data (randval (floor (* str 0.8))) (encode-rgb 125 125 125) :hpstr))
      (1 (create-player-damaged-data (randval (floor (* 0.8 agi))) (encode-rgb 255 125 0) :stragi))
      (2 (create-player-damaged-data (randval (floor (* 0.8 hp))) (encode-rgb 0 125 255) :hpagi)))))

;;ヒドラ
(defmethod update-battle-monster-action ((hydra hydra))
  (with-slots (hp) hydra
    (incf hp)
    (create-player-damaged-data (randval (floor (* hp 0.7))) (encode-rgb 255 255 255) :hp)))

;;バブル
(defmethod update-battle-monster-action ((bubble bubble))
  (with-slots (str agi) bubble
    (create-player-damaged-data (randval (floor (+ str agi) 3)) (encode-rgb 125 255 0) :hpstragi)))
    
;;オーク
(defmethod update-battle-monster-action ((orc orc))
  (with-slots (str) orc
    (if (>= (random 3) 1)
	(create-player-damaged-data (randval (floor (* 0.9 str))) (encode-rgb 255 255 255) :hp)
	(if (>= (random 3) 1)
	    (create-player-damaged-data "ミス" (encode-rgb 255 255 255) :hp)
	    (create-player-damaged-data (floor (randval (* str 1.4))) (encode-rgb 255 255 0) :hp)))))
;;スライム
(defmethod update-battle-monster-action ((slime slime))
  (with-slots (str agi) slime
    (if (= (random 2) 0)
	(create-player-damaged-data (1+ (randval agi)) (encode-rgb 255 0 255) :agi)
	(create-player-damaged-data (randval (floor str 2)) (encode-rgb 255 255 255) :hp))))

;;brigand
(defmethod update-battle-monster-action ((brigand brigand))
  (with-slots (str agi) brigand
    (case (random 3)
      (0 (create-player-damaged-data (1+ (floor (randval str) 2)) (encode-rgb 255 255 255) :hp))
      (1 (create-player-damaged-data (1+ (floor (randval str) 3)) (encode-rgb 0 255 255) :str))
      (2 (create-player-damaged-data (1+ (floor (randval agi) 2)) (encode-rgb 255 0 255) :agi)))))

;;yote1
(defmethod update-battle-monster-action ((yote1 yote1))
  (with-slots (str agi) yote1
    (create-player-damaged-data (randval (floor str 2)) (encode-rgb 255 255 255) :hp)))

;;モゲモス
(defmethod update-battle-monster-action  ((mogemos mogemos))
  (with-slots (str agi) mogemos
    (case (random 3)
      (0 (create-player-damaged-data (1+ (floor (randval str) 3)) (encode-rgb 255 255 255) :hp))
      (1 (create-player-damaged-data (1+ (floor (randval str) 4)) (encode-rgb 0 255 255) :str))
      (2 (create-player-damaged-data (1+ (floor (randval agi) 6)) (encode-rgb 255 0 255) :agi)))))
;;--------------------------------------------------------------------------------------------------------

;;プレイヤーのHP減らす
(defun decrease-player-status (dmg)
  (with-slots (region num) dmg
    (with-slots (str agi hp) *p*
      (when (numberp num)
	(case region
	  (:hp (setf hp (max 0 (- hp num))))
	  (:str (setf str (max 1 (- str num))))
	  (:agi (setf agi (max 1 (- agi num))))
	  (:hpstr (setf hp (max 0 (- hp num))
			str (max 0 (- str num))))
	  (:stragi (setf str (max 0 (- str num))
			 agi (max 0 (- agi num))))
	  (:hpagi (setf hp (max 0 (- hp num))
			agi (max 0 (- agi num))))
	  (:hpstragi (setf hp (max 0 (- hp num))
			   str (max 0 (- str num))
			   agi (max 0 (- agi num)))))))))

;;バトル時の敵のターン更新
(Defun update-battle-monsters-turn (donjon)
  (with-slots (damage battle-state damage-flag attack-num damage-sound max-attack-num agi hp) *p*
    (with-slots (battle-monsters) donjon
      (setf dir *guard*)
      (loop :for monster :in battle-monsters
            :do (with-slots (kind agi) monster
		  (let ((attack-num (if (eq kind *mogemos*) (max 3 (floor agi 30)) 1)))
		    (loop :repeat attack-num
			  :do
			     (let* ((dmg (update-battle-monster-action monster)))
			       (decrease-player-status dmg)
			       (push dmg damage))))))
      (setf damage-flag t
	    battle-state :action-select
	    damage-sound (length damage))
      ;;(sound-play *damage-wav* )
      (if (<= hp 0)
	  (setf (game/battle-state *game*) :wait-gameover)
	  (setf (game/battle-state *game*) :player-turn
		max-attack-num (1+ (truncate (/ (max 0 agi) 15))))))))

;;ダメーじ受けた効果音
(defun play-damaged-sound ()
  (with-slots (frame) *game*
    (with-slots (damage-sound) *p*
      (when (and (> damage-sound 0)
		 (zerop (mod frame 7)))
	(sound-play *damage-wav*)
	(decf damage-sound)))))
          

;;プレイヤーの状態画像
(defun update-player-wait-img ()
  (with-slots (damage-flag dir) *p*
    (cond
      (damage-flag (setf dir *guard*))
      (t (setf dir *left*)))))

;;ダメージ消えるまで待機
(defun wait-end-damage ()
  (with-slots (damage-flag) *p*
    (unless damage-flag
      (setf (game/state *game*) :gameover))))

;;バトル関係更新
(defun update-battle ()
  (with-slots (battle-state donjon) *game*
    (update-player-wait-img)
    (case battle-state
      (:player-turn
       (update-battle-player-turn donjon))
      (:enemy-turn
       (update-battle-monsters-turn donjon))
      (:wait-gameover
       (wait-end-damage)))
    (update-player-damage-timer)
    (update-enemy-damage-timer)
    ;;(delete-dead-monsters)
    (play-damaged-sound)
    (battle-end? donjon)))


;;----------------------------------------------------------
(defun increase-frame ()
  (with-slots (frame) *game*
    (incf frame)
    (when (= frame 1000)
      (setf frame 0))))


;;ゲームループ
(defun main-game-loop (hwnd)
  (with-slots (state donjon) *game*
    (case state
      (:gameclear
       (update-gameover hwnd))
      (:title
       (update-gameover hwnd))
      (:gameover
       (update-gameover hwnd))
      (:battle
        (update-battle))
        ;;(debug-battle-monsters donjon))
      (:explore
       (with-slots (explore-state) *p*
	 (cond
	   ((eq explore-state :get-weapon)
	    (update-get-weapon))
	   ((or (eq explore-state :get-potion)
		(eq explore-state :get-hammer)
		(eq explore-state :mogezou-sword))
	    (update-get-item))
	   ((eq explore-state :level-up)
	    (update-level-up))
	   (t
            (update-explore donjon))))))
    (increase-frame)
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
  (load-images)
  ;;(init-game)
  (create-game)
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
                    (main-game-loop hwnd)
                    (invalidate-rect hwnd nil nil)))))
            (msg-lparam msg))
        (progn
        ;; (delete-font)
        ;; (delete-imgs)
        ;; (delete-brush)
         (unregister-class myclass))))))
