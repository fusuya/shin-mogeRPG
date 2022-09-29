(in-package :casket2022aki)


;;文字幅取得
(defun moge-char-width (char)
    (if (<= #x20 (char-code char) #x7e)
        1
	2))
;;string全体の文字幅
(defun string-width (string)
  (apply #'+ (map 'list #'moge-char-width string)))
;;最低n幅もったstring作成
(defun minimum-column (n string)
  (let ((pad (- n (string-width string))))
    (if (> pad 0)
	(concatenate 'string string (make-string pad :initial-element #\ ))
        string)))

(defun fuchidori-text (x y str hdc &key (textcolor (encode-rgb 0 0 0)) (fuchicolor (encode-rgb 255 255 255)))
  (set-text-color hdc fuchicolor)
  (text-out hdc str (+ x 2) y)
  (text-out hdc str (- x 2) y)
  (text-out hdc str x (+ y 2))
  (text-out hdc str x (- y 2))
  (set-text-color hdc textcolor)
  (text-out hdc str x y))


(defun render-bar (hdc left top bot hp-right decreased-hp-right &key (hp-color +green+)
                                                                     (dec-hp-color +red+))
  ;;残りHP
  (select-object hdc (aref *brush* hp-color))
  (rectangle hdc left top hp-right bot)
  ;;減ったHP
  (select-object hdc (aref *brush* dec-hp-color))
  (rectangle hdc hp-right top decreased-hp-right bot))

;;HPバー表示
(defun calc-bar (left bot max-w bar-h nownum maxnum hdc)
  (let* ((nowhp (floor (* (/ nownum maxnum) max-w)))
         (top (- bot bar-h))
         (hp-right (+ left nowhp))
         (decreased-hp (- max-w nowhp))
         (decreased-hp-right (+ hp-right decreased-hp)))
    (render-bar hdc left top bot hp-right decreased-hp-right)))


(defun trans-blt (x y x-src y-src w-src h-src w-dest h-dest hdc hmemdc)
  (transparent-blt hdc x y hmemdc x-src y-src :width-source w-src
       :height-source h-src
       :width-dest w-dest :height-dest h-dest
       :transparent-color (encode-rgb 0 255 0)))


(defun render-monster (monster hdc hmemdc)
  (select-object hmemdc *monsters-img*)
  (with-slots (kind drawx drawy walk-img w h w2 h2) monster
    (trans-blt drawx drawy (* walk-img w) kind w h w2 h2 hdc hmemdc)))


(defun render-monsters (donjon hdc hmemdc)
  (with-slots (monsters) donjon
    (loop :for monster :in monsters
          :do (render-monster monster hdc hmemdc))))


;; y : up 64 down 96 left 32 right 0
(defun render-player (hdc hmemdc)
  (with-slots (posx posy dir drawx drawy walk-img) *p*
    (select-object hmemdc *p-walk-img*)
    (let ((adj 4)
          (w 24) (h 32))
      (trans-blt (+ drawx adj) drawy (* walk-img w) dir w h w h hdc hmemdc))))


;;bロック壊れた画像表示
(defun render-break-block (hdc hmemdc)
  (with-slots (breakblock) *p*
    (loop :for blk :in breakblock
	  :do (with-slots (drawx drawy break-img) blk
		(select-object hmemdc *break-block-img*)
		(trans-blt (- drawx 48) (- drawy 48) (* 128 break-img) 0
			   128 128 128 128 hdc hmemdc)))))


(defun render-obj-img (x y cell item hdc hmemdc)
  (select-object hmemdc *objs-img*)
  (let ((posx (* x *cell-size*))
        (posy (* y *cell-size*)))
    (trans-blt posx posy (* cell *cell-size*) 0 *cell-size* *cell-size* *cell-size* *cell-size* hdc hmemdc)
    (when item
      (trans-blt posx posy (* item *cell-size*) 0 *cell-size* *cell-size* *cell-size* *cell-size* hdc hmemdc))))


;;ステージのオブジェクト表示
(defun render-stage (donjon hdc hmemdc)
  (with-slots (stage) donjon
    (loop :for y :from 0 :below *h-cell-num*
          :do (loop :for x :from 0 :below *w-cell-num*
                    :do (with-slots (cell item breaked break-img) (aref stage y x)
                          (render-obj-img x y cell item hdc hmemdc))))))
			  ;;(when (> breaked 0)
			  ;;  (render-break-block x y break-img hdc hmemdc)))))))

(defun render-donjon (donjon hdc hmemdc)
  (render-stage donjon hdc hmemdc)
  (render-break-block hdc hmemdc))

(defun render-test (hdc)
  (with-slots (posx posy drawx drawy dir maxhp maxstr maxagi) *p*
    (select-object hdc *font40*)
    (set-text-color hdc (encode-rgb 255 0 0))
    (text-out hdc (format nil "dir:~d" dir) *donjon-w* 0)
    (text-out hdc (format nil "posx:~d" posx) *donjon-w* 40)
    (text-out hdc (format nil "posy:~d" posy) *donjon-w* 80)
    (text-out hdc (format nil "drawx:~d" drawx) *donjon-w* 120)
    (text-out hdc (format nil "drawy:~d" drawy) *donjon-w* 160)
    (text-out hdc (format nil "maxhp:~d" maxhp) *donjon-w* 200)
    (text-out hdc (format nil "maxstr:~d" maxstr) *donjon-w* 240)
    (text-out hdc (format nil "maxagi:~d" maxagi) *donjon-w* 280)))

;;ステータス表示
(defun render-status (donjon hdc hmemdc)
  (with-slots (floor-num) donjon
    (with-slots (posx posy drawx drawy dir hp str agi maxhp maxstr maxagi potion hammer weapon exp max-exp lv) *p*
      (let ((font (create-font "ＭＳ ゴシック" :height 28))
	    (font2 (create-font "ＭＳ ゴシック" :height 23)))
	(select-object hdc font)
	(set-text-color hdc (encode-rgb 155 255 200))
	(text-out hdc (format nil "地下~2,'0d階" floor-num) *donjon-w* 5)
	(set-text-color hdc (encode-rgb 115 155 255))
	(text-out hdc "ステータス" *donjon-w* 45)
	(set-text-color hdc (encode-rgb 255 255 255))
	(text-out hdc (format nil " LV  ~2d" lv) *donjon-w* 80)
	(text-out hdc (format nil " HP ~3d/~d" hp maxhp) *donjon-w* 110)
	(text-out hdc (format nil "STR ~3d/~d" str maxstr) *donjon-w* 140)
	(text-out hdc (format nil "AGI ~3d/~d" agi maxagi) *donjon-w* 170)
	(text-out hdc (format nil "x ~d" potion) 840 210)
	(text-out hdc (format nil "x ~d" hammer) 840 255)
	(text-out hdc (format nil "現在の武器:~A" (weapon/name weapon)) 500 610)
	(set-text-color hdc (encode-rgb 212 254 44))
	(text-out hdc (format nil "exp ~3d/~d" exp max-exp) 800 300)
	(set-text-color hdc (encode-rgb 0 254 0))
	(select-object hdc font2)
	(text-out hdc "Aキーで回復薬使用" 500 640)
	(text-out hdc "Xキーでハンマー使用(壁壊せる)" 500 665)
	(text-out hdc "shift+方向キーでダッシュ" 500 690)
	(set-text-color hdc (encode-rgb 220 24 120))
	(text-out hdc "zキー:決定" 250 665)
	(text-out hdc "xキー:キャンセル" 250 690)
	(calc-bar 805 350 150 20 exp max-exp hdc)
	(select-object hmemdc *objs-img*)
	(trans-blt *donjon-w* 210 (* +potion-img+ 32) 0 32 32 32 32 hdc hmemdc)
	(trans-blt *donjon-w* 250 (* +hammer+ 32) 0 32 32 32 32 hdc hmemdc)
	(delete-object font)
	(delete-object font2)))))

;;枠
(defun render-waku (x y w h hdc hmemdc)
  (select-object hmemdc *waku-img*)
  (trans-blt x y 0 0 128 128 w h hdc hmemdc))

;;枠黒塗りつぶし
(defun render-waku-black (x y w h hdc hmemdc)
  (select-object hmemdc *waku-black*)
  (trans-blt x y 0 0 128 128 w h hdc hmemdc))

;;もげぞーの剣イベント
(defun render-mogezou-sword-event (hdc hmemdc)
  (let ((font (create-font "ＭＳ ゴシック" :height 28)))
    (select-object hdc font)
    (render-waku-black 190 200 420 100 hdc hmemdc)
    (set-text-color hdc (encode-rgb 155 225 125))
    (text-out hdc "もげぞーの剣が輝きだし" 240 210)
    (set-text-color hdc (encode-rgb 148 85 225))
    (text-out hdc "ｼﾝもげぞうの光剣に進化した！" 210 250)
    
    (delete-object font)))

;;アイテムゲット
(defun render-item-get-window (hdc hmemdc str)
  (let ((font (create-font "ＭＳ ゴシック" :height 28)))
    (select-object hdc font)
    (render-waku-black 200 200 400 100 hdc hmemdc)
    (set-text-color hdc (encode-rgb 255 255 255))
    (text-out hdc (format nil "~Aを手に入れた!" str) 240 230)
    (delete-object font)))
  
			       
;;レベルアップ画面
(defun render-level-up-window (hdc hmemdc)
  (with-slots (maxhp maxagi maxstr cursor lv) *p*
    (let ((font (create-font "ＭＳ ゴシック" :height 28)))
      (select-object hdc font)
      (set-text-color hdc (encode-rgb 255 255 255))
      (render-waku-black 180 100 600 280 hdc hmemdc)
      (set-text-color hdc (encode-rgb 255 100 255))
      (text-out hdc "レベルアップ！" 370 110)
      (set-text-color hdc (encode-rgb 95 115 195))
      (text-out hdc (format nil "Lv ~d → LV ~d" lv (1+ lv)) 380 140)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc "上昇させたいステータスを選んでください" 210 170)
      (loop :for i :from 0 :to 2
	    :for y :in '(210 250 290)
	    :for x = 360
	    :do
	       (if (= i cursor)
		   (progn
		     (select-object hdc (aref *brush* +white+))
		     (rectangle hdc x y (+ x 250) (+ y 30))
		     (set-text-color hdc (encode-rgb 0 0 0)))
		   (progn
		     (set-text-color hdc (encode-rgb 255 255 255))))
	       (cond
		 ((= i 0)
		  (text-out hdc (format nil "最大HP ~d → ~d" maxhp (+ maxhp 3)) x y))
		 ((= i 1)
		  (text-out hdc (format nil "最大STR ~d → ~d" maxstr (+ maxstr 1)) x y))
		 ((= i 2)
		  (text-out hdc (format nil "最大AGI ~d → ~d" maxagi (+ maxagi 1)) x y))))
      (set-text-color hdc (encode-rgb 90 175 90))
      (cond
	((= lv 2) (text-out hdc "二段斬りを覚えた！" 350 330))
	((= lv 4) (text-out hdc "なぎ払いを覚えた！" 350 330))
	((= lv 6) (text-out hdc "乱れ斬りを覚えた！" 350 330))
	((= lv 8) (text-out hdc "突撃を覚えた！" 350 330))
	((= lv 9) (text-out hdc "暗黒を覚えた！" 350 330)))
 
      (delete-object font))))

;;アイテムをゲットした時のメッセージウィンドウ
(defun render-weapon-get-window (hdc hmemdc)
  (with-slots (weapon get-item) *p*
    (let ((font (create-font "ＭＳ ゴシック" :height 28)))
      (select-object hdc font)
      (set-text-color hdc (encode-rgb 255 255 255))
      (render-waku-black 0 400 730 200 hdc hmemdc)
      (text-out hdc (format nil "~Aを手に入れた" (weapon/name get-item)) 20 410)
      (text-out hdc (format nil "得:~A 威力:~3d 増HP:~3D 増agi:~2d" (minimum-column 18 (weapon/name get-item)) (weapon/power get-item)  (weapon/inchp get-item) (weapon/incagi get-item)) 20 450)
      (text-out hdc (format nil "現:~A 威力:~3d 増HP:~3D 増agi:~2d" (minimum-column 18 (weapon/name weapon)) (weapon/power weapon)  (weapon/inchp weapon) (weapon/incagi weapon)) 20 480)
      (text-out hdc "装備しますか？" 20 550)
      (with-slots (diffpower diffhp diffagi) get-item
	(set-text-color hdc (cdr diffpower))
	(text-out hdc (format nil "~A" (car diffpower)) 385 510)
	(set-text-color hdc (cdr diffhp))
	(text-out hdc (format nil "~A" (car diffhp)) 525 510)
	(set-text-color  hdc (cdr diffagi))
	(text-out hdc (format nil "~A" (car diffagi)) 650 510)
	(delete-object font)))))

;;アイテム装備するか選択肢
(defun render-weapon-equip? (hdc hmemdc)
  (with-slots (cursor explore-state) *p*
    (let ((font (create-font "ＭＳ ゴシック" :height 28)))
      (select-object hdc font)
      (render-waku-black 810 460 140 120 hdc hmemdc)
      (loop :for i :from 0
	    :for str :in '("はい" "いいえ")
	    :for y :in '(480 520)
	    :for x = 830
	    :do
	       (if (and (= i cursor) (eq explore-state :get-weapon))
		   (progn
		     (select-object hdc (aref *brush* +white+))
		     (rectangle hdc x y (+ x 90) (+ y 30))
		     (set-text-color hdc (encode-rgb 0 0 0))
		     (text-out hdc (format nil "~a" str) x y))
		   (progn
		     (set-text-color hdc (encode-rgb 255 255 255))
		     (text-out hdc (format nil "~a" str) x y))))
      (delete-object font))))

;;バトル時のプレーヤーのステータス表示
(defun render-battle-player-status (hdc hmemdc)
  (with-slots (hp maxhp str maxstr agi maxagi potion) *p*
    (let ((font (create-font "ＭＳ ゴシック" :height 27)))
      (render-waku 470 600 500 128 hdc hmemdc)
      (select-object hdc font)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc "You" 490 605)
      (text-out hdc (format nil "HP ~3d/~d" hp maxhp) 600 605)
      (calc-bar 750 625 200 15 hp maxhp hdc)
      (text-out hdc (format nil "STR ~3d/~d" str maxstr) 585 635)
      (text-out hdc (format nil "AGI ~3d/~d" agi maxagi) 585 665)
      (text-out hdc "回復薬:Ａキー" 490 690)
      ;;(fuchidori-text 490 640 "もげ" hdc :textcolor (encode-rgb 255 255 255) :fuchicolor (encode-rgb 255 255 0))
      (text-out hdc (format nil "x ~d" potion) 720 690)
      (select-object hmemdc *objs-img*)
      (trans-blt 680 690 (* 32 +potion-img+) 0 32 32 32 32 hdc hmemdc)
      (delete-object font))))

;;バトル時のプレイヤー待機画像描画
(defun render-battle-wait-player (hdc hmemdc)
  (with-slots (drawy drawx dir) *p*
    (select-object hmemdc *p-walk-img*)
    (let ((w 24) (h 32))
      (trans-blt drawx drawy 0 dir w h (* w 2) (* h 2) hdc hmemdc))))

;;暗黒アニメーション
(defun render-ankoku-animation (walk-img drawy hdc hmemdc)
  (select-object hmemdc *p-atk-img*)
  (let ((w 48) (h 32))
    (trans-blt 750 drawy (* walk-img w) 32 w h (* w 2) (* h 2) hdc hmemdc)))

;;突撃のアニメーション
(defun render-assault-animation (walk-img drawy hdc hmemdc)
  (select-object hmemdc *p-atk-img*)
  (let ((w 48) (h 32))
    (trans-blt 750 drawy (* walk-img w) 0 w h (* w 2) (* h 2) hdc hmemdc)))

;;通常の攻撃アニメーション
(defun render-normal-atk-animation (walk-img drawy hdc hmemdc)
  (select-object hmemdc *p-atk-img*)
  (let ((w 48) (h 32))
    (trans-blt 750 drawy (* walk-img w) 0 w h (* w 2) (* h 2) hdc hmemdc)))


;;ポーションのアニメ
(defun render-potion-animation (walk-img drawy hdc hmemdc)
  (select-object hmemdc *p-walk-img*)
  (let ((w 24) (h 32))
    (trans-blt 800 drawy (* walk-img w) *heal* w h 48 64 hdc hmemdc)))

;;プレイヤーの攻撃アニメーション
(defun render-battle-atk-player (selected-skill walk-img drawy hdc hmemdc)
  (cond
    ((= selected-skill +potion+)
     (render-potion-animation walk-img drawy hdc hmemdc))
    ((= selected-skill +assault+)
     (render-assault-animation walk-img drawy hdc hmemdc))
    ((= selected-skill +ankoku+)
     (render-ankoku-animation walk-img drawy hdc hmemdc))
    (t
     (render-normal-atk-animation walk-img drawy hdc hmemdc))))

;;敵のダメージ表示
(defun render-damage (damage hdc)
  (with-slots (drawx drawy num color) damage
    (let ((font (create-font "MSゴシック" :height 28)))
      (select-object hdc font)
      ;;(fuchidori-text drawx drawy (format nil "~2a" num) hdc
      ;;                :fuchicolor (encode-rgb 255 255 255)
      ;;               :textcolor color)
      (set-text-color hdc color)
      (text-out hdc (format nil "~2a" num) drawx drawy)
      (delete-object font))))

;;斬られるアニメーション
(defun render-enemy-slashed (hdc hmemdc)
  (with-slots (walk-img selected-enemy) *p*
      (with-slots (battle-monsters) (game/donjon *game*)
	(select-object hmemdc *slash-img*)
	(let* ((target (nth selected-enemy battle-monsters)))
	  (when target
	    (with-slots (drawx drawy w h w2 h2) target
              (trans-blt drawx drawy (* walk-img w) 0 32 32 w2 h2 hdc hmemdc)))))))

;;薙ぎ払いエフェクト
(defun render-enemy-swinged (hdc hmemdc)
  (with-slots (walk-img) *p*
    (select-object hmemdc *slash-img*)
    (let* ((w 32) (h 32))
      (trans-blt 30 100 (* walk-img w) 0 w h (* w 12) (* h 12) hdc hmemdc))))

;;暗黒のエフェクト
(defun render-enemy-ankoku (hdc hmemdc)
  (with-slots (walk-img walk-num ankoku-img) *p*
    (when (> walk-img 0)
      (when (zerop (mod walk-num 5))
        (incf ankoku-img)
        (when (>= ankoku-img 4)
          (setf ankoku-img 0)))
      (select-object hmemdc *ankoku-img*)
      (let ((w 32) (h 32) (x 920))
        (loop :repeat 3
              :for y :from 0
              :do
                 (let ((x2 (- (+ x (* y 40)) (+ (* walk-num 14) (* walk-img 60)))))
                   (trans-blt x2 (+ 290 (* y 80)) (* ankoku-img 32) 0 w h (* w 2) (* h 2) hdc hmemdc)
                   (trans-blt x2 (- 290 (* y 80)) (* ankoku-img 32) 0 w h (* w 2) (* h 2) hdc hmemdc)))))))

;;敵が斬られるエフェクト
(defun render-enemy-damage-effect (hdc hmemdc)
  (with-slots (selected-skill) *p*
    (cond
      ((or (= selected-skill +slash+)
           (= selected-skill +double-slash+))
       (render-enemy-slashed hdc hmemdc))
      ((= selected-skill +ankoku+)
       (render-enemy-ankoku hdc hmemdc))
      ((= selected-skill +swing+)
       (render-enemy-swinged hdc hmemdc)))))

;;バトル時のプレイヤー描画
(defun render-battle-player (hdc hmemdc)
  (with-slots (battle-state walk-img damage drawy selected-skill) *p*
    (case battle-state
      (:attack-animation
       (render-battle-atk-player selected-skill walk-img drawy hdc hmemdc)
       (render-enemy-damage-effect hdc hmemdc))
      (t (render-battle-wait-player hdc hmemdc)))
    (loop :for dmg :in damage
          :do (render-damage dmg hdc))))


;;バトルアクション選択画面描画
(defun render-player-action (hdc hmemdc)
  (with-slots (skill cursor battle-state) *p*
    (let ((font (create-font "MSゴシック" :height 33))
          (skill-pos '((15 610) (15 650) (15 690)
                       (200 610) (200 650) (200 690))))
      (render-waku 0 600 480 128 hdc hmemdc)
      (select-object hdc font)
      (loop :for skl :in skill
            :for pos :in skill-pos
            :for i :from 0
            :do
             (let ((x (car pos))
                   (y (cadr pos))
                   (name (skill/name skl)))
               (if (and (= i cursor) (eq battle-state :action-select))
                   (progn
                     (select-object hdc (aref *brush* +white+))
                     (rectangle hdc x y (+ x 180) (+ y 35))
                     (set-text-color hdc (encode-rgb 0 0 0))
                     (text-out hdc (format nil "~a" name) x y))
                   (progn
                    (set-text-color hdc (encode-rgb 255 255 255))
                    (text-out hdc (format nil "~a" name) x y)))))
      (delete-object font))))

(defun render-monster-lv (x y lv hdc)
  (select-object hdc *font20*)
  (set-text-color hdc (encode-rgb 255 255 255))
  (text-out hdc (format nil "Lv:~d" lv) x y))

;;バトル時の敵画像描画
(defun render-battle-monsters (donjon hdc hmemdc)
  (with-slots (battle-monsters) donjon
    (with-slots (cursor battle-state) *p*
      (loop :for monster :in battle-monsters
            :for i :from 0
            :do (with-slots (drawx drawy lv damage hp h2 w2 hp maxhp) monster
                  (let ((hpbar-x drawx) (hpbar-y (+ drawy h2 13))
                        (lv-x (+ drawx 12)) (lv-y (- drawy 23)))
                    (when (and (= i cursor) (eq battle-state :enemy-select))
                      (select-object hdc (aref *brush* +white+))
                      (rectangle hdc drawx drawy (+ drawx w2) (+ drawy h2)))
                    (render-monster monster hdc hmemdc)
                    (calc-bar hpbar-x hpbar-y w2 8 hp maxhp hdc)
                    (render-monster-lv lv-x lv-y lv hdc)
                    (when damage
                      (render-damage damage hdc))
                    ))))))



(defun render-battle (donjon hdc hmemdc)
  (render-battle-player-status hdc hmemdc)
  (render-player-action hdc hmemdc)
  (render-battle-monsters donjon hdc hmemdc)
  (render-battle-player hdc hmemdc))

;;現在の階層を表示
(defun render-floor-num (hdc donjon)
  (with-slots (floor-num) donjon
    (let ((font (create-font "ＭＳ ゴシック" :height 28)))
      (select-object hdc font)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc (format nil "地下~d階" floor-num) 10 610)
      (delete-object font))))

;;現在の経過時間表示
(defun render-time (hdc start-time)
  (multiple-value-bind (h m s ms) (get-hms (- (get-internal-real-time) start-time))
    (let ((font (create-font "ＭＳ ゴシック" :height 28)))
      (select-object hdc font)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc "経過時間" 10 610)
      (set-text-color hdc (encode-rgb 255 255 255))
      (text-out hdc (format nil "~2,'0d:~2,'0d:~2,'0d:~2,'0d" h m s ms) 10 650)
      (delete-object font))))

;;再出撃かやめるか選択し部分
(defun restart-or-quit (hdc hmemdc)
  (with-slots (cursor) *p*
    (let ((font (create-font "ＭＳ ゴシック" :height 58))
	  (str-list (if (eq (game/state *game*) :title) '("出　撃" "やめる") '("再出撃" "やめる"))))
      (select-object hdc font)
      (render-waku-black 380 500 240 200 hdc hmemdc)
      (loop :for i :from 0
	    :for str :in str-list
	    :for y :in '(520 620)
	    :for x = 400
	    :do
	       (if (= i cursor)
		   (progn
		     (select-object hdc (aref *brush* +white+))
		     (rectangle hdc x y (+ x 190) (+ y 60))
		     (set-text-color hdc (encode-rgb 0 0 0))
		     (text-out hdc (format nil "~a" str) x y))
		   (progn
		     (set-text-color hdc (encode-rgb 255 255 255))
		     (text-out hdc (format nil "~a" str) x y))))
      (delete-object font))))

;;ゲームオーバー画面
(defun render-game-over (hdc hmemdc)
  (let ((font (create-font "ＭＳ ゴシック" :height 158))
	(title-str  (if (eq (game/state *game*) :title) "ｼﾝもげRPG" "GAME OVER")))
    (select-object hdc font)
    (fuchidori-text 130 100 title-str hdc :fuchicolor (encode-rgb 255 255 255) :textcolor (encode-rgb 255 0 0))
    (restart-or-quit hdc hmemdc)
    (delete-object font)))

;;ゲームクリア
(defun render-game-clear (hdc hmemdc)
  (with-slots (start-time end-time) *game*
    (let ((font (create-font "ＭＳ ゴシック" :height 120))
	  (font2 (create-font "ＭＳ ゴシック" :height 60))
	  (font3 (create-font "ＭＳ ゴシック" :height 30))
	  (n (- end-time start-time)))
      (multiple-value-bind (h m s ms) (get-hms n)
	(select-object hdc font)
	(fuchidori-text 10 100 "Congratulations!" hdc :fuchicolor (encode-rgb 0 0 200) :textcolor (encode-rgb 0 255 255))
	(select-object hdc font2)
	(fuchidori-text 300 250 "クリアタイム" hdc :fuchicolor (encode-rgb 120 60 200) :textcolor (encode-rgb 110 255 25))
	(fuchidori-text 300 350 (format nil "~2,'0d:~2,'0d:~2,'0d:~2,'0d" h m s ms) hdc :fuchicolor (encode-rgb 120 60 200) :textcolor (encode-rgb 110 255 25))
	(when (string= (weapon/name (player/weapon *p*)) "ｼﾝもげぞうの光剣")
	  (select-object hmemdc *p-walk-img*)
	  (trans-blt 10 450 (* *down* 32) 0 24 32 48 64 hdc hmemdc)
	  (select-object hdc font3)
	  (text-out hdc "<お前がナンバーワンだ" 60 450))
	(restart-or-quit hdc hmemdc)
	(delete-object font)
	(delete-object font2)
	(delete-object font3)
	))))


(defun render-game (hdc hmemdc)
  (with-slots (state donjon start-time) *game*
    (with-slots (explore-state) *P*
      (case state
	((:title :gameover)
	 (render-game-over hdc hmemdc))
	(:gameclear
	 (render-game-clear hdc hmemdc))
	(:battle
	 (render-battle donjon hdc hmemdc))
	(t
	 (render-donjon donjon hdc hmemdc)
	 (render-player hdc hmemdc)
	 (render-monsters donjon hdc hmemdc)
	 (when (eq :get-weapon explore-state)
	   (render-weapon-get-window hdc hmemdc)
	   (render-weapon-equip? hdc hmemdc))
	 (when (eq :get-potion explore-state)
	   (render-item-get-window hdc hmemdc "ポーション"))
	 (when (eq :get-hammer explore-state)
	   (render-item-get-window hdc hmemdc "ハンマー"))
	 (when (eq :mogezou-sword explore-state)
	   (render-mogezou-sword-event hdc hmemdc))
	 (when (eq :level-up explore-state)
	   (render-level-up-window hdc hmemdc))
	 (render-time hdc start-time)
	 (render-status donjon hdc hmemdc))))))
