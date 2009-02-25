;;; 1.1.1 式
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;=>57

;; インデントするなら
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
;; 冗長っぽいのは引数が必ず二つだからか

;;; 1.1.2 名前と環境
(setq size 2)
;=> 2
(* 5 size)
;=> 10

(setq |pi| 3.14159)
;=> 3.14159
(setq radius 10)
;=> 10

(setq circumference (* 2 |pi| radius))
;=> 62.831802

;; 大域環境とでてくるから defparameter の方が良かったか

;;; 1.1.4 合成手続き
(defun square (x) (* x x))
;=> SQUARE

(square 21)
;=> 441
(square (+ 2 5))
;=> 49
(square (square 3))
;=> 81

(defun sum-of-squares (x y)
  (+ (square x) (square y)))
;=> SUM-OF-SQUARES
(sum-of-squares 3 4)
;=> 25

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))
;=> F
(f 5)
;=> 136

;;; 1.1.5 手続き作用の置き換えモデル
; 正規順序の評価 (normal-order evalution) 「完全に展開し、簡約する」
;   inline展開とかマクロ展開はこっちかな
; 作用的順序の評価 (applicative-order evalution) 「引数を評価し、作用させる」
;   lispの関数呼び出しはこっち

;;; 1.1.6 条件式と述語
(defun |abs| (x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
;=> |abs|

(defun |abs| (x)
  (cond ((< x 0) (- x))
        (t x)))
;=> |abs|

(defun |abs| (x)
  (if (< x 0) 
      (- x)
      x))
;=> |abs|

(defpackage :sicp)
(defun sicp::>= (x y)
  (or (> x y) (= x y)))
;=> SICP::>=

(defun sicp::>= (x y)
  (not (< x y)))
;=> SICP::>=
