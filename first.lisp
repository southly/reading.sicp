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

;;; 問題1.1
10
;=> 10
(+ 5 3 4)
;=> 12
(- 9 1)
;=> 8
(/ 6 2)
;=> 3
(+ (* 2 4) (- 4 6))
;=> 6
(defparameter a 3)
;=> A
(defparameter b (+ a 1))
;=> B
(+ a b (* a b))
;=> 19
(= a b)
;=> NIL
(if (and (> a b) (< b (* a b)))
    b
    a)
;=> 3
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (t 25))
;=> 16
(+ 2 (if (> b a) b a))
;=> 6
(* (cond ((> a b) a)
         ((< a b) b)
         (t -1))
   (+ a 1))
;=> 16

;;; 問題1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))
;=> -37/150

;;; 問題1.3
(defun foo (x y z)
  (cond ((and (< x y) (< x z))
         (+ (* y y) (* z z)))
        ((and (< y x) (< y z))
         (+ (* x x) (* z z)))
        (t
         (+ (* x x) (* y y)))))
;=> FOO
(foo 1 2 3)
;=> 13
(foo 2 1 3)
;=> 13
(foo 2 3 1)
;=> 13

;;; 問題1.4
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))
;=> A-PLUS-ABS-B
(a-plus-abs-b 1 9)
;=> 10
(a-plus-abs-b 1 -9)
;=> 10

;;; 問題1.5
(defun p ()
  (p))
;=> P
(defun test (x y)
  (if (= x 0)
      0
      y))
;=> TEST
(test 0 (p))                            ; 無限ループ
                                        ; (trace p) としておけば無限ループになっているのがよく分かる

(defmacro q ()
  '(q))
;=> Q
(defmacro test-q (x y)
  `(if (= ,x 0)
       0
       ,y))
;=> TEST-Q
(test-q 0 (q))
;=> 0

;;; 1.1.7 例: Newton法による平方根
(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
;=> SQRT-ITER
(defun improve (guess x)
  (average guess (/ x guess)))
;=> IMPROVE
(defun average (x y)
  (/ (+ x y) 2))
;=> AVERAGE
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
;=> GOOD-ENOUGH?
(defun square (x)
  (* x x))
;=> SQUARE
(defun |sqrt| (x)
  (sqrt-iter 1.0 x))
;=> |sqrt|

(|sqrt| 9)
;=> 3.0000916
(|sqrt| (+ 100 37))
;=> 11.7047
(|sqrt| (+ (|sqrt| 2) (|sqrt| 3)))
;=> 1.7739279
(square (|sqrt| 1000))
;=> 1000.0004

;;; 問題1.6
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (t else-clause)))
;=> NEW-IF
(new-if (= 2 3) 0 5)
;=> 5
(new-if (= 1 1) 0 5)
;=> 0
(defun sqrt-iter (guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
; new-ifではすべての引数が評価されるので
; 必ず sqrt-iter が呼ばれることになり
; 無限にループする

;;; 問題1.7
(mapcar (lambda (x)
          (list x (sqrt x) (|sqrt| x)))
        '(1.0e-3 1.0e-6 1.0e-9 1.0e-12))
;=>  ((0.001 0.03162278 0.041245427) 
;     (1.e-6 0.001 0.031260654)
;     (1.e-9 3.1622778e-5 0.03125001) 
;     (1.e-12 1.e-6 0.03125))
; 絶対値の小さい数については機能しないということ

(mapcar (lambda (x)
          (list x (sqrt x) (|sqrt| x)))
        '(1.0e3 1.0e4 1.0e5 1.0e6))
; 絶対値の大きな数については桁落ちの誤差で求められない

(defun sqrt-iter (next guess x)
  (if (good-enough? next guess)
      guess
      (sqrt-iter (improve next x)
                 next
                 x)))
;=> SQRT-ITER
(defun good-enough? (new old)
  (< (/ (abs (- new old)) old) 0.001))
;=> GOOD-ENOUGH?
(defun |sqrt| (x)
  (sqrt-iter 1.0 x x))
;=> |sqrt|
(|sqrt| 9)
;=> 3.0000916
(|sqrt| (+ 100 37))
;=> 11.705106
(|sqrt| (+ (|sqrt| 2) (|sqrt| 3)))
;=> 1.7739279
(square (|sqrt| 1000))
;=> 1001.21716
(mapcar (lambda (x)
          (list x (sqrt x) (|sqrt| x)))
        '(1.0e-3 1.0e-6 1.0e-9 1.0e-12))
;=> ((0.001 0.03162278 0.031642016)
;    (1.e-6 0.001 0.0010005538)
;    (1.e-9 3.1622778e-5 3.1638658e-5)
;    (1.e-12 1.e-6 1.000455e-6))
(mapcar (lambda (x)
          (list x (sqrt x) (|sqrt| x)))
        '(1.0e3 1.0e4 1.0e5 1.0e6))
;=> ((1000.0 31.622776 31.642015)
;    (10000.0 100.0 100.00714)
;    (100000.0 316.22775 316.22925)
;    (1000000.0 1000.0 1000.55383))

; 誤差が大き過ぎかな
; 0.001 じゃ大きかったか

;;; 問題1.8
(defun cubic-root-iter (next guess x)
  (if (good-enough? next guess)
      guess
      (cubic-root-iter (improve-cubic next x)
                       next
                       x)))
;=> CUBIC-ROOT-ITER
(defun improve-cubic (guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))
;=> IMPROVE-CUBIC
(defun cubic-root (x)
  (cubic-root-iter 1.0 x x))
;=> CUBIC-ROOT
(cubic-root 8)
;=> 2.000005
(cubic-root 27)
;=> 3.0012743

