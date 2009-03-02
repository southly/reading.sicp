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

;;; 1.1.8 ブラックボックス抽象としての手続き
(defun square (x) (* x x))
;=> SQUARE
(defun square (x) (exp (double (log x))))
;=> SQUARE
(defun double (x) (+ x x))
;=> DOUBLE

(defun square (x) (* x x))
;=> SQUARE
(defun square (y) (* y y))
;=> SQUARE
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
;=> GOOD-ENOUGH?

(defun |sqrt| (x)
  (sqrt-iter 1.0 x))
;=> |sqrt|
(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
;=> SQRT-ITER
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
;=> GOOD-ENOUGH?
(defun improve (guess x)
  (average guess (/ x guess)))
;=> IMPROVE

(defun |sqrt| (x)
  (labels ((good-enough? (guess x)
             (< (abs (- (square guess) x)) 0.001))
           (improve (guess x)
             (average guess (/ x guess)))
           (sqrt-iter (guess x)
             (if (good-enough? guess x)
                 guess
                 (sqrt-iter (improve guess x) x)))
           (average (x y)
             (/ (+ x y) 2)))
    (sqrt-iter 1.0 x)))
;=> |sqrt|
(|sqrt| 2)
;=> 1.4142157

(defun |sqrt| (x)
  (labels ((good-enough? (guess)
             (< (abs (- (square guess) x)) 0.001))
           (improve (guess)
             (average guess (/ x guess)))
           (sqrt-iter (guess)
             (if (good-enough? guess)
                 guess
                 (sqrt-iter (improve guess))))
           (average (x y)
             (/ (+ x y) 2)))
    (sqrt-iter 1.0)))
;=> |sqrt|
(|sqrt| 2)
;=> 1.4142157

;;; 1.2 手続きとその生成するプロセス
;;; 線形再帰と反復
(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;=> FACTORIAL
(factorial 6)
;=> 720
(trace factorial)
;=> (FACTORIAL)
(factorial 6)
;;   0: (FACTORIAL 6)
;;     1: (FACTORIAL 5)
;;       2: (FACTORIAL 4)
;;         3: (FACTORIAL 3)
;;           4: (FACTORIAL 2)
;;             5: (FACTORIAL 1)
;;             5: FACTORIAL returned 1
;;           4: FACTORIAL returned 2
;;         3: FACTORIAL returned 6
;;       2: FACTORIAL returned 24
;;     1: FACTORIAL returned 120
;;   0: FACTORIAL returned 720
;=> 720
(untrace)
;=> T

(defun factorial (n)
  (fact-iter 1 1 n))
;=> FACTORIAL
(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;=> FACT-ITER
(factorial 6)
;=> 720
(trace fact-iter)
;=> (FACT-ITER)
;; (factorial 6)
;;   0: (FACTORIAL 6)
;;     1: (FACT-ITER 1 1 6)
;;       2: (FACT-ITER 1 2 6)
;;         3: (FACT-ITER 2 3 6)
;;           4: (FACT-ITER 6 4 6)
;;             5: (FACT-ITER 24 5 6)
;;               6: (FACT-ITER 120 6 6)
;;                 7: (FACT-ITER 720 7 6)
;;                 7: FACT-ITER returned 720
;;               6: FACT-ITER returned 720
;;             5: FACT-ITER returned 720
;;           4: FACT-ITER returned 720
;;         3: FACT-ITER returned 720
;;       2: FACT-ITER returned 720
;;     1: FACT-ITER returned 720
;;   0: FACTORIAL returned 720
;=> 720
(untrace)
;=> T

;;; 問題1.9
; 前者が線形再帰的で後者が線形反復的。
; 後者が末尾再帰

(defpackage :sicp)
;=> #<PACKAGE "SICP">
(defun sicp::+ (a b)
  (if (= a 0)
      b
      (inc (sicp::+ (dec a) b))))
;=> SICP::+
(defun inc (x)
  (+ x 1))
;=> INC
(defun dec (x)
  (- x 1))
;=> DEC
(sicp::+ 4 5)
;=> 9
(trace sicp::+)
;=> (SICP::+)
;;   0: (SICP::+ 4 5)
;;     1: (SICP::+ 3 5)
;;       2: (SICP::+ 2 5)
;;         3: (SICP::+ 1 5)
;;           4: (SICP::+ 0 5)
;;           4: SICP::+ returned 5
;;         3: SICP::+ returned 6
;;       2: SICP::+ returned 7
;;     1: SICP::+ returned 8
;;   0: SICP::+ returned 9
;=> 9

(defun sicp::+ (a b)
  (if (= a 0)
      b
      (sicp::+ (dec a) (inc b))))
;=> SICP::+
(sicp::+ 4 5)
;=> 9
(trace sicp::+)
;=> (SICP::+)
(sicp::+ 4 5)
;;   0: (SICP::+ 4 5)
;;     1: (SICP::+ 3 6)
;;       2: (SICP::+ 2 7)
;;         3: (SICP::+ 1 8)
;;           4: (SICP::+ 0 9)
;;           4: SICP::+ returned 9
;;         3: SICP::+ returned 9
;;       2: SICP::+ returned 9
;;     1: SICP::+ returned 9
;;   0: SICP::+ returned 9
;=> 9
(untrace)
;=> T

;;; 問題1.10
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (A (- x 1) 
              (A x (- y 1))))))
;=> A
(A 1 10)
;=> 1024
(A 2 4)
;=> 65536
(A 3 3)
;=> 65536
(defun f (n)
  (A 0 n))
;=> F
(defun g (n)
  (A 1 n))
;=> G
(defun h (n)
  (A 2 n))
;=> H
(defun k (n)
  (* 5 n n))
;=> K
(trace A)
;=> (A)

(f 5)
;=> 10
(f 7)
;=> 14
; (f n) は「2*n」を計算する

(g 5)
;=> 32
(g 3)
;=> 8
; (g n) は「2^n」を計算する

(h 1)
;=> 2
(h 2)
;=> 4
(h 3)
;=> 16
(h 4)
;=> 65536
; (h n)は「2^(2^n)」を計算する

;;; 1.2.2 木構造再帰
(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))
;=> FIB
(fib 4)
;=> 3
(fib 5)
;=> 5
(fib 6)
;=> 8
(fib 7)
;=> 13
(defun fib (n)
  (fib-iter 1 0 n))
;=> FIB
(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
;=> FIB-ITER
(fib 7)
;=> 13

;; 例: 両替の計算
(defun count-change (amount)
  (cc amount 5))
;=> COUNT-CHANGE
(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (t (+ (cc amount
                  (- kinds-of-coins 1))
              (cc (- amount
                     (first-denomination kinds-of-coins))
                  kinds-of-coins)))))
;=> CC
(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;=> FIRST-DENOMINATION
(count-change 100)
;=> 292

;;; 問題1.11
(defun f (n)
  (cond ((< n 3) n)
        (t (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))
;=> F
(f 5)
;=> 25
(defun f-iter (a b c count)
  (cond ((= count 0) a)
        ((= count 1) b)
        ((= count 2) c)
        (t
         (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))))
;=> F-ITER
(defun f (n)
  (f-iter 0 1 2 n))
;=> F
(f 5)
;=> 25

;;; 問題1.12
