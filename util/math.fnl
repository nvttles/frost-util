;; Hey, I finally decided to finish this x.

(local X {})

;; Constants
(set X.pi math.pi)
(set X.tau (* 2 math.pi))
(set X.e math.exp 1)
(set X.inf math.huge)

;; Degrees/Radians
(fn X.deg [r] (* r (/ 180 X.pi)))
(fn X.rad [d] (* d (/ X.pi 180)))

;; Clamp a number between min and max
(fn X.clamp [val min max]
  (if (< val min) min
      (> val max) max
      val))

;; Linear interpolation between a and b by t
(fn X.lerp [a b t]
  (+ (* (- 1 t) a) (* t b)))

;; Inverse linear interpolation
(fn X.invlerp [a b v]
  (/ (- v a) (- b a)))

;; Map a number from one range to another
(fn X.map [v inMin inMax outMin outMax]
  (X.lerp outMin outMax (X.invlerp inMin inMax v)))

;; Round to decimal places
(fn X.round [n places]
  (let [mult (math.pow 10 places)]
    (/ (math.floor (+ (* n mult) 0.5)) mult)))

;; Floor to decimal places
(fn X.floor [n places]
  (let [mult (math.pow 10 places)]
    (/ (math.floor (* n mult)) mult)))

;; Ceiling to decimal places
(fn X.ceil [n places]
  (let [mult (math.pow 10 places)]
    (/ (math.ceil (* n mult)) mult)))

;; Factorial (non-recursive for stack safety)
(fn X.factorial [n]
  (var result 1)
  (for [i 2 n]
    (set result (* result i)))
  result)

;; Greatest common divisor (Euclidean algorithm)
(fn X.gcd [a b]
  (while (not (= b 0))
    (let [t b]
      (set b (% a b))
      (set a t)))
  a)

;; Least common multiple
(fn X.lcm [a b]
  (if (or (= a 0) (= b 0))
    0
    (math.abs (/ (* a b) (X.gcd a b)))))

;; Check if number is power of two
(fn X.power-of-two? [n]
  (and (> n 0) (= 0 (bit.band n (- n)))))

;; Random float in [min, max)
(fn X.random-float [min max]
  (+ min (* (- max min) (math.random))))

;; Random integer in [min, max]
(fn X.random-int [min max]
  (math.random min max))

;; Sign of a number
(fn X.sign [n]
  (if (= n 0) 0 (if (> n 0) 1 -1)))

;; Absolute difference
(fn X.abs-diff [a b]
  (math.abs (- a b)))

;; Approximate equality
(fn X.approx= [a b epsilon]
  (<= (X.abs-diff a b) (or epsilon 1e-9)))

;; Average of list of numbers
(fn X.average [xs]
  (/ (X.sum xs) (# xs)))

;; Sum of list
(fn X.sum [xs]
  (var total 0)
  (each [_ x (ipairs xs)]
    (set total (+ total x)))
  total)

;; Min in list
(fn X.min [xs]
  (var m (xs 1))
  (each [_ x (ipairs xs)]
    (if (< x m)
      (set m x)))
  m)

;; Max in list
(fn X.max [xs]
  (var m (xs 1))
  (each [_ x (ipairs xs)]
    (if (> x m)
      (set m x)))
  m)

;; Normalize a value in [min, max] to [0,1]
(fn X.normalize [v min max]
  (/ (- v min) (- max min)))

;; Remap + clamp
(fn X.remap-clamped [v inMin inMax outMin outMax]
  (X.clamp (X.map v inMin inMax outMin outMax) outMin outMax))

;; Square a number
(fn X.square [n] (* n n))

;; Cube a number
(fn X.cube [n] (* n n n))

;; Median of sorted list
(fn X.median [xs]
  (let [n (# xs)
        mid (math.floor (/ n 2))]
    (if (= (% n 2) 0)
      (/ (+ (xs mid) (xs (+ mid 1))) 2)
      (xs (+ mid 1)))))

;; Export
X
