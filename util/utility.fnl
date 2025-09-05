;; mathx.fnl - a small math utilities module for Fennel/Lua
;; Save as mathx.fnl and require it with (local mathx (require :mathx))

(local math math) ;; expose Lua math via Fennel

;; --- Basic helpers ---------------------------------------------------------
(fn clamp [x lo hi]
  "Clamp x to the inclusive range [lo, hi]."
  (if (< x lo) lo (if (> x hi) hi x)))

(fn lerp [a b t]
  "Linear interpolation between a and b by t in [0,1]."
  (+ a (* (- b a) t)))

(fn map-range [x in-min in-max out-min out-max]
  "Map x from one range to another (no clamping)."
  (+ out-min (* (/ (- x in-min) (- in-max in-min)) (- out-max out-min))))

(fn approx-eq [a b eps]
  (let [eps (or eps 1e-12)]
    (<= (math.abs (- a b)) eps)))

(fn sign [x]
  (cond
    (> x 0) 1
    (< x 0) -1
    :else 0))

(fn round [x]
  (math.floor (+ x 0.5)))

;; --- Number theory --------------------------------------------------------
(fn gcd [a b]
  "Greatest common divisor (Euclid)."
  (let [a (math.abs a)
        b (math.abs b)]
    (while (not= b 0)
      (let [t b]
        (set b (% a b))
        (set a t)))
    a))

(fn lcm [a b]
  (if (or (= a 0) (= b 0)) 0 (math.floor (/ (math.abs (* a b)) (gcd a b)))))

(fn factorial [n]
  (assert (>= n 0) "factorial: n must be >= 0")
  (let [res 1
        i 1]
    (while (<= i n)
      (set res (* res i))
      (set i (+ i 1)))
    res))

(fn is-prime [n]
  (if (<= n 1) false
    (if (<= n 3) true
      (if (or (= (% n 2) 0) (= (% n 3) 0)) false
        (let [i 5]
          (while (<= (* i i) n)
            (if (or (= (% n i) 0) (= (% n (+ i 2)))) (return false))
            (set i (+ i 6)))
          true)))))

;; --- Combinatorics --------------------------------------------------------
(fn n-choose-k [n k]
  "Compute n choose k (combinatorial)."
  (if (or (< k 0) (> k n)) 0
    (let [k (if (> k (- n k)) (- n k) k) ; symmetry
          numer 1
          denom 1
          i 1]
      (while (<= i k)
        (set numer (* numer (+ n (- k) i)))
        (set denom (* denom i))
        (set i (+ i 1)))
      (/ numer denom))))

(fn permutations [n k]
  "nPk = n*(n-1)*...*(n-k+1)"
  (if (or (< k 0) (> k n)) 0
    (let [res 1 i 0]
      (while (< i k)
        (set res (* res (- n i)))
        (set i (+ i 1)))
      res)))

;; --- Stats ----------------------------------------------------------------
(fn sum [xs]
  (let [s 0 i 1]
    (while (<= i (# xs))
      (set s (+ s (table.get xs i)))
      (set i (+ i 1)))
    s))

(fn mean [xs]
  (if (== (# xs) 0) 0 (/ (sum xs) (# xs)))

(fn median [xs]
  (if (== (# xs) 0) nil
    (let [arr (table.clone xs) ; copy
          cmp (fn [a b] (< a b))]
      (table.sort arr cmp)
      (let [n (# arr)]
        (if (= (% n 2) 1)
          (table.get arr (math.floor (/ (+ n 1) 2)))
          (/ (+ (table.get arr (/ n 2)) (table.get arr (+ (/ n 2) 1))) 2))))))

(fn variance [xs sample?]
  (let [n (# xs)]
    (if (<= n 1) 0
      (let [m (mean xs)
            s 0 i 1]
        (while (<= i n)
          (set s (+ s (math.pow (- (table.get xs i) m) 2)))
          (set i (+ i 1)))
        (/ s (if sample? (- n 1) n))))))

(fn stddev [xs sample?]
  (math.sqrt (variance xs sample?)))

;; --- Random / utils -------------------------------------------------------
(fn rand-int [lo hi]
  "Inclusive random integer between lo and hi."
  (+ lo (math.floor (* (math.random) (+ 1 (- hi lo))))))

;; --- Vector helpers (2D/3D simple) ---------------------------------------
(fn vec-add [a b]
  (map (fn [i] (+ (table.get a i) (table.get b i))) (ipairs a))) ;; returns iterator; but better to provide explicit

(fn vec-sub [a b]
  (let [res {} i 1]
    (while (<= i (# a))
      (table.insert res (- (table.get a i) (table.get b i)))
      (set i (+ i 1)))
    res))

(fn vec-dot [a b]
  (let [s 0 i 1]
    (while (<= i (# a))
      (set s (+ s (* (table.get a i) (table.get b i))))
      (set i (+ i 1)))
    s))

;; --- Module export -------------------------------------------------------
(local exports
  {:clamp clamp
   :lerp lerp
   :map-range map-range
   :approx-eq approx-eq
   :sign sign
   :round round

   :gcd gcd
   :lcm lcm
   :factorial factorial
   :is-prime is-prime

   :n-choose-k n-choose-k
   :permutations permutations

   :sum sum
   :mean mean
   :median median
   :variance variance
   :stddev stddev

   :rand-int rand-int

   :vec-sub vec-sub
   :vec-dot vec-dot})

exports

;; --- Small examples (for running as a script) -----------------------------
;; Uncomment the lines below to see quick tests when running with fennel.
;; (print "clamp 5 in [0,3] =>" (clamp 5 0 3))
;; (print "lerp 0->10 @0.25 =>" (lerp 0 10 0.25))
;; (print "gcd(54,24) =>" (gcd 54 24))
;; (print "is-prime 97 =>" (is-prime 97))
;; (print "5 choose 2 =>" (n-choose-k 5 2))
;; (print "mean [1,2,3,4] =>" (mean
