;;;;    Advent of Code 2023 - Day 6 part One and Two
;;;;    https://adventofcode.com/2023/day/6
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The product of all winings is:           1624896
;;;;    The product of the big race winings is: 32583852
;;;;
;;;;    (cl) by Arno Jacobs, 2023-12-06
;;;;    
;;;; https://github.com/Arno65/Advent-of-Code-2023/blob/main/p06ab.clj

(ns p06ab
  (:require [clojure.math :as m]))

;;; Race data - part 1
(def times     '(56   97   78   75))
(def distances '(546 1927 1131 1139))

;;; Race data - part 2
(def big-time     '(       56977875))
(def big-distance '(546192711311139))

;;;; Using abc-formula for solving quadratic formula 
;;;; Solve:   - dt^2 + time * dt = distance
;;;; count-wins = floor (dt_high) - floor (dt_low)
;;;
(defn discriminant [a b c]
  (- (* b b) (* 4 a c)))

(defn abc [a b c]
  (let [d (discriminant a b c)
        two-a (* 2 a)
        minus-b (- b)
        sqrt-d (m/sqrt d)]
      (cond
       (< d 0) nil
       (= d 0) (list (/ minus-b two-a))
       :else 
       (list (/ (+ minus-b sqrt-d) two-a)
             (/ (- minus-b sqrt-d) two-a)))))

(defn count-wins [time distance]
  (let [solutions (map #(m/floor %) (abc -1 time (- distance)))]
    (if (= 2 (count solutions))
      (-> (- (last solutions)
             (first solutions))
          abs
          int)
      0)))
  
(defn work-count-wins [times distances]
  (if (= 0 (count times))
    nil
    (cons (count-wins (first times) (first distances))
          (work-count-wins (rest times) (rest distances)))))

(defn product-all-winnings [times distances] 
  (reduce * (work-count-wins times distances)))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2023 - day 6  (Clojure)")
  (print   "The product of all winings is:           ")
  (println (product-all-winnings times distances))

  (print   "The product of the big race winings is: ")
  (println (product-all-winnings big-time big-distance))

  (println "0K.\n"))

;; Run in terminal via: clojure -M p06ab.clj
(program)

;;; "Elapsed time: 0.628375 msecs"

