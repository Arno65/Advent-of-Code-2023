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

(ns p06ab)

;;; Race data - part 1
(def times     '(56   97   78   75))
(def distances '(546 1927 1131 1139))

;;; Race data - part 2
(def big-time     '(       56977875))
(def big-distance '(546192711311139))


(defn travel-distance [total-time charge-time]
  (* charge-time (- total-time charge-time)))

(defn count-wins [time distance] 
  (reduce + (for [charge-time (range 1 (- time 1))
        :let [counter (if (< distance (travel-distance time charge-time)) 1 0)]] 
    counter)))
              
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

