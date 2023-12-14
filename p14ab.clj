;;;;    Advent of Code 2023 - Day 14 part One and Two
;;;;    https://adventofcode.com/2023/day/14
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The total load on the north support beams: 109596
;;;;    The total load after 1000000000 cycles:     96105
;;;;
;;;;    (cl) by Arno Jacobs, 2023-12-14
;;;;    

(ns p14ab
  (:require [clojure.string :as str]))

;;; Read the data and split lines
(defn get-lines 
  [file]
  (->> file
       slurp
       (str/split-lines)))

;;; All characters into a 2D-list structure
(def data-set 
  (map seq (get-lines "./data/inputDay14_2023.txt")))

;;; Some initials
(def RollingStone (char \O))
(def SteadyRock   (char \#))
(def FreeSpace    (char \.))
(def mega-cycles 1000000000)

;;;  Part 1

;; Create a list of indexes for all Rolling Stones, first position has index 1
(defn RollingStones-indexes 
  [ix mirror-line] 
  (if (empty? mirror-line)
    nil
    (if (= RollingStone (first mirror-line))
      (cons ix (RollingStones-indexes (+ 1 ix) (rest mirror-line)))
      (RollingStones-indexes (+ 1 ix) (rest mirror-line)))))

;;; Get the indexes of the Rolling Stones and sum them all
(defn calculate-load 
  [mirror-line]
  (->> mirror-line
       (RollingStones-indexes 1)
       (reduce +)))

;;; Transpose a 2D-list structure
(defn transpose 
  [mx-2D]
  (apply mapv vector mx-2D))

;;; Create a list of lists, seperated on the first given element
(defn seperate-on
  [element elements]
  (if (empty? elements)
    nil
    (if (= element (first elements))
      (cons (take-while #(= element %1) elements)
            (seperate-on element (drop-while #(= element %1) elements)))
      (cons (take-while #(not= element %1) elements)
             (seperate-on element (drop-while #(not= element %1) elements))))))

;;; A mirror-line-part with a Rolling Stone will place those Stones most left
(defn roll-part-to-the-left 
  [mirror-line-part]
  (cons (filter #(= RollingStone %1) mirror-line-part) 
        (filter #(not= RollingStone %1) mirror-line-part)))

;;; Roll all rolling stones to the left, a steady rock will stop the rolling ones.
(defn roll-left
  [mirror-line]
  (->> mirror-line
       (seperate-on SteadyRock)
       (map roll-part-to-the-left)
       flatten))

;;; Handy for part 2
;;; Roll all rolling stones to the right, a steady rock will stop the rolling ones.
(defn roll-right
  [mirror-line]
  (->> mirror-line 
       reverse 
       roll-left
       reverse))

;;; Per mirror line, roll all balls left, reverse line and calculate load
(defn roll-and-calculate-load 
  [mirror-line]
  (->> mirror-line
       roll-left
       reverse
       calculate-load))

;;; Work the complete mirror, turn North anti-clockwise
(defn total-load 
  [mirror]
  (->> mirror
       transpose
       (map roll-and-calculate-load)
       (reduce +)))

;;; Part 2 

;;;; One cycle: North -> West -> South -> East
;;;;
;;;; North:  T                 roll                 T
;;;; West:                     roll
;;;; South:  T  map (reverse)  roll  map (reverse)  T
;;;; East:      map (reverse)  roll  map (reverse)

;;; Roll all rolling stones up nort
(defn roll-north 
  [mirror]
  (->> mirror
       transpose
       (map roll-left)
       transpose))

;;; Roll all rolling stones left to the west
(defn roll-west
  [mirror]
  (map roll-left mirror))

;;; Roll all rolling stones down south
(defn roll-south
  [mirror]
  (->> mirror
       transpose
       (map roll-right) 
       transpose))

;;; Roll all rolling stones right to the east
(defn roll-east
  [mirror]
  (map roll-right mirror))

;;; Roll all four directions in an anti clockwise sequence - once
(defn one-cycle 
  [mirror]
  (->> mirror 
       roll-north
       roll-west
       roll-south
       roll-east))

;;; Work a complete cycle n-times
(defn n-cycles 
  [count mirror]
  (if (< count 2)
    (one-cycle mirror)
    (n-cycles (- count 1) (one-cycle mirror))))

;;; There is repetition after some cycles. Find that cycle
(defn get-modulo-cycle
  [start-cycle current-cycle modulo-counter]
  (let [next-cycle (one-cycle current-cycle)]
    (if (= next-cycle start-cycle) 
      modulo-counter
      (get-modulo-cycle start-cycle next-cycle (+ 1 modulo-counter)))))

;;; Calculate the total load after running all cycles 
(defn total-load-after-all-cycles 
  [mirror]
  (->> mirror
       transpose
       (map reverse)
       (map calculate-load)
       (reduce +)))

;;; Calculate the total load after running all cycles  
(defn work-to-pattern-start
  [mirror previous-mirrors cycles]
  (let [in-set ((set previous-mirrors) mirror)]
    (if (nil? in-set)
      (work-to-pattern-start (one-cycle mirror) 
                             (cons mirror previous-mirrors) 
                             (+ cycles 1))
      (list cycles mirror))))

;;; The works. Find pattern start. Find cycle. Calculate remaining cycles. And work.
(defn work-mega-cycles
  [cycles mirror]
  (let [pattern-info (work-to-pattern-start mirror nil 0),
        current-mirror (first (rest pattern-info)),
        modulo-cycle (get-modulo-cycle current-mirror current-mirror 1),
        remaining-cycles (mod (- cycles (first pattern-info)) modulo-cycle)]
    (total-load-after-all-cycles (n-cycles remaining-cycles current-mirror))))

;;; The 'main' program - - -
(defn program   []
  (println "Advent of Code 2023 - day 14 (Clojure)")
  (print   "The total load on the north support beams: ")
  (println (total-load data-set))
  (print   "The total load after ")
  (print mega-cycles)
  (print " cycles:     ")
  (println (work-mega-cycles mega-cycles data-set))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p14ab.clj
(program)



