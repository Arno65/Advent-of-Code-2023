;;;;    Advent of Code 2023 - Day 2 part One and Two
;;;;    https://adventofcode.com/2023/day/2
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The sum of possible game ID is:       2545
;;;;    The sum of the power of all sets is: 78111
;;;;
;;;;    (cl) by Arno Jacobs, 2023-12-04
;;;;    

(ns p02ab
  (:require [clojure.string :as str]))


;;;; First - read the data-set
(defn get-lines [file]
  (->> file
       slurp
       (str/split-lines)))

(def data-set (get-lines "./data/inputDay02_2023.txt"))

(def max-red 12)
(def max-green 13)
(def max-blue 14)


;;; Part 1

;;; Will return 0 if string does NOT start with a digit 
(defn parse-int [s]
  (Integer. (re-find #"[0-9]*" (apply str (cons \0 s)))))

;;; Check if 'long' string starts with 'short' string
(defn equal-short-start [short long]
  (= (compare short (apply str (take (count short) long))) 0))

;;; Get the maximum colour values per Game line
(defn get-maximum [colour parts maximum-value]
  (let [value (parse-int (first parts))]
  (if (< (count parts) 2)
     maximum-value
     (if (and (equal-short-start colour (nth parts 1))
              (< maximum-value value)) 
        (get-maximum colour (drop 2 parts) value)
        (get-maximum colour (drop 2 parts) maximum-value)))))
     
(defn maximum-RGB [line]
  (let [parts (str/split line #" ")]
    (list (parse-int (nth parts 1))
          (get-maximum "red" parts 0)
          (get-maximum "green" parts 0)
          (get-maximum "blue" parts 0))))

(defn possible-game [game] 
  (and (>= max-red (nth game 1))
       (>= max-green (nth game 2))
       (>= max-blue (nth game 3))))
  
(defn summed-possible-games [games]
  (let [maximum-RGBs (map maximum-RGB games)]
    (->> maximum-RGBs
         (filter possible-game)
         (map first)
         (reduce +))))

;;; Part 2

(defn find-power [game]
  (->> game 
       maximum-RGB
       rest
       (reduce *)))

(defn find-powers [games]
  (->> games
       (map find-power)
       (reduce +)))


;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2023 - day 1  (Clojure)")
  (print   "The sum of possible game ID is:       ")
  (println (summed-possible-games data-set))
  (print   "The sum of the power of all sets is: ")
  (println (find-powers data-set))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p01ab.clj
(program)



