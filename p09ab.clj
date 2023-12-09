;;;;    Advent of Code 2023 - Day 9 part One and Two
;;;;    https://adventofcode.com/2023/day/9
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The sum of these extrapolated future values: 1939607039
;;;;    The sum of these extrapolated past values:         1041
;;;;
;;;;    (cl) by Arno Jacobs, 2023-12-09
;;;;    

(ns p09ab
  (:require [clojure.string :as str]))

;;;; First - read the data-set
(defn parse-line [line]
  (mapv parse-long (str/split (str/trim line) #" ")))

(defn get-lines [file]
  (into []
   (for [line (str/split-lines (slurp file))]
     (parse-line line))))

(def values (get-lines "data/inputDay09_2023.txt"))


(defn delta-lists [long-list short-list]
  (if (empty? short-list)
    nil
    (cons (- (first short-list) (first long-list)) 
          (delta-lists (rest long-list) (rest short-list)))))

(defn deltas [values]
  (delta-lists values (rest values)))

;;; Part 1

(defn all-future-delta-zeros [border-values values]
  (let [new-border-values (cons (last values) border-values)]
  (if (every? zero? values)
    new-border-values
    (all-future-delta-zeros new-border-values (deltas values)))))

(defn calculate-to-future [values]
  (reduce + (all-future-delta-zeros nil values)))

(defn work-future [values]
  (reduce + (map calculate-to-future values)))

;;; Part 2

(defn all-past-delta-zeros [border-values values]
  (let [new-border-values (cons (first values) border-values)]
    (if (every? zero? values)
      new-border-values
      (all-past-delta-zeros new-border-values (deltas values)))))

(defn extrapolate-past [sign current-value values]
  (if (empty? values) 
    (* sign current-value)
    (extrapolate-past (- sign) (- (first values) current-value) (rest values))))
                      
(defn calculate-to-past [values]
  (extrapolate-past -1 0 (reverse (all-past-delta-zeros nil values))))

(defn work-past [values]
  (reduce + (map calculate-to-past values)))

(defn program []
  (println "Advent of Code 2023 - day 9  (Clojure)")
  (print   "The sum of these extrapolated future values: ")
  (println (work-future values))
  (print   "The sum of these extrapolated past   values:       ")
  (println (work-past values))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p08ab.clj
(program)


