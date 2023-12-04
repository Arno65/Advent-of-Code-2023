;;;;    Advent of Code 2022 - Day 1 part One and Two
;;;;    https://adventofcode.com/2023/day/1
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The sum of all of the    calibration values: 54679
;;;;    The sum of all of the re-calibration values: 54885
;;;;
;;;;    (cl) by Arno Jacobs, 2023-12-01
;;;;    

;;; Refactor with the help of (https://clojuredocs.org/clojure.core/-%3E)
;;; `->`  as  the "thread-first" macro and   
;;;           the threaded value appears in each function call in the first position 
;;;           in the argument list
;;; '->>' as  the "thread-last" macro
;;;           the threaded value appears in each function call in the final position 
;;;           in the argument list
;;; to help make code more readable by removing nesting.

;;;; First - read the data-set
;;;
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (->> file
       slurp
       (str/split-lines)))

(def data-set (get-lines "./data/inputDay01_2023.txt"))

;;;  Part 1

;;; DIY 'isDigit'
(defn isDigit [c] (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c))

;;; Character digits into Int by substracting 48 from the character value
;;; Calibration part 1, 10 * first digit + last digit
;;;
(defn calibrate-line [line]
  (let [digits (map #(- (int %) 48) (filter isDigit line))]
    (+ (* 10 (first digits)) 
       (last digits))))

;;; Calibrate line by line and sum all values
(defn calibrate [data-set]
  (->> data-set
       (map calibrate-line)
       (reduce +)))


;;; Part 2

(defn starts-with [spelled line]
  (= spelled (->> line 
                  (take (count spelled)) 
                  (apply str))))

;;; Convert all spelled digits to digits
;;; Next use the calibrate function from part 1
;;;
(defn add-digit [line]
  (cond 
    (starts-with "zero" line)  "0"
    (starts-with "one" line)   "1"
    (starts-with "two" line)   "2"
    (starts-with "three" line) "3"
    (starts-with "four" line)  "4"
    (starts-with "five" line)  "5"
    (starts-with "six" line)   "6"
    (starts-with "seven" line) "7"
    (starts-with "eight" line) "8"
    (starts-with "nine" line)  "9"
    :else (first line)))

(defn pre-calibrate [line]
  (if (= 0 (count line)) 
    ""
    (cons (add-digit line) (pre-calibrate (rest line)))))

(defn pre-calibrate-lines [data-set]
  (map #(apply str (pre-calibrate %)) data-set))

(defn re-calibrate [data-set]
  (-> data-set
      pre-calibrate-lines
      calibrate))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2023 - day 1  (Clojure)")
  (print   "The sum of all of the calibration values:    ")
  (println (calibrate data-set))
  (print   "The sum of all of the re-calibration values: ")
  (println (re-calibrate data-set))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p01ab.clj
(program)

