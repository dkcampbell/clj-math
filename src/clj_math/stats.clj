(ns clj-math.stats [:require [clojure.contrib [math :as math]]])

; Scale a number between 0 and 1.
(defn scale 
  "Scale a number between 0 and 1."
  [n min max] 
  (/ n (- max min)))

; Calculate the mean of a list.
(defn mean 
  "Calculate the arithmetic mean of a collection."
  [xs]
  (/ (apply + xs) (count xs)))

; Calculate the median of a list.
; Naieve implementation
(defn median 
  "Calculate the median of a list."
  [xs]
  (let [ordered-list (sort xs)
        length (count ordered-list)]
    (if (even? length) 
      (mean [(nth ordered-list (quot length 2))
             (nth ordered-list (- (quot length 2) 1))])
      (nth ordered-list (quot length 2)))))

; Find the most common element in a list.
; If more than one number occurs equally as often. The first number in the 
; list will be returned.
(defn mode 
  "Find the most common element in a list."
  [xs]
  (let [tally (frequencies xs)]
    (ffirst (into 
          (sorted-map-by #(compare (get tally %2) (get tally %1))) tally))))

; Calculate variance
(defn variance 
  "Calculate the variance of a list."
  [xs]
  (let [m (mean xs)
        length (count xs)]
    (/ (apply + (map #(math/expt (- %1 m) 2) xs)) (- length 1))))

; Standard Deviation
(defn standard-deviation 
  "Calculate standard deviation of a list."
  [xs]
  (math/sqrt (variance xs)))

; Root mean square
(defn rms
  "Calculate the root mean square"
  [xs]
  (let [len (count xs)]
    (math/sqrt (/ (reduce + (map #(* % %) xs)) len))))

; Quartiles
;;(defn quartiles [xs]
;  (let [ordered-list (sort xs)
;        length (count ordered-list)]
;    ))
