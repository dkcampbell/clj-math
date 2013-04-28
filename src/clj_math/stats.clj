(ns clj-math.stats [:require [clojure.contrib [math :as math]]])

; Scale a number between 0 and 1.
(defn scale [n min max] 
  (/ n (- max min)))

; Calculate the mean of a list.
(defn mean [xs]
  (/ (apply + xs) (count xs)))

; Calculate the median of a list.
; Naieve implementation
(defn median [xs]
  (let [ordered-list (sort xs)
        length (count ordered-list)]
    (if (even? length) 
      (mean [(nth ordered-list (quot length 2))
             (nth ordered-list (- (quot length 2) 1))])
      (nth ordered-list (quot length 2)))))

; Find the most common element in a list.
; If more than one number occurs equally as often. The first number in the 
; list will be returned.
(defn mode [xs]
  (let [tally (frequencies xs)]
    (ffirst (into 
          (sorted-map-by #(compare (get tally %2) (get tally %1))) tally))))

; Calculate variance
(defn variance [xs]
  (let [m (mean xs)
        length (count xs)]
    (/ (apply + (map #(math/expt (- %1 m) 2) xs)) (- length 1))))

; Standard Deviation
(defn standard-deviation [xs]
  (math/sqrt (variance xs)))

; Quartiles
;;(defn quartiles [xs]
;  (let [ordered-list (sort xs)
;        length (count ordered-list)]
;    ))
