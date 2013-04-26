(ns clj-math.stats)

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
