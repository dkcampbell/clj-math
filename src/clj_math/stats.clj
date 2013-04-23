(ns clj-math.stats)

; Scale a number between 0 and 1.
(defn scale [n min max] 
	(/ n (- max min)))
