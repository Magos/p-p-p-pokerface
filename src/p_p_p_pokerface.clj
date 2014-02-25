(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (-> rank
        str
        Integer/valueOf)
    (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank)))


(defn suit [[_ suit]]
  (str suit))

(defn- rank-freqs [hand]
  (->> hand
       (map rank)
       frequencies
       vals))

(defn pair? [hand]
  (boolean (some (partial = 2) (rank-freqs hand))))

(defn three-of-a-kind? [hand]
  (boolean (some (partial = 3) (rank-freqs hand))))

(defn four-of-a-kind? [hand]
  (boolean (some (partial = 4) (rank-freqs hand))))

(defn flush? [hand]
  (every? (partial = (suit (first hand))) (map suit hand)))

(defn full-house? [hand]
  (= #{3 2} (set (rank-freqs hand))))


(defn two-pairs? [hand]
  (let[freqs (rank-freqs hand)]
    (or (= (set freqs) #{4 1})
        (= (sort freqs) '(1 2 2)))))


(defn straight? [hand]
  (let[ranks (sort (map rank hand))
       partitioned (partition 2 1 ranks)]
    (or
     (= '(2 3 4 5 14) ranks)
     (every? (partial = 1) (map #(let[[a b] %](- b a)) partitioned)))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)   8
   (four-of-a-kind? hand)   7
   (full-house? hand)       6
   (flush? hand)            5
   (straight? hand)         4
   (three-of-a-kind? hand)  3
   (two-pairs? hand)        2
   (pair? hand)             1
   :else 0
   ))
