(ns ttt.core)

(def wins [[0 1 2][3 4 5][6 7 8][0 3 6][1 4 7][2 5 8][0 4 8][2 4 6]])

(defn win-for [mark board]
  (first
    (filter
      (fn [win] (= #{mark} (into #{} (map #(board %1) win))))
      wins)))

(defn win-for? [mark board]
  (not (nil? (win-for mark board))))

(defprotocol Player
  (move [_ board]))

(deftype FakePlayer [moves]
  Player
  (move [_ board]
    (if-let [move (first @moves)]
      (do (swap! moves rest) move)
      (first (filter nil? board)))))

(defn fake-player [moves]
  (FakePlayer. (atom moves)))

(def empty-board (vec (take 9 (repeat nil))))

(defn draw? [board]
  (= 0 (count (filter nil? board))))

(defn opponent [mark]
  (if (= :x mark) :o :x ))

(defn possible-moves [board]
  (filter identity (map #(if (= nil (board %1)) %1 nil) (range 9))))

(defn highest-scoring-move [scores]
  (first (first (reverse (sort-by second (seq scores))))))

(declare score-for-move)

(defn score-for-move! [board mark]
  (cond
    (win-for? mark board) (inc (count (filter nil? board)))
    (draw? board) 0
    :else (* -1 (apply + (map
                           #(score-for-move (assoc board %1 (opponent mark)) (opponent mark))
                           (possible-moves board))))))

(def score-for-move (memoize score-for-move!))

(defn score-moves [board mark]
  (reduce
    #(assoc %1 %2 (score-for-move (assoc board %2 mark) mark))
    {} (possible-moves board)))

(defn best-move [board mark]
  (let [scores (score-moves board mark)]
    (highest-scoring-move scores)))

(deftype UnbeatableAI [mark]
  Player
  (move [_ board]
    (best-move board mark)))

(defn unbeatable-ai [mark]
  (UnbeatableAI. mark))

(defn play [x o]
  (letfn [(player [mark] (if (= :x mark) x o))]
    (loop [mark :x board empty-board history []]
      (let [opponent (opponent mark)]
        (cond
          (win-for? opponent board) {:winner opponent :board board :history history}
          (draw? board) {:winner nil :board board :history history}
          :else (let [m (move (player mark) board)]
                  (recur opponent (assoc board m mark) (conj history m))))))))
