(ns ttt.core
  (:require [clojure.java.io :refer [reader]]))

(def wins [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])

(defn win-for [mark board]
  (first (filter
           (fn [win] (= #{mark} (into #{} (map #(board %1) win))))
           wins)))

(defn win-for? [mark board]
  (not (nil? (win-for mark board))))

(defprotocol View
  (show-board [_ board])
  (show-message [_ message])
  (get-move [_]))

(deftype ConsoleView []
  View
  (show-board [_ board]
    (print
      (apply format
        " %s | %s | %s \n-----------\n %s | %s | %s \n-----------\n %s | %s | %s \n\n"
        (map #(if-let [m (board %1)] (name m) %1) (range 9)))))
  (show-message [_ message] (println message))
  (get-move [_]
    (print "choose move: ") (flush)
    (let [input (read *in*)]
      (if (and (integer? input) ((set (range 9)) input)) input (recur)))))

(declare ^:dynamic *view*)

(defprotocol Player
  (move [_ board]))

(deftype HumanPlayer []
  Player
  (move [_ board] (get-move *view*)))

(def empty-board (vec (take 9 (repeat nil))))

(defn draw? [board]
  (= 0 (count (filter nil? board))))

(defn opponent [mark]
  (if (= :x mark) :o :x ))

(defn possible-moves [board]
  (filter identity (map #(if (= nil (board %1)) %1 nil) (range 9))))

(defn highest-scoring-move [scores]
  (first (first (reverse (sort-by second (seq scores))))))

(def score-for-move
  (memoize
    (fn [board mark me?]
      (cond
        (win-for? mark board) (* (if me? 1 -1) (inc (count (filter nil? board))))
        (draw? board) 0
        :else (apply (if me? min max)
                (map
                  #(score-for-move (assoc board %1 (opponent mark)) (opponent mark) (not me?))
                  (possible-moves board)))))))

(defn score-moves [board mark]
  (reduce
    #(assoc %1 %2 (score-for-move (assoc board %2 mark) mark true))
    {} (possible-moves board)))

(defn best-move [board mark]
  (let [scores (score-moves board mark)]
    (highest-scoring-move scores)))

(deftype UnbeatableAI [mark]
  Player
  (move [_ board] (best-move board mark)))

(defn play [x o]
  (letfn [(player [mark] (if (= :x mark) x o))]
    (loop [mark :x board empty-board history []]
      (show-board *view* board)
      (let [opponent (opponent mark)]
        (cond
          (win-for? opponent board) {:winner opponent :board board :history history}
          (draw? board) {:winner nil :board board :history history}
          :else (let [m (move (player mark) board)]
                  (if (nil? (board m))
                    (do (show-message *view* (format "%s takes %s\n" mark m))
                      (recur opponent (assoc board m mark) (conj history m)))
                    (recur mark board history))))))))

(defn -main []
  (binding [*view* (ConsoleView.)]
    (show-message *view* "Welcome to Tic Tac Toe.  You're X and it's your move.\n")
    (let [x (HumanPlayer.)
          o (UnbeatableAI. :o )
          game (play x o)]
      (show-message *view*
        (if (:winner game) (format "%s wins!" (name (:winner game))) "draw game.")))))