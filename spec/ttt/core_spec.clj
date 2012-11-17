(ns ttt.core-spec
  (:require [speclj.core :refer :all ]
            [ttt.core :refer :all ]))

(deftype FakePlayer [moves]
  Player
  (move [_ board]
    (if-let [move (first @moves)]
      (do (swap! moves rest) move)
      (first (filter #(nil? (board %1)) (range 9))))))

(defn fake-player [moves]
  (FakePlayer. (atom moves)))

(deftype FakeView [moves boards messages]
  View
  (show-board [_ board] (swap! boards conj board))
  (show-message [_ message] (swap! messages conj message))
  (get-move [_] (let [m (first moves)] (swap! moves rest) m)))

(defn fake-view [moves]
  (FakeView. (atom moves) (atom []) (atom [])))

(describe "TTT"

  (it "detects win"
    (should= true (win-for? :x [:x :x :x nil nil nil nil nil nil]))
    (should= true (win-for? :x [nil nil nil :x :x :x nil nil nil]))
    (should= true (win-for? :x [nil nil nil nil nil nil :x :x :x ]))
    (should= true (win-for? :x [:x nil nil :x nil nil :x nil nil]))
    (should= true (win-for? :x [nil :x nil nil :x nil nil :x nil]))
    (should= true (win-for? :x [nil nil :x nil nil :x nil nil :x ]))
    (should= true (win-for? :x [nil nil :x nil :x nil :x nil nil]))
    (should= true (win-for? :x [:x nil nil nil :x nil nil nil :x ]))
    (should= true (win-for? :o [:o :o :o nil nil nil nil nil nil]))
    (should= false (win-for? :x [nil nil nil nil nil nil nil nil nil]))
    (should= false (win-for? :o [:x :x :x nil nil nil nil nil nil])))

  (it "detects draws"
    (should= false (draw? (vec (take 9 (repeat nil)))))
    (should= true (draw? [:x :x :o :o :o :x :x :x :o ]))
    (should= false (draw? [:x :x :o :o :o :x :x nil :o ])))

  (context "Playing game"

    (with view (fake-view (range 9)))
    (around [it] (binding [*view* @view] (it)))

    (it "plays a cats game"
      (let [x (fake-player [0 1 5 6 7])
            y (fake-player [2 3 4 8])
            game (play x y)]
        (should= nil (:winner game))
        (should= [:x :x :o :o :o :x :x :x :o ] (:board game))
        (should= [0 2 1 3 5 4 6 8 7] (:history game))))

    (it "plays a game where x wins"
      (let [x (fake-player [0 1 2])
            y (fake-player [3 4 5])
            game (play x y)]
        (should= :x (:winner game))
        (should= [:x :x :x :o :o nil nil nil nil] (:board game))
        (should= [0 3 1 4 2] (:history game))))

    (it "plays a game where o wins"
      (let [x (fake-player [0 1 8])
            y (fake-player [3 4 5])
            game (play x y)]
        (should= :o (:winner game))
        (should= [:x :x nil :o :o :o nil nil :x ] (:board game))
        (should= [0 3 1 4 8 5] (:history game))))

    (it "plays a game with AI"
      (let [x (ttt.core.UnbeatableAI. :x )
            y (fake-player [])
            game (play x y)]
        (should= :x (:winner game))))

    (it "plays a game with view"
      (let [x (fake-player [0 1 2])
            y (fake-player [3 4 5])
            game (play x y)]
        (should= 6 (count @(.boards @view)))))

    (it "plays a game with invalid moves"
      (let [x (fake-player [0 0 1 1 2])
            y (fake-player [3 3 0 1 4 5])
            game (play x y)]
        (should= :x (:winner game))
        (should= [:x :x :x :o :o nil nil nil nil] (:board game))
        (should= [0 3 1 4 2] (:history game))))
    )

  (context "Console View"

    (with view (ttt.core.ConsoleView.))

    (it "shows a message"
      (should= "Helo Thar!\n"
        (with-out-str (show-message @view "Helo Thar!"))))

    (it "shows board"
      (should= (str
                 " x | o | 2 \n"
                 "-----------\n"
                 " 3 | 4 | x \n"
                 "-----------\n"
                 " o | x | 8 \n\n"
                 )
        (with-out-str (show-board @view [:x :o nil nil nil :x :o :x nil]))))

    (it "gets a valid move"
      (should= 4 (with-in-str "4\n" (binding [*out* (java.io.StringWriter.)] (get-move @view))))
      (should= 0 (with-in-str "0\n" (binding [*out* (java.io.StringWriter.)] (get-move @view))))
      (should= 8 (with-in-str "8\n" (binding [*out* (java.io.StringWriter.)] (get-move @view)))))

    (it "ignores invalid move"
      (should= 4 (with-in-str "blah\n-1\n10\n4\n"
                   (binding [*out* (java.io.StringWriter.)] (get-move @view)))))

    )

  (context "Unbeatable AI"

    (it "takes only move"
      (should= 6 (move (ttt.core.UnbeatableAI. :x ) [:x :x :o :o :x :x nil :o :o ])))

    (it "takes an easy win"
      (should= 0 (move (ttt.core.UnbeatableAI. :x ) [nil :x :x :o :o nil :x :o nil])))

    (it "takes a less easy win"
      (should= 8 (move (ttt.core.UnbeatableAI. :x ) [:o :x :x nil :o :x nil :o nil])))

    (it "blocks opponent from winning"
      (should= 2 (move (ttt.core.UnbeatableAI. :x ) [:o :o nil nil nil :x nil :x nil])))

    (it "blocks opponent from winning 2"
      (should= 7 (move (ttt.core.UnbeatableAI. :o ) [:o :x nil nil :x nil nil nil nil])))

    (it "create a fork in bottom right"
      (should= 8 (move (ttt.core.UnbeatableAI. :o ) [nil :x nil
                                                     :x nil :o
                                                     nil :o nil])))

    (it "create a fork in top left"
      (should= 0 (move (ttt.core.UnbeatableAI. :x ) [nil :x nil :x nil :o nil :o nil])))

    )

  )