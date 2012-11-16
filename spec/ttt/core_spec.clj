(ns ttt.core-spec
  (:require [speclj.core :refer :all ]
            [ttt.core :refer :all ]))

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

  ;  (it "plays a game with AI"
  ;    (let [x (AI.)
  ;          y (fake-player [])
  ;          game (play x y)]
  ;      (println "game: " game)
  ;      (should= :x (:winner game))))

  (context "Unbeatable AI"

    (it "takes only move"
      (should= 6 (move (unbeatable-ai :x ) [:x :x :o
                                            :o :x :x
                                            nil :o :o])))
    (it "takes an easy win"
      (should= 0 (move (unbeatable-ai :x ) [nil :x :x
                                            :o :o nil
                                            :x :o nil])))
    (it "takes a less easy win"
      (should= 8 (move (unbeatable-ai :x ) [:o :x :x
                                            nil :o :x
                                            nil :o nil])))
    (it "blocks opponent from winning"
      (should= 2 (move (unbeatable-ai :x ) [:o :o nil
                                            nil nil :x
                                            nil :x nil])))
    (it "create a fork in bottom right"
      (should= 8 (move (unbeatable-ai :o ) [nil :x nil
                                            :x nil :o
                                            nil :o nil])))
    (it "create a fork in top left"
      (should= 0 (move (unbeatable-ai :x ) [nil :x nil
                                            :x nil :o
                                            nil :o nil])))

    )

  )