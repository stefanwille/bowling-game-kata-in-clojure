(ns bowlinggamekata.core-test
  (:require [clojure.test :refer :all]
            [bowlinggamekata.core :refer :all]))

(defn pin-value 
  "Adds the pins in the given frame"
  [frame]
  (reduce + frame))

(defn spare? 
  "Tests if the given frame is a spare"
  [frame] (and (= (count frame) 2) (= (pin-value frame) 10)))

(defn strike? 
  "Tests if the given frame is a strike"
  [frame] (= frame [10]))

(defn next-n-rolls
  "Return the next n rolls starting from the given frame"
  [n game frame-index] (take n (flatten (drop frame-index game))))

(defn value-of-next-n-rolls
  "Return the sum of the next n rolls"
  [n game frame-index]
  (let
   [sum (reduce + (next-n-rolls n game frame-index))]
    sum))

(defn frame-bonus
  "Returns the bonus for the given frame"
  [game frame-index frame]
  (cond (spare? frame) (value-of-next-n-rolls 1 game (+ frame-index 1))
        (strike? frame) (value-of-next-n-rolls 2 game (+ frame-index 1))
        :else 0))

(defn score-frame 
  "Returns the score for the given frame, which is the sum of all pins plus the bonus"
  [game frame-index]
  (let [frame (nth game frame-index)
        pinvalue (pin-value frame)
        bonus (frame-bonus game frame-index frame)]
    (+ pinvalue bonus)))

(deftest next-n-rolls-test
  (testing "with 2 rolls in 1 frame"
    (is (= '(2 3) (next-n-rolls 2 [[2 3]] 0))))
  (testing "with 2 rolls in 2 frame"
    (is (= '(2 3) (next-n-rolls 2 [[2] [3]] 0))))
  (testing "with 2 rolls in 2 frame and a leading frame"
    (is (= '(2 3) (next-n-rolls 2 [[1] [2] [3]] 1)))))

(deftest value-of-next-n-rolls-test
  (testing "with 2 rolls in 1 frame"
    (is (= 5 (value-of-next-n-rolls 2 [[2 3]] 0))))
  (testing "with 2 rolls in 2 frame"
    (is (= 5 (value-of-next-n-rolls 2 [[2] [3]] 0))))
  (testing "with 2 rolls in 2 frame and a leading frame"
    (is (= 5 (value-of-next-n-rolls 2 [[1] [2] [3]] 1)))))

(deftest is-spare-test
  (testing "for a spare"
    (is (= true (spare? [3 7]))))
  (testing "for a strike"
    (is (= false (spare? [10]))))
  (testing "for a normal frame"
    (is (= false (spare? [3 5])))))

(deftest strike?-test
  (testing "for a strike"
    (is (= true (strike? [10]))))
  (testing "for a spare"
    (is (= false (strike? [3 7]))))
  (testing "for a normal frame"
    (is (= false (strike? [3 2])))))

(deftest score-frame-test
  (testing "in general the score is the value of the two rolls"
    (is (= 7 (score-frame [[3 4]] 0))))
  (testing "for a spare the bonus is the value of the next roll"
    (is (= 11 (score-frame [[3 7] [1 5]] 0))))
  (testing "for a strike the bonus is the value of the next 2 rolls"
    (is (= 17 (score-frame [[10] [3 4]] 0)))))
