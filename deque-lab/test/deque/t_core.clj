(ns deque.t-core
  (:use midje.sweet)
  (:use [deque.core])
  (:import [deque.core Deque] ))

(facts "about deque-size"
  (let [x '()
        y '(3 4)
    ndq (Deque. x y (+ (count x) (count y)))]

    (fact "it counts the correct size of the deque"
      (deque-size ndq) => 2)))

(facts "about push-front"
  (let [x '()
        y '(2 9 4)
    ndq (Deque. x y (+ (count x) (count y)))]

    (fact "adds an element to the front of the deque"
        (push-front ndq 4) => (Deque. '(4) '(2 9 4) 4))

    (fact "increments size properly"
        (-> (push-front ndq 4) :size) => 4)))

(facts "about push-back"
  (let [x '(3)
        y '(9 1 6)
    ndq (Deque. x y (+ (count x) (count y)))]

    (fact "adds an element to the front of the deque"
        (push-back ndq 5) => (Deque. '(3) '(5 9 1 6) 5))

   (fact "increments size properly"
       (-> (push-back ndq 5) :size) => 5)))

(facts "about flip-front"
  (let [x '()
        y '(2 3 1)
    ndq (Deque. x y (+ (count x) (count y)))
        {:keys [front back size]} ndq]

    (fact "flips back to front"
        (flip-front ndq) => (Deque. '(1 3 2) '() 3))))

(facts "about flip-back"
  (let [x '(8 2 9)
        y '()
    ndq (Deque. x y (+ (count x) (count y)))
        {:keys [front back size]} ndq]

    (fact "flips front to back"
        (flip-back ndq) => (Deque. '() '(9 2 8) 3))))

(facts "about front"
  (let [x '(7 5)
        y '(9 3)
        z '()
    ndq (Deque. x y (+ (count x) (count y)))
    ndz (Deque. z y (+ (count z) (count y)))]

    (fact "returns front (front not empty)"
        (front ndq) => 7)

    (fact "returns flipped front (front empty)"
        (front ndz) => 3)))

(facts "about back"
  (let [x '(7 5)
        y '(9 3)
        z '()
    ndq (Deque. x y (+ (count x) (count y)))
    ndz (Deque. x z (+ (count x) (count z)))]

    (fact "returns back (back not empty)"
        (back ndq) => 9)

    (fact "returns flipped back (back empty)"
        (back ndz) => 5)))

(facts "about pop-front"
  (let [x '(7 5)
        y '(9 3)
        z '()
        w '()
    ndq (Deque. x y (+ (count x) (count y)))
    ndz (Deque. z y (+ (count z) (count y)))
    ndw (Deque. w z (+ (count w) (count z)))
        {:keys [front back size]} ndq
        {:keys [fz bz sz]} ndz]

    (fact "pop from front (front not empty)"
        (pop-front ndq) => (Deque. '(5) '(9 3) 3))

    (fact "pop from front (front empty)"
        (pop-front ndz) => (Deque. '(9) '() 1))

    (fact "size decrements properly when front is not empty"
    (-> (pop-front ndq):size) => 3)

    (fact "size decrements properly when front is empty"
    (-> (pop-front ndz):size) => 1)

    (fact "size remains zero"
    (-> (pop-back ndw):size) => 0)))

(facts "about pop-back"
  (let [x '(7 5)
        y '(9 3)
        z '()
        w '()
    ndq (Deque. x y (+ (count x) (count y)))
    ndz (Deque. x z (+ (count x) (count z)))
    ndw (Deque. w z (+ (count w) (count z)))
        {:keys [front back size]} ndq
        {:keys [fz bz sz]} ndz]

    (fact "pop from back (back not empty)"
        (pop-back ndq) => (Deque. '(7 5) '(3) 3))

    (fact "pop from back (back empty)"
        (pop-back ndz) => (Deque. '() '(7) 1))

    (fact "size decrements properly when back is not empty"
    (-> (pop-back ndq):size) => 3)

    (fact "size decrements properly when back is empty"
    (-> (pop-back ndz):size) => 1)

    (fact "size remains zero"
    (-> (pop-back ndw):size) => 0)))

(facts "about this lab"
  (fact "the student never started it."
        (+ 1 2)  => 3))
