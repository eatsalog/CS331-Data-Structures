(ns heap-lab.t-core
	(:require [midje.sweet :refer :all]
            [heap-lab.core :refer :all])
	(:import [heap_lab.core Heap]))

(defn heap-equal [h1]
  (fn [h2]
    (and (= (:size h1) (:size h2))
         (= (take (:size h1) (:data h1))
            (take (:size h2) (:data h2))))))

(facts "about numbers"
       (fact "one plus one is two."
             (+ 1 1)  =>  2)
       (fact "two plus one is three."
             (+ 2 1)  =>  3))

;        1
;       / \
;      /   \
;     2     3
;    / \   / \
;   7   8 X   X

(def heap (Heap. 5 [1 2 3 7 8 nil nil]))
(def heap2 (Heap. 3 [1 19 32]))

(facts "about make-heap"
       (fact "makes empty heap"
             (make-heap 2) => (heap-equal (Heap. 0 [nil nil]))
             (make-heap 0) => (heap-equal (Heap. 0 [])))
  )

(facts "about top"
       (fact "returns top element of heap"
             (top heap) => 1
             (top (make-heap 0)) => nil
             (top (make-heap 1)) => nil)
  )

(facts "about add"
       (fact "propery add element into heap"
             (add heap 0) => (heap-equal (Heap. 6 [0 2 1 7 8 3 nil]))
             (-> heap (add 0) (add 11)) => (heap-equal (Heap. 7 [0 2 1 7 8 3 11]))
             (-> heap (add 0) (add 11) (add 13)) => (heap-equal (Heap. 8 [0 2 1 7 8 3 11 13])))
  )

(facts "about delete"
       (fact "properly deletes element from heap"
             (delete heap) => (heap-equal (Heap. 4 [2 7 3 8 nil nil]))
             (-> heap delete delete) => (heap-equal (Heap. 3 [3 7 8 nil nil nil]))
             (-> heap delete delete delete) => (heap-equal (Heap. 2 [7 8 nil nil nil nil]))
             (-> heap delete delete delete delete) => (heap-equal (Heap. 1 [8 nil nil nil nil nil]))
             (delete (make-heap 0)) => (heap-equal (Heap. 0 []))
             (delete (make-heap 1)) => (heap-equal (Heap. 0 [nil]))
             (delete heap2) => (heap-equal (Heap. 2 [19 32 nil]))
             (-> heap2 delete delete) => (heap-equal (Heap. 1 [32 nil nil])))
       )
