(ns deque.core)

(defrecord Deque [front back size])

;; # Your Work

(defn make-deque
  "Create an empty deque."
  []
  (Deque. '() '() 0))

(defn deque-size
  "Return the size of a deque."
  [dq]
  (:size dq))

(defn push-front
  "Adds an element to the front of the deque."
  [dq elt]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (Deque. (cons elt f) b (inc s)))
)

(defn push-back
  "Adds an element to the back fo the deque."
  [dq elt]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (Deque. f (cons elt b) (inc s)))
)

(defn flip-front
  "Flip the back list to the front list, if necessary."
  [dq]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (Deque. (reverse b) '() (count (:back dq))))
)

(defn flip-back
  "Flip the front list to the back list, if necessary."
  [dq]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (Deque. '() (reverse f) (count (:front dq))))
)
AAA
(defn front
  "Return the front element of the deque.  May cause a flip."
  [dq]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (cond (empty? f) (first (:front (flip-front dq)))
    :else (first f)))
)

(defn back
  "Return the back element of the deque.  May cause a flip."
  [dq]
 (let [f (:front dq)
       b (:back dq)
       s (:size dq)]
    (cond (empty? b) (first (:back (flip-back dq)))
    :else (first b)))
)

(defn pop-front
  "Pops/dequeues an element from the front of the deque."
  [dq]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (cond (and (empty? b) (empty? f)) (Deque. '() '() 0)
    (empty? f) (Deque. (rest (:front (flip-front dq))) '() (dec s))
    :else (Deque. (rest f) b (dec s))))
)

(defn pop-back
  "Pops/dequeues an element from the back of the deque."
  [dq]
  (let [f (:front dq)
        b (:back dq)
        s (:size dq)]
    (cond (and (empty? b) (empty? f)) (Deque. '() '() 0)
     (empty? b) (Deque. '() (rest (:back (flip-back dq))) (dec s))
          :else (Deque. f (rest b) (dec s))))
)
