(ns heap-lab.core)
(declare move-up percolate-down swap minimize)

;; # Array Based Heaps
;;
;; Just in time for thanksgiving, a simple lab about Heaps!
;;
;; We will use vectors to handle this, with a top-level record
;; to keep track of the vector and the size.

(defrecord Heap [size data])

;; We will initialize this using the `make-heap` function.

(defn make-heap
  "Creates an empty heap.  Specify the size for the data vector.
The vector will be populated with `nil`."
  [capacity]
  (Heap. 0 (apply vector (repeat capacity nil))))

;; To access the elements of the heap, we will use these functions
;; `get`, `left`, `right`, and `parent`.

(defn heap-get
  "Return the value of the heap vector at the given index.
Throws an exception if the index is out of the range.
this is part of the implementation, not for public consumption."
  [heap loc]
  (cond (>= loc (count (:data heap)))
        (throw (Exception. (str "Get called with " loc " but last vector slot is " (dec (count (:data heap))))))

        :otherwise
        (get-in heap [:data loc])))

(defn heap-set
  "Set the value of the heap vector at the given index.
Throws an exception if the index is out of the range.
this is part of the implementation, not for public consumption."
  [heap loc value]
  (cond (>= loc (count (:data heap)))
        (throw (Exception. (str "Get called with " loc " but last vector slot is " (dec (count (:data heap))))))

        :otherwise
        (assoc-in heap [:data loc] value)))

(defn heap-left
  "Return the left index."
  [loc]
  (inc (* loc 2)))

(defn heap-right
  "Return the right index."
  [loc]
  (+ 2 (* loc 2)))

(defn heap-parent
  "Return the parent index."
  [loc]
  (int (/ (dec loc) 2)))
;; Now it's time for your code!  You need these three, but you are welcome to
;; write helper functions if you want (e.g., `percolate-down`.)  Do **not** write
;; `midje` tests for them, because they are not part of the spec.

(defn top
  "Return the top element of a heap.
If the heap has no elements, return `nil`."
  [{:keys [size data] :as heap}]
  (if (zero? size) nil
        (first data)
  ))

(defn swap
  [heap a b]
    (-> heap (assoc a (heap b)) (assoc b (heap a)))
  )

(defn minimize
  [& more]
  (let [s (remove nil? more)]
    (if (empty? s) nil (reduce min s)))
  )

(defn move-up
  [heap loc]
  (let [value (get heap loc)
        parent-id (heap-parent loc)
        parent-value (heap parent-id)]
    (if (neg? (compare value parent-value))
      (move-up (swap heap loc parent-id) parent-id)
      heap))
  )

(defn percolate-down
  [heap loc]
  (let [left-id (heap-left loc)
        left-value (get heap left-id)
        right-id (heap-right loc)
        right-value (get heap right-id)
        value (get heap loc)
        min-value (minimize left-value right-value)]
    (cond (nil? min-value) heap
          (pos? (compare value min-value)) (if (= min-value left-value)
                                             (percolate-down (swap heap loc left-id) left-id)
                                             (percolate-down (swap heap loc right-id) right-id))
          :else-greater-than-right-and-left heap))
  )

(defn add
  "Adds a new element to the heap.
If the data vector is too small, we resize it."
  [{:keys [size data] :as heap} value]
  (let [s-data (if (= size (count data)) (conj data nil) data)]
    (Heap. (inc size) (move-up (assoc s-data size value) size)))
  )

(defn delete
  "Deletes the first element of the heap.
Returns the new heap."
  [{:keys [size data] :as heap}]
  (if (zero? size) heap
    (let [newplaces (assoc (into [(get data (dec size))] (rest data)) (dec size) nil)]
      (Heap. (dec size) (percolate-down newplaces 0))))
  )
