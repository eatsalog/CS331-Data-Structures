(ns bst.core
  (:refer-clojure :exclude [find]))

;; # Introduction
;;
;; In this lab you get to write a BST like the one we did in class, only
;; this time it is a dictionary structure and not a set.
;; As such, the "data" element from before will have a key and value instead.

(defrecord BST [root size])
(defrecord BNode [left key value right])

(declare find)

(defn make-node
  ([key value]  (make-node nil key value nil))
  ([left key value right] (BNode. left key value right))
  )

(defn make-tree []
  (BST. nil 0))

;; # Size
;;
;; A warmup function.

(defn size "Return the size of the tree."
  [t]
  (:size t)
  )

;; # Add
;;
;; The nodes will be entered into the tree on the basis of their key.
;; If someone tries to add a key that is already there, we replace the value
;; with the new entry.

(defn add-helper
  [{:keys [left key value right] :as root} nu-key nu-value]
  (cond (nil? root)
        (BNode. left nu-key nu-value right)

        (pos? (compare nu-key key))
        (BNode. left key value (add-helper right nu-key nu-value))

        (zero? (compare nu-key key))
        (BNode. left key nu-value right)

        :else (BNode. (add-helper left nu-key nu-value) key value right)
  ))

(defn add "Add a key and value to the BST."
  [bst nk nv]
  (if (find bst nk) (BST. (add-helper (:root bst) nk nv) (:size bst))
    (BST. (add-helper (:root bst) nk nv) (inc (:size bst))))
  )

;; # Find
;;
;; We need two versions of find.  The first one takes a key and returns the
;; value.  The second takes a value and returns the key.  Note that the second
;; version of the function must search the entire tree!  If the search item is not
;; there, return nil.

(defn find-helper
  [node look-key]
  (cond (nil? node) nil
        (zero? (compare look-key (:key node))) (:value node)
        (neg? (compare look-key (:key node))) (find-helper (:left node) look-key)
        :else                    (find-helper (:right node) look-key)
  ))

(defn find "Look for a key and return the corresponding value."
  [bst look-key]
  (find-helper (:root bst) look-key)
  )

(defn find-key-helper
  [node look-value]
  (cond (nil? node) nil
        (zero? (compare (:value node) look-value)) (:key node)
         (not (nil? (find-key-helper (:left node) look-value))) (find-key-helper (:left node) look-value)
        :else (find-key-helper (:right node) look-value))
  )


(defn find-key "Look for a value and return the corresponding key."
  [bst look-value]
  (find-key-helper (:root bst) look-value)
  )

(defn successor
  [node]
  (cond (nil? (:left node)) node
        (nil? (:right (:left node))) (:left node)
        :else (loop [lastRight (:left node)]

    (if (nil? (:right lastRight)) lastRight (recur (:right lastRight))))
  ))

;; # Delete
;;
;; Similarly, we have two versions of delete.  Please use the predecessor node if
;; you need to delete a child with two elements.


(defn delete-helper [node victim]
  (cond (nil? node) nil
        (neg? (compare victim (:key node)))
        (BNode. (delete-helper (:left node) victim) (:key node) (:value node) (:right node))

        (pos? (compare victim (:key node)))
        (BNode. (:left node) (:key node) (:value node) (delete-helper (:right node) victim))

        :else
        (cond (and (nil? (:left node))
                   (nil? (:right node))) nil
              (and (not (nil? (:left node))) (nil? (:right node))) (:left node)
              (and (not (nil? (:right node))) (nil? (:left node))) (:right node)
              :two-children
              (let [succ (successor node)]
                 (if (nil? succ) (BNode. nil (:key (:left node)) (:value (:left node)) (:right node))
                (BNode. (delete-helper (:left node) (:key succ))
                        (:key succ) (:value succ) (:right node))))))
  )


(defn delete [bst victim]
  (let [k (find bst victim)]
    (if (nil? k) bst
      (BST. (delete-helper (:root bst) victim) (dec (:size bst)))))
  )

(defn delete-value [bst victim]
  (let [key (find-key bst victim)]
    (if (nil? key) bst (delete bst key)))
   )

;; # Map Tree
;;
;; This function takes a tree t and maps a function f over it.
;; If your tree is ((x 3 x) 5 ((x 7 x) 6 x)), then (map-tree t inc)
;; will return ((x 4 x) 6 ((x 8 x) 7 x))


(defn map-tree-helper
  [node f]
  (if (nil? node) nil
       (BNode. (map-tree-helper (:left node) f)
               (:key node)
               (f (:value node))
               (map-tree-helper (:right node) f)))
  )

(defn map-tree
  [t f]
  (BST. (map-tree-helper (:root t) f) (:size t))
  )
