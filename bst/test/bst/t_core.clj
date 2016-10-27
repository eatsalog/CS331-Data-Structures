(ns bst.t-core
  (:refer-clojure :exclude [find])
  (:use midje.sweet)
  (:use [bst.core]))

(defn inorder "visits every nodes inorder"
  ([root f]
     (letfn [(go [dir] (when-not (nil? (dir root)) (inorder (dir root) f)))]
       (do (go :left) (f root) (go :right)))))


(defn v "returns tree values as inorder vector"
  ([bst] (with-local-vars [vec []]
           (inorder (:root bst) #(var-set vec (conj @vec (:value %)))) @vec)))

;; (root, 9)
;;         a
;;        / \
;;       b   ---f
;;      /      / \
;;     c      g   h
;;    / \    /
;;   d   e  i

(def test-tree (-> (make-tree)
               (add 2 "a") (add 1 "b") (add -1 "c") (add -2 "d") (add 0 "e") ;lefty
               (add 5 "f") (add 4 "g") (add 3 "i") (add 6 "h"))) ;righty

(facts "about size"

  (fact "returns size effectively"
       (size (make-tree)) => 0))

(facts "about add"

  (fact "adds a key effectively to the tree"
        (v (add test-tree 4.1 "j")) => ["d" "c" "e" "b" "a" "i" "g" "j" "f" "h"]
        (v (add test-tree -3 "j")) => ["j" "d" "c" "e" "b" "a" "i" "g" "f" "h"]
        (v (add test-tree 7 "j")) => ["d" "c" "e" "b" "a" "i" "g" "f" "h" "j"])

  (fact "increases the size properly"
        (size (-> (make-tree) (add 0 "j") (add 0 "a"))) => 1
        (size (add (make-tree) 1 "j")) => 1
        (size (add test-tree 2 "j")) => 9
        (size (add test-tree 1 "j")) => 9
        (size (add test-tree 6 "j")) => 9
        (size (add test-tree -2 "j")) => 9
        (size (add test-tree -5 "j")) => 10))

(facts "about find"

  (fact "finds the key of a value"
        (find test-tree 1) => "b"
        (find test-tree -2) => "d"
        (find test-tree 6) => "h"
        (find test-tree 3) => "i"
        (find test-tree 2) => "a"
        (find test-tree 11) => nil))


(facts "about find-key"

  (fact "finds key based on value effectively"
      (find-key test-tree "b") => 1
      (find-key test-tree "j") => nil))

(facts "about delete"

  (fact "deletes key from BST right and left of tree"
    (v (delete test-tree 1)) => ["d" "c" "e" "a" "i" "g" "f" "h"]
    (v (delete test-tree 2)) => ["d" "c" "e" "b" "i" "g" "f" "h"]
    (v (delete test-tree 3)) => ["d" "c" "e" "b" "a" "g" "f" "h"]
    (v (delete test-tree 4)) => ["d" "c" "e" "b" "a" "i" "f" "h"]
    (v (delete test-tree 5)) => ["d" "c" "e" "b" "a" "i" "g" "h"]
    (v (delete test-tree 6)) => ["d" "c" "e" "b" "a" "i" "g" "f"]
    (v (delete test-tree 7)) => ["d" "c" "e" "b" "a" "i" "g" "f" "h"]
    (v (delete test-tree -1)) => ["d" "e" "b" "a" "i" "g" "f" "h"]
    (v (delete test-tree -2)) => ["c" "e" "b" "a" "i" "g" "f" "h"]
    (v (delete test-tree 0)) => ["d" "c" "b" "a" "i" "g" "f" "h"])

  (fact "decrements size properly"
      (size (delete test-tree 1)) => (dec (size test-tree))
      (size (delete test-tree 99)) => (size test-tree)))

(facts "about delete-value"

  (fact "deletes key from BST right and left of tree"
    (v (delete-value test-tree "b")) => ["d" "c" "e" "a" "i" "g" "f" "h"]
    (v (delete-value test-tree "d")) => ["c" "e" "b" "a" "i" "g" "f" "h"]
    (v (delete-value test-tree "g")) => ["d" "c" "e" "b" "a" "i" "f" "h"]
    (v (delete-value test-tree "h")) => ["d" "c" "e" "b" "a" "i" "g" "f"])

  (fact "decrements size properly"
    (size (delete-value test-tree "b")) => (dec (size test-tree))
    (size (delete-value test-tree "j")) => (size test-tree)))

(facts "about map-tree"

  (fact "maps function over tree"
    (v (map-tree test-tree #(str "a" %))) => ["ad" "ac" "ae" "ab" "aa" "ai" "ag" "af" "ah"]))
