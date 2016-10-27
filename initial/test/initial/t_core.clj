(ns initial.t-core
  (:use midje.sweet)
  (:use [initial.core]))

(facts "about plus"
  (fact "it adds numbers."
        (plus)   => 0
        (plus 10)  => 10
        (plus 10 20) => 30
        (plus 10 20 30 40 50) => 150))

(facts "about socialist plus"
       (fact "it subsidized fewer than two elements" 
             (socialist-plus 10) => 11
             (socialist-plus) => 1
        )
       (fact "it does nothing with two elements. Except add."
             (socialist-plus 10 11) => 21
       )

       (fact "it taxes the result if there are more than 2 elements."
             (socialist-plus 1 2 3) => 5
       )
       )

(facts "about capitalist plus"
       (fact "it taxes fewer than two elements"
             (capitalist-plus 2) => 1
             )
       (fact "it does nothing with two elements. Except add."
             (capitalist-plus 1 2) => 3
             )
       (fact "it subsidizes the result if there are more than 2 elements."
             (capitalist-plus 1 2 3) => 7
             ))

(facts "about communist plus"
  (fact "it only returns 10."
        (communist-plus)   => 10
        (communist-plus 10)  => 10
        (communist-plus 10 20) => 10
        (communist-plus 10 20 30 40 50) => 10))

(facts "about political extreemist plus"
  (fact "it multiplies instead of adds."
        (political-extreemist-plus)   => 1 
        (political-extreemist-plus 10)  => 10
        (political-extreemist-plus 10 20) => 200
        (political-extreemist-plus 10 20 30) => 6000))


(facts "about this lab"
       (fact "the student never bothered to start it."
       true => true))
