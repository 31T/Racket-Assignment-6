;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 06, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct node (key val left right))
;;A Node is a (make-node Num Str BT BT)

;; A binary tree (BT) is one of:
;;* empty
;;* Node

(define exampleBT (make-node 1 "a"
                             (make-node 7 "b" empty empty)
                             (make-node 3 "c" (make-node 7 "d" empty empty) empty)))

(define exampleBT2 (make-node 2 "a" empty (make-node 6 "b" empty empty)))

;;Part A

;;(define height bt) consumes a BT (bt) and produces the number of nodes along the longest
;; path from the root to a leaf node in the BT
;;height: BT -> Nat
;;Examples:
(check-expect (height empty) 0)
(check-expect (height exampleBT) 3)

(define (height bt)
  (cond
    [(empty? bt) 0]
    [else
     (max
      (+ 1 (height (node-left bt)))
      (+ 1 (height (node-right bt))))]))

;;Tests:
(check-expect (height exampleBT2) 2)


;;Part B

;;(right-or-left-tree bt los) consumes a BT (bt) and a los and produces the BT
;; on the left or right of the consumed BT depending on what symbol is first in los
;;right-or-left-tree: BT (listof Sym) -> BT
;;requires: the symbols in los are either 'L or 'R and both los and bt are not empty
;;Examples:
(check-expect (right-or-left-tree exampleBT (list 'L 'L))
              (make-node 7 "b" empty empty))
(check-expect (right-or-left-tree exampleBT2 (list 'L)) empty)

(define (right-or-left-tree bt los)
  (cond
    [(symbol=? 'L (first los)) (node-left bt)]
    [else (node-right bt)]))


;;(find-in-tree bt los) consumes a BT (bt) and a los and produces the key of the node
;; rooted at the tree after following the path described by los starting from the root
;; and produces false if the path goes beyond any existing leaf
;;find-in-tree: BT (listof Sym) -> (anyof Num Bool)
;;requires: all symbols in los are either 'L or 'R
;;Examples:
(check-expect (find-in-tree exampleBT empty) 1)
(check-expect (find-in-tree empty 2) false)
(check-expect (find-in-tree exampleBT (list 'R 'L)) 7)

(define (find-in-tree bt los)
  (cond
    [(empty? bt) false]
    [(empty? los) (node-key bt)]
    [else
     (find-in-tree (right-or-left-tree bt los) (rest los))]))

;;Tests:
(check-expect (find-in-tree exampleBT2 (list 'R)) 6)
(check-expect (find-in-tree exampleBT (list 'L 'L)) false)


;;Part C

;;(prune bt n) consumes a BT (bt) and a number (n) and produces a new BT where all
;; subtrees rooted at n (as a key) have been removed from the consumed BT
;;prune: BT Num -> BT
;;Examples:
(check-expect (prune exampleBT 1) empty)
(check-expect (prune empty 1) empty)
(check-expect (prune exampleBT 7)
              (make-node 1 "a" empty (make-node 3 "c" empty empty)))

(define (prune bt n)
  (cond
    [(empty? bt) empty]
    [(= (node-key bt) n) empty]
    [else (make-node (node-key bt) (node-val bt)
                     (prune (node-left bt) n)
                     (prune (node-right bt) n))]))

;;Tests
(check-expect (prune exampleBT2 2) empty)
(check-expect (prune exampleBT2 6) (make-node 2 "a" empty empty))

