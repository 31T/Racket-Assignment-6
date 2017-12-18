;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname anagrams) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 06, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define listA (list (list 1 2) 'A 'A (list 1) (list 1 2) 3))

(define listB (list (list 'red 9) (list 'blue 2) (list 3 2)))

(define listC (list (list 'blue 2) (list 3 2) (list 'red 9)))

(define listD (list (list 'blue 2) (list 3 2) (list 'black 9)))

;;Part A

;;(insert char loc) consumes a char and a loc and creates a new
;; alphabetically sorted loc with char
;;insert: Char (listof Char) -> (listof Char)
;;requires: loc is alphabetically sorted or empty
;;Examples:
(check-expect (insert #\a (string->list "hi")) (string->list "ahi"))
(check-expect (insert #\a empty) (list #\a))

(define (insert char loc)
  (cond
    [(empty? loc) (cons char empty)]
    [(char<=? char (first loc)) (cons char loc)]
    [else (cons (first loc) (insert char (rest loc)))]))


;;(sort-chars loc) consumes a loc (listof Char) and produces a new
;; alphabetically sorted (listof Char) from loc
;;sort-chars: (listof Char) -> (listof Char)
;;Examples:
(check-expect (sort-chars empty) empty)
(check-expect (sort-chars (string->list "order")) (string->list "deorr"))

(define (sort-chars loc)
  (cond
    [(empty? loc) empty]
    [else (insert (first loc) (sort-chars (rest loc)))]))

;;Tests:
(check-expect (sort-chars (string->list "ordered")) (string->list "ddeeorr"))
(check-expect (sort-chars (string->list "zut")) (string->list "tuz"))


;;Part B

;;(anagrams/sort? str1 str2) consumes two strings (str1 and str2) and produces
;; true if str1 and str2 are anagrams of each other and false otherwise
;;anagrams/sort?: Str Str -> Bool
;;Examples:
(check-expect (anagrams/sort? "listen" "silent") true)
(check-expect (anagrams/sort? "" "") true)

(define (anagrams/sort? str1 str2)
  (cond
    [(equal? (sort-chars (string->list str1)) (sort-chars (string->list str2))) true]
    [else false]))

;;Tests:
(check-expect (anagrams/sort? "amazing" "gnizama") true)
(check-expect (anagrams/sort? "captain" "lord") false)


;;Part C

;;(remove-elem elem loa) consumes an elem and a (listof Any) loa and produces a new loa
;; with all instances of elem removed
;;remove-elem: Any (listof Any) -> (listof Any)
;;Examples:
(check-expect (remove-elem 5 empty) empty)
(check-expect (remove-elem 5 (list 5 'Blue 5 'Red)) (list 'Blue 'Red))

(define (remove-elem elem loa)
  (cond
    [(empty? loa) empty]
    [(equal? (first loa) elem) (remove-elem elem (rest loa))]
    [else (cons (first loa) (remove-elem elem (rest loa)))]))


;;(freq-count-elem/acc freq-so-far elem loa) consumes a freq-so-far, an elem, and
;; a loa and adds the number of times that elem appears in loa to freq-so-far
;;freq-count-elem/acc: Nat Any (listof Any) -> Nat
;;Examples:
(check-expect (freq-count-elem/acc 0 5 (list 5 5 5 1 'L 2)) 3)
(check-expect (freq-count-elem/acc 0 5 empty) 0)

(define (freq-count-elem/acc freq-so-far elem loa)
  (cond
    [(empty? loa) freq-so-far]
    [(equal? elem (first loa))
     (freq-count-elem/acc (add1 freq-so-far) elem (rest loa))]
    [else
     (freq-count-elem/acc freq-so-far elem (rest loa))]))


;;(freq-count loa) consumes a (listof Any) loa and produces a new list of pairs where
;; the first element of each pair is an element from loa and the second element
;; of each pair is the number of times the element appears in loa
;;freq-count: (listof Any) -> (listof (list Any Nat))
;;Examples: 
(check-expect (freq-count listA)
              (list (list (list 1 2) 2) (list 'A 2) (list (list 1) 1) (list 3 1)))
(check-expect (freq-count empty) empty)

(define (freq-count loa)
  (cond
    [(empty? loa) empty]
    [else (cons (list (first loa) (freq-count-elem/acc 0 (first loa) loa))
                (freq-count (remove-elem (first loa) loa)))]))

;;Tests:
(check-expect (freq-count '(red 7 9 (7 9) red 9))
              '((red 2)(7 1)(9 2)((7 9) 1)))
(check-expect (freq-count (list 1 2 3)) (list (list 1 1) (list 2 1) (list 3 1)))


;;Part D

;;(elem-in-list? elem list2) consumes an elem and a list2 and produces true if elem
;; is in list2 and false otherwise
;;elem-in-list?: (list Any Nat) (listof (list Any Nat) -> Bool
;;Examples:
(check-expect (elem-in-list? (list 'Red 2) empty) false)
(check-expect (elem-in-list? (list 3 2) listC) true)
(check-expect (elem-in-list? (list 3 1) listC) false)

(define (elem-in-list? elem list2)
  (cond
    [(empty? list2) false]
    [(equal? (first elem) (first (first list2)))
     (cond
       [(equal? (rest elem) (rest (first list2))) true]
       [else false])]
    [else (elem-in-list? elem (rest list2))]))


;;(freq-equiv? list1 list2) consumes a list1 and a list2 and produces true if they
;; are rearrangements of each other and false otherwise
;;freq-equiv?: (listof (list Any Nat)) (listof (list Any Nat)) -> Bool
;;Examples:
(check-expect (freq-equiv? empty empty) true)
(check-expect (freq-equiv? empty listD) false)
(check-expect (freq-equiv? listD empty) false)
(check-expect (freq-equiv? listC listB) true)

(define (freq-equiv? list1 list2)
  (cond
    [(and (empty? list2) (empty? list1)) true]
    [(empty? list1) false]
    [(empty? list2) false]
    [(elem-in-list? (first list1) list2)
     (freq-equiv? (rest list1) (remove-elem (first list1) list2))]
    [else false]))

;;Tests
(check-expect (freq-equiv? listD listB) false)
(check-expect (freq-equiv? listC listD) false)


;;Part E

;;(anagrams/count? str1 str2) consumes a str1 and a str2 and produces true if the two
;; strings are anagrams of each other and false otherwise
;;anagrams/count?: Str Str -> Bool
;;Examples:
(check-expect (anagrams/count? "hi" "ih") true)

(define (anagrams/count? str1 str2)
  (freq-equiv?
   (freq-count (string->list str1))
   (freq-count (string->list str2))))

;;Tests:
(check-expect (anagrams/count? "silent" "listen") true)
(check-expect (anagrams/count? "eleven plus two" "twelve plus one") true)
(check-expect (anagrams/count? "hi" "hill") false)

