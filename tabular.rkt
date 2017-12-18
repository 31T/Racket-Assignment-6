;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 06, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

(define tableA (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))

(define tableB (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define tableC (list (list 9 5 7 13) (list 8 13 12 14) (list 8 11 8 12)))

(define tableD (list (list 2 4 6 8) (list 10 12 14 16) (list 18 20 22 24)))

(define tableE (list (list 17 8 11 22) (list 11 20 17 20) (list 7 12 5 12)))

;;Part A
 
;;(mult-by-row n lon) consumes a number (n) and a (listof Num) (lon) and
;; produces a new (listof Num) with each number in lon being multiplied by n
;;mult-by-row: Num (listof Num) -> (listof Num)
;;Examples:
(check-expect (mult-by-row 5 empty) empty)
(check-expect (mult-by-row 5 (list 1 2 3)) (list 5 10 15))

(define (mult-by-row n lon)
  (cond
    [(empty? lon) empty]
    [else (cons (* n (first lon)) (mult-by-row n (rest lon)))]))


;;(mult-by n table) consumes a number (n) and a Table (table) and produces
;; a new Table with each number in the consumed Table being multiplied by n
;;mult-by: Num Table -> Table
;;Examples:
(check-expect (mult-by 2 tableB) tableD)
(check-expect (mult-by 2 empty) empty)

(define (mult-by n table)
  (cond
    [(empty? table) empty]
    [else (cons (mult-by-row n (first table)) (mult-by n (rest table)))]))

;;Tests:
(check-expect (mult-by 10 tableA)
              (list (list 80 30 40 90) (list 30 70 50 60) (list -10 10 -30 0)))


;;Part B

;;(check-row r-count row lon) consumes two natural numbers (r-count and row) and a
;; (listof Num) lon and produces the number in the desired column of the row (called
;; row) and produces false if no such number exists
;;check-row: Nat Nat (listof Num) -> (anyof Bool Num)
;;Examples:
(check-expect (check-row 0 2 empty) false)
(check-expect (check-row 0 2 (list 2 3 4)) 4)
(check-expect (check-row 0 3 (list 1 2)) false)

(define (check-row r-count row lon)
  (cond
    [(empty? lon) false]
    [(= r-count row) (first lon)]
    [else (check-row (add1 r-count) row (rest lon))]))


;;(check-columns c-count row column table) consumes three natural numbers (c-count
;; row and column) and a Table (table) and produces the number in the desired row
;; and column of the consumed Table and produces false if no such number exists
;;check-columns: Nat Nat Nat Table -> (anyof Bool Num)

(define (check-columns c-count row column table)
  (cond
    [(empty? table) false]
    [(= c-count column) (check-row 0 row (first table))]
    [else (check-columns (add1 c-count) row column (rest table))]))


;;(get-elem row column table) consumes two natural numbers (row and column) and a
;; Table (table) and produces the number in the desired row and column of the consumed
;; Table and produces false if no such number exists
;;get-elem: Nat Nat Table -> (anyof Bool Num)
;;Examples:
(check-expect (get-elem 1 1 empty) false)
(check-expect (get-elem 0 0 tableA) 8)

(define (get-elem row column table)
  (check-columns 0 column row table))

;;Tests:
(check-expect (get-elem 2 2 tableB) 11)
(check-expect (get-elem 10 10 tableB) false)
(check-expect (get-elem 1 2 tableA) 5)


;;Part C

;;(find-row-elem r-count col-n lon) consumes two natural numbers (r-count and col-n)
;; and a (listof Num) lon and produces the number in the desired column of lon
;; and produces empty if no such number exists
;;find-row: Nat Nat (listof Num) -> (anyof (listof Num) empty)
;;Examples:
(check-expect (find-row-elem 0 2 empty) empty)
(check-expect (find-row-elem 0 2 (list 1 2 3)) (list 3))

(define (find-row-elem r-count col-n lon)
  (cond
    [(empty? lon) empty]
    [(= r-count col-n) (cons (first lon) empty)]
    [else (find-row-elem (add1 r-count) col-n (rest lon))]))


;;(col col-n table) consumes a natural number col-n and a Table (table) and produces
;; the numbers in the desired column of the consumed Table and produces empty if no
;; such number exists
;;col: Nat Table -> (anyof (listof Num) empty)
;;Examples:
(check-expect (col 2 tableA) (list 4 5 -3))
(check-expect (col 2 empty) empty)

(define (col col-n table)
  (cond
    [(empty? table) empty]
    [else (append (find-row-elem 0 col-n (first table))
                  (col col-n (rest table)))]))

;;Tests:
(check-expect (col 2 tableB) (list 3 7 11))
(check-expect (col 6 tableB) empty)


;;Part D

;;(sum-tables-row lon1 lon2) consumes two (listof Num)s called lon1 and lon2 and
;; produces a new (listof Num) with each number being the numbers from lon1 and lon2
;;sum-tables-row: (listof Num) (listof Num) -> (listof Num)
;;requires: lon1 and lon2 have the same number of list elements
;;Examples:
(check-expect (sum-tables-row empty empty) empty)
(check-expect (sum-tables-row (list 1 2) (list 3 4)) (list 4 6))

(define (sum-tables-row lon1 lon2)
  (cond
    [(empty? lon1) empty]
    [else (cons (+ (first lon1) (first lon2))
                  (sum-tables-row (rest lon1) (rest lon2)))]))

    
;;(sum-tables table1 table2) consumes a Table (table1) and a Table (table2) and produces
;; a new Table which results from adding up the elements pairwise between table1 and
;; table2
;;sum-tables: Table Table -> Table
;;requires: table1 and table2 must have the same dimensions (rows and columns)
;;Examples:
(check-expect (sum-tables tableA tableB) tableC)
(check-expect (sum-tables empty empty) empty)

(define (sum-tables table1 table2)
  (cond
    [(empty? table1) empty]
    [else (cons (sum-tables-row (first table1) (first table2))
                (sum-tables (rest table1) (rest table2)))]))

;;Tests:
(check-expect (sum-tables tableA tableC) tableE)
(check-expect (sum-tables empty empty) empty)
