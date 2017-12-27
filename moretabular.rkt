;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname moretabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 07, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

(define tableA (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))

(define tableB (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define tableC (list (list 4 3 2 1) (list 8 7 6 5) (list 12 11 10 9)))

(define tableD (list (list 1 0 0 -1) (list 2 3 4 5.6)))

(define lotA (list (list (list 1 0 0 1) (list 2 3 4 5.6))
                   (list (list 1 0 0 -1) (list 2 3 4 5))
                   (list (list 2 1 1 0) (list 3 4 5 6.6))))


;;Part A

;;(mirror table) consumes a Table (table) and produces a new Table with all the 
;; elements in the rows of the consumed Table (table) reversed
;;mirror: Table -> Table
;;Examples:
(check-expect (mirror empty) empty)
(check-expect (mirror tableB) tableC)

(define (mirror table)
  (local
    [;;(reverse-row row) consumes a row and produces a new row with all the elements
     ;; in the consumed row reversed
     ;;reverse-row: (listof Num) -> (listof Num)
     (define (reverse-row row)
       (cond
         [(empty? row) empty]
         [else (append (reverse-row (rest row)) (list (first row)))]))]
    (cond
      [(empty? table) empty]
      [else (cons (reverse-row (first table))
                  (mirror (rest table)))])))

;;Tests:
(check-expect (mirror (list (list -1 -2 4 0))) (list (list 0 4 -2 -1)))
(check-expect (mirror tableC) tableB)


;;Part B

;;(element-apply-many lof table) consumes a lof and a table and produces a
;; (listof Table) that result from applying each function to each element in the
;; consumed Table (table)
;;element-apply-many: (listof (anyof (Num -> Num) (Num -> Int) (Num -> Nat))) ->
;; (listof Table)
;;Examples:
(check-expect (element-apply-many (list abs floor add1) tableD) lotA)
(check-expect (element-apply-many (list abs floor add1) empty) (list empty empty empty))
(check-expect (element-apply-many empty tableA) empty)

(define (element-apply-many lof table)
  (local
    [;;(apply-funct funct row) consumes a funct and a row and produces a new row
     ;; made from applying funct to all elements of the consumed row
     ;; apply-funct: (anyof (Num -> Num) (Num -> Int) (Num -> Nat)) (listof Num) ->
     ;; (listof (anyof Num Int Nat))
     (define (apply-funct funct row)
       (cond
         [(empty? row) empty]
         [else (cons (funct (first row)) (apply-funct funct (rest row)))]))]
    (local
      ;;(funct-table funct table) consumes a funct and a table and produces a new Table
      ;; with funct applied to all elements of the consumed Table (table)
      ;;funct-table: (anyof (Num -> Num) (Num -> Int) (Num -> Nat)) Table -> Table
      [(define (funct-table funct table)
         (cond
           [(empty? table) empty]
           [else (cons (apply-funct funct (first table))
                       (funct-table funct (rest table)))]))]
      (cond
        [(empty? lof) empty]
        [else (cons (funct-table (first lof) table)
                    (element-apply-many (rest lof) table))]))))

;;Tests:
(check-expect (element-apply-many (list sqr) tableD) (list (list (list 1 0 0 1)
                                                                 (list 4 9 16 31.36))))
(check-expect (element-apply-many (list abs floor add1) tableD) lotA)


;;Part C


;; (apply-function f arg) produces the result of f with the given argument arg.
;; (apply-function: (X -> Y) X -> Y
(define (apply-function f arg)
  (f arg))


;;(scale-smallest table offset) consumes a table and offset and produces a function
;; that consumes a number, multiplies that number by the smallest element in the consumed
;; Table (table) and adds offset
;;scale-smallest: Table Num -> (Num -> Num)
;;requires: table is non-empty (at least one row and column)
;;Examples:
(check-expect (apply-function (scale-smallest tableA 2.4) 7) -18.6)

(define (scale-smallest table offset)
  (local
    [;;(smallest-in-row smallest-so-far row) consumes a smallest-so-far and a row and
     ;; produces the smaller of the smallest number in row and smallest-so-far
     ;;smallest-in-row: Num (listof Num) -> Num
     (define (smallest-in-row smallest-so-far row)
       (cond
         [(empty? row) smallest-so-far]
         [(<= smallest-so-far (first row)) (smallest-in-row smallest-so-far (rest row))]
         [else (smallest-in-row (first row) (rest row))]))]
    (local
      [;;(smallest-in-table smallest-so-far table) consumes a smallest-so-far and a
       ;; table and produces the smaller of the smallest number in the consumed Table
       ;; (table) and smallest-so-far
       ;;smallest-in-table: Num Table -> Num
       (define (smallest-in-table smallest-so-far table)
         (cond
           [(empty? table) smallest-so-far]
           [(<= smallest-so-far (smallest-in-row smallest-so-far (first table)))
            (smallest-in-table smallest-so-far (rest table))]
           [else (smallest-in-table
                  (smallest-in-row smallest-so-far (first table)) (rest table))]))]
      (local [;;(my-scale-smallest-function num) consumes a num and produces the number
              ;; resulting from multiplying num by the smallest element in the given
              ;; Table and adding the offset
              ;;my-scale-smallest-function: Num -> Num
              (define (my-scale-smallest-function num)
                (+ offset (* (smallest-in-table (first (first table)) table) num)))]
        my-scale-smallest-function))))

;;Tests:
(check-expect (apply-function (scale-smallest tableB 3) 2) 5)
(check-expect (apply-function (scale-smallest tableA 0) 1) -3)            
(check-expect (apply-function (scale-smallest tableA 3.1415) 0) 3.1415)


