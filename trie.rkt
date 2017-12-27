;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname trie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRENDAN ZHANG (20720995)
;; CS135 Fall 2017
;; Assignment 07, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a07lib.rkt")


;;Part A

(define a-tnode (make-tnode #\a false (list (make-tnode #\t true empty))))

(define c-tnode
  (make-tnode #\c false
              (list (make-tnode #\o false
                                (list (make-tnode #\o true empty)
                                      (make-tnode #\w true empty)))
                    (make-tnode #\s false
                                (list (make-tnode #\1 false
                                                  (list (make-tnode #\1 false
                                                                    (list (make-tnode #\5 true empty)
                                                                          (make-tnode #\6 true empty)))
                                                        (make-tnode #\3 false
                                                                    (list (make-tnode #\5 true empty)
                                                                          (make-tnode #\6 true empty))))))))))

(define a-c-trie (make-trie (list a-tnode c-tnode)))


;;Part B


;;trie-template: Trie -> Any
(define (trie-template trie)
  (list-tnode-template (trie-children trie)))


;;list-tnode-template: (listof TNode) -> Any
(define (list-tnode-template lotnode)
  (cond
    [(empty? lotnode) ...]
    [else (... (tnode-template (first lotnode))...
               (list-tnode-template (rest lotnode))...)]))


;;tnode-template: TNode -> Any
(define (tnode-template tnode)
  (cond
    [(... (tnode-key tnode) ...) ...]
    [(tnode-ends-word? tnode) ...]
    [else (... (list-tnode-template (tnode-children tnode))...)]))


;;Part C


;;(in-tnode? char loc tnode) consumes a char, loc, and tnode and produces true if
;; the char is in the key of the consumed TNode (tnode) and false otherwise
;;in-tnode?: Char (listof Char) TNode -> Bool
;;Examples:
(check-expect (in-tnode? #\b (list #\b #\e) a-tnode) false)

(define (in-tnode? char loc tnode)
  (cond
    [(and (char=? char (tnode-key tnode)) (not (tnode-ends-word? tnode)))
     (in-list-tnode? (rest loc) (tnode-children tnode))]
    [(and (char=? char (tnode-key tnode)) (empty? (rest loc))) true]
    [(and (char=? char (tnode-key tnode)) (cons? (tnode-children tnode)))
     (in-list-tnode? (rest loc) (tnode-children tnode))]
    [else false]))


;;(in-list-tnode? loc lotnode) consumes a loc and lotnode and produces true if
;; the word represented by loc is represented in the (listof TNode) lotnode and
;; produces false otherwise
;;in-list-tnode?: (listof Char) (listof TNode) -> Bool

(define (in-list-tnode? loc lotnode)
  (cond
    [(empty? lotnode) false]
    [(empty? loc) false]
    [(in-tnode? (first loc) loc (first lotnode)) true]
    [else (in-list-tnode? loc (rest lotnode))]))


;;(in-trie? str trie) consumes a str and a trie and produces true if str is represented
;; in the consumed Trie (trie) and false otherwise
;;in-trie?: Str Trie -> Bool
;;Examples:
(check-expect (in-trie? "hi" (make-trie empty)) false)
(check-expect (in-trie? "" a-c-trie) false)

(define (in-trie? str trie)
  (in-list-tnode? (string->list str) (trie-children trie)))

;;Tests:
(check-expect (in-trie? "ha" h-u-trie) true)
(check-expect (in-trie? "hate" h-u-trie) false)


;;Part D

;;(tnode-words tnode loc-so-far lst-so-far) consumes a tnode, loc-so-far, lst-so-far and
;; adds the list of words represented by the consumed TNode (tnode) to lst-so-far and
;; produces the resulting list of words
;;tnode-words: TNode (listof Char) (listof Str) -> (listof Str)
;;Examples:
(check-expect (tnode-words a-tnode (list #\c) (list "bat")) (list "bat" "cat"))
(check-expect (tnode-words a-tnode empty empty) (list "at"))

(define (tnode-words tnode loc-so-far lst-so-far)
  (cond
    [(and (tnode-ends-word? tnode) (cons? (tnode-children tnode)))
     (lotnode-words
      (tnode-children tnode)
      (append loc-so-far (list (tnode-key tnode)))
      (append lst-so-far
              (list (list->string (append loc-so-far (list (tnode-key tnode)))))) true)]
    [(tnode-ends-word? tnode)
     (lotnode-words empty empty
                    (append lst-so-far
                            (list (list->string
                                        (append loc-so-far (list (tnode-key tnode))))))
                    false)]
    [else
     (lotnode-words (tnode-children tnode)
                    (append loc-so-far (list (tnode-key tnode))) lst-so-far false)]))


;;(lotnode-words lotnode loc-so-far lst-so-far repeat?) consumes a lotnode, loc-so-far,
;; lst-so-far, and repeat? and adds the list of words represented by lotnode to
;; lst-so-far and produces the resulting list of words
;;lotnode-words: (listof TNode) (listof Char) (listof Str) Bool -> (listof Str)

(define (lotnode-words lotnode loc-so-far lst-so-far repeat?)
  (cond
    [(empty? lotnode) lst-so-far]
    [(empty? (rest lotnode))
     (append (tnode-words (first lotnode) loc-so-far lst-so-far) empty)]
    [repeat? (append (tnode-words (first lotnode) loc-so-far lst-so-far)
                     (lotnode-words (rest lotnode) loc-so-far empty true))] 
    [else (append (tnode-words (first lotnode) loc-so-far lst-so-far)
                  (lotnode-words (rest lotnode) loc-so-far lst-so-far false))]))


;;(list-words trie) consumes a trie and produces the list of words represented in the
;; consumed Trie (trie)
;;list-words: Trie -> (listof Str)
;;Examples:
(check-expect (list-words (make-trie empty)) empty)
(check-expect (list-words a-c-trie)
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))

(define (list-words trie)
  (lotnode-words (trie-children trie) empty empty false))

;;Tests:
(check-expect (list-words h-u-trie) (list "ha" "hat" "he" "hot" "use"))
(check-expect (list-words c-d-trie)
              (list "cat" "catch" "cater" "catnip" "cattle" "dig" "dog" "dogfish"
                    "donald" "donut" "doze"))


;;Part E


;;
(define (is-end-word? char lotnode loc)
  (cond
    [(empty? lotnode) false]
    [(empty? (rest loc)) true]
    [(tnode-ends-word? (first lotnode)) true]
    [else false]))

;;
(define (my-make-tnode lotnode loc)
  (cond
    [(empty? loc) empty]
    [(empty? (rest loc))
     (make-tnode (first loc) true empty)]
    [else (make-tnode (first loc) (is-end-word? (first loc) lotnode loc) (list (my-make-tnode lotnode (rest loc))))]))

;;
(define (insert-tnode tnode loc)
  (make-tnode (first loc)
              (or (empty? (rest loc))
                  (tnode-ends-word? tnode))
              (insert-lotnode (rest loc) (tnode-children tnode))))

;;
(define (insert-lotnode lotnode loc)
  (cond
    [(empty? loc) lotnode]
    [(empty? lotnode) (list (my-make-tnode lotnode loc))]
    [(char=? (first loc) (tnode-key (first lotnode)))
     (cons (insert-lotnode (first lotnode) (rest lotnode)))]
    [(char<? (first loc) (tnode-key (first lotnode)))
     (cons (my-make-tnode (rest lotnode) loc) lotnode)]
    [else (cons (first lotnode) (insert-lotnode (rest lotnode) loc))]))


;;(insert-word str trie) consumes a str and a trie and produces a new Trie from
;; inserting str into the consumed Trie (trie)
;;insert-word: Str Trie -> Trie
;;Examples:
(check-expect (list-words (insert-word "" blank-trie)) empty)
(check-expect (list-words (insert-word "" a-c-trie)) (list-words a-c-trie))
(check-expect (list-words (insert-word "hi" blank-trie)) (list "hi"))

(define (insert-word str trie)
  (make-trie (insert-lotnode (string->list str) (trie-children trie))))

;;Tests:
(check-expect (list-words (insert-word "att" a-c-trie))
              (list "at" "att" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))


;;Part F

;;(insert-some-words los trie) consumes a los and a trie and inserts the words in
;; los into the consumed Trie (trie) and produces a new Trie
;;insert-some-words: (listof Str) Trie -> Trie
;;Examples:
(check-expect (list-words (insert-some-words (list "hi" "hill") blank-trie))
              (list "hi" "hill"))
(check-expect (list-words (insert-some-words (list "") blank-trie)) empty)

(define (insert-some-words los trie)
  (cond
    [(empty? los) trie]
    [else (insert-some-words (rest los) (insert-word (first los) trie))]))

;;Tests:
;(check-expect (list-words (insert-some-words (list "ate" "att") a-c-trie))
;              (list "at" "ate" "att" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))


;;Part G

;;(list-completions str trie) consumes a str and a trie and produces a list of words
;; in the consumed Trie (trie) that have str as a prefix
;;list-completions: Str Trie -> (listof Str)
;;Examples:
(check-expect (list-completions "" a-c-trie) (list-words a-c-trie))
(check-expect (list-completions "ha" h-u-trie) (list "ha" "hat"))

(define (list-completions str trie)
  (local
    [;;(is-str-prefix? loc1 loc2) consumes a loc1 and loc2 and produces true if
     ;; loc2 represents a wordwhich is a prefix of the word represented by loc1 and
     ;; false otherwise
     ;;is-str-prefix?: (listof Char) (listof Char) -> Bool
     (define (is-str-prefix? loc1 loc2)
       (cond
         [(empty? loc1) true]
         [(empty? loc2) false]
         [(char=? (first loc1) (first loc2))
          (is-str-prefix? (rest loc1) (rest loc2))]
         [else false]))]
    (local
      [;;(prefix-list str los) consumes a str and a los and produces the list of words
       ;; in los that have str as its prefix
       ;;prefix-list: Str (listof Str) -> (listof Str)
       (define (prefix-list str los)
         (cond
           [(empty? los) empty]
           [(is-str-prefix? (string->list str) (string->list (first los)))
            (cons (first los) (prefix-list str (rest los)))]
           [else (prefix-list str (rest los))]))]
      (prefix-list str (list-words trie)))))

;;Tests:
(check-expect (list-completions "ate" a-c-trie) empty)
(check-expect (list-completions "cat" c-d-trie)
              (list "cat" "catch" "cater" "catnip" "cattle"))


