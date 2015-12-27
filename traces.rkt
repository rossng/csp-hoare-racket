#lang racket

;;; Communicating Sequential Processes - CAR Hoare

#| This file contains Racket implementations of the LISP examples given in the book
Processes are represented as functions, events represented by symbols. If a symbol
passed to a function is not a possible first event, the process will return 'bleep.
Otherwise, it will return the function representing the subsequent behaviour of the
process. |#

;;; p26 (1.7)

#| Define the catenation operator, which appends one trace to another |#
(define (⌒ t1 t2)
  (if
   (null? t1) t2
   (cons (car t1) (⌒ (cdr t1) t2))))

(define append ⌒)

#| Define the set membership operator, which recursively tests the membership of an event
an event set |#
(define (∈ event set)
  (cond
   [(null? set) #f]
   [(eq? event (car set)) #t]
   [else (∈ event (cdr set))]))

(define is-member ∈)

#| Define the restriction operator, which filters a trace to only contain the events specified
in a set |#
(define (↾ trace event-set)
  (cond
    [(null? trace) '()]
    [(∈ (car trace) event-set) (cons (car trace) (↾ (cdr trace) event-set))]
    [else (↾ (cdr trace) event-set)]))

(define restrict ↾)

#| Implement the partial ordering of traces. s <= t if s is a prefix of t. |#
(define (≤ t1 t2)
  (cond
    [(null? t1) #t]
    [(null? t2) #f]
    [else (and (eq? (car t1) (car t2)) (≤ (cdr t1) (cdr t2)))]))

(define is-prefix ≤)