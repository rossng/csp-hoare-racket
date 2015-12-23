#lang racket

;;; Communicating Sequential Processes - CAR Hoare

#| This file contains Racket implementations of the LISP examples given in the book
Processes are represented as functions, events represented by symbols. If a symbol
passed to a function is not a possible first event, the process will return 'bleep.
Otherwise, it will return the function representing the subsequent behaviour of the
process. |#

;;; p17 (1.4)

#| Here, we define the STOP process, which never engages in any event.
Any event passed to STOP will result in 'bleep. |#
(define (stop event) 'bleep)

#| Here, we implement a simple process (coin -> STOP).
This process engages in the event coin, then behaves like process stop. |#
(define (coin-to-stop event)
    (case event
      ['coin stop]
      [else 'bleep]))