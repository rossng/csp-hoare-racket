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
(define (STOP event) 'bleep)

#| Here, we implement a simple process (coin -> STOP).
This process engages in the event coin, then behaves like process stop. |#
(define (coin-to-stop event)
  (case event
    ['coin STOP]
    [else 'bleep]))

#| Here, we define a general function for prefixing (i.e. guarding) processes
prefix takes an event c and a process P and returns a function acting as (c -> P) |#
(define (prefix c P)
  (lambda (event)
    (cond
      [(eq? event c) P]
      [else 'bleep])))

#| We can define the (coin -> STOP) process again using our new prefix function. |#
(define coin-to-stop2 (prefix 'coin STOP))

#| We can also create a process where there is a choice of first event.
choice2 takes events c, d and processes P, Q, returning a process that acts
like (c -> P | d -> Q) |#
(define (choice2 c P d Q)
  (lambda (event)
    (cond
      [(eq? event c) P]
      [(eq? event d) Q]
      [else 'bleep])))

#| We can use choice2 to define a vending machine that either takes 1p and dispenses
a biscuit, or takes 2p and dispenses a chocolate, then stops.
We can represent this as (in1p -> biscuit -> STOP | in2p -> choc -> STOP) |#
(define choice-vending-machine
  (choice2 'in1p (prefix 'biscuit STOP) 'in2p (prefix 'choc STOP)))

#| We can also recursively define a vending machine that acts as (μX . coin -> choc -> X)
i.e. it takes a coin and dispenses a chocolate, in a loop, forever. |#
(define infinite-vending-machine
  (prefix 'coin (prefix 'choc (lambda (event) (infinite-vending-machine event)))))