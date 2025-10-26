#lang racket

(require "chez-init.rkt")

(provide scheme-value? apply-cont halt-cont all-positive?-cps my-even?-cps even-filter-cps valid-even-list?-cps make-cps and-eval grade-tests eval-one-exp)
;-----------------------------------------------
; Macros
;-----------------------------------------------

; Problem 1: and-eval

; Define a new syntax and-eval. and-eval takes
; a list of expressions and evaluates them one
; after another until one expression evaluates
; to false. In that case, it returns the list
; of true values so far, and does not evaluate
; anything else in the list. If it never encounters
; a false value, it returns the whole list.
; For example:
; (and-eval #t 'a 4 #f 19 2) ; returns '(#t a 4)
; (and-eval (display "this displays") 'a ((lambda (x) (* x x)) 3) (even? 19) (display "this doesn't") 15) ; returns '(#<void> a 9)
;
; A few requirements:
; A:
; and-eval must use short circuiting. That is, if there is a false value,
; it doesn't evaluate anything beyond it. For example:
; (and-eval #f (display "hello")) ; does not display hello, returns '()
; (and-eval (display "hello") #f) ; displays hello, returns '(#<void>)

; B:
; Any of the expressions in and-eval can be complex and mutate each other
; (let
;   ([x (make-vector 1)])
;     (vector-set! x 0 3)
;     (and-eval
;       (vector-ref x 0)
;       (begin (vector-set! x 0 14) 'set)
;       (vector-ref x 0)
;       (< (vector-ref x 0) 7)
;       27)) ; returns '(3 set 14)

(define-syntax and-eval
  (syntax-rules ()
    [(_) 'nyi]))

; Problem 2: grade-tests
; You will write a portion of the grading code that has been used in this
; class to grade the assignments and exams. The syntax for this problem is
;
; (grade-tests testcase+)
; where
; testcase :: (e1 e2 pts) | (e1 e2 eq-op? pts).
;
; A testcase (e1 e2 pts) passes when e1 is equal? to e2.
; A testcase (e1 e2 eq-op? pts) when e1 is eq-op? to e2.
;
; You should sum up all the points for the testcases that
; passed and return the score of the assignment between
; 0 and 1; divide the earned points by the total points.
; For example:
;
; (grade-tests
;    ('foo 'foo 3) ; passes
;    ('bar 'baz 2) ; fails
;    (4 6 (lambda (x y) (eq? (modulo x 2) (modulo y 2))) 4)) ; passes
;
; returns 7/9. Note that 7/9 is a number and not a string, it's just
; Racket's fun way of formatting rationals.

(define-syntax grade-tests
  (syntax-rules ()
    [(_) 'nyi]))


;-----------------------------------------------
; Continuation Passing Style
;-----------------------------------------------

; Here is a list of procedures covert them all to cps

(define scheme-value?
  (lambda (val)
    #t))

(define-datatype continuation continuation?
  [halt-cont]
 )

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
      )))

(define all-positive?
  (lambda (lst)
  (cond
    [(null? lst) #t] 
    [(<= (car lst) 0) #f] 
    [else (all-positive? (cdr lst))])))

(define all-positive?-cps
  (lambda (lst cont)
    'nyi))

(define make-cps
  (lambda (proc)
    (lambda (val cont)
      (apply-cont cont (proc val)))))

(define my-even?
  (lambda (n)
  (cond [(= n 0) 0]
        [(> n 0) (not (my-even? (- n 1)))]
        [else (not (my-even? (+ n 1)))]
        )))

(define my-even?-cps
  (lambda (n cont)
    'nyi))

(define even-filter
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(my-even? (car lst))
       (cons (car lst) (even-filter (cdr lst)))]
      [else (even-filter (cdr lst))])))

(define even-filter-cps
  (lambda (lst cont)
  'nyi))

(define valid-even-list?
  (lambda (lst)
  (and (list? lst)
       (all-positive? lst)
       (> (length (even-filter lst)) 5))))

(define valid-even-list?-cps
  (lambda (lst cont)
  'nyi))


;-----------------------------------------------
; Syntax Expand
;-----------------------------------------------

; Problem 1: mapcond

; This is a problem that requires you to modify your interpreter.
;
; cut and paste your interpreter code at the bottom of this file
; Use syntax-expand to implement a command called mapcond that acts
; a little like cond, but a bit more convenient to use
;
; (mapcond negative? (1 'pos) (3 'alsopos) (-2 'neg) (-4 'toolate)) ; returns 'neg
;
; The first parameter of mapcond is a predicate "pred?" that takes one variable
; The remaining parameters are cases. Each case has two elements: a condition "con"
; and a body "bod". mapcond should find the first case where (pred? con) is true,
; and then return the result of executing that case's body. If no (pred? con) is
; true, then mapcond should return #void (like how cond does when none are true)
;
; mapcond should not evaluate (pred? con) more than once for each case, it should
; only evaluate one body at most, and it should short-circuit after finding
; a (pred? con) that is true.
; For example:
;
; (mapcond (lambda (x) (display "pred evaled") (negative? x)) (1 'pos) (3 'alsopos) (-2 'neg) (-4 'toolate)) ; returns 'neg, prints "pred evaled" 3 times
;
; (mapcond negative? (2 (display "not neg!")) (4 (display "also not neg!")) (-1 'neg) (-3 (display "too late!"))) ; returns 'neg, doesn't print anything
;
; You are required to implement this problem using syntax-expand (and parse-exp).
; You should not need to make any changes to eval-exp to get this feature to work.

; I've included this little function to make the test cases run initially.
; delete it once your paste in your interpreter.
(define eval-one-exp
  (lambda (x) 'nyi))



;-----------------------------------------------
; General Interpreter
;-----------------------------------------------

; Problem 1: multivar
;;
;; Usually when we have a variable, we look for its binding in the environment.
;; But what if I wanted to look for multiple variables, and return the first one I found?
;; For example:
;; (let ([x 'coolval]) (let ([y 'coolerval]) (multivar x y))) ; returns 'coolerval
;;
;; I want you to write a new expression type multivar that takes a list of variables.
;; When you evaluate multivar, find the most recent binding of ANY variable in that
;; list and return it. You don't have to worry about a "tie" in binding depth such
;; as (let ([x 'coolval] [y 'coolerval']) (multivar x y)); this can return anything and won't be tested.
;;
;; multivar should work even if some variables aren't defined yet:
;; (let ([y 'coolval]) (multivar x y)) ; returns 'coolval without complaint
;;
;; multivar should work with global bindings and can be used in complex expressions:
;; ((multivar y banana futon +) 2 3) ; returns 5
;;
;; Note that multivar does not quote the variables inside of it -- they are directly in there, not symbols