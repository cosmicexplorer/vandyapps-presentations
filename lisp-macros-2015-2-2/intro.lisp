;;; comments look like this in lisp
;; or like this
; or like this

;;; let's evaluate a sexp (C-c C-j to eval in slime)
(+ 2 3)

;;; lisp uses bignum arithmetic by default
;;; this can be kinda slow sometimes
(+ (/ 5 9) (/ 2 3))

(defun x-++ (x) (+ x 1))

;;; this is a list
(list 1 2 3)

;;; this is also a list
(list + 1 2 3)                          ; this is WEIRD!!!!
;;; this is better
(list '+ 1 2 3)
;;; common lisp is a lisp-2, meaning symbols can have both function and variable
;;; values
(+ 1 2 3)
(setq + 6000000)
;;; error!!!! not defined
;;; the plus didn't have this error because it was already defined as a function
;;; isn't this fun??!?!?!?!
(set 'adkfjadk 34)
(list adkfjadk 1 2 3)

;;; talk about eval and list, and read, and how lisp parses things
(eval (list + 1 2 3))
(defvar l)
(setq l (read-from-string "(+ 1 2 3)"))
(eval l)
;;; show invalid string

;;; let's make some metaprograms!!!!!!!

((lambda (x) (+ x 1)) 3)

(alksjdhflashdkjf dasdfasdf)

(setf (symbol-function '+) (lambda (a b) (- a b)))
