;;; lisp programs are just big lists, evaluated in a special way
;;; `defmacro' does this for us in a neat and "easy" way

;;; motivating example 1: implement `setf'
;;; `setf' is a way to generically set values to arbitrary "places"
;;; a "place" is some place that stores memory
;;; you can imagine it like overloading the c++ assignment operator (=)

(defun my-setf (place expr)
  "PLACE is a quoted expression (A LIST), and EXPR is the value we will set it
to."
  (cond
    ;; if it's a list expression
    ((listp place)
     (case (car place)
       (car (rplaca (second place) expr))
       (cdr (rplacd (second place) expr))
       (t (error "we don't understand this form!"))))
    ;; if it's a variable
    ((symbolp place)
     (set place expr))
    (t (error "we can't modify atoms!"))))

;;; let's turn this into a macro
(defmacro my-setf-macro (place expr)
  (cond ((symbolp place) `(set ',place ,expr))
        ((listp place)
         (case (first place)
           (car `(rplaca ,(second place) ,expr))
           (cdr `(rplacd ,(second place) ,expr))
           ;; compile-time error!
           (t (error "we don't understand this form!"))))
        ;; compile-time error!
        (t (error "we can't modify atoms!"))))

;;; why are compile-time errors important?

(defvar a nil)
;;; this compiles...and it really shouldn't
(defvar some-function
  (lambda ()
    (my-setf '(asdf a) 3)))
;;; this will FAIL to compile; and that's GREAT
(defvar some-function-with-macro
  (lambda ()
    (my-setf-macro '(asdf a) 3)))

(setf (cdr a) 3)                        ; also works lol

;;; c++ does generic assignment with special syntax, lisp doesn't need to
;;; because macros create their own syntax. this relates HEAVILY to the tech
;;; talk i did earlier today on compiling css expressions.

;;; motivating example 2: jit-compile a regular expression
;;; mention cl-ppcre
;;; accept either a function (do this at runtime), or a string (can evaluate at
;;; compile time)

;;; let's write this!

;;; next time: implementing a programming language in lisp!
