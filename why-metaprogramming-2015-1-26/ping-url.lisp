;;; if a url is given, then ping url twice, once on start, once on end;
;;; could potentially model a timing operation?

(ql:quickload :drakma)

(defun ping-url (url) (print (drakma:http-request url)))

(defmacro ping-before-after-when-url (url &rest body)
  (let ((url-evalled (gensym)))
    `(let ((,url-evalled ,url))
       (if ,url-evalled
           (progn
             (ping-url ,url-evalled)
             ,@body
             (ping-url ,url-evalled))
           (progn ,@body)))))

(defun print-stuff () (print "hey"))

;; (print (macroexpand-1 '(ping-before-after-when-url nil (print-stuff))))

;;; could use multiple if statements, but how easy is that to debug?
;;; ,@body of code is actually printed out twice in the output: code generation
