(display "Hello, World!")
(def greet (lambda (who)
             (cat "Hello, " who "!")))
(display (greet "Martiny"))
(display "a")(display "b")
; This is a comment for fun and profit!
(def addn (lambda (n)
            (lambda (x)
              (+ x n))))
(let ((add5 (addn 5)))
  (display (add5 10)))

(if (= 1 1)
    (display "True")
    (display "False"))

(let ((+ (lambda (a b)
           (+ a (- b 1)))))
  (display (+ 3 4)))

(def ^ (lambda (a b)
         (if (= b 1)
             a
             (* a (^ a (- b 1))))))
(display (^ 2 8))
;; (display (^ 2 65536)) ; bye-bye, stack

(def *^* (lambda (a b acc)
           (if (= b 1)
               (* a acc)
               (*^* a (- b 1) (* a acc)))))
(display (*^* 2 8 1))
;; (display (*^* 2 65536 1)) ; stack my ass

;; (defmacro unless (cond iffalse iftrue)
;;   (quasiquote if (unquote cond)
;;               (unquote iftrue)
;;               (unquote iffalse)))

;; (unless (= 1 2)
;;   (display "False")
;;   (display "True"))

(def debug (get Lua "debug"))
(display (print (call (get debug "getinfo") 1)))
