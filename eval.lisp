(defmacro cond args
  (letrec ((gencond (lambda ((tcond tbody) &rest tail)
                      (let ((tail (if (= (len tail) 0)
                                      (quote ())
                                      (apply gencond tail))))
                        (quasiquote if (unquote tcond)
                                    (unquote tbody)
                                    (unquote tail))))))
          (apply gencond args)))

(def eval (lambda (sexp ctx)
            (cond
              ((symbol? sexp)
               (get ctx (symbol->string sexp)))
              ((list? sexp)
               (destructuring-bind (fn &rest arguments) sexp
                 (let ((fv (eval fn ctx)))
                   (fv eval ctx (map (lambda (arg) (eval ctx arg)) arguments)))))
              ((= 1 1)
              sexp))))

(display (print (eval (quote +) ctx)))
(display (print (eval (quote + 1 1) (set (list) "+" (get ctx "+")))))
