;evalue les expression contenu dans lexpr liste
;return  : expressions evalués 
(defun map-eval-li (lexpr env)
  (if (atom lexpr)
      ()
    (cons (eval-li (car lexpr) env) (map-eval-li (cdr lexpr) env)) 
    )
)

(defun eval-li (expr env)
  (case (car expr)

	(:const 
	 (cadr expr))
    
	(:var 
	 (aref env (cadr expr)))
    
	(:set-var 
	 (setf (aref env (cadr expr)) (eval-li (cddr expr) env)))  
    
	(:if 
	 (if (eval-li (second expr) env)
	     (eval-li (third expr) env)
	   (eval-li (fourth expr) env)))
	
	(:call
	 (apply (second expr) (map-eval-li (cddr expr) env))
	 )

	(:defun
	 (lisp2li (list 'set-defun (list 'quote (second expr)) 
			(list 'quote (cons (+ 1 (length (third expr)))
					   (if (cddddr expr)
					       (cons :progn (map-lisp2li (cdddr expr) (make-stat-env (third expr) 1)))
					     (lisp2li (cadddr expr) (make-stat-env (third expr) 1 ))))))
		  ()))
	
	)
  )



(defun get-defun-li(nom-fonction)
  (get nom-fonction ':defun))

(defun set-defun-li(nom-fonction expr)
  (setf (get nom-fonction nom-fonction ':defun) expr))