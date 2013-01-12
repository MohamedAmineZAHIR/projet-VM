;evalue les expression contenu dans lexpr liste
;return  : expressions evalués 



(defun map-eval-li (lexpr env)
  ;(if (atom lexpr)
  
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
		  
	 (:mcall 
	  (let ((fun (get (second expr) :defun)))
                  (eval-li (cdr fun) (make-env-eval-li (car fun) (cdr expr) env))))
	  
	 (:unknown 
	  (eval-li (map-eval-li (caddr expr ) env)))
	
	)
  )
  
  
  ;(:mcall 
   ;(let (( fun (get-defun-li (second expr)))
    ;(nenv (make-array (+ 1 (cdr fun))))
    ;(fill-env nenv(make-eval-li (cddr expr) env)))
    ;(me

 
;Construit l'environnement de eval

 (defun make-env-eval-li (taille args oenv)
  (map-eval-li-array (cdr args) oenv 1 (make-array taille)))


;Donne nom defun a la fonction reconnu
(defun get-defun-li(nom-fonction)
  (get nom-fonction ':defun))

(defun set-defun-li(nom-fonction expr)
  (setf (get nom-fonction nom-fonction ':defun) expr))
