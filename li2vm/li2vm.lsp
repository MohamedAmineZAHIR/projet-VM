(defun make-lst(nom-lst)
  (progn 
    (setf (get nom-lst ':corps) ())
    (setf (get nom-lst ':defun) ())
    )
  )

;affiche instruction vm
(defun affiche-instr(lst)
  (if (NULL (cdr lst))
      (print (car lst))
    (progn
      (print (car lst))
      (affiche-instr (cdr lst))
      )
    )
  )

;ajout instruction
(defun ajout-instr(instr lst)
  (setf (get lst ':corps) (cons  instr (get lst ':corps)))
  )

(defun map-li2vm (lexpr env lst)
  ;(format t "$>echo : ~s ~%" lexpr)
  (if (NULL (cdr lexpr))
	(li2vm (car lexpr) env lst)
    (progn
      (li2vm (car lexpr) env lst) 
      (map-li2vm (cdr lexpr) env lst)
      ) 
    )
)

(defun li2vm (expr env lst)
  (cond
   ((equal (car expr) :const) (li2vm-const (cdr expr) lst))
   ((equal (car expr) :var) (li2vm-var (cdr expr) env lst))
   ((equal (car expr) :call) (li2vm-call expr env lst))
   ((equal (car expr) :if) (li2vm-if expr env lst))
   ((equal (car expr) :defun (li2vm-defun expr env lst)))
   )
  )

(defun li2vm-const (expr lst)
  (ajout-instr (cons ':CONST expr) lst)
  )

(defun li2vm-var (expr env lst)
  (ajout-instr (cons ':VAR (aref env (car expr))) lst)
  )
	    

(defun li2vm-call (expr env lst)
  (map-li2vm (cddr expr) env lst)
   (ajout-instr (cons (first expr) (second expr)) lst)
)

(defun li2vm-if (expr env lst)
  (li2vm (second expr) env lst)
  (ajout-instr (cons ':SKIPNIL 2) lst)
  (li2vm (third expr) env lst)
  (li2vm (fourth expr) env lst)
  )

(defun li2vm-defun (expr env lst)
 ; (format t "$> ~s ~%" expr)
  ;(format t "$> ~s ~%" (first expr))
  ;(format t "$> ~s ~%" (second expr))
  ;(format t "$> ~s ~%" (third expr))
  ;(format t "$> ~s ~%" (fourth expr))
  (set-defun 
   (list (second expr)) 
   (cons 
    (+ 1 (length (third expr)));taille env
    (list (fourth expr));corps
    )
   lst
   )
  )

(defun get-defun(nom-fonction)
  (get nom-fonction ':defun))

(defun set-defun(nom-fonction expr lst)
  (setf (get lst ':defun) (cons (list nom-fonction expr) (get lst ':defun)))
  )

(defun make-stat-env (nenv x)
  )