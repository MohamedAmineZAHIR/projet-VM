(defun make-lst(nom-lst)
  (progn 
    (setf (get nom-lst ':corps) ())
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

