;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C R E A T I O N ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vm (nom-vm taille1 taille2)
	(let  	(
			(result1
				(progn
					(setf (get nom-vm ':size) taille1)
					(make-array (list taille1) )
				)
			)
			(result2
				(progn
					(setf (get nom-vm ':size) taille2)
					(make-array (list taille2) )
				)
			)
		)
		(setf (get nom-vm ':dataStack) result1)
		(setf (get nom-vm ':controlStack) result2)

		;La taille de la pile Data
		(write-tableau-ctl nom-vm 0 taille1)
	  
		;La taille de la pile Control	  
		(write-tableau-ctl nom-vm 1 taille2)

		;Sommet de la pile SP
		(write-tableau-ctl nom-vm 2 taille1)

		;Pointer Counter PC
		(write-tableau-ctl nom-vm 3 -1)
	  
		;Frame Pointer FP
		(write-tableau-ctl nom-vm 4 0)
	)
)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C H A R G E M E N T ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;la fct "chargement": 
;écrit les valeurs des registres du microprocesseur dans la mémoire
;charge de la mémoire


(defun load-vm (nom-vm code)
	(loop for instruction in code
		do (write-tableau-dt nom-vm (+ (get-tableau-ctl nom-vm 3) 1) instruction)
	           (write-tableau-ctl nom-vm 3 (+ (get-tableau-ctl nom-vm 3) 1))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; I N T E R P R E T A T I O N  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun interpreter-vm (nom-vm expr)
  (cond ((consp expr) 
	    (if (equal (car expr) ':CONST)
	        (const nom-vm expr))
	  
	    (if (equal (car expr) ':VAR)
	      (var nom-vm expr))
	    
	    (if (equal (car expr) ':SET-VAR)
	      (set-var nom-vm expr))
	  
	    (if (equal (car expr) ':STACK)
	      (stack nom-vm expr))
	  
            (if (equal (car expr) ':CALL)
	      (call nom-vm expr))
	  
            (if (equal (car expr) ':RTN)
	      (rtn nom-vm))
	  
            (if (equal (car expr) ':SKIPNIL)
	      (skip nom-vm expr "n"))
	  
	    (if (equal (car expr) ':SKIPTRUE)
	      (skip nom-vm expr "t"))
	  
	    (if (equal (car expr) ':SKIP)
	      (skip nom-vm expr "hola"))	    
	  
	    (if (equal (car expr) ':STORE)
	      (storing nom-vm expr))
	  
	    (if (equal (car expr) ':LOAD)
	      (loading nom-vm expr)))
    
        (t (print "Type inconnu...!"))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;E X E C U T I O N ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-vm (nom-vm)
  (loop for i from 0
      while(<= i (get-tableau-ctl nom-vm 3))
      do (show-total-vm nom-vm) 
	 (interpreter-vm nom-vm (get-tableau-dt nom-vm i))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; A S S E M B L E U R ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun const (nom-vm expr)
  (write-tableau-dt nom-vm (- (get-tableau-ctl nom-vm 2) 1) (cadr expr))
  (write-tableau-ctl nom-vm 2 (- (get-tableau-ctl nom-vm 2) 1)))

(defun skip (nom-vm expr b)
  (cond ((equal b "n") 
	      (if(equal (summit nom-vm) nil)
		 (incr nom-vm 'pc (cadr expr))))
    
	((equal b "t")
	      (if(not (equal (summit nom-vm) nil))
		 (incr nom-vm 'pc (cadr expr)))) 
	(t (incr nom-vm 'pc (cadr expr)))))
  


(defun rtn (nom-vm)
  (print (get-tableau-dt nom-vm (get-tableau-ctl nom-vm 2))))


(defun var (nom-vm expr)
  (write-tableau-dt nom-vm (- (get-tableau-ctl nom-vm 2) 1) (get-tableau-dt nom-vm (- (get-tableau-ctl nom-vm 0) (cadr expr))))
  (write-tableau-ctl nom-vm 2 (- (get-tableau-ctl nom-vm 2) 1)))


(defun set-var (nom-vm expr)
  (write-tableau-dt nom-vm (- (get-tableau-ctl nom-vm 1) (cadr expr)) (summit nom-vm)))


(defun stack (nom-vm expr)
  (write-tableau-ctl nom-vm 4 (+ (get-tableau-ctl nom-vm 4) (cadr expr))))


(defun storing (nom-vm expr)
    (print ""))


(defun loading (nom-vm expr)
    (print ""))

;(get-tableau-ctl nom-vm 4) ----> 2
(defun call (nom-vm expr)
  (let 
    ((result (apply (cadr expr) (construire nom-vm () 2 0))))
    
    (loop for i from 0
      while(< i 2)
      do (write-tableau-dt nom-vm (+ (get-tableau-ctl nom-vm 2) i) nil))
    
    ;incrementer le SP
    (write-tableau-ctl nom-vm 2 (+ (get-tableau-ctl nom-vm 2) 1))
    ;empiler le resultat de la fonction
    (write-tableau-dt nom-vm (get-tableau-ctl nom-vm 2) result))
    ;initialiser le FP à 0
  ;(write-tableau-ctl nom-vm 4 0))
)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; M A N I P U L A T I O N ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Getters/Setters
(defun write-tableau-dt (nom-vm indice valeur)
	(setf (aref (get nom-vm ':dataStack) indice) valeur))

(defun get-tableau-dt (nom-vm indice)
	(aref (get nom-vm ':dataStack) indice))


(defun write-tableau-ctl (nom-vm indice valeur)
	(setf (aref (get nom-vm ':controlStack) indice) valeur))

(defun get-tableau-ctl (nom-vm indice)
	(aref (get nom-vm ':controlStack) indice))


;L'elt qui se trouve au sommet de la pile 
;mais le cas de si la pile est vide n'est pas distingué
(defun summit (nom-vm)
  (get-tableau-dt nom-vm (get-tableau-ctl nom-vm 2)))


;modifier la valeur du tableau en la incrementant
(defun incr (nom-vm expr valeur)
  (cond ((equal expr 'sp) (write-tableau-ctl nom-vm 2 (+ (get-tableau-ctl nom-vm 2) valeur)))
        ((equal expr 'pc) (write-tableau-ctl nom-vm 3 (+ (get-tableau-ctl nom-vm 3) valeur)))
	((equal expr 'fp) (write-tableau-ctl nom-vm 4 (+ (get-tableau-ctl nom-vm 4) valeur)))))

;modifier la valeur du tableau en la décrementant
(defun decr (nom-vm expr valeur)
  (cond ((equal expr 'sp) (write-tableau-ctl nom-vm 2 (- (get-tableau-ctl nom-vm 2) valeur)))
        ((equal expr 'pc) (write-tableau-ctl nom-vm 3 (- (get-tableau-ctl nom-vm 3) valeur)))
	((equal expr 'fp) (write-tableau-ctl nom-vm 4 (- (get-tableau-ctl nom-vm 4) valeur)))))


;Construire une cellule à partir des elements du tableau dataStack
(defun construire (nom-vm expr nombre cpt)
	 (if (= nombre 0)
	   expr
	   (construire nom-vm (cons (get-tableau-dt nom-vm (+ (get-tableau-ctl nom-vm 2) cpt)) expr) (decf nombre) (incf cpt))))





;Affichage total
(defun show-total-vm (nom-vm)
	(format t "****************************************** ~%" )
	(format t "******V I R T U A L    M A C H I N E****** ~%" )
	(format t "****************************************** ~%" )
	(format t "*********************************by zma*** ~%" )
	
	(format t "--> Mémoire DATA: ~%")
	(loop  	for i from 0    	; cpt qui s'incrémente...
		for indice = i
		while(< indice (get-tableau-ctl nom-vm 0))
		do  (format t "Cellule[ ] : ~s ~%" (get-tableau-dt nom-vm indice)) 
		  
	)
	(format t "~%~%" )
	(format t "--> Mémoire CONTROL: ~%")
	(loop  	for i from 0    	; cpt qui s'incrémente...
		for indice = i
		while(< indice (get-tableau-ctl nom-vm 1))
		do  (format t "Cellule[ ] : ~s ~%" (get-tableau-ctl nom-vm indice)) 
		  
	)
)


;affichage des cpsts non nul de la machine virtuelle
(defun show-vm (nom-vm)
	(format t "****************************************** ~%" )
	(format t "******V I R T U A L    M A C H I N E****** ~%" )
	(format t "****************************************** ~%" )
	
	(format t "--> Mémoire DATA: ~%")
	(loop  	for i from 0    	
		for indice = i
		while (< indice (get-tableau-ctl nom-vm 0))
	        ;afficher que les case non null de la mémoire
		do (if (not (equal nil (get-tableau-dt nom-vm indice))) (format t "Cellule[ ] : ~s ~%" (get-tableau-dt nom-vm indice)) )
		  
	)
	(format t "~%~%" )
	(format t "--> Mémoire CONTROL: ~%")
	(loop  	for i from 0    	
		for indice = i
		while (< indice (get-tableau-ctl nom-vm 1))
	        ;afficher que les case non null de la mémoire
		do (if (not (equal nil (get-tableau-ctl nom-vm indice))) (format t "Cellule[ ] : ~s ~%" (get-tableau-ctl nom-vm indice)))))


;initialisation de la mémoire
(defun reset-vm (nom-vm)
  (loop  for i from 0    	
	 for indice = i
	 while (< indice (get-tableau-ctl nom-vm 0))
	   do (write-tableau-dt nom-vm indice nil))
  
  (write-tableau-ctl nom-vm 2 (get-tableau-ctl nom-vm 0))
  (write-tableau-ctl nom-vm 3 -1))

