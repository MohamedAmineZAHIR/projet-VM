(load "li2vm.lsp")

(make-lst 'toto)
;(li2vm '(:const 5) '() 'toto)
;(li2vm '(:var 2) #(() 3 4) 'toto)
;(li2vm '(:if (:const A) (:const B) (:const C)) #(() 2) 'toto)
;(li2vm '(:call <= (:const A) (:const B))  #(() 2) 'toto)
(li2vm '(:if (:call <= (:const A) (:const B))(:const C)(:const D)) #(() 2) 'toto)
;(li2vm-defun '(:defun fact ((:var 1)) (:if (:call <= (:const A) (:const B))(:const C)(:const D))) #(() 2) 'toto)
;(li2vm-defun '(:defun fibo ((:var 1)) (:if (:call <= (:const A) (:const B))(:const C)(:const D))) #(() 2) 'toto)



(affiche-instr (reverse (get 'toto ':corps)))     