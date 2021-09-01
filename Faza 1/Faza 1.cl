(defun unesiN ()
	 (format t "Unesi n :~%")
		(setq n (read))
)

(defun unesiVrstu ()
	 (format t "Unesi vrstu :~%")
		(setq vrsta (read))
)

(defun unesiKolonu ()
	 (format t "Unesi kolonu :~%")
		(setq kolona (read))
)

(defun getEmptyFieldsNumber (n rowIndex)
    (abs(- (- n 1) rowIndex))
)

(defun rowInit (rowIndex columnIndex)
    (cond
        ((and (< rowIndex (1- n)) (< columnIndex 0)) '() )
        ((and (< columnIndex (getEmptyFieldsNumber n rowIndex)) (>= rowIndex (1- n) )) '())
        (t (cons (cons columnIndex '(-)) (rowInit rowIndex (1- columnIndex)) ))
    )
)

(defun matrixFactory (rowIndex numForChar)
    (cond
        ((> rowIndex (- (* 2 n) 1)) '())
        (t 
            (cons 
                (cons 
                    (code-char (+ numForChar 0))
                    (list (reverse (rowInit 0 (1- n) ) ) )
                ) 
                (matrixFactory (1+ rowIndex)(1+ numForChar))
            )
        )
    )
)

(defun kreirajMatricu (x y)
    (cond
        ((> x (- (* 2 n) 1)) '())
        (t 
            (cons
                 (cons  
                    (code-char (+ y 0))
                    (matrixFactory n 65)
                 )
                 (kreirajMatricu (1+ x)(1+ y))
            )
        )
    )
)

(defun vratiVrstu (row matrica)
      (if (= row 0) (car matrica) (vratiVrstu (1- row) (cdr matrica)))
)

(defun vratiVrednost (vrsta kolona)
      (if (= kolona 0) (car vrsta) (vratiVrednost (cdr vrsta) (1- kolona)))     
)

(defun vratiStubic (row column)
      (vratiVrednost (vratiVrstu row playground) (1+ column)) 
)

(defun izdvojiPoljaStubica (row column)
      (car (cdr (vratiStubic row column)))
)

(defun ispitaj (stub)
      (if (listp (car stub)) (ispitajDublje (car stub) stub) '())
)

(defun ispitajDublje (stubOpet stub)
      (if (equalp (cadr stubOpet) '-) t (ispitaj (cdr stub))) 
)

(defun vratiKonkretnuVrednost (arg stub)
      (if (= arg 0) 
         (car stub) 
         (vratiKonkretnuVrednost (1- arg) (cdr stub)))
)

(defun getValue (arg1 arg2 arg3) 
     (cadr (vratiKonkretnuVrednost arg3 (izdvojiPoljaStubica arg1 arg2)))
)

(defun drawGeneric6()
                (format t "~%0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z~%")
              )

(defun drawGeneric5()
                (format t "~%0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O~%")
              )

(defun drawGeneric4()
                (format t "~%0 1 2 3 4 5 6 7 8 9 A B C D E F~%")
              )

(defun potezX1 (row column lista simbol)
  (if (= row 0) (setq playground (cons (cons (car (car lista)) (potezX2 column (cdr (car lista)) simbol)) (cdr lista))) (setq playground (cons (car lista) (potezX1 (1- row) column (cdr lista) simbol))))
)  

(defun potezX2 (col vr simbol) 
  (if (= col 0) (cons (cons (car (car vr)) (potezX3 (car vr) simbol)) (cdr vr)) (cons (car vr) (potezX2 (1- col) (cdr vr) simbol)))
)

(defun potezX3 (stubic simbol)
  (potezX4 (list (car (cdr stubic))) 0 n (list (car (cdr stubic))) simbol)
)

(defun potezX4 (s nivo brojac prvobitniStubic simbol)
  (if (= brojac 0) prvobitniStubic (if (equalp (last (car (car s))) '(-)) (potezX5 (car s) nivo simbol) (potezX4 (list (cdr (car s))) (1+ nivo) (1- brojac) prvobitniStubic simbol)))
)

(defun potezX5 (zaZamenu lvl simbol)
     (case n
    (4
       (case lvl
            (0 
		(list '((0 X) (1 -) (2 -) (3 -)))	
 	    )
            (1 
		(list '((0 X) (1 X) (2 -) (3 -)))			
            )
            (2 
		(list '((0 X) (1 X) (2 X) (3 -)))				
	    )
            (3 
		(list '((0 X) (1 X) (2 X) (3 X)))				
	    )	   
      )
    )
    (6
       (case lvl
            (0 
		(list '((0 X) (1 -) (2 -) (3 -) (4 -) (5 -)))	
 	    )
            (1 
		(list '((0 X) (1 X) (2 -) (3 -) (4 -) (5 -)))			
            )
            (2 
		(list '((0 X) (1 X) (2 X) (3 -) (4 -) (5 -)))				
	    )
            (3 
		(list '((0 X) (1 X) (2 X) (3 X) (4 -) (5 -)))				
	    )	
            (4 
		(list '((0 X) (1 X) (2 X) (3 X) (4 X) (5 -)))				
	    )
            (5 
		(list '((0 X) (1 X) (2 X) (3 X) (4 X) (5 X)))				
	    )   
      )
    ))    
)

(defun drawElements4 (el1 el2 el3 el4)
 (format t "~a ~a ~a ~a " el1 el2 el3 el4) )

(defun drawElements6 (el1 el2 el3 el4 el5 el6)
 (format t "~a ~a ~a ~a ~a ~a " el1 el2 el3 el4 el5 el6) )

(defun stampaj()
	(case n
		(4 
			(drawGeneric4)
			(loop for i from 0 to (1- n) do
			(drawElements4 " " " " " " (getValue i 3 3)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 " " " " (getValue i 2 3) (getValue i 3 2)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 " " (getValue i 1 3) (getValue i 2 2) (getValue i 3 1)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 3) (getValue i 1 2) (getValue i 2 1) (getValue i 3 0)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 2) (getValue i 1 1) (getValue i 2 0) " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 1) (getValue i 1 0) " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 0) " " " " " "))
			(drawGeneric4)
 		 )
		 (6 
			(drawGeneric6)
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " " " " " " " (getValue i 5 5)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " " " " " (getValue i 4 5) (getValue i 5 4)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " " " (getValue i 3 5) (getValue i 4 4) (getValue i 5 3)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " (getValue i 2 5) (getValue i 3 4) (getValue i 4 3) (getValue i 5 2)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " (getValue i 1 5) (getValue i 2 4) (getValue i 3 3) (getValue i 4 2) (getValue i 5 1)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 5) (getValue i 1 4) (getValue i 2 3) (getValue i 3 2) (getValue i 4 1) (getValue i 5 0)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 4) (getValue i 1 3) (getValue i 2 2) (getValue i 3 1) (getValue i 4 0) " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 3) (getValue i 1 2) (getValue i 2 1) (getValue i 3 0) " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 2) (getValue i 1 1) (getValue i 2 0) " " " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 1) (getValue i 1 0) " " " " " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 0) " " " " " " " " " "))
			(drawGeneric6)
		   )
		   
	)
)

(defun chooseFirst ()
    (format t "Who is playing first? Enter 'C' for computer, or 'p' for person:~%")
		(setq entry (read))
        (if (not (or (equalp entry 'C) (equalp entry 'P) )) (progn (format t "You are invalid.~%") '())  
        ;; else
            (progn      
                (setq isPerson (cond
                                ((equalp entry 'C) '())
                                ((equalp entry 'P) t)
                            )
                )
                t
            )
        )
)

(defun start ()
  (defvar n)
  (defvar playground)
  (defvar vrsta)
  (defvar kolona)
  (unesiN)
  (setq playground (kreirajMatricu n 65))
  (loop for i from 0 to 5 do
        (unesiVrstu)
        (unesiKolonu)
        (potezX1 vrsta kolona playground 'X)
        (stampaj)
  )
)	    

(start)