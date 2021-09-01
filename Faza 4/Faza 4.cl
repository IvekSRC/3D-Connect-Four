(defun unesiN ()
   (format t "~%Unesi n : ") (setq n (read))
)

(defun odigrajPotez ()
   (unesiVrstu) (unesiKolonu)
)

(defun unesiVrstu ()
   (format t "Unesi vrstu  (Opseg 0 ... n-1) : ") (setq vrsta (read))
   (if (>= vrsta n) (unesiVrstu))
   (if (< vrsta 0) (unesiVrstu))
)

(defun unesiKolonu ()
   (format t "Unesi kolonu  (Opseg 0 ... n-1) : ") (setq kolona (read))
   (if (>= kolona n) (unesiKolonu))
   (if (< kolona 0) (unesiKolonu))
)

(defun koIgraPrvi ()
   (format t "~%Ko igra prvi (X / O) : ") (setq igraPrviIgrac (read))
   (if (equalp igraPrviIgrac 'O) (setq naPotezu (1+ naPotezu)) (if (not (equalp igraPrviIgrac 'X)) (koIgraPrvi)))
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

(defun vratiStubic (row column lista)
      (vratiVrednost (vratiVrstu row lista) (1+ column)) 
)

(defun izdvojiPoljaStubica (row column lista)
      (car (cdr (vratiStubic row column lista)))
)

(defun ispitaj (stub limit)
  (if (= limit 0)
       '()
       (if (equalp (last (car stub)) '(-))
          t
          (ispitaj (cdr stub) (1- limit))
       )
  )
)

(defun vratiKonkretnuVrednost (arg stub)
      (if (= arg 0) 
         (car stub) 
         (vratiKonkretnuVrednost (1- arg) (cdr stub))
	  )
)

(defun getValue (arg1 arg2 arg3 lista) 
     (cadr (vratiKonkretnuVrednost arg3 (izdvojiPoljaStubica arg1 arg2 lista)))
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

(defun potez (row column lista simbol)
  (if (= row 0) (setq lista (cons (cons (car (car lista)) (potez2 column (cdr (car lista)) simbol)) (cdr lista))) (setq lista (cons (car lista) (potez (1- row) column (cdr lista) simbol))))
) 

(defun potez2 (col vr simbol) 
  (if (= col 0) (cons (cons (car (car vr)) (potez3 (car vr) simbol)) (cdr vr)) (cons (car vr) (potez2 (1- col) (cdr vr) simbol)))
)

(defun potez3 (stubic simbol)
  (potez4 (list (car (cdr stubic))) 0 n (list (car (cdr stubic))) simbol)
)

(defun potez4 (s nivo brojac prvobitniStubic simbol)
  (if (= brojac 0) prvobitniStubic (if (equalp (last (car (car s))) '(-)) (potez5 (car prvobitniStubic) nivo simbol) (potez4 (list (cdr (car s))) (1+ nivo) (1- brojac) prvobitniStubic simbol)))
)

(defun potez5 (zaZamenu lvl simbol)
     (case simbol 
           ('X
				(case n
     (4
       (case lvl
            (0 
		(list (cons '(0 X) (cdr zaZamenu)))
 	    )
            (1 
		(list (cons (car zaZamenu) (cons '(1 X) (cdr (cdr zaZamenu)))))			
            )
            (2 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons '(2 X) (cdr (cdr (cdr zaZamenu)))))))				
	    )
            (3 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons '(3 X) (cdr (cdr (cdr (cdr zaZamenu)))))))))				
	    )	   
      )
     )
     (6
       (case lvl
            (0 
		(list (cons '(0 X) (cdr zaZamenu)))	
 	    )
            (1 
		(list (cons (car zaZamenu) (cons '(1 X) (cdr (cdr zaZamenu)))))			
            )
            (2 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons '(2 X) (cdr (cdr (cdr zaZamenu)))))))				
	    )
            (3 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons '(3 X) (cdr (cdr (cdr (cdr zaZamenu)))))))))				
	    )	
            (4 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons (cadddr zaZamenu ) (cons '(4 X) (cdr (cdr (cdr (cdr (cdr zaZamenu)))))))))))				
	    )
            (5 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons (cadddr zaZamenu ) (cons (nth 4 zaZamenu) (list '(5 X))))))))				
	    )   
      )
      ))
            )
           ('O
				(case n
     (4
       (case lvl
            (0 
		(list (cons '(0 O) (cdr zaZamenu)))
 	    )
            (1 
		(list (cons (car zaZamenu) (cons '(1 O) (cdr (cdr zaZamenu)))))			
            )
            (2 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons '(2 O) (cdr (cdr (cdr zaZamenu)))))))				
	    )
            (3 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons '(3 O) (cdr (cdr (cdr (cdr zaZamenu)))))))))				
	    )	   
      )
     )
     (6
       (case lvl
            (0 
		(list (cons '(0 O) (cdr zaZamenu)))	
 	    )
            (1 
		(list (cons (car zaZamenu) (cons '(1 O) (cdr (cdr zaZamenu)))))			
            )
            (2 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons '(2 O) (cdr (cdr (cdr zaZamenu)))))))				
	    )
            (3 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons '(3 O) (cdr (cdr (cdr (cdr zaZamenu)))))))))				
	    )	
            (4 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons (cadddr zaZamenu ) (cons '(4 O) (cdr (cdr (cdr (cdr (cdr zaZamenu)))))))))))				
	    )
            (5 
		(list (cons (car zaZamenu) (cons (cadr zaZamenu) (cons (caddr zaZamenu) (cons (cadddr zaZamenu ) (cons (nth 4 zaZamenu) (list '(5 O))))))))			
	    )   
      )
      ))
           )
     )		   
)

(defun drawElements4 (el1 el2 el3 el4)
   (format t "~a ~a ~a ~a " el1 el2 el3 el4) 
)

(defun drawElements6 (el1 el2 el3 el4 el5 el6)
   (format t "~a ~a ~a ~a ~a ~a " el1 el2 el3 el4 el5 el6) 
)

(defun stampaj(lista)
	(case n
		(4 
			(drawGeneric4)
			(loop for i from 0 to (1- n) do
			(drawElements4 " " " " " " (getValue i 3 3 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 " " " " (getValue i 2 3 lista) (getValue i 3 2 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 " " (getValue i 1 3 lista) (getValue i 2 2 lista) (getValue i 3 1 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 3 lista) (getValue i 1 2 lista) (getValue i 2 1 lista) (getValue i 3 0 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 2 lista) (getValue i 1 1 lista) (getValue i 2 0 lista) " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 1 lista) (getValue i 1 0 lista) " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements4 (getValue i 0 0 lista) " " " " " "))
			(drawGeneric4)
 		 )
		 (6 
			(drawGeneric6)
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " " " " " " " (getValue i 5 5 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " " " " " (getValue i 4 5 lista) (getValue i 5 4 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " " " (getValue i 3 5 lista) (getValue i 4 4 lista) (getValue i 5 3 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " " " (getValue i 2 5 lista) (getValue i 3 4 lista) (getValue i 4 3 lista) (getValue i 5 2 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 " " (getValue i 1 5 lista) (getValue i 2 4 lista) (getValue i 3 3 lista) (getValue i 4 2 lista) (getValue i 5 1 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 5 lista) (getValue i 1 4 lista) (getValue i 2 3 lista) (getValue i 3 2 lista) (getValue i 4 1 lista) (getValue i 5 0 lista)))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 4 lista) (getValue i 1 3 lista) (getValue i 2 2 lista) (getValue i 3 1 lista) (getValue i 4 0 lista) " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 3 lista) (getValue i 1 2 lista) (getValue i 2 1 lista) (getValue i 3 0 lista) " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 2 lista) (getValue i 1 1 lista) (getValue i 2 0 lista) " " " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 1 lista) (getValue i 1 0 lista) " " " " " " " "))
			(format t "~%")
			(loop for i from 0 to (1- n) do
			(drawElements6 (getValue i 0 0 lista) " " " " " " " " " "))
			(drawGeneric6)
		   )		   
	)
)

(defun odrediPobednika (lista)
   (setq skorX 0)
   (setq skorO 0) 
   (proveriVisinu n lista)
   (proveraPoNivoima) 
   (proveraPoNivoimaSuprotanSmer1)
   (proveriGlavnuDijagonalu11)
   (proveriGlavnuDijagonalu21)
   (proveriDijagonaluUPenjanju11)
   (proveriDijagonaluUPenjanju21)
   (proveriGlavnuDijagonaluUPenjanju1)
   (proveriGlavnuDijagonaluUPadanju1)
   (proveriGlavnuDijagonaluUPadanju21)
   (proveriGlavnuDijagonaluUPenjanju21)
   (proveriDijagonaluUPenjanju31)
   (proveriDijagonaluUPenjanju41)
   (if (> skorX skorO)
       (format t "Igra je zavrsena, pobednik je igrac X.")
   )
   (if (> skorO skorX)
       (format t "Igra je zavrsena, pobednik je igrac O.")
   )
   (if (= skorO skorX)
       (format t "Igra je zavrsena, rezultat je neresen.")
   )
)

(defun proveriVisinu (dimenzijaIgre lista)
   (loop for i from 0 to (1- dimenzijaIgre) do
      (loop for j from 0 to (1- dimenzijaIgre) do
         (proveriVisinuStubica (izdvojiPoljaStubica i j lista) n)
      )
   )                 
)

(defun proveriVisinuStubica (stubicZaProveru brojac)
  (if (= brojac 1)
      (proveraPoslednjaDvaPolja stubicZaProveru)
      (proveraSusednihPolja stubicZaProveru brojac)
  )
)

(defun proveraPoslednjaDvaPolja (stubicZaProveru)
  (if (equalp (last (car stubicZaProveru)) '(X))
      (setq skorX (1+ skorX))
  )
  (if (equalp (last (car stubicZaProveru)) '(O))
      (setq skorO (1+ skorO))
  )
)

(defun proveraSusednihPolja (stubicZaProveru brojac)
  (if (= brojac n)
      (proveraPrvaDvaSusednaPolja stubicZaProveru brojac)
      (proveraOstalihSusednihPolja stubicZaProveru brojac)
  )                       
)

(defun proveraPrvaDvaSusednaPolja (stubicZaProveru brojac)
   (if (not (equalp (last (car stubicZaProveru)) '(-)))
      (proveraOstalihSusednihPolja stubicZaProveru brojac)
   )                              
)

(defun proveraOstalihSusednihPolja (stubicZaProveru brojac)
   (if (equalp (last (car stubicZaProveru)) (last (cadr stubicZaProveru)))
       (proveriVisinuStubica (cdr stubicZaProveru) (1- brojac))
   )                         
)

(defun proveraPoNivoima ()
   (loop for i from 0 to (1- n) do
      (proveraPoVrstiK (cdr (vratiVrstu i playground)))
   )  
)

(defun proveraPoVrstiK (vrsta)
   (loop for i from 0 to (1- n) do
      (proveraPoVrstiKNivouK vrsta n i)
   )  
)

(defun proveraPoVrstiKNivouK (vrsta dimenzijaIgre nivo)
   (if (not (equalp (last (nth nivo (car (cdr (car vrsta))))) '(-)))
      (proveraPoVrstiKNivouKDublje vrsta dimenzijaIgre nivo n)
   )         
)

(defun proveraPoVrstiKNivouKDublje (vrsta dimenzijaIgre nivo brojac)
   (if (= brojac 1)
      (proveraPoslednjaDvaPoljaPoNivou vrsta nivo)
      (proveraSusednihPoljaPoNivou vrsta dimenzijaIgre nivo brojac)
   )  
)

(defun proveraPoslednjaDvaPoljaPoNivou (vrsta nivo)
   (if (equalp (last (nth nivo (car (cdr (car vrsta))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (nth nivo (car (cdr (car vrsta))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveraSusednihPoljaPoNivou (vrsta dimenzijaIgre nivo brojac)
   (if (equalp (last (nth nivo (car (cdr (car vrsta))))) (last (nth nivo (car (cdr (cadr vrsta))))))
       (proveraPoVrstiKNivouKDublje (cdr vrsta) dimenzijaIgre nivo (1- brojac))
   )
)

(defun proveraPoNivoimaSuprotanSmer1 ()
   (loop for i from 0 to (1- n) do
      (proveraPoNivoimaSuprotanSmer2 (car (cdr (nth (1+ i) (nth 0 playground)))) (1+ i) i)
   )
)

(defun proveraPoNivoimaSuprotanSmer2 (stubic stubicZaIspitivanje vrsta) 
   (loop for i from 0 to (1- n) do
      (proveraPoNivoimaSuprotanSmer3 stubic stubicZaIspitivanje i vrsta)
   )  
)

(defun proveraPoNivoimaSuprotanSmer3 (stubic stubicZaIspitivanje nivo vrsta) 
   (if (not (equalp (last (nth nivo stubic)) '(-)))
      (proveraPoNivoimaSuprotanSmer4 stubic stubicZaIspitivanje nivo (1- n) vrsta)
   )  
)

(defun proveraPoNivoimaSuprotanSmer4 (stubic stubicZaIspitivanje nivo brojac vrsta)
   (if (= brojac 0)
      (proveraPoNivoimaSuprotanSmer5 stubicZaIspitivanje nivo vrsta)
      (proveraPoNivoimaSuprotanSmer6 stubic stubicZaIspitivanje nivo brojac vrsta)
   ) 
)

(defun proveraPoNivoimaSuprotanSmer5 (stubicZaIspitivanje nivo vrsta)
   (if (equalp (last (nth nivo (car (cdr (nth stubicZaIspitivanje (nth 0 playground)))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (nth nivo (car (cdr (nth stubicZaIspitivanje (nth 0 playground)))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveraPoNivoimaSuprotanSmer6 (stubic stubicZaIspitivanje nivo brojac vrsta)
   (if (equalp (last (nth nivo (car (cdr (nth stubicZaIspitivanje (nth brojac playground)))))) (last (nth nivo (car (cdr (nth stubicZaIspitivanje (nth (1- brojac) playground)))))))
       (proveraPoNivoimaSuprotanSmer4 stubic stubicZaIspitivanje nivo (1- brojac) vrsta)
   )
)

(defun proveriGlavnuDijagonalu11 ()
   (loop for i from 0 to (1- n) do
      (proveriGlavnuDijagonalu12 i)
   )
)

(defun proveriGlavnuDijagonalu12 (nivo)
   (if (not (equalp (last (nth nivo (car (cdr (car (cdr (car playground))))))) '(-)))
      (proveriGlavnuDijagonalu13 nivo (1- n))
   )   
)

(defun proveriGlavnuDijagonalu13 (nivo brojac)
   (if (= brojac 0)
      (proveriGlavnuDijagonalu14 nivo)
      (proveriGlavnuDijagonalu15 nivo brojac)
   )
)

(defun proveriGlavnuDijagonalu14 (nivo)
   (if (equalp (last (nth nivo (car (cdr (car (cdr (car playground))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (nth nivo (car (cdr (car (cdr (car playground))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriGlavnuDijagonalu15 (nivo brojac)
   (if (equalp (last (car (car (cdr (nth brojac (cdr (nth brojac playground))))))) (last (car (car (cdr (nth (1- brojac) (cdr (nth (1- brojac) playground))))))))
       (proveriGlavnuDijagonalu13 nivo (1- brojac))
   )
)

(defun proveriGlavnuDijagonalu21 ()
   (loop for i from 0 to (1- n) do
      (proveriGlavnuDijagonalu22 i)
   )
)

(defun proveriGlavnuDijagonalu22 (nivo)
   (if (not (equalp (last (nth nivo (car (cdr (last (cdr (car playground))))))) '(-)))
      (proveriGlavnuDijagonalu23 nivo n)
   )   
)

(defun proveriGlavnuDijagonalu23 (nivo brojac)
   (if (> brojac 0)
      (proveriGlavnuDijagonalu24 nivo)
      (proveriGlavnuDijagonalu25 nivo brojac)
   )
)

(defun proveriGlavnuDijagonalu24 (nivo)
   (if (equalp (last (nth nivo (car (cdr (car (last (car playground))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (nth nivo (car (cdr (car (last (car playground))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriGlavnuDijagonalu25 (nivo brojac)
   (if (equalp (last (car (car (cdr (nth (1- brojac) (cdr (nth (1- brojac) playground))))))) (last (car (car (cdr (nth (2- brojac) (cdr (nth (2- brojac) playground))))))))
       (proveriGlavnuDijagonalu23 nivo (1- brojac))
   )
)

(defun proveriDijagonaluUPenjanju11 ()
   (loop for i from 0 to (1- n) do
      (proveriDijagonaluUPenjanju12 i)
   )
)

(defun proveriDijagonaluUPenjanju12 (nivo)
   (if (not (equalp (last (car (car (cdr (nth nivo (cdr (car playground))))))) '(-)))
      (proveriDijagonaluUPenjanju13 nivo (1- n))
   )   
)

(defun proveriDijagonaluUPenjanju13 (nivo brojac)
   (if (> brojac 0)
      (proveriDijagonaluUPenjanju14 nivo brojac)
      (proveriDijagonaluUPenjanju15 nivo brojac)
   )
)

(defun proveriDijagonaluUPenjanju14 (nivo brojac)
   (if (equalp (last (nth brojac (car (cdr (nth nivo (cdr (nth brojac playground))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (nth brojac (car (cdr (nth nivo (cdr (nth brojac playground))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriDijagonaluUPenjanju15 (nivo brojac)
   (if (equalp (last (nth brojac (car (cdr (nth nivo (cdr (nth brojac playground))))))) (last (nth (1- brojac) (car (cdr (nth nivo (cdr (nth (1- brojac) playground))))))))
       (proveriDijagonaluUPenjanju13 nivo (1- brojac))
   )
)

(defun proveriDijagonaluUPenjanju21 ()
   (loop for i from 0 to (1- n) do
      (proveriDijagonaluUPenjanju22 i 0)
   )
)

(defun proveriDijagonaluUPenjanju22 (nivo nivo2)
   (if (not (equalp (last (car (last (last (car (cdr (nth nivo (cdr (car playground))))))))) '(-)))
      (proveriDijagonaluUPenjanju23 nivo nivo2 (1- n))
   )   
)

(defun proveriDijagonaluUPenjanju23 (nivo nivo2 brojac)
   (if (= brojac 0)
      (proveriDijagonaluUPenjanju24 nivo nivo2 brojac)
      (proveriDijagonaluUPenjanju25 nivo nivo2 brojac)
   )
)

(defun proveriDijagonaluUPenjanju24 (nivo nivo2 brojac)
   (if (equalp (last (car (last (last (car (cdr (nth nivo (cdr (car playground))))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (last (last (car (cdr (nth nivo (cdr (car playground))))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriDijagonaluUPenjanju25 (nivo nivo2 brojac)
   (if (equalp (last (nth nivo2 (car (cdr (nth nivo (cdr (nth brojac playground))))))) (last (nth (1+ nivo2) (car (cdr (nth nivo (cdr (nth (1- brojac) playground))))))))
       (proveriDijagonaluUPenjanju23 nivo (1+ nivo2) (1- brojac))
   )
)

(defun proveriGlavnuDijagonaluUPenjanju1 ()
   (if (not (equalp (last (car (car (cdr (car (cdr (car playground))))))) '(-)))
      (proveriGlavnuDijagonaluUPenjanju2 (1- n) (1- n))
   )
)

(defun proveriGlavnuDijagonaluUPenjanju2 (nivo brojac)
   (if (= brojac 0)
      (proveriGlavnuDijagonaluUPenjanju3 nivo brojac)
      (proveriGlavnuDijagonaluUPenjanju4 nivo brojac)
   )  
)

(defun proveriGlavnuDijagonaluUPenjanju3 (nivo brojac)
   (if (equalp (last (car (car (cdr (car (cdr (car playground))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (car (cdr (car (cdr (car playground))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriGlavnuDijagonaluUPenjanju4 (nivo brojac)
   (if (equalp (last (nth brojac (car (cdr (nth brojac (cdr (nth brojac playground))))))) (last (nth (1- brojac) (car (cdr (nth (1- brojac) (cdr (nth (1- brojac) playground))))))))
       (proveriGlavnuDijagonaluUPenjanju2 (1- nivo) (1- brojac))
   )
)

(defun proveriGlavnuDijagonaluUPadanju1 ()
   (if (not (equalp (last (car (last (last (car (cdr (car (cdr (car playground))))))))) '(-)))
      (proveriGlavnuDijagonaluUPadanju2 0 (1- n))
   )
)

(defun proveriGlavnuDijagonaluUPadanju2 (brojac vrsta)
   (if (= vrsta 0)
      (proveriGlavnuDijagonaluUPadanju3)
      (proveriGlavnuDijagonaluUPadanju4 brojac vrsta)
   )  
)

(defun proveriGlavnuDijagonaluUPadanju3 ()
   (if (equalp (last (car (last (last (car (cdr (car (cdr (car playground))))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (last (last (car (cdr (car (cdr (car playground))))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriGlavnuDijagonaluUPadanju4 (brojac vrsta)
   (if (equalp (last (nth brojac (car (cdr (nth vrsta (cdr (nth vrsta playground))))))) (last (nth (1+ brojac) (car (cdr (nth (1- vrsta) (cdr (nth (1- vrsta) playground))))))))
       (proveriGlavnuDijagonaluUPadanju2 (1+ brojac) (1- vrsta))
   )
)

(defun proveriGlavnuDijagonaluUPadanju21 ()
   (if (not (equalp (last (car (last (car (cdr (car (last (cdr (car playground))))))))) '(-)))
      (proveriGlavnuDijagonaluUPadanju22 0 (1- n))
   )
)

(defun proveriGlavnuDijagonaluUPadanju22 (brojac vrsta)
   (if (= vrsta 0)
      (proveriGlavnuDijagonaluUPadanju23)
      (proveriGlavnuDijagonaluUPadanju24 brojac vrsta)
   )  
)

(defun proveriGlavnuDijagonaluUPadanju23 ()
   (if (equalp (last (car (last (car (cdr (car (last (cdr (car playground))))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (last (car (cdr (car (last (cdr (car playground))))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriGlavnuDijagonaluUPadanju24 (brojac vrsta)
   (if (equalp (last (nth brojac (car (cdr (nth brojac (cdr (nth vrsta playground))))))) (last (nth (1+ brojac) (car (cdr (nth (1+ brojac) (cdr (nth (1- vrsta) playground))))))))
       (proveriGlavnuDijagonaluUPadanju22 (1+ brojac) (1- vrsta))
   )
)

(defun proveriGlavnuDijagonaluUPenjanju21 ()
   (if (not (equalp (last (car (car (cdr (car (last (cdr (car playground)))))))) '(-)))
      (proveriGlavnuDijagonaluUPenjanju22 0 (1- n))
   )
)

(defun proveriGlavnuDijagonaluUPenjanju22 (brojac vrsta)
   (if (= vrsta 0)
      (proveriGlavnuDijagonaluUPenjanju23)
      (proveriGlavnuDijagonaluUPenjanju24 brojac vrsta)
   )  
)

(defun proveriGlavnuDijagonaluUPenjanju23 ()
   (if (equalp (last (car (car (cdr (car (last (cdr (car playground)))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (car (cdr (car (last (cdr (car playground)))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriGlavnuDijagonaluUPenjanju24 (brojac vrsta)
   (if (equalp (last (nth vrsta (car (cdr (nth brojac (cdr (nth vrsta playground))))))) (last (nth (1- vrsta) (car (cdr (nth (1+ brojac) (cdr (nth (1- vrsta) playground))))))))
       (proveriGlavnuDijagonaluUPenjanju22 (1+ brojac) (1- vrsta))
   )
)

(defun proveriDijagonaluUPenjanju31 ()
   (loop for i from 0 to (1- n) do
      (proveriDijagonaluUPenjanju32 i)
   )
)

(defun proveriDijagonaluUPenjanju32 (nivo)
   (if (not (equalp (last (car (car (cdr (car (cdr (nth nivo playground))))))) '(-)))
      (proveriDijagonaluUPenjanju33 nivo (1- n))
   )   
)

(defun proveriDijagonaluUPenjanju33 (nivo brojac)
   (if (= brojac 0)
      (proveriDijagonaluUPenjanju34 nivo brojac)
      (proveriDijagonaluUPenjanju35 nivo brojac)
   )
)

(defun proveriDijagonaluUPenjanju34 (nivo brojac)
   (if (equalp (last (car (car (cdr (car (cdr (nth nivo playground))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (car (cdr (car (cdr (nth nivo playground))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriDijagonaluUPenjanju35 (nivo brojac)
   (if (equalp (last (nth brojac (car (cdr (nth brojac (cdr (nth nivo playground))))))) (last (nth (1- brojac) (car (cdr (nth (1- brojac) (cdr (nth nivo playground))))))))
       (proveriDijagonaluUPenjanju33 nivo (1- brojac))
   )
)

(defun proveriDijagonaluUPenjanju41 ()
   (loop for i from 0 to (1- n) do
      (proveriDijagonaluUPenjanju42 i)
   )
)

(defun proveriDijagonaluUPenjanju42 (nivo)
   (if (not (equalp (last (car (last (car (cdr (car (cdr (nth nivo playground)))))))) '(-)))
      (proveriDijagonaluUPenjanju43 nivo (1- n) 0)
   )   
)

(defun proveriDijagonaluUPenjanju43 (nivo brojac nivo2)
   (if (= brojac 0)
      (proveriDijagonaluUPenjanju44 nivo brojac)
      (proveriDijagonaluUPenjanju45 nivo brojac nivo2)
   )
)

(defun proveriDijagonaluUPenjanju44 (nivo brojac)
   (if (equalp (last (car (last (car (cdr (car (cdr (nth nivo playground)))))))) '(X))
      (setq skorX (1+ skorX))
   )
   (if (equalp (last (car (last (car (cdr (car (cdr (nth nivo playground)))))))) '(O))
      (setq skorO (1+ skorO))
   )
)

(defun proveriDijagonaluUPenjanju45 (nivo brojac nivo2)
   (if (equalp (last (nth nivo2 (car (cdr (nth brojac (cdr (nth nivo playground))))))) (last (nth (1+ nivo2) (car (cdr (nth (1- brojac) (cdr (nth nivo playground))))))))
       (proveriDijagonaluUPenjanju43 nivo (1- brojac) (1+ nivo2))
   )
)

(defun generisiSveMogucePoteze (lista)
   (setq moguciSledeciPotezi '())
   (generisiMogucePoteze 0 lista)     
)

(defun generisiMogucePoteze (brojac lista)
   (generisiMogucePotezeVrsteK brojac 0 '() lista)
   (if (< brojac n)
      (generisiMogucePoteze (1+ brojac) lista)
   )  
)

(defun generisiMogucePotezeVrsteK (vrsta kolona generisanaPoljaVrste lista)   
   (if (equalp (ispitaj (car (cdr (vratiStubic vrsta kolona lista))) n) 't)
      (setq generisanaPoljaVrste (append generisanaPoljaVrste (list (list vrsta kolona))))
   )
   (if (< kolona n)      
      (generisiMogucePotezeVrsteK vrsta (1+ kolona) generisanaPoljaVrste lista)
      (setq moguciSledeciPotezi (append moguciSledeciPotezi generisanaPoljaVrste))
   )  
)

(defun igracProtivIgraca ()
  (loop while (> krajIgre 0) do 
        (odigrajPotez)
        (setq previousMove (list vrsta kolona))
        (if (equalp (ispitaj (izdvojiPoljaStubica vrsta kolona playground) n) '())  
            (format t "Nema mesta na ovom stubicu, unesite ponovo koordinate.")
            (format t "Ima mesta, potez je odigran.")
        )
        (if (not (equalp (ispitaj (izdvojiPoljaStubica vrsta kolona playground) n) '()))  
            (setq krajIgre (1- krajIgre))
        )
        (if (equalp (ispitaj (izdvojiPoljaStubica vrsta kolona playground) n) '())  
           (setq naPotezu (1- naPotezu))
        )
        (if (= (mod naPotezu 2) 0) 
          (setq playground (potez vrsta kolona playground 'X)) 
          (setq playground (potez vrsta kolona playground 'O)))
        (stampaj playground)
        (setq naPotezu (1+ naPotezu))
  )
)

(defun igracProtivRacunara ()
  (loop while (> krajIgre 0) do 
      (if (= (mod naPotezu 2) 0)
          (odigrajPotez) 
          (potezRacunara playground)
      )
      (setq previousMove (list vrsta kolona))
      (if (equalp (ispitaj (izdvojiPoljaStubica vrsta kolona playground) n) '())  
          (format t "Nema mesta na ovom stubicu, unesite ponovo koordinate.")
          (format t "Ima mesta, potez je odigran.")
      )
      (if (not (equalp (ispitaj (izdvojiPoljaStubica vrsta kolona playground) n) '()))  
          (setq krajIgre (1- krajIgre))
      )
      (if (equalp (ispitaj (izdvojiPoljaStubica vrsta kolona playground) n) '())  
          (setq naPotezu (1- naPotezu))
      )
      (if (= (mod naPotezu 2) 0) 
          (setq playground (potez vrsta kolona playground 'X)) 
          (setq playground (potez vrsta kolona playground 'O))
      )
      (stampaj playground)
      (setq naPotezu (1+ naPotezu))        
  )
)

(defun potezRacunara (lista)
   (generisiSveMogucePoteze lista)
   (setq nasumicniPotez (random (length moguciSledeciPotezi))) 
   (setq vrsta (car (nth nasumicniPotez moguciSledeciPotezi)))
   (setq kolona (cadr (nth nasumicniPotez moguciSledeciPotezi)))
)

(defun MinMax (state move alpha beta currentDepth isMyMove)
    (generisiSveMogucePoteze state)
    (cond
        ((zerop currentDepth) (list move (evaluate state)))
        (t
            (setq result
                     (if isMyMove
                         (MaxPlay moguciSledeciPotezi '() currentDepth alpha beta isMyMove state )
                         (MinPlay moguciSledeciPotezi '() currentDepth alpha beta isMyMove state )
                     )
            ) 
            (cond
		 ((null moguciSledeciPotezi) (list move (evaluate state)))
                 ((equalp currentDepth depth) (car result))
                 (t (list move (cadr result)))
            )            
        )       
    )
)

(defun MaxPlay (movesList bestMove depth alpha beta isMyMove previousState)
    (if isMyMove (setq simbol 'X) (setq simbol 'O))
    (cond 
        ((null movesList) (list bestMove alpha))
        (t 
            (let*
                (                
		    (previousMoveState (potez (car (car movesList)) (cadr (car movesList)) previousState simbol))
                    (minMove (MinMax previousMoveState (car movesList) alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (>= alpha (cadr minMove)) (list bestMove alpha) minMove))
                )
                (if (or (> (cadr newMove) beta) (null movesList))
                        (list bestMove (cadr newMove))
                        (MaxPlay (cdr movesList) (car newMove) depth (cadr newMove) beta isMyMove previousState)
                )
            )
        )
    )
)

(defun MinPlay (movesList bestMove depth alpha beta isMyMove previousState)
    (if isMyMove (setq simbol 'X) (setq simbol 'O))
    (cond 
        ((null movesList) (list bestMove beta))
        (t 
            (let*
                (                
		    (previousMoveState (potez (car (car movesList)) (cadr (car movesList)) previousState simbol))
                    (maxMove (MinMax previousMoveState (car movesList) alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (<= beta (cadr maxMove)) (list bestMove beta) maxMove))
                )
                (if (or (< (cadr newMove) alpha) (null (cdr movesList)))
                        (list bestMove (cadr newMove))
			(MinPlay (cdr movesList) (car newMove) depth alpha (cadr newMove) isMyMove previousState)
                )
            )
        )
    )
)

(defun evaluate (move)
   (random 100)
)

(defun start ()
  (defvar n)
  (defvar vrsta)
  (defvar kolona)
  (defvar moguciSledeciPotezi)
  (defvar skorX)
  (defvar skorO)
  (setq naPotezu 0)
  (setq depth 1) 
  (setq result '())
  (unesiN)
  (setq krajIgre (* n (* n n)))
  (setq playground (kreirajMatricu n 65))
  (koIgraPrvi)
  (igracProtivRacunara)
  (odrediPobednika playground)
)

(start)