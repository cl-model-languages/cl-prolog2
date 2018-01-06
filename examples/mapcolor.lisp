
;; CLI interface example.
;; Run this script via
;;
;; ros install cl-prolog2
;; cl-prolog2 mapcolor.lisp

;; code based on https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_1.html

(adjacent 1 2)
(adjacent 2 1) 
(adjacent 1 3)
(adjacent 3 1) 
(adjacent 1 4)
(adjacent 4 1) 
(adjacent 1 5)
(adjacent 5 1) 
(adjacent 2 3)
(adjacent 3 2) 
(adjacent 2 4)
(adjacent 4 2) 
(adjacent 3 4)
(adjacent 4 3) 
(adjacent 4 5)
(adjacent 5 4) 
(color 1 red a)    (color 1 red b) 
(color 2 blue a)   (color 2 blue b) 
(color 3 green a)  (color 3 green b) 
(color 4 yellow a) (color 4 blue b) 
(color 5 blue a)   (color 5 green b)

(:- (conflict ?coloring)
    (adjacent ?x ?y)  
    (color ?x ?color ?coloring)  
    (color ?y ?color ?coloring))


(:- (conflict ?r1 ?r2 ?coloring)
    (adjacent ?r1 ?r2)  
    (color ?r1 ?color ?coloring)  
    (color ?r2 ?color ?coloring))


;; there are several infix operators.
;; :- , >, <, -> etc.
;; let's mark variables with ? prefix.
;; 

(:- main
    (forall (conflict ?coloring)
            (writeln (conflict ?coloring)))
    (forall (conflict ?r1 ?r2 ?coloring)
            (writeln (conflict ?r1 ?r2 ?coloring)))
    halt)

(:- (initialization main)) 
