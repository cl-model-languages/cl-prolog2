


(in-package :cl-prolog)

(named-readtables:in-readtable :fare-quasiquote)

(defun sort-clauses (rules)
  "Destructively sort the rules by dictionary ordering.
Useful for avoiding noncontiguous-clauses errors (rules for the same clauses are not written adjacently).
Facts (rules without conditions) are given precedence to the standard rules.
Sorting is stable.
However, since it alters the ordering of the rules and Prolog checks the rules from top-to-bottom,
it may corrupt the program especially when cut operator (!) is involved."
  (stable-sort rules
        (lambda (a b)
          (ematch* (a b)
            ((`(:- (,name1 ,@args1) ,@_) `(:- (,name2 ,@args2) ,@_))
             (cond
               ((string< name1 name2) t)
               ((string= name1 name2)
                (< (length args1) (length args2)))
               ((string> name1 name2) nil)))

            ((`(:- (,name1 ,@args1) ,@_) `(,name2 ,@args2))
             (cond
               ((string< name1 name2) t)
               ((string= name1 name2)
                (< (length args1) (length args2)))
               ((string> name1 name2) nil)))
            
            ((`(,name1 ,@args1) `(:- (,name2 ,@args2) ,@_))
             (cond
               ((string< name1 name2) t)
               ((string= name1 name2)
                (<= (length args1) (length args2)))
               ((string> name1 name2) nil)))
            
            ((`(,name1 ,@args1) `(,name2 ,@args2))
             (cond
               ((string< name1 name2) t)
               ((string= name1 name2)
                (< (length args1) (length args2)))
               ((string> name1 name2) nil)))))))

(defun print-sexp (&key swi)
  "This function returns a cl-prolog program for a prolog rule print-sexp/1,
which prints a prolog term in a SEXP form.

print-sexp prints atoms/numbers as atoms/numbers, a term as a list, and a list as a list.

When SWI is non-nil, use compound_name_arguments/2 instead of =../2 for printing a term.
"
  `((:- (print-sexp (list))
        (write "()")
        !)
    (:- (print-sexp ?term)
        (atomic ?term)
        (write ?term)
        !)
    (:- (print-sexp (list* ?car ?cdr))
        (write "(")
        (print-sexp ?car)
        (print-sexp-list-aux ?cdr)
        (write ")")
        !)
    
    (:- (print-sexp ?term)
        ,@(if swi
              `((compound_name_arguments ?term ?head ?args)
                (print-sexp (list* ?head ?args)))
              `((=.. ?term ?l)
                (print-sexp ?l))))
        
    (print-sexp-list-aux (list))
    (:- (print-sexp-list-aux (list* ?car ?cdr))
        (atomic ?car) !
        (write " ")
        (print-sexp ?car)
        (print-sexp-list-aux ?cdr))
    (:- (print-sexp-list-aux (list* ?car ?cdr))
        (write " \\n")
        (print-sexp ?car)
        (print-sexp-list-aux ?cdr))))
