


(in-package :cl-prolog)

(named-readtables:in-readtable :fare-quasiquote)

(defun sort-clauses (rules)
  "Sort the rules by dictionary ordering.
Useful for avoiding noncontiguous-clauses errors (rules for the same clauses are not written together).
Facts (rules without conditions) are given precedence to the standard rules."
  (sort rules
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
