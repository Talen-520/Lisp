;;;Tao Hu
;;;solution to problem A 
(defun my-sum (L)
  (let ((X (sum (cdr L))))
    (+ (car L) X)))

;;;solution to problem B 
(defun my-neg-nums (L)
            (let ((X (neg-nums (cdr L))))
            (if (<  (car L) 0) 
              (cons X (car L)) 
              X)))
;;;solution to problem C 
(defun my-inc-list-2 (L n)
  (let ((X (inc-list-2 (cdr L) n)))
    (cons (+ (car L) n) X)))

;;;solution to problem D 
(defun my-insert (n L)
  (let ((X (insert n (cdr L))))
    (if (< (car L) (car X)); n is already inserted into X
      (cons (car L) X)
      (cons X L))))
      
;;;solution to problem E 
(defun my-isort (L)
  (let ((X (isort (cdr L))))
    (insert (car L) X)))
            
;;;solution to problem F 
;;;(SPLIT-LIST '(B C D 1 2 3 4 5)) =>  ((B D 2 4) (C 1 3 5))  
(defun my-split-list (L)
                     (let ((X (split-list (cdr L))));;;list (cdr L) == (C D 1 2 3 4 5) after split ((C 1 3 5) (D 2 4))
                       (if(null L)
                          (list NIL NIL)
                          (list (cons (car L) (cadr X)) (car X)))))

;;;solution to problem G
(defun my-partition (L p)                           
                    (let ((X (partition (cdr L) p))) 
                      (if (< (car L) p)
                          (list (cons (car L) (car X)) (cadr X))
                          (list (car X) (cons (car L) (cadr X))))))
                        
;;;solution to problem 1
(defun SUM (L)
  (if (null L)
      0
      (if (not (numberp (car L)))
          'ERR!
          (let ((X (SUM (cdr L))))
            (if (numberp X)
                (+ (car L) X)
                'ERR!)))))
;;;solution to problem 2
(defun NEG-NUMS (L)
  (if (null L)
      NIL
      (if (not (numberp (car L)))
          'ERR
          (let ((X (NEG-NUMS (cdr L))))
            (if (< (car L) 0)
                (cons (car L) X)
                X)))))

;;;solution to problem 3
(defun INC-LIST-2 (L p)
  (if (null L)
      NIL
      (if (or (not (numberp (car L))) (not (numberp p)))
          'ERR
          (cons (+ (car L) p) (INC-LIST-2 (cdr L) p)))))
          
;;;solution to problem 4
(defun INSERT (N L)
  (if (null L);;;()
      (list N);;;8->(8)
      (if (< N (car L))
          (cons N L)
          (cons (car L) (INSERT N (cdr L))))))

;;;solution to problem 5
(defun ISORT (L)
  (if (null L)
      NIL  ; If L is empty, return an empty list
      (insert (car L) (ISORT (cdr L)))))

;;;solution to problem 6
(defun SPLIT-LIST (L)
  (if (null L)
      (list NIL NIL)  
      (let ((X (split-list (cddr L))))   
        (list (cons (car L) (car X)) 
              (if (null (cadr L))
                  (cadr X)  
                  (cons (cadr L) (cadr X)))))))  
                  
;;;solution to problem 7
(defun PARTITION (L N)
  (if (null L)
      (list nil nil)   
      (let ((X (PARTITION (cdr L) N)))   
        (if (< (car L) N)
            (list (cons (car L) (car X))  
                  (cadr X))  
            (list (car X)  ; 
                  (cons (car L) (cadr X)))))))

;;;solution to problem 8
(defun pos (e L)
  (cond ((endp L) 0)   
        ((equal e (car L)) 1)  
        (t (let ((X (pos e (cdr L)))) 
             (if (zerop X)   
                 0  
                 (+ 1 X))))))  

;;;solution to problem 9
(defun SPLIT-NUMS (N)
  (if (< N 0)
      (list nil nil) 
      (let ((X (SPLIT-NUMS (- N 1)))) 
        (if (evenp N)  
            (list (cons N (car X)) (cadr X))   
            (list (car X) (cons N (cadr X)))))))   

;;;solution to problem 10
(defun SET-UNION (s1 s2)
  (cond ((null s1) s2)   
        ((null s2) s1)   
        ((member (car s1) s2)  ; if the first element of s1 is in s2,
         (SET-UNION (cdr s1) s2))  ; skip it and continue with the rest of s1
        (t  ; otherwise,
         (cons (car s1)  ; keep the first element of s1 and
               (SET-UNION (cdr s1) s2)))))  

;;;solution to problem 11
(defun SET-REMOVE (x s)
  (cond
   ((null s) nil)  
   ((equal (car s) x) (SET-REMOVE x (cdr s)))  
   (t (cons (car s) (SET-REMOVE x (cdr s))))))

;;;solution to problem 12   
(defun SET-EXCL-UNION (s1 s2)
  (cond 
    ((null s1) s2)  
    ((null s2) s1)  
    ((member (car s1) s2)  ; If the first element of s1 is a member of s2,
     (set-excl-union (cdr s1) (SET-REMOVE (car s1) s2)))  ;  helping functions SET-REMOVE is from problem 11
    (t  ; Otherwise,
     (cons (car s1) (SET-EXCL-UNION (cdr s1) s2)))))   

;;;solution to problem 13
(defun SINGLETONS (e)
  (cond ((null e) nil)
        ((member (car e) (cdr e)) (SINGLETONS (SET-REMOVE (car e) e)));;;helping functions SET-REMOVE from problem 11
        (t
         (cons (car e) (SINGLETONS (cdr e))))))

;this is the first solution I wrote, this version will turn list to set consist all unique element in the list 
;(defun SINGLETONS (e)
;  (if (null e)
;      nil
;      (if (member (car e) (cdr e))
;      (SINGLETONS (cdr e))
;     (cons (car e) (SINGLETONS (cdr e)))
;      )))
