;;;Tao Hu
;;;solution to problem A 
(defun my-index (n L)
  (let ((X (index (- n 1) (cdr L))))
    (if X
        X
        'ERROR)))
;;;solution to problem B
(defun my-min-first (L)
        (let ((X (min-first (cdr L))))
          (if (> (car L) (car X)) 
              (cons (car X) (cons (car L) (cdr X)))
              (cons (car L) X))))
              
;;;solution to problem C 
(defun my-ssort (L)  ;;; return sorted list
  (let* ((L1 (min-first L))  ;;; return list with first min value
         (X  (ssort (cdr L1))))
    (cons (car L1) X)))
    
;;; part 2
;;;solution to problem 1 
(defun INDEX (N L)
  (cond
    ((or (<= N 0) (null L))  'ERROR)
    ((= N 1) (car L))
    (t (INDEX(- N 1)(CDR L)))))
    
;;;solution to problem 2
(defun min-first (L)
  (let ((min-val (apply #'min L))) ;;; min is built-in function to get min value of the list
    (if (eql (car L) min-val)
        L
        (cons min-val (remove min-val L :count 1)))))

;;;solution to problem 3
(defun SSORT(L)
  (if (endp L)  
      nil
      (let ((X (min-first L)))
      (cons (car X) (SSORT (cdr X))))))
      
;;;solution to problem 4
(defun qsort (L)
  (if (endp L)
      NIL
      (let ((pL (partition (cdr L) (car L)))) ;;;return two list ( < car L) (>= car l)
      (append (qsort(car pL))
              (list (car L))
              (qsort(cadr pL))))))

;;;solution to problem 4

