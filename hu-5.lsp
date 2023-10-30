;;;Tao Hu
;;;solution to problem A 
(defun my-index (n L)
  (let ((X (index (- n 1) (cdr L))))
    (if X
        X
        'ERROR)))
;;;B
(defun my-min-first (L)
        (let ((X (min-first (cdr L))))
          (if (> (car L) (car X)) 
              (cons (car X) (cons (car L) (cdr X)))
              (cons (car L) X))))
              
;;;C 
(defun my-ssort (L)  ;;; return sorted list
  (let* ((L1 (min-first L))  ;;; return list with first min value
         (X  (ssort (cdr L1))))
    (cons (car L1) X)))
    
;;; part 2
;;; 1 
(defun INDEX (N L)
  (cond
    ((or (<= N 0) (null L))  'ERROR)
    ((= N 1) (car L))
    (t (INDEX(- N 1)(CDR L)))))
