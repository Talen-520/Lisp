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

;;;solution to problem 5
(defun merge-lists (L1 L2)
  (cond ((null L1) L2)                     
        ((null L2) L1)                   
        ((< (car L1) (car L2))
         (cons (car L1) (merge-lists (cdr L1) L2))) 
        (t
         (cons (car L2) (merge-lists L1 (cdr L2)))))) 

;;;solution to problem 6
(defun msort (L)
  (if (or (endp L) (endp (cdr L)))  ; base case: L is empty or has only one element
      L
      (let* ((split-result (split-list L))  ; split the list into two halves
             (first-half (car split-result))
             (second-half (cadr split-result)))
        (merge-lists (msort first-half)  ; recursively sort the first half
                     (msort second-half)))))  ; recursively sort the second half

;;;solution to problem 7
(defun REMOVE-ADJ-DUPL (L);;;(a b a c)
    (cond
       ((endp L) NIL)                                     ; If the list is empty, return NIL
       ((endp (cdr L)) L)                                 ; If there's only one element left, return it
       ((eql (car L) (car (cdr L))) (remove-adj-dupl (cdr L)))  ; If this element is same as next, skip this one
       (t (cons (car L) (remove-adj-dupl (cdr L))))))

;;;solution to problem 8
(defun UNREPEATED-ELTS (L) ;;;in the book input (a b a a a c c) output is (a b) I assume this is error,the expected output should be b
  (if (null lst)
      '()
      (let* ((current-elem (car lst))
             (rest (cdr lst)))
        (if (not (member current-elem rest))
            (cons current-elem (UNREPEATED-ELTS rest))
            (UNREPEATED-ELTS rest)))))

;;;solution to problem 9
(defun REPEATED-ELTS (L)
  (if (null L)
      '()
      (if (member (car L) (cdr L))
          (cons (car L) (REPEATED-ELTS (remove (car L) (cdr L))))
          (REPEATED-ELTS (cdr L)))))

;;;solution to problem 10
(defun COUNT-REPETITIONS (L)
  (if (null lst)
      '()
      (let* ((current-elem (car lst))
             (rest (cdr lst))
             (count (count current-elem lst)))
        (cons (list count current-elem) (count-repeated-elements (remove current-elem lst))))))
