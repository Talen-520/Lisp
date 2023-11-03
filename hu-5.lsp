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
  (if (null L)
      '()
      (let* ((curr (car L))
             (rest (cdr L)))
        (if (not (member curr rest))
            (cons curr (UNREPEATED-ELTS rest))
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
  (if (null L)
      '()
      (let* ((curr (car L))
             (rest (cdr L))
             (count (count curr L)))
        (cons (list count curr) (count-repeated-elements (remove curr L))))))
        
;;;solution to problem 11
(defun SUBSET (func L)
  (if (null L)
      '()
      (let ((curr (car L))
            (rest (cdr L)))
        (if (funcall func curr)
            (cons curr (SUBSET func rest))
            (SUBSET func rest)))))

;;;solution to problem 12
(defun OUR-SOME (func L)
  (if (null L)
      nil ; If the list is empty, return nil.
      (let ((curr (car L))
            (rest (cdr L)))
        (if (funcall func curr)
            L ; If the function returns true, return the rest of the list.
            (OUR-SOME func rest)))))


(defun OUR-EVERY (func L)
  (if (null L)
      t ; If the list is empty, return true (T).
      (let ((curr (car L))
            (rest (cdr L)))
        (if (funcall func curr)
            (OUR-EVERY func rest) ; Continue checking the rest of the list.
            nil)))) ; If the function returns false, return nil.
;;;solution to problem 13
;;;modified partition 
(defun PARTITION1 (p L pivot)
  (if (null L)
      (list nil nil)   
      (let ((X (PARTITION1 p (cdr L) pivot)))   
        (if (funcall p (car L) pivot)
            (list (cons (car L) (car X))  
                  (cadr X))  
            (list (car X)  ; 
                  (cons (car L) (cadr X)))))))
;;; solution QSORT1
(defun QSORT1 (p L)
  (if (endp L)
      NIL
      (let* ((pivot (car L))
             (pL (PARTITION1 p (cdr L) pivot))) ;;;return two lists; one with elements satisfying the predicate with respect to pivot, and the other with the rest
      (append (QSORT1 p (car pL))
              (list pivot)
              (QSORT1 p (cadr pL))))))

;;;solution to problem 14
;; Helper function transforms the element at the specified index in the list using the provided function.
(defun transform-element (L idx func)
  (mapcar (lambda (x y) (if (eql idx y) (funcall func x) x))
          L
          (loop for j from 1 to (length L) collect j)))

(defun foo (func L)
  (let ((len (length L)))
    (loop for i from 1 to len
          collect (transform-element L i func))))

;;;solution to problem 15 (a)
(defun tr-add (L acc)
  (if (null L) 
      acc
      (tr-add (cdr L) (+ acc (car L)))))

(defun tr-mul (L acc)
  (if (null L) 
      acc
      (tr-mul (cdr L) (* acc (car L)))))

(defun tr-fac (n acc)
  (if (<= n 1)
      acc
      (tr-fac (- n 1) (* acc n))))

;;;solution to problem 15 (b)
;Important: To prevent stack overflow, enter (compile 'tr-fac) at clisp's > prompt 
; before calling SLOW-PRIMEP with large arguments.

(defun slow-primep (n)
  (if (= (mod (tr-fac (- n 1) 1) n) (- n 1))
      t
      nil))
(defun next-prime (n)
  (if (slow-primep n)
      n
      (next-prime (+ n 1))))

;;;solution to problem 16
(defun transpose1 (M)
  (if (endp (car M))
      nil
      (cons (mapcar #'car M) (transpose1 (mapcar #'cdr M)))))

(defun transpose2 (M)
  (if (endp (cdr M))
      (list (mapcar #'list (car M)))
      (mapcar #'cons (car M) (transpose2 (mapcar #'cdr M)))))

(defun transpose3 (M)
  (apply #'mapcar (cons #'list M)))

