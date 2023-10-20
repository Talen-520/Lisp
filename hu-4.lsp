
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
            (INSERT car(L) X)))
            
;;;solution to problem F 
(defun my-split-list (L)
          (let ((X (split-list (cdr L))))
            __________________________________ ))
