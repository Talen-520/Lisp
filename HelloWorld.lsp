;;; Tao Hu
;;; Solution to Problem 1
(defun MIN-2 (A B)
  (cond
    ((and (numberp A) (numberp B))
     (if (<= A B)
         A
         B))
    (t 'ERROR)))


;;; Solution to Problem 2
(defun SAFE-AVG (a b)
  (cond ((and (numberp a) (numberp b))
    (/ (+ a b) 2))
  ))

;;; Solution to Problem 3
(defun ODD-GT-MILLION (x)
  (cond ((and (integerp x) (> x 1000000) (not (= (mod x 2) 0)))
         T)))

;;; Solution to Problem 4
(defun MULTIPLE-MEMBER (a b)
  (and (or (symbolp a) (numberp a))
  (listp b) 
  (equal 2 (count a b))  )
)


;;; Solution to Problem 5

(defun MONTH->INTEGER (x)
  (cond
   ((equal x 'JANUARY) 1)
   ((equal x 'FEBRUARY) 2)
   ((equal x 'MARCH) 3)
   ((equal x 'APRIL) 4)
   ((equal x 'MAY) 5)
   ((equal x 'JUNE) 6)
   ((equal x 'JUNE) 7)
   ((equal x 'AUGUST) 8)
   ((equal x 'SEPTEMBER) 9)
   ((equal x 'SEPTEMBER) 10)
   ((equal x 'NOVEMBER) 11)
   ((equal x 'DECEMBER) 12)
   (t 'ERROR)
))

;;; Solution to Problem 6
(defun SCORE->GRADE (s)
    (cond ((>= s 90) 'A)
    ((and (< s 90) (>= s 87)) 'A-)
    ((and (< s 87) (>= s 83)) 'B+)
    ((and (< s 83) (>= s 80)) 'B)
    ((and (< s 80) (>= s 77)) 'B-)
    ((and (< s 77) (>= s 73)) 'C+)
    ((and (< s 73) (>= s 70)) 'C)
    ((and (< s 70) (>= s 60)) 'D)
    ((< s 60) 'F)
    (t NIL))
)

;;; Solution to Problem 7
(defun GT (a b)
  (and (numberp a) (numberp b) (> a b) )
)

;;; Solution to Problem 8
(defun SAME-PARITY (a b)
  (and
    (integerp a)
    (integerp b)
    (equal (% a 2) (% b 2))
))


;;; Solution to Problem 9
(defun SAFE-DIV (a b)
  (and
      (integerp a)
      (integerp b)
      (not (zerop b))
      (/ a b)
      )
)



