;;;; ----- Set-Theory-Functions.lisp ----- ;;;;

(defparameter *testset* '(5 2 9))
(defparameter *major-chord* '(6 9 2))
(defparameter *opus-16-3* '(0 4 8 9 11))

; ----- flesh out later ----- ;
(defparameter *named-chords*
  '(((0 1 2) 'chromatic)
  	((0 1 3) 'phrygian)
  	((0 1 4) 'chord-of-the-split-third)
  	((0 1 6) 'viennese)
  	((0 2 4) 'do-re-mi)
  	((0 2 5) 'minor-seventh-omit-five)
  	((0 2 6) 'dominant-seventh-omit-five)
  	((0 2 7) 'quintal-quartal)
    ((0 3 6) 'diminished)
    ((0 3 7) 'minor)
    ((0 4 7) 'major)
    ((0 4 8) 'augmented)
    ((0 3 6 9) 'fully-diminished)
    ((0 3 6 10) 'half-diminished)
    ((0 3 7 10) 'minor-seventh)
    ((0 4 7 10) 'dominant-seventh)
    ((0 4 7 11) 'major-seventh)))

; (car *named-chords*)
; ((0 2 7) 'QUINTAL-QUARTAL)

(defun safe-sort (alist &optional (predicate '<))
  "Safer sorting."
  (let ((temporary 
         (loop for x in alist 
           collect x)))
    (sort temporary predicate)))

; (safe-sort '(4 2 8 9 1 3))
; => (1 2 3 4 8 9)

(defun chromatic-scale (&optional (alpha 0) (omega 11))
  "Chromatic scale."
  (loop for i from alpha to omega append (list i)))

; (chromatic-scale)
; => (0 1 2 3 4 5 6 7 8 9 10 11)

; (chromatic-scale '0 24)
; => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)

(defun complement-set (pcc-a)
  "Complement set."
  (let ((pcc-c (chromatic-scale)))
    (safe-sort (set-difference pcc-c pcc-a) #'<)))

; (complement-set '(0 1 2 4 7 8))

; (complement-set '(0 1 2 4 7 8))
; => (3 5 6 9 10 11)

(defun transpose (pcc n)
  "Transpose set."
  (mapcar #'(lambda (i) (mod (+ n i) 12)) pcc))

; (transpose *testset* 6)
; => (11 8 3)

(defun invert (pcc n)
  "Invert set."
  (mapcar #'(lambda (i) (mod (- n i) 12)) pcc))

; (invert *testset* 0)
; => (7 10 3)
; (invert *testset* 1)
; => (8 11 4)

;; ----- CPP-Form, i.e. "T-Normal-Form," according to Cope ----- ;;

(defun cpp-form (pcc)
  "Stacks all members in a PCC to a traditional root inversion chord."
  (stable-sort (copy-seq pcc) #'<))

; (cpp-form *testset*)
; => (2 5 9)
; (cpp-form *opus-16-3*)
; => (0 4 8 9 11)

(defun t-cpp-form (pcc)
  "Stacked traditional PCC as chord starting at zero."
  (let ((sorted-pcc (cpp-form pcc)))
    (mapcar #'(lambda (x) (mod (- x (car sorted-pcc)) 12)) sorted-pcc)))

; (t-cpp-form *testset*)
; => (0 3 7)
; (t-cpp-form *opus-16-3*)
; => (0 4 8 9 11)

;; ----- Normal Form ----- ;;

(defun rotations (pcc)
  "Create all possible rotations from a sorted set."
  (let ((sorted-pcc (stable-sort (copy-seq pcc) #'<)))
    (loop for i from 0 below (length sorted-pcc)
      collect (append 
               (subseq sorted-pcc i (length sorted-pcc))
               (subseq sorted-pcc 0 i)))))

; (rotations '(4 9 1))
; => ((1 4 9) (4 9 1) (9 1 4))

(defun fast-normal-form (rotations)
  "Finds set with smallest interval from first and last PC in set, and adds that interval as key to the set."
  (if (null rotations) nil
    (cons
     (list
      (mod (- (car (last (car rotations))) (caar rotations)) 12)
      (car rotations))
     (fast-normal-form (cdr rotations)))))

; (fast-normal-form (rotations '(4 9 1)))

(defun min-list (lst)
  "Finds the lowest key."
  (if (null lst) nil
    (cons
     (caar lst)
     (min-list (cdr lst)))))

; (min-list '((8 (1 4 9)) (9 (4 9 1)) (7 (9 1 4)))))

(defun find-smallest-key (closest)
  "Finds the smallest key from a group of sets."
  (reduce #'min (min-list closest)))

; (find-smallest-key '((7 (4 8 9 11 0)) (4 (8 9 11 0 4))))

(defun find-dupes (lst match)
  "Find sets with duplicate keys and group them together."
  (loop for i from 0 below (length lst)
    if (equal (car (nth i lst)) match)
    collect (cadr (nth i lst))))

; (find-dupes '((8 (1 4 9)) (9 (4 9 1)) (7 (9 1 4))) 7)

(defun next-to-last (dupes &optional (i 0))
  "Drills down to find smallest interval between first and second to last PC in set."
  (if (null dupes) nil
    (cons
     (list
      (mod (- (car (subseq (car dupes) (- (length (car dupes)) (+ i 2)) (- (length (car dupes)) (+ i 1)))) (caar dupes)) 12)
      (car dupes))
     (next-to-last (cdr dupes) i))))

; (next-to-last '((4 8 9 11 0) (8 9 11 0 4)))

(defun inter-normal-form (keyed-pcc &optional (i 0))
  "Recursively finds smallest key."
  (let* ((matcher (find-smallest-key keyed-pcc))
         (dupes (find-dupes keyed-pcc matcher)))
    (if (> (length dupes) 1)
      (inter-normal-form (next-to-last dupes (+ i 0)) (+ i 1))
      (cadar (stable-sort (copy-seq keyed-pcc) #'< :key #'car)))))

; (inter-normal-form (fast-normal-form (rotations '(1 4 9))))    

(defun normal-form (pcc)
  "Find normal form."
  (inter-normal-form (fast-normal-form (rotations pcc))))

; (normal-form *testset*)
; => (2 5 9)
; (normal-form *major-chord*)
; => (2 6 9)
; (normal-form '(3 1 5 2 8 9))
; => (1 2 3 5 8 9)
; (normal-form '(0 8 11 4 9))
; => (8 9 11 0 4)
; (normal-form '(4 9 1))
; => (9 1 4)

;; ----- T-Normal Form ----- ;;

(defun t-normal-form (normal-form)
  "Create transposed (to '0') normal form."
  (mapcar #'(lambda (x) (mod (- x (car normal-form)) 12)) normal-form))

; (t-normal-form (normal-form *testset*))
; => (0 3 7)
; (t-normal-form (normal-form *major-chord*))
; => (0 4 7)
; (t-normal-form (normal-form '(3 1 5 2 8 9)))
; => (0 1 2 4 7 8)
; (t-normal-form (normal-form '(0 8 11 4 9)))
; => (0 1 3 4 8)
; (t-normal-form (normal-form '(4 9 1)))
; => (0 4 7)
; (t-normal-form (normal-form '(11 2 8 4)))
; => (0 3 6 8)

;; ----- Prime Form ----- ;;

(defun all-transpositions (pcc)
  "Creates transposition scheme for prime form."
  (remove-duplicates 
   (loop for i from 0 below 12 
     collect (transpose pcc i)) :test 'equalp))

; (all-transpositions *testset*)
; => ((5 2 9) (6 3 10) (7 4 11) (8 5 0) (9 6 1) (10 7 2) (11 8 3) (0 9 4) (1 10 5) (2 11 6) (3 0 7) (4 1 8))

(defun all-inversions (pcc)
  "Creates inversion scheme for prime form."
  (remove-duplicates 
   (loop for i from 0 below 12 
     collect (invert pcc i)) :test 'equalp))

; (all-inversions *testset*)
; => ((3 7 10) (4 8 11) (0 5 9) (1 6 10) (2 7 11) (0 3 8) (1 4 9) (2 5 10) (3 6 11) (0 4 7) (1 5 8) (2 6 9))

(defun prime-form (pcc)
  "Prime form of a set, by summing all rotations. Smallest sum is prime form."
  (let* ((all-ti-forms 
          (append (all-transpositions pcc) (all-inversions pcc)))
         (sums 
          (loop for ti in all-ti-forms 
            collect (cons (loop for pc in ti sum pc) ti))))
    (safe-sort (cdar (stable-sort (copy-seq sums) #'< :key #'car)) #'<)))

; (prime-form '(3 1 5 2 8 9))
; => (0 1 2 4 7 8)
; (prime-form '(7 1 8))
; => (0 1 6)
; (prime-form '(2 6 9))
; => (0 3 7)
; (prime-form '(0 4 8 9 11))
; => (0 1 3 4 8)
; (prime-form *major-chord*)
; => (0 3 7)

;; ----- Interval Vectors ----- ;;

(defun all-intervals-from (pc pcc)
  "Type of intervals in a set."
  (loop for i in pcc 
    collect (min (abs (mod (- i pc) 12))
                 (abs (mod (- pc i) 12)))))

; (all-intervals-from (car '(0 8 11 4 9)) (cdr '(0 8 11 4 9)))
; => (4 1 4 3)

(defun all-intervals (pcc)
  "Amount of interval types in a set."
  (if (null pcc) nil
    (append 
     (all-intervals-from (car pcc) (cdr pcc)) 
     (all-intervals (cdr pcc)))))

; (all-intervals '(0 8 11 4 9))
; => (4 1 4 3 3 4 1 5 2 5)

(defun interval-vector (pcc)
  "Set interval vector."
  (let ((intervals 
         (all-intervals pcc)))
    (loop for i below 6 
      collect (count (1+ i) intervals))))

; (interval-vector *testset*)
; => (0 0 1 1 1 0)
; (interval-vector '(0 8 11 4 9))
; => (2 1 2 3 2 0)

;; ----- Relationships of two sets ----- ;;

; ----- Transpositional ----- ;

(defun transpositionally-related (pcc-1 pcc-2)
  "Are two sets transpositionall related?"
  (if (equal (length pcc-1) (length pcc-2))
    (let* ((nf-set-1 (normal-form pcc-1))
           (nf-set-2 (normal-form pcc-2))
           (results (remove-duplicates (mapcar #'- nf-set-2 nf-set-1 ))))
      (if (> (length results) 1)
        '(not transpositionally related)
        (car results)))
    (princ '(Ensure that sets are of equal size))))
    
; (transpositionally-related '(0 1 4) '(3 4 7) )

; ----- Inversional ----- ;

(defun ixy (pc-1 pc-2)
  "Translates from Ixy (x & y are stacked), and creates the index sum."
  (mod (+ pc-1 pc-2) 12))

; (ixy 0 2)
; => 2
; (ixy 11 4)
; => 3
; (ixy 9 4)
; => 1
; (ixy 2 1)

(defun inversionally-related (set-1 set-2)
  "Are two sets inversionally related?"
  (if (equal (length set-1) (length set-2))
    (let* ((nf-set-1 (normal-form set-1))
           (rnf-set-2 (reverse (normal-form set-2)))
           (results (remove-duplicates (mapcar #'ixy nf-set-1 rnf-set-2))))
      (if (> (length results) 1)
        'no-relationship
        (car results)))
    (princ '(Ensure that sets are of equal size))))
      
; (inversionally-related '(11 8 7) '(4 1 5))
; => 0
; (inversionally-related '(7 8 11) '(7 10 11))
; => 6
; Berios Sequenza for Flute:
; (inversionally-related '(1 4 6) '(9 11 2))
; => 3
; (inversionally-related '(9 11 2) '(0 3 5))
; => 2

; ----- Checking for inversional relationship for each Ixy pair manually ----- ;
; ----- Berio Sequenza for Flute, Straus, p. 51 ----- ;
; (mapcar #'ixy (normal-form '(1 4 6)) (reverse (normal-form '(9 11 2))))
; (mapcar #'ixy (normal-form '(9 11 2)) (reverse (normal-form '(0 3 5))))

; ----- Schoenberg Piano Piece, Op. 11, No. 1, Straus, p. 56 ----- ;
; (mapcar #'ixy (normal-form '(7 8 11)) (reverse (normal-form '(7 10 11))))
; (ixy 7 11)
; (mapcar #'ixy (normal-form '(7 10 11)) (reverse (normal-form '(8 9 0))))
; (ixy 11 8)

;;; ----- END Set-Theory-Functions.lisp ----- ;;;
