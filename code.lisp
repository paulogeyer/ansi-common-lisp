(defun meh ()
  (princ "Some text"))

(defun sum (n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

(defun addn (n)
  #'(lambda (x)
      (+ x n)))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun askem (string)
  (format t "~A" string)
  (read))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares (i end)
  (if (> i end)
      'done
      (progn
	(format t "~A ~A~%" i (* i i))
	(show-squares (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length (lst)
  (if (null lst)
      0
      (+ (our-length (cdr lst)) 1)))

(defun return-fourth (lst)
    (car (cdr (cdr (cdr lst)))))

(defun return-greater (a b)
  (if (> a b)
      a
      b))

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

(defun include-listp (lst)
  (let ((i (car lst)))
    (if (and (not (null i))
	     (listp i))
	t
	(if (cdr lst)
	    (include-listp (cdr lst))
	    nil))))

(defun print-dots (n)
  (dotimes (i n)
    (format t "."))
  (format t "~%"))

(defun print-dots (n)
  (if (= n 0)
      nil
      (progn
	(format t ".")
	(print-dots (1- n)))))

(defun count-symbol (s lst)
  (let ((acc 0))
    (dolist (symbol lst)
      (if (eql symbol s)
	  (incf acc 1)))
    acc))

(defun count-symbol (s lst)
  (if (not (null lst))
      (if (eql (car lst) s)
	  (1+ (count-symbol s (cdr lst)))
	  (count-symbol s (cdr lst)))
      0))

;; pg 37, compress
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

;; pg 38
(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt)
		    rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

;; (count-symbol 'a '(a b c d a))

;; pg 39
(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

; pg 41
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
	    (our-copy-tree (cdr tr)))))

;; pg 42
(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

;; pg 46
(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (let ((mid (/ len 2)))
	   (equal (subseq s 0 mid)
		  (reverse (subseq s mid)))))))

;; pg 47
(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

;; pg 48
(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

;; pg 49
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
	   (proper-list? (cdr x)))))

;; pg 51
(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
	 (if (eql key (car pair))
	     pair
	     (our-assoc key (cdr alist))))))

;; pg 52
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

;; pg 56 (exercises)
(defun new-union (lst1 lst2)
  (let ((output (copy-list lst1)))
    (dolist (i lst2)
      (if (not (member i lst1))
	  (setf output (append output (list i)))))
    output))

(defun occurrences (lst)
  (let ((occ '()))
    (dolist (i lst)
      (if (assoc i occ)
	  (incf (cdr (assoc i occ)))
	  (push (cons i 1) occ)))
    (sort occ #'> :key #'cdr)))

(defun pos+ (lst)
  (let ((acc 0)
	(output '()))
    (dolist (i lst)
      (push (+ i acc) output)
      (incf acc))
    (reverse output)))

(defun showdots (lst)
  (if (not (null lst))
      (progn
	(princ "(")
	(princ (car lst))
	(if (cdr lst)
	    (progn
	      (princ " . ")
	      (showdots (cdr lst)))
	    (princ " . NIL"))))
  (princ ")"))
	       
