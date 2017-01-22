(defun power-set (s)
  (reduce #'(lambda (item ps)
              (append (mapcar #'(lambda (e) (cons item e))
                              ps)
                      ps))
          s
          :from-end t
          :initial-value '(())))


(defun len-onesubsets (s)
	(setf s (remove-duplicates s :test #'equalp))
	(sort s '<)
	(remove-if-not
		(lambda (x) (= (list-length x) (- (list-length s) 1)))
		(power-set s)))

(defun len-smaller-subsets (s)
	(setf s (remove-duplicates s :test #'equalp))
	(sort s '<)
	(remove-if
		(lambda (x) (= (list-length x) (list-length s)))
		(power-set s)))


(defun apriori (data &key (min-sup 1/10) (min-conf 1/10))
	(setq all-data (list-length data))
	(setq lkunion '())
	(setq l (make-hash-table :test #'equalp))
	(dolist (d data)
		(dolist (i d)
			(if (gethash i l)
				(incf (gethash i l))
				(setf (gethash i l) 1))))

	;DEBUG
	(loop for key being the hash-keys in l using (hash-value value)
				do (format t "~{~D ~} : ~F~%" (if (listp key) key (list key)) value))

	(loop for key being the hash-keys in l using (hash-value value)
				do (if (< (/ value all-data) min-sup) (remhash key l) (setf value (setf (gethash key l) (/ value all-data)))))


	(do ((k 1 (incf k)))
		((equalp (loop for key being the hash-keys in l collect key) '()) (setf lkunion (reverse lkunion)))
		(let ((c (make-hash-table :test 'equalp)) (temp '()))
		 (push l lkunion)

		 ;Ck+1 = candidates generated from Lk
		 (if (equal k 1)
			 (progn
				 (loop for key being the hash-keys in l
							 do (setq temp (remove-duplicates (union (if (listp temp) temp (list temp)) (if (listp key) key (list key)) :test #'equalp))))
				 (loop for i on temp
							 do (let ((x (first i)) (sub (nthcdr 1 i)))
										(dolist (a sub)
											(setf (gethash (sort (union (if (listp x) x (list x)) (if (listp a) a (list a)) :test #'equalp) '<) c) 0)))))
			 (progn
				 (loop for key being the hash-keys in l
							 do (push (if (listp key) key (list key)) temp))

				 (loop for i on temp
							 do (let ((x (copy-tree (first i))) (sub (copy-tree (nthcdr 1 i))))
										(sort x '<)
										(dolist (a sub)
											(sort a '<)
											;Apriori Property (Self Join)
											(if (and (equalp (subseq x 0 (- k 1)) (subseq a 0 (- k 1))) (not (equalp x a)))
												;Pruning
												(if (subsetp (len-onesubsets (union x a :test #'equalp)) temp :test #'equalp)
													(setf (gethash (sort (union x a :test #'equalp) '<) c) 0))))))))


		 ;for each transaction t in database do
		 ;increment the count of all candidates in Ck+1 that are contained in t

		 (loop for key being the hash-keys in c using (hash-value value)
					 do (dolist (d data)
								(if (subsetp key d :test #'equalp) (setf (gethash key c) (incf value)))
								(format t "~{~D ~} subset of ~{~D ~}: ~A : ~F~%" (if (listp key) key (list key)) (if (listp d) d (list d)) (subsetp key d :test #'equalp) value)))  ;DEBUG

		 ;Lk+1 = candidates in Ck+1 with min_support

		 (setf deltemp '())
		 (loop for key being the hash-keys in c using (hash-value value)
					 do (if (< (/ value all-data) min-sup)
								(push key deltemp)
								(setf (gethash key c) (/ value all-data))))
		 (dolist (i deltemp)
			 (remhash i c))

		 (setq l c)))


	;CREATING ARs
	(setq ar-dic (make-hash-table :test #'equalp))
	(do ((k (1- (length lkunion)) (decf k)))
		((<= k 0) ar-dic)
		(let ((tmp (nth k lkunion)))
			(loop for key being the hash-keys in tmp using (hash-value value)
						do (progn
								 (setf pset (len-smaller-subsets key))
								 (setf temp '())
								 (dolist (l pset)
									 (if (> (length l) 0)
										 (let* ((lvalue (gethash (if (= (length l) 1) (nth 0 l) l) (nth (1- (length l)) lkunion))))
											 (if (>= (/ value lvalue) min-conf)
												 (progn
													 (setf temp '())
												 	 (push l temp)
												 	 (setf notl (sort (set-difference (copy-tree key) (copy-tree l) :test #'equal) '<))
												 	 (if (gethash (reverse (push notl temp)) ar-dic)
														 ()
														 (setf (gethash (reverse (push notl temp)) ar-dic) (list :confidence (/ value lvalue) :lift (/ value (* lvalue (gethash (if (= (length notl) 1) (nth 0 notl) notl) (nth (1- (length notl)) lkunion))))))))))))))))





	(list lkunion ar-dic))





(defun apriori-preprocess (data &key (min-sup 1/10) (min-conf 1/10))
	(coerce data 'list)
	(setq all-data (list-length data))
	(if (> min-sup 1)
		(setf min-sup (/ min-sup all-data)))

	(setq temp (make-hash-table :test #'equalp))

	(setf k 0)
	(dolist (d data)
		(dolist (i d)
			(if (gethash i temp)
				()
				(progn
					(setf (gethash i temp) k)
					(incf k)))))

	(defparameter *ref-map* (make-array (hash-table-count temp)))

	(setq new-data '())
	(dolist (d data)
		(push (loop for i in d collect (gethash i temp)) new-data))

	;(print new-data) ;DEBUG

	(loop for key being the hash-keys in temp using (hash-value value)
				do (setf (aref *ref-map* value) key))

	(apriori (reverse new-data) :min-sup min-sup :min-conf min-conf))



(defun main ((path "data.txt") &key (min-sup 1/10) (min-conf 1/10))
	(let ((read-data (open path)) (data '()))
		(setq tmp '())
		(do ((temp (read-line read-data nil) (read-line read-data nil)))
			((equalp temp nil) 't)
			(if (equalp temp "")
				(progn
					(push (reverse tmp) data)
					(setf tmp '()))
				(progn
					(push temp tmp))))

		(let ((write-data (open "result.txt" :direction :output :if-exists :supersede)))
		(setf result (apriori-preprocess (reverse data) :min-sup min-sup :min-conf min-conf))

		(setf lkunion (nth 0 result))


		(format write-data "~3%")
		(dolist (lk lkunion)
			(loop for key being the hash-keys in lk using (hash-value value)
						do (progn
								 (setf key (if (listp key) key (list key)))
								 (setq l (loop for i in key collect (aref *ref-map* i)))
								 (format write-data "~{~D ~}: ~{~A ~^, ~}: ~F~%" (if (listp key) key (list key)) (if (listp l) l (list l)) value))))
		(format write-data "~3%")

		(loop for key being the hash-keys in (nth 1 result) using (hash-value value)
					do (progn
							 (setf key1 (nth 0 key))
							 (setf key2 (nth 1 key))
							 (setq l1 (loop for i in key1 collect (aref *ref-map* i)))
							 (setq l2 (loop for i in key2 collect (aref *ref-map* i)))
							 (format write-data "~{~D ~} -> ~{~D ~}: ~{~A ~^, ~} ->  ~{~A ~^, ~} ( CONFIDENCE: ~F  LIFT: ~F )~%" (if (listp key1) key1 (list key1)) (if (listp key2) key2 (list key2)) (if (listp l1) l1 (list l1)) (if (listp l2) l2 (list l2)) (getf value :confidence) (getf value :lift))))
		(close write-data))


		(close read-data)))


(main "data.txt" :min-sup 5/10  :min-conf 5/10)

