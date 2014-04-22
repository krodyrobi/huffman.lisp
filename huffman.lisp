
;;; params
;;; 	filepath => path to the file that will be tokenized
;;; objective
;;;		tokenize a string and count occurences of all chars
;;; return
;;;		hash table key (char) => occurence
(defun parse-input (filepath)  ;rename to parse input
	(let ((in (open filepath :direction :input :if-does-not-exist nil))
		  (table (make-hash-table)))
		(do
			((c (read-char in nil) (read-char in nil)))
			((null c))
			(if (gethash c table)
					(setf (gethash c table) (+ 1 (gethash c table)))
					(setf (gethash c table) 1))
		)
		(close in)
		table
	)
)

;;; params
;;; 	first node
;;; 	second node
;;; objective
;;;		see if first node is smaller than the second node
;;; return
;;;		boolean
(defun node-smaler? (a b)
	(if (< (weight a) (weight b))
		t
		nil
	)
)


;;;;;;; HELPER FUNCTIONS FOR TREE MANUPULATION

;;; params
;;; 	hash table of occurence of chars
;;; objective
;;;		generate a list of leaf nodes
;;; return
;;;		list
(defun init-huff-list (table)
	(let ((lst '()))
		(MAPHASH #'(lambda (k v) (setq lst (cons (list v k) lst))) table)
		lst
	)
)

;;; params
;;; 	list of nodes
;;; objective
;;;		sort the list of nodes ascending
;;; return
;;;		sorted list

(defun my-node-sort (lst)
	(sort lst #'node-smaler?)
)

;;; params
;;;		node => a huffman tree node
;;; objective
;;;		see if node is actualy a leaf : leaves have only 2 elements a weight and a char
;;; return
;;;		boolean
(defun leaf? (node)
	(if	(= 2 (length node))
		t
		nil
	)
)


;;; params
;;;		node => a huffman tree node
;;; objective
;;;		get a nodes weight
;;; return
;;;		numeric weight
(defun weight (node)
	(car node)
)


;;; params
;;;		node => a huffman tree node
;;; objective
;;;		get right child of specified node
;;; return
;;;		node right child
(defun right-child (node)
	(caddr node)
)

;;; params
;;;		node => a huffman tree node
;;; objective
;;;		get left child of specified node
;;; return
;;;		node left child
(defun left-child (node)
	(cadr node)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; MAKE SURE WHEN uniting 2 trees if can't find 2 adequate trees use an empty leaf!!!


;;;;; TESTS for leaf weight and child functions
;(setq testtree '(9 (1 a) ()))
;(print (right-child testtree))
;(print (left-child testtree))

;(print (leaf? (left-child testtree)))
;(print (leaf? (right-child testtree)))

;(print (weight (right-child testtree)))
;(print (weight (left-child testtree)))
;(print (weight testtree))

;(setq testtree '(9 (1 a)))

;(print (right-child testtree))
;(print (leaf? (right-child testtree)))
;(print (weight (right-child testtree)))
;;;;; END test set



;;;;;; TEST node smaller, init function and huffman sort
;(print (node-smaler? '(1 a) '(2 b)))
;(print (node-smaler? '(10 a) '(2 b)))
;(print (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt")))
;(print (my-node-sort (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt"))))
;;;;;; END test set



;;;;;; TEST hash table creation
;(MAPHASH #'(lambda (k v) (print (list k v))) (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt"))
;;;;;; END test set
