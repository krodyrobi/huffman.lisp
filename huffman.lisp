; prints the menu of the app
(defun menu-helper-msg ()
	(princ "Commands:")
	(princ "-------------------------------")
	(terpri)
	(princ "c,C filename      -> compress")
	(terpri)
	(princ "d,D filename.huff -> decompress")
	(terpri)
)


;;; params
;;; 	string => the string that will be tokenized
;;; objective
;;;		tokenize a string using the space as a delimiter  OBS: multiple spaces will return empty strings
;;; return
;;; 	list of tokens
(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


;;; params
;;; 	string    => the string that will be checked
;;; 	extension => the extension that should be the finishing substring of the given string
;;; return
;;; 	boolean
(defun check-extension (string ext)
	(if (> (length ext) (length string))
		nil

		(if (string= (subseq string (- (length string) (length ext)) (length string)) ext)
			t
			nil
		)
	)
)

(defun parse-command (string)
	(remove-if #'(lambda (x) 
				(string= "" x))
	(split-by-one-space string))
)


;;; params
;;; 	list      => command , file
;;; objective
;;;		See if the given command is a valid choice
;;; return
;;; 	boolean
(defun validate-command (lst)
	(if (= (length lst) 2)
		(if (string= (string-downcase (car lst)) "c")
			(if (check-extension (cadr lst) ".huff")
				(progn 
					(print "Can't compress a .huff file already compressed")
					(terpri)
					nil
				)
				t
			)

			(if (string= (string-downcase (car lst)) "d")
				(if (check-extension (cadr lst) ".huff")
					t
					(progn 
						(print "Can't decompress a file that is not .huff")	
						(terpri)
						nil
					)
				)
			)
		)
		(progn 
			(print "Command not valid ex: c C://test.txt")
			(terpri)
			nil
		)
	)
)


(defun main ()
	(do ((running t))

		((null running) t)
		(menu-helper-msg)

		(progn
			(clear-input)
			(setq raw-command (read-line))
		)

		(if (string= "" raw-command)
			(progn 
				(print "exiting...")
				(setq running nil)
			)
			(progn
				(setq command (parse-command raw-command))
				(if (validate-command command)
					(if (string= (string-downcase (car command)) "c")
						(compress (cadr command))
						(decompress (cadr command))
					)
				)
			)
		)
	)
)


;(main)





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
		(setf (gethash 'EOF table) 1)
		table
	)
)

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
;;; 	list of leaf nodes
;;; objective
;;;		generate a huff tree
;;; return
;;;		huff tree
(defun huff-tree (lst)
	(labels ((huff-tree-label (ls)
		(do ((tree ls)
			(left nil)
			(right nil))

			((<= (length tree) 1) (car tree) )

			(setq tree (my-node-sort tree))
			(setq left (car tree))
			(setq right (cadr tree))
			(setq tree (cons (node-merge left right) (cddr tree)))
		)))

		(if (not (null lst))
			(if (= (length lst) 1)
				(node-merge (car lst) '())
				(huff-tree-label lst)
			)
			nil
		)
	)
)


;;; params
;;; 	a huffman tree
;;; objective
;;;		generate a huff tree hash table
;;; return
;;; 	table key => char value => code
(defun huff-code-hash (tree)
	(let ((table (make-hash-table)))
		
		;; local function node-haser
		;; it parses each child and determines if it is a leaf constructing its binary code in the process
		(labels ((node-hasher (node prev-code)
			(let ((left (left-child node))
				  (right (right-child node)))

				(if left
					(if (leaf? left)
						(setf (gethash (cadr left) table) (concatenate 'string prev-code "0"))
						(node-hasher left (concatenate 'string prev-code "0"))))

				(if right
					(if (leaf? right)
						(setf (gethash (cadr right) table) (concatenate 'string prev-code "1"))
						(node-hasher right (concatenate 'string prev-code "1"))))
			)))

			;; body of label
			;; initial check if tree exists and or if it is made out of only a leaf
			(if tree
				(if (leaf? tree)
					(setf (gethash (cadr tree) table) "0")
					(progn
						(if (leaf? (left-child tree))
							(setf (gethash (cadr (left-child tree)) table) "0")
							(node-hasher (left-child tree) "0"))

						(if (leaf? (right-child tree))
							(setf (gethash (cadr (right-child tree)) table) "1")
							(node-hasher (right-child tree) "1"))
					)
				)
			)
	
			;; return the hash tabel
			table
		)
	)
)


;;; params
;;; 	file to be encoded
;;; 	output file path
;;; 	hash tabel with codes to be used when encoding
;;; objective
;;;		output the ecoded string to the file
;;; return
;;;		t
(defun huffman-encode (fin fout table)
	(let ((in (open fin :direction :input :if-does-not-exist nil))
		  (out (open fout :direction :output :if-exists :overwrite :if-does-not-exist :create)))

		(do*
			((c (read-char in nil) (read-char in nil))
			 (code (gethash c table) (gethash c table)))

			((null c))

			(if code
				;(map nil #'(lambda (x)
				;	(print x)
				;	(if (eq x "0") (write-byte #*1 out) (write-byte #*0 out)) code))
				;(write-byte code out)
				(write-string code out)
				;	(write-char (gethash c table) out)
				
				;;;;; PRINT BIT BY BIT?
			)
		)
		(close in)
		(close out)
		t
	)
)


;;; params
;;; 	file to be decoded
;;; 	output file path
;;; 	huffman tree
;;; objective
;;;		output the decoded string to the file
;;; return
;;;		t
(defun huffman-decode (file-in file-out tree)
	(let ((in (open file-in :direction :input :if-does-not-exist nil))
		  (out (open file-out :direction :output :if-exists :overwrite :if-does-not-exist :create)))
		
		(do
			((c (read-char in nil) (read-char in nil))
			(node tree))
			
			((null c) t)

			
			(if (string-equal c "0")
				(setq node (left-child node))
				(setq node (right-child node)))

			(if (leaf? node)
				(progn
					(write-char (cadr node) out)
					(setq node tree)
				)
			)
		)
		
		(close in)
		(close out)
	)
)



;;;;;;; HELPER FUNCTIONS FOR TREE MANIPULATION

;;; params
;;; 	first node
;;; 	second node
;;; objective
;;;		create a new node with total weight of both of its children
;;; return
;;;		merged node
(defun node-merge (left right)
	(list (+ (weight left) (weight right)) left right)
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
	(if (car node)
		(car node)
		0
	)
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



;;;;;; TEST node smaller, init function ,huffman sort ,huffman tree generation
;(print (node-smaler? '(1 a) '(2 b)))
;(print (node-smaler? '(10 a) '(2 b)))
;(print (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt")))
;(print (my-node-sort (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt"))))
;(print (huff-tree (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt"))))
;;;;;; END test set



;;;;;; TEST hash table creation

	
;(huffman-encode "C:\\Users\\Robi\\Desktop\\test_huf_in.txt" "C:\\Users\\Robi\\Desktop\\test_huf_o.txt" (huff-code-hash (huff-tree (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt")))))
;(huffman-decode "C:\\Users\\Robi\\Desktop\\test_huf_o.txt" "C:\\Users\\Robi\\Desktop\\test_huf_o_dec.txt" (huff-tree (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt"))))
;(MAPHASH #'(lambda (k v) (print (list k v))) (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt"))
;(MAPHASH #'(lambda (k v) (print (list k v))) (huff-code-hash (huff-tree (init-huff-list (parse-input "C:\\Users\\Robi\\Desktop\\test_huf_in.txt")))))
;;;;;; END test set
