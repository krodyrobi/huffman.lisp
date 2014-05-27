; prints the menu of the app
(defun menu-helper-msg ()
	(terpri)
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
    (loop for i = 0 then (+ 1 j)
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

;;; params
;;; 	string => command string
;;; return
;;; 	list => command arg list
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


;;; main method for UI and action selection
(defun main ()
	(do ((running t))

		((null running) t)
		(menu-helper-msg)

		(clear-input)
		(setq raw-command (read-line))
	
		(if (string= "" raw-command)
			(progn 
				(print "exiting...")
				(setq running nil))
			(progn
				(setq command (parse-command raw-command))
				(if (validate-command command)
					(if (string= (string-downcase (car command)) "c")
						(huffman-encode (cadr command) (concatenate 'string (cadr command) ".huff"))
						(huffman-decode (cadr command) (concatenate 'string (cadr command) ".orig"))))))))



;;; params
;;; 	filepath => path to the file that will be tokenized
;;; objective
;;;		tokenize a string and count occurences of all chars
;;; return
;;;		hash table key (char) => occurence
(defun parse-input (filepath)  ;rename to parse input
	(let ((in (open filepath :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8)))
		  (table (make-hash-table :test 'equal)))
		(do
			((c (read-byte in nil) (read-byte in nil)))
			((null c))
			(setq c (code-char c))
			(if (gethash c table)
					(setf (gethash c table) (+ 1 (gethash c table)))
					(setf (gethash c table) 1))
		)
		(close in)
		(setf (gethash "EOF" table) 1)
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
	(let ((table (make-hash-table :test 'equal)))
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

		
			(if (leaf? (left-child tree))
				(setf (gethash (cadr (left-child tree)) table) "0")
				(node-hasher (left-child tree) "0"))

			(if (leaf? (right-child tree))
				(setf (gethash (cadr (right-child tree)) table) "1")
				(node-hasher (right-child tree) "1"))
		)
			
	
		;; return the hash tabel
		table
	)
)


;;; params
;;; 	a huffman tree
;;; objective
;;;		generate a huff tree header for the given tree
;;; return
;;; 	string in binary form
(defun generate-huff-header (tree)
	(let ((output nil)
		  (leaf-count 0))

		;struc = tree
		(labels ((generator (struct)
			(if (leaf? struct)
				(progn 
					(setq leaf-count (+ 1 leaf-count))
					(setq output (concatenate 'string output "1"))
					(if (string= "EOF" (cadr struct))
						(setq output (concatenate 'string output (make-string 9 :initial-element #\1)))
						(setq output (concatenate 'string output "0" (number2bitstr (char-code (cadr struct)) 8)))))

				(progn
					(setq output (concatenate 'string output "0"))
					(generator (left-child struct))
					(generator (right-child struct))))))
		
			(generator tree))
		(setq output (concatenate 'string (number2bitstr (- leaf-count 1) 8) output))))

(defun number2bitstr (c size)
	(let ((result c))
		(setq result (format nil "~B" result))
		(setq result (concatenate 'string (make-string (- size (length result)) :initial-element #\0) result))))



;;; params
;;; 	file to be encoded
;;; 	output file path
;;; 	hash tabel with codes to be used when encoding
;;; objective
;;;		output the ecoded string to the file
;;; return
;;;		t
(defun huffman-encode (fin fout)
	(let ((in (open fin :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8)))
		  (out (open fout :direction :output :if-does-not-exist :create :element-type '(unsigned-byte 8)))
		  (tree '())
		  (tree-map nil)
		  (cur_byte 0)
		  (bit_pos 0))		

		(setq tree (huff-tree (init-huff-list (parse-input fin))))
		(setq tree-map (huff-code-hash tree))
		(setq header-str (generate-huff-header tree))

		(labels ((printer (str)
			(loop for c across str do 
				(setq bit_pos (+ 1 bit_pos))
				(if (eql c #\1)
					(setq cur_byte (+ 1 cur_byte)))

				(if (= bit_pos 8)
					(progn
						(write-byte cur_byte out)
						(setq bit_pos 0)
						(setq cur_byte 0)))

				(setq cur_byte (ash cur_byte 1)))))
			
			;; body of label


			;; queue header on stream
			(printer header-str)
			
			;; queue content on stream
			(do ((ch (read-byte in nil) (read-byte in nil))) ;ini
				((null ch)) ; until

				(printer (gethash (code-char ch) tree-map)))

			;;queue EOF on stream
			(printer (gethash "EOF" tree-map))

			;;dump remaining uncompleted byte
			(write-byte (ash cur_byte (- 8 (+ bit_pos 1))) out)
		)		
		
		(close in)
		(close out)
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
(defun huffman-decode (file-in file-out)
	(let* ((in (open file-in :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8) ))
		  (out (open file-out :direction :output :if-does-not-exist :create :element-type '(unsigned-byte 8) ))
		  (tree '())
		  (counter (+ 1 (read-byte in)))  ;; EOF is not counted
		  (cur_byte (read-byte in))
		  (cur_bit_value 0)
		  (bit_pos 0)
		  (charcode 0))

		;; read tree from header
		(labels ((generator ()
					(cond
						((= counter 0) nil)
						((= 0 cur_bit_value) 
							(progn
								(reader)
								(list 1 (generator) (generator))))
						((= 1 cur_bit_value)
							(progn
								(do ((steps 9 (- steps 1)))
									((= 0 steps) t)
									
									(reader)
									(if (= 1 cur_bit_value)
										(setq charcode (+ 1 charcode)))
									
									(if (not (= 1 steps))
										(setq charcode (ash charcode 1)))
								)

								(reader)
								(setq oc charcode)
								

								(setq charcode 0)
								(setq counter (- counter 1))

								(if (= 511 oc)
									(list 1 "EOF")
									(list 1 (code-char oc)))
							))
						(t nil)))
				 (reader ()
					
					(if (= bit_pos 8)
						(progn 
							(setq bit_pos 0)
							(setq cur_byte (read-byte in))))
					
					
					
					(if (= (logand cur_byte 128) 128)
						(setq cur_bit_value 1)
						(setq cur_bit_value 0))

					(setq bit_pos (+ 1 bit_pos))
					(setq cur_byte (ash cur_byte 1))
				))
			

 
			(reader)
			(setq tree (generator)) ;; tree is now formed ready for parsing of file..

			(do ((finished nil)
				 (cur_tree tree))
				
				(finished)

				(if (leaf? cur_tree)
					(if (not (stringp (cadr cur_tree)))
							(progn
								(write-byte (char-code (cadr cur_tree)) out)
								(setq cur_tree tree))
							(setq finished t))

					(progn
						(if (= 0 cur_bit_value)
							(setq cur_tree (left-child cur_tree))
							(setq cur_tree (right-child cur_tree)))

						(reader)))
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


(main)
;(print (validate-command '("c" "C:\\Users\\Robi\\Desktop\\test_huf_in.txt") ))
