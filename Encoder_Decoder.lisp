;;;;;;;;;;;;;Oğuzhan ŞENTÜRK;;;;;;;;;;;;;
;;;;;;;;;;;osenturk@gtu.edu.tr;;;;;;;;;;;

(defun read-as-list (filename)
	; This Function read dictionary
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	; Implement this function...
	(setq paragraph_list nil)
	(setq word_list nil)
	(let ((input(open filename :if-does-not-exist nil))) ;open file and file does not exist return nil
		(when input (loop for ch = (read-char input nil) while ch do ;read char until nil
			(if (equal ch #\Newline) ;newline check
				(progn
				(nreverse word_list)
				(push word_list paragraph_list)
				(setq word_list nil))
				(progn ; else
				(setq ch (character-to-symbol ch)) ; char to symbol
				(push ch word_list)))) 
		(close input)))
	(nreverse word_list) ; last word
	(push word_list paragraph_list)
	(nreverse paragraph_list)
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defun read-document (filename)
	; This function read document
	(setq document_list nil)
	(setq paragraph_list nil)
	(setq word_list nil)
	(let ((input(open filename :if-does-not-exist nil))) ;open file and file does not exist return nil
		(when input (loop for ch = (read-char input nil) while ch do ;read char until nil
			(cond 
				((equal ch #\Newline) ;newline check
					(nreverse word_list)
					(push word_list paragraph_list)
					(setq word_list nil)
					(nreverse paragraph_list)
					(push paragraph_list document_list)
					(setq paragraph_list nil))
				((equal ch #\Space) ;space check
					(nreverse word_list)
					(push word_list paragraph_list)
					(setq word_list nil))
				(
					(setq ch (character-to-symbol ch)) ; char to symbol
					(push ch word_list))))
		(close input)))
	(nreverse word_list) ;last word
	(push word_list paragraph_list)
	(setq word_list nil)
	(nreverse paragraph_list)
	(push paragraph_list document_list)
	(nreverse document_list)
)

(defun encoder-paragraph (paragraph)
	;This function encode paragraph
	(setq paragraph_list nil)
	(loop for x in paragraph do ;loop for paragraph's element
		(setq text (list-to-string x)) ;list to string '(h e l l o) -> "h e l l o". read-char needs string
		(setq word_list nil)
		(with-input-from-string (is text)
		    (do ((c (read-char is) (read-char is nil 'the-end))) ; read char until end of string
		        ((not (characterp c)))
		     	(case c
		     		(#\A ( push 'c word_list))  
		     		(#\B ( push 'd word_list))
		     		(#\C ( push 'b word_list))
		     		(#\D ( push 'a word_list))
		     		(#\E ( push 'e word_list))
		     		(#\F ( push 'f word_list))
		     		(#\G ( push 'g word_list))
		     		(#\H ( push 'h word_list))
		     		(#\I ( push 'i word_list))
		     		(#\J ( push 'j word_list))
		     		(#\K ( push 'k word_list))
		     		(#\L ( push 'l word_list))
		     		(#\M ( push 'm word_list))
		     		(#\N ( push 'n word_list))
		     		(#\O ( push 'o word_list))
		     		(#\P ( push 'p word_list))
		     		(#\Q ( push 'q word_list))
		     		(#\R ( push 'r word_list))
		     		(#\S ( push 's word_list))
		     		(#\T ( push 't word_list))
		     		(#\U ( push 'u word_list))
		     		(#\V ( push 'v word_list))
		     		(#\W ( push 'w word_list))
		     		(#\X ( push 'x word_list))
		     		(#\Y ( push 'y word_list))
		     		(#\Z ( push 'z word_list)))))
		(nreverse word_list)
		(push word_list paragraph_list))
	(nreverse paragraph_list)
)

(defun encoder-document (document)
	(setq document_list nil)
	(loop for x in document do ;loop for item of document
		(setq paragraph_list nil)
		(loop for y in x do ;loop for item of paragraph
			(setq text (list-to-string y))  ;list to string '(h e l l o) -> "h e l l o". read-char needs string
			(setq word_list nil)
			(with-input-from-string (is text)
			    (do ((c (read-char is) (read-char is nil 'the-end))) ; read char until end of string
			        ((not (characterp c)))
			        (case c
			     		(#\A ( push 'c word_list))
			     		(#\B ( push 'd word_list))
			     		(#\C ( push 'b word_list))
			     		(#\D ( push 'a word_list))
			     		(#\E ( push 'e word_list))
			     		(#\F ( push 'f word_list))
			     		(#\G ( push 'g word_list))
			     		(#\H ( push 'h word_list))
			     		(#\I ( push 'i word_list))
			     		(#\J ( push 'j word_list))
			     		(#\K ( push 'k word_list))
			     		(#\L ( push 'l word_list))
			     		(#\M ( push 'm word_list))
			     		(#\N ( push 'n word_list))
			     		(#\O ( push 'o word_list))
			     		(#\P ( push 'p word_list))
			     		(#\Q ( push 'q word_list))
			     		(#\R ( push 'r word_list))
			     		(#\S ( push 's word_list))
			     		(#\T ( push 't word_list))
			     		(#\U ( push 'u word_list))
			     		(#\V ( push 'v word_list))
			     		(#\W ( push 'w word_list))
			     		(#\X ( push 'x word_list))
			     		(#\Y ( push 'y word_list))
			     		(#\Z ( push 'z word_list)))))
			(nreverse word_list)
			(push word_list paragraph_list))
		(nreverse paragraph_list)
		(push paragraph_list document_list))
	(nreverse document_list)
)

(defun character-to-symbol (ch) ; #\a -> a
	(case ch
		(#\a (return-from character-to-symbol 'a ))
		(#\b (return-from character-to-symbol 'b ))
 		(#\c (return-from character-to-symbol 'c ))
 		(#\d (return-from character-to-symbol 'd ))
 		(#\e (return-from character-to-symbol 'e ))
 		(#\f (return-from character-to-symbol 'f ))
 		(#\g (return-from character-to-symbol 'g ))
 		(#\h (return-from character-to-symbol 'h ))
 		(#\i (return-from character-to-symbol 'i ))
 		(#\j (return-from character-to-symbol 'j ))
 		(#\k (return-from character-to-symbol 'k ))
 		(#\l (return-from character-to-symbol 'l ))
 		(#\m (return-from character-to-symbol 'm ))
 		(#\n (return-from character-to-symbol 'n ))
 		(#\o (return-from character-to-symbol 'o ))
 		(#\p (return-from character-to-symbol 'p ))
 		(#\q (return-from character-to-symbol 'q ))
 		(#\r (return-from character-to-symbol 'r ))
 		(#\s (return-from character-to-symbol 's ))
 		(#\t (return-from character-to-symbol 't ))
 		(#\u (return-from character-to-symbol 'u ))
 		(#\v (return-from character-to-symbol 'v ))
 		(#\w (return-from character-to-symbol 'w ))
 		(#\x (return-from character-to-symbol 'x ))
 		(#\y (return-from character-to-symbol 'y ))
 		(#\z (return-from character-to-symbol 'z )))
)

(defun list-to-string (lst) ;'(h e l l o) -> "h e l l o".
    (format nil "~{~A~}" lst)
)

(defun spell-checker-0 (word filename)
	;you should implement this function
	(setq text (read-as-list filename)) ;dictionary list
 	(loop for x in text do
	    (when (equal x word)
			(return-from spell-checker-0 T))) ;if word occurs in dictionary return T
 	(return-from spell-checker-0 nil) ;if word not occurs in dictionary return nil
)

(defun spell-checker-1 (word filename)
 	;you should implement this function
 	(setq emptlist (make-hash-table)) ;empty hash table
 	(setq text (read-as-list filename)) ;dictionary list
 	(loop for x in text do
 		(setf (gethash (sxhash x) emptlist) x)) ;make unique hash each item
 	(when (gethash (sxhash word) emptlist)
 		(return-from spell-checker-1 T)) ;if hashes doesnt match return nil
 		(return-from spell-checker-1 nil) ;if hashes match return T
)

(defun permutations (lst paragraph) ;permutations function
    (let* ((full_list (cons 'head (copy-list lst)))       ;sequential binding    
        (perm (make-array (length lst))))
     	(labels ((fo (p i &aux (q (cdr p))) 			;local function fo
                (cond
                  	((null (cdr q))   
                    	(setf (svref perm i) (car q))  
                    	(setf text (generate-cipher (coerce perm 'list) paragraph)) ;go to generate-cipher
                    	(if (not(equalp text nil)) ; nil means generate new perm
                     	(return-from permutations text))) ; return decoded text
                  	((loop while q do 
                        (setf (svref perm i) (car q))  
                        (rplacd p (cdr q))   
                        (fo full_list (1+ i))          
                        (rplacd p q)              
                        (pop p)  
                        (pop q))))))              
        (fo full_list 0))) 	;call local function fo
)

(defun generate-cipher(key paragraph)
 	(setq cipher_alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
 	(setq element_count (length key))
	(dotimes (y element_count) ; 0 -> element_count
		(setf (nth y cipher_alphabet) (nth y key)))
	(setq decoded_text (control-cipher paragraph cipher_alphabet))
	(if (equalp decoded_text nil) ; decoding is false return nil
		(return-from generate-cipher nil) 
		(return-from generate-cipher decoded_text)) ;decoding is true return list
)

(defun control-cipher (paragraph cipher_alphabet)
	(setq plain_alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(setq paragraph2 nil)
	(loop for word in paragraph do
	(setq word2 nil)
		(loop for ch in word do
			(setq flag 0)
			(setq index 0)
			(loop while (>= flag 0) do
				 (if (equalp ch (nth index cipher_alphabet)) ;for find index
					(setq flag -1) ; terminate loop
					(incf index))) ; index ++
			(push (nth index plain_alphabet) word2)) ;decoding
			(nreverse word2)
			(if (equalp T (spell-checker-1 word2 "dictionary1.txt")) ; if decoded word occurs dictionary push list
				(push word2 paragraph2)
				(return-from control-cipher nil))) ;if any decoded word doesnt occur dictionary return nil
	(nreverse paragraph2) ; if nil doesnt return , decoding succeed and return list
)

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph)
	;you should implement this function
	(permutations '(a b c d) paragraph) ; i change first four item for testing
)

(defun Code-Breaker (document)
  	;you should implement this function
  	(setq document_list nil)
  	(loop for x in document do
  			(setq paragraph (Gen-Decoder-A x))
  			(push paragraph document_list))
  	(nreverse document_list)
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Reading ....")
	(print "....................................................")
	(terpri)
	(setq doc (read-document "document2.txt"))
	(print doc)
	(terpri)
	(print "....................................................")
	(print "Encoding ....")
	(print "....................................................")
	(terpri)
	(setq en_doc(encoder-document doc))
	(print en_doc)
	(terpri)
	(print "....................................................")
	(print "Decoding ....")
	(print "....................................................")
	(terpri)
	(setq decoded (Code-Breaker en_doc))
	(print decoded)
)

;; test code...
(test_on_test_data)
