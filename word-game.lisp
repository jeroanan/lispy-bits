(ql:quickload "alexandria")
(ql:quickload "split-sequence")
(ql:quickload "cl-ppcre")

(defparameter *words-list* "/usr/share/dict/words")
(defparameter *minimum-word-length* 4)
(defparameter *maximum-word-length* 6)
(defparameter *number-of-lives* 10)

(defparameter *banned-characters* '(#\' #\é #\Å))

(defun game ()
  (let* ((chosen-word (get-word))
	 (obscured-word (make-string
			 (length chosen-word)
			 :initial-element #\*)))
    (play-round *number-of-lives* chosen-word obscured-word '())))
 
(defun play-round (lives-remaining chosen-word word-so-far guesses)
  (if (= lives-remaining 0)
      (format t "~%Game Over. The words was ~A.~%" chosen-word)
      (if (equal chosen-word word-so-far)
	  (format t "~%*** You win! ***~%")
	  (progn
	    (format t
		    "~%~A GUESSES: ~A LIVES: ~A~%"
		    word-so-far
		    guesses
		    lives-remaining)
	    (format t "Guess: ")
	    (let ((guess (string-downcase (write-to-string (read)))))
	      (if (not (is-valid-guess guess guesses))
		  (play-round lives-remaining chosen-word word-so-far guesses)
		  (if (check-guess guess chosen-word)
		      (play-round lives-remaining
				  chosen-word
				  (update-word-so-far guess
						      chosen-word
						      word-so-far)
				  guesses)
		      (play-round (- lives-remaining 1)
				  chosen-word
				  word-so-far
				  (cons (char guess 0) guesses)))))))))

(defun is-valid-guess (guess guesses)
  (let ((ptrn (ppcre:create-scanner "^[a-z]$")))
    (and (ppcre:scan ptrn guess)
	 (not (member (char guess 0) guesses)))))

(defun check-guess (guess chosen-word)
  (let* ((ptrn (ppcre:create-scanner (string-trim '(#\r #\n) guess))))
    (ppcre:scan ptrn chosen-word)))

(defun update-word-so-far (guess chosen-word word-so-far)
  (let ((guess-char (char guess 0)))
    (defun do-it (c word-so-far)
      (if (eq c (length chosen-word))
	      word-so-far
	      (let ((word-char (char chosen-word c))
		    (word-so-far-char (char word-so-far c)))
		(if (and (equal #\* word-so-far-char)
			 (equal guess-char word-char))
		    (do-it (+ c 1) (interpolate-char guess-char word-so-far c))
		    (do-it (+ c 1) word-so-far))))))
    (do-it 0 word-so-far))

(defun interpolate-char (char string index)
  (let* ((first-part (if (> index 0)
			 (subseq string 0 index)
			 ""))
	 (second-part (if (= index (- (length string) 1))
			     ""
			     (subseq string (+ index 1)))))
    (concatenate 'string first-part (string char) second-part)))
	
(defun get-word ()
  (let* ((words (read-words))
	 (r (random (length words) (make-random-state t))))
    (first (subseq words r (+ r 1)))))

(defun read-words ()
  (let* ((words (alexandria:read-file-into-string *words-list*))
	 (word-list (split-sequence:split-sequence #\Newline words)))
    (remove-if-not (lambda (x) (is-suitable-word? x)) word-list)))

(defun is-suitable-word? (word)
  (and (<= (length word) *maximum-word-length*)
       (>= (length word) *minimum-word-length*)
       (not (is-unsuitable-word? word))))

(defun is-unsuitable-word? (word)
  (or (equal "" word)
      (contains-banned-characters? word *banned-characters*)))

(defun contains-banned-characters? (word bc)  
  (if (eq bc '())
      NIL
      (if (find (car bc) word)
	  T
	  (contains-banned-characters? word (cdr bc)))))

			   
