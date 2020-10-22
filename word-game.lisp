;;;; A simple "hang-man"-type game.

;;;; By Dave Wilson, October 2020

(in-package :cl)

(defpackage #:word-game
  (:use #:cl)
  (:export #:game
	   #:*words-list*
	   #:*miniumum-word-length*
	   #:*maximum-word-length*
	   #:*number-of-lives*))

(in-package :word-game)

;;; quicklisp packages
(ql:quickload "alexandria")
(ql:quickload "split-sequence")
(ql:quickload "cl-ppcre")

;;; Global variables

; *words-list* should be a file containing one word per line
(defparameter *words-list* "/usr/share/dict/words")

; The minimum length of word we will play with
(defparameter *minimum-word-length* 4)

; The maximum length of word we will play with
(defparameter *maximum-word-length* 6)

; The number of chances the player has to guess the word
(defparameter *number-of-lives* 10)

; We don't want words that contain any of these characters
(defparameter *banned-characters* '(#\' #\é #\Å))

;;; Code starts here

(defun is-valid-guess (guess guesses)
  "Is the guess a letter and one that wasn't previously guessed"
  (let ((ptrn (ppcre:create-scanner "^[a-z]$")))
    (and (ppcre:scan ptrn guess)
	 (not (member (char guess 0) guesses)))))

(defun check-guess (guess chosen-word)
  "Does the guessed character appear in the word?"
  (let* ((ptrn (ppcre:create-scanner (string-trim '(#\r #\n) guess))))
    (ppcre:scan ptrn chosen-word)))

(defun interpolate-char (char string index)
  "Substitute the character at index in string with char"
  (let* ((first-part (if (> index 0)
			 (subseq string 0 index)
			 ""))
	 (second-part (if (= index (- (length string) 1))
			     ""
			     (subseq string (+ index 1)))))
    (concatenate 'string first-part (string char) second-part)))

(defun update-word-so-far (c guess chosen-word word-so-far)
  "Place instances of guess into word-so-far in the correct places"
  (if (eq c (length chosen-word))
      word-so-far
      (let ((guess-char (char guess 0))
	    (word-char (char chosen-word c))
	    (word-so-far-char (char word-so-far c)))
	(if (and (equal #\* word-so-far-char)
		 (equal guess-char word-char))
	    (update-word-so-far (+ c 1)
	      guess
	      chosen-word
	      (interpolate-char guess-char word-so-far c))
	    (update-word-so-far (+ c 1)
	      guess
	      chosen-word
	      word-so-far)))))

(defun contains-banned-characters? (word bc)  
  "Does the word contain a banned chracter from list bc"
  (if (eq bc '())
      NIL
      (if (find (car bc) word)
	  T
	  (contains-banned-characters? word (cdr bc)))))

(defun is-unsuitable-word? (word)
  "A word is unsuitable or empty"
  (or (equal "" word)
      (contains-banned-characters? word *banned-characters*)))

(defun is-suitable-word? (word)
  "A word is suitable if it's not unsuitable and falls within length range"
  (and (<= (length word) *maximum-word-length*)
       (>= (length word) *minimum-word-length*)
       (not (is-unsuitable-word? word))))

(defun read-words ()
  "Read al words from *words-list* and then remove unsuitable ones."
  (let* ((words (alexandria:read-file-into-string *words-list*))
	 (word-list (split-sequence:split-sequence #\Newline words)))
    (remove-if-not (lambda (x) (is-suitable-word? x)) word-list)))

(defun get-word ()
  "Select a word at random from words-list and downcase it"
  (let* ((words (read-words))
	 (r (random (length words) (make-random-state t))))
    (string-downcase (first (subseq words r (+ r 1))))))

(defun play-round (lives-remaining chosen-word word-so-far guesses)
  "Play the game"
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
				  (update-word-so-far 0
						      guess
						      chosen-word
						      word-so-far)
				  guesses)
		      (play-round (- lives-remaining 1)
				  chosen-word
				  word-so-far
				  (cons (char guess 0) guesses)))))))))

(defun game ()
  "Choose a word and start the game"
  (let* ((chosen-word (get-word))
	 (obscured-word (make-string
			 (length chosen-word)
			 :initial-element #\*)))
    (play-round *number-of-lives* chosen-word obscured-word '())))
			   
