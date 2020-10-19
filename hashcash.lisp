(ql:quickload "uuid")
(ql:quickload "base64")
(ql:quickload "sha1")
(ql:quickload "local-time")

(defvar hashcash-version 1)
(defparameter partial-pre-image-bits 20)
(defun generate-hashcash (resource-string)
  (let* ((the-uuid (uuid:make-v4-uuid))
	(random-data (base64:base64-encode (write-to-string the-uuid))))
    (try (random
	  (expt 2 160)
	  (make-random-state))
	 random-data
	 resource-string)))

(defun try (counter random-data resource-string)
  (let* ((c (base64:base64-encode (write-to-string counter)))
	 (header (concatenate 'string
			      (write-to-string hashcash-version) ":"
			      (write-to-string partial-pre-image-bits) ":"
			      (get-datestring) ":"
			      resource-string "::"
			      random-data ":"
			      c)))
    (if (verify-hashcash header)
	header
	(try (+ counter 1) random-data resource-string))))
			      
(defun get-datestring ()
  (local-time:format-timestring nil
				(local-time:now)
				:format '(:year
					  (:month 2)
					  (:day 2)
					  (:hour 2)
					  (:min 2))))

(defun verify-hashcash (header-string)
  (let* ((hashed (sha1:sha1-hex header-string))
	 (partial-pre-image-bytes (/ partial-pre-image-bits 4))
	 (zero-string (make-string partial-pre-image-bytes :initial-element #\0))
	 (check-string (subseq hashed 0 partial-pre-image-bytes)))
    (string= zero-string check-string)))
