(ql:quickload "uuid")
(ql:quickload "base64")
(ql:quickload "sha1")
(ql:quickload "local-time")

;; The hashcash version to use. Only 1 is supported.
(defvar *hashcash-version* 1)

;; The number of bits at the beginning of a candidate hashcas that must
;; be zero for it to be a successful hashcash.
(defparameter *partial-pre-image-bits* 20)

(defun verify-hashcash (header-string)
  "Take a header string and verify whether it is a valid hashcash"
  (let* ((hashed (sha1:sha1-hex header-string))
	 (partial-pre-image-bytes (/ *partial-pre-image-bits* 4))
	 (zero-string (make-string partial-pre-image-bytes :initial-element #\0))
	 (check-string (subseq hashed 0 partial-pre-image-bytes)))
    (string= zero-string check-string)))

(defun get-datestring ()
  "Get the current date and time in hashcash format"
  (local-time:format-timestring nil
				(local-time:now)
				:format '(:year
					  (:month 2)
					  (:day 2)
					  (:hour 2)
					  (:min 2))))

(defun try (counter random-data resource-string)
  "Generate a potential hashcash based on a counter and random string. If
it verifies as valid hashcash then return it."
  (let* ((c (base64:base64-encode (write-to-string counter)))
	 (header (concatenate 'string
			      (write-to-string *hashcash-version*) ":"
			      (write-to-string *partial-pre-image-bits*) ":"
			      (get-datestring) ":"
			      resource-string "::"
			      random-data ":"
			      c)))
    (if (verify-hashcash header)
	header
	(try (+ counter 1) random-data resource-string))))

(defun generate-hashcash (resource-string)
  "Generate a hashcash"
  (let* ((the-uuid (uuid:make-v4-uuid))
	(random-data (base64:base64-encode (write-to-string the-uuid))))
    (try (random
	  (expt 2 160)
	  (make-random-state))
	 random-data
	 resource-string)))
