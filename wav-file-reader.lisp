;;;; wav-file-reader.lisp

(in-package #:wav-file-reader)

(defparameter *file* #p"E:/Development/samples/sample.wav")


(define-binary-type unsigned-integer-bigendian (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type ub1 () (unsigned-integer-bigendian :bytes 1 :bits-per-byte 8))
(define-binary-type ub2 () (unsigned-integer-bigendian :bytes 2 :bits-per-byte 8))
(define-binary-type ub3 () (unsigned-integer-bigendian :bytes 3 :bits-per-byte 8))
(define-binary-type ub4 () (unsigned-integer-bigendian :bytes 4 :bits-per-byte 8))


(define-tagged-binary-class
	wav  ()
	((riff-identifier (iso-8859-1-string :length 4))
	 (size (ub4))
	 (wave-identifier (iso-8859-1-string :length 4)))
	(:dispatch (if (and (string-equal riff-identifier "RIFF")
						(string-equal wave-identifier "WAVE"))
				   'wav-1
				   (error "File is not a WAVE file"))))



(define-binary-class
	wav-1 (wav)
	((format-chunk (iso-8859-1-string :length 4 ))
	 (format-data-length (ub4))
	 (type-of-format (ub2))
	 (number-of-channel (ub2))
	 (sample-rate (ub4))
	 (bytes (ub4))
	 (bytes-per-sample*channels (ub2))
	 (bits-per-sample (ub2))
	 (data-chunk-identifier (iso-8859-1-string :length 4))
	 (file-size (ub4))))
					 

(defun tt()
  (with-open-file (stream *file* :element-type 'unsigned-byte)
	(read-value 'wav stream )))
	
