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

(define-binary-type unsigned-bytes (bytes)
  (:reader (in)
		   (loop with array = (make-array bytes :element-type '(unsigned-byte 8))
				 for i from 0 to (1- bytes) do
				   (setf (aref array i) (read-byte in))
				 finally (return array)))
  (:writer (out value)
		   (loop for byte across value do
				 (write-byte byte out))))

(define-binary-type signed-2bytes (size)
  (:reader (in)
		   (loop with array = (make-array size :element-type '(signed-byte 16))
				 for i from 0 to (1- size)
				 for byte1 = (read-byte in)
				 for byte2 = (read-byte in)
				 for int = 0 do
				   (setf (ldb (byte 8 8) int) byte1
						 (ldb (byte 8 0) int) byte2)
				   (if (= (ldb (byte 1 7) byte1) 1)
				   	   (setf int (- int (expt 2 16))))
				   (setf (aref array i) int)
				 finally (return array)))
  (:writer (out value)
		   (loop for byte across value do
			 (write-byte byte out))))

(define-tagged-binary-class
	wav  ()
	((riff-identifier (iso-8859-1-string :length 4))
	 (size (ub4)) ;; file-size - 4
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
	 (data-size (ub4)) ;;file-size - 44
	 (data (signed-2bytes :size (/ data-size 2)))))

(defun tt()
  (with-open-file (stream *file* :element-type 'unsigned-byte)
	 (read-value 'wav stream)))

	
