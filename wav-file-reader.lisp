;;;; wav-file-reader.lisp

(in-package #:wav-file-reader)

(defparameter *file* #p"E:/Development/samples/sample.wav")


;;
;; WAV File Reader 
;;

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

(define-binary-type signed-integer-1byte ()
  (:reader (in)
		   (let ((byte (read-byte in)))
			 (if (= (ldb (byte 1 7) byte) 1)
				 (setf byte (- byte (expt 2 8))))
			 byte))
  (:writer (out value)
		   (declare (ignore out value))
		   (error "Writing bytes not implemented")))

(define-binary-type signed-integer-2bytes ()
  (:reader (in)
		   (let ((byte2 (read-byte in))
				 (byte1  (read-byte in))
				 (int 0))
			 (setf (ldb (byte 8 8) int) byte1
				   (ldb (byte 8 0) int) byte2)
			 (if (= (ldb (byte 1 7) byte1) 1)
				 (setf int (- int (expt 2 16))))
			 int))
  (:writer (out value)
		   (declare (ignore out value))
		   (error "Writing bytes not implemented")))

(define-binary-type 2channels-8bit-data (size)
  (:reader (in)
		   (let ((data (make-array (list 2 size) :element-type '(unsigned-byte 8))))
			 (loop for i from 0 to (1- size) do 
			   (setf (aref data 0 i) (read-byte in))
			   (setf (aref data 1 i) (read-byte in)))
			 data))
  (:writer (out value)
		   (loop for i from 0 to (1- size) do 
			 (loop for c from 0 to 1 do
			   (write-byte (aref value c i) out)))))

(define-binary-type multichannel-data (channels size reader-writer-type array-element-type)
  (:reader (in)
		   (if (and (= channels 2)
					(equal array-element-type '(unsigned-byte 8)))
			   (read-value '2channels-8bit-data in :size size)
			   (let ((data (make-array (list channels size) :element-type array-element-type)))
				 (loop for i from 0 to (1- size) do 
				   (loop for c from 0 to (1- channels) do
					 (setf (aref data c i) (read-value reader-writer-type in))))
				 data)))
  (:writer (out value)
		   (loop for i from 0 to (1- size) do 
			 (loop for c from 0 to (1- channels) do
			   (write-value reader-writer-type out (aref value c i))))))


(define-tagged-binary-class
	riff  ()
	((riff-identifier (iso-8859-1-string :length 4))
	 (size (ub4)) 
	 (wave-identifier (iso-8859-1-string :length 4)))
	(:dispatch (if (and (string-equal riff-identifier "RIFF")
						(string-equal wave-identifier "WAVE"))
				   'wav
				   (error "File is not a WAVE file"))))


(define-binary-class
	wav (riff)
	((format-chunk (iso-8859-1-string :length 4 ))
	 (format-data-length (ub4))
	 (type-of-format (ub2))
	 (channels (ub2))
	 (sample-rate (ub4))
	 (bytes (ub4))
	 (bytes-per-sample*channels (ub2))
	 (bits-per-sample (ub2))
	 (data-chunk-identifier (iso-8859-1-string :length 4))
	 (data-size (ub4)) ;;file-size - 44
	 (data (multichannel-data :channels channels
							  :size (/ data-size (/ bits-per-sample 8) channels)
							  :reader-writer-type (ecase bits-per-sample
													(16 'signed-integer-2bytes)
													(8 'ub1))
							  :array-element-type (ecase bits-per-sample
													(16 '(signed-byte 16))
													(8 '(unsigned-byte 8)))))))

(defparameter *data* nil)
(defun read-wav()
  (with-open-file (stream *file* :element-type 'unsigned-byte)
	(let ((v (read-value 'riff stream)))
	  (setf *data* (slot-value v 'data))
	  v)))

(defun select-channel (stereo-data channel)
  (destructuring-bind (channels samples) (array-dimensions stereo-data)
	(if (< channels channel)
		(error "Channel ~a doesn't exist in audio data" channel))
	(make-array samples
				:element-type (array-element-type stereo-data)
				:displaced-to stereo-data 
				:displaced-index-offset (* channel samples))))

;;
;; Display wave file 
;;
(defun display (data)
