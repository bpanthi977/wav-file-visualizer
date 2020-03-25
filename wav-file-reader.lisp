;;;; wav-file-reader.lisp

(in-package #:wav-file-reader)

(defparameter *file* #p"E:/Development/samples/sample.wav")


;;
;; WAV File Reader 
;;

;; Data type reader and writer definitions
(define-binary-type unsigned-integer-littleendian (bytes bits-per-byte)
  (:reader (in)
		   (loop with value = 0
				 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte do
				   (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
				 finally (return value)))
  (:writer (out value)
		   (loop for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
				 do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type ub1 () (unsigned-integer-littleendian :bytes 1 :bits-per-byte 8))
(define-binary-type ub2 () (unsigned-integer-littleendian :bytes 2 :bits-per-byte 8))
(define-binary-type ub3 () (unsigned-integer-littleendian :bytes 3 :bits-per-byte 8))
(define-binary-type ub4 () (unsigned-integer-littleendian :bytes 4 :bits-per-byte 8))

(define-binary-type signed-integer-2bytes ()
  (:reader (in)
		   ;; Byte ordering is little endian for WAV file 
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

(define-binary-type multichannel-data (channels size reader-writer-type array-element-type)
  (:reader (in)
		   (let ((data (make-array (list channels size) :element-type array-element-type)))
			 (loop for i from 0 to (1- size) do 
			   (loop for c from 0 to (1- channels) do
				 (setf (aref data c i) (read-value reader-writer-type in))))
			 data))
  (:writer (out value)
		   (loop for i from 0 to (1- size) do 
			 (loop for c from 0 to (1- channels) do
			   (write-value reader-writer-type out (aref value c i))))))

;; RIFF file definition

(define-tagged-binary-class
	riff  ()
	((riff-identifier (iso-8859-1-string :length 4))
	 (size (ub4)) 
	 (wave-identifier (iso-8859-1-string :length 4)))
	(:dispatch (if (and (string-equal riff-identifier "RIFF")
						(string-equal wave-identifier "WAVE"))
				   'wav
				   (error "File is not a WAVE file"))))

;; WAV file definition 

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
	 (data-size (ub4)) 
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
  "Return single channel data from multichannel data"
  (destructuring-bind (channels samples) (array-dimensions stereo-data)
	(if (< channels channel)
		(error "Channel ~a doesn't exist in audio data" channel))
	(make-array samples
				:element-type (array-element-type stereo-data)
				:displaced-to stereo-data 
				:displaced-index-offset (* channel samples))))

;;
;; Wave file Visualization
;;

(defun display (data)
  (sdl:with-init ()
	(sdl:window 1000 600)
	(sdl:clear-display sdl:*black*)
    (sdl:initialise-default-font)
	(let* ((l (second (array-dimensions data)))
		   (width 1000)
		   (scalex (/ width l))
		   (height 300)
		   (scaley (/ height (expt 2 16))))
	  (loop with d1 = (select-channel data 0)
			with d2 = (select-channel data 1)
			for i from 0 below l
			with x = 0
			with base-y = 150 do
			  (incf x scalex)
			  (sdl:draw-line-* (truncate x) base-y (truncate x) (truncate (- base-y (* (aref d1 i) scaley))))
			  (sdl:draw-line-* (truncate x) (+ 300 base-y) (truncate x) (truncate (- base-y -300 (* (aref d2 i) scaley))))))
	(sdl:update-display)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-up-event (:key k)
						 (case k
						   (:sdl-key-q (sdl:push-quit-event)))))))


;;
;;; Sine wave Data Generator 
;;

(defun generate-cos-wave (freq time N)
  (let* ((data (make-array N :element-type 'single-float)))
	(loop for i from 0 to (1- N)
		  with step = (/ time N)
		  with time = 0
		  with f = (* 2 pi freq) do
			(setf (aref data i) (coerce (cos (* f time)) 'single-float))
			(incf time step))
	data))

;;
;;; Discrete Fourier Transform 
;; 

;; Defintion Method / Bruteforce method 
(defun dft (data &optional (max 1000))
  (let ((transform (make-array (min max (length data)))))
	(loop with N = (min max (length data))
		  for |n| from  0 to (1- N) do
			(setf (aref transform |n|)
				  (loop for k from 0 to (1- N)
						with coeff = (* -2 pi #C(0 1) |n| (/ N))
						summing (* (aref data k) (exp (* coeff k))))))
	transform))

;; FFT (TODO)


;;
;;; Source and DFT Visualization 
;;

(defun shift-and-clip (data pos length type)
  (let ((l (length data)))
	(setf pos (min pos (1- l)))
	(setf length (min length (- l pos)))
	(make-array length :element-type type :displaced-to data :displaced-index-offset pos)))

(defun display-source-and-spectrum (&optional data (scroll? nil) )
  (let* ((f 1) source dft
		 (updated? t)
		 (scaley 150)
		 (scaley2 10))
	(sdl:with-init ()
	  (sdl:initialise-default-font)
	  (sdl:window 500 600)
	  
	  (sdl:with-events ()
		(:quit-event  () T)
		(:key-up-event (:key k)
					   (setf updated? t)
					   (case k
						 (:sdl-key-a (decf f))
						 (:sdl-key-d (incf f))
						 (:sdl-key-q (sdl:push-quit-event))))
		(:idle () 
			   (when updated?
				 (sdl:clear-display sdl:*black*)

				 (if data
					 (setf source (shift-and-clip data (* f 300) 500 '(signed-byte 16))
						   dft (dft source 1500)
						   scaley (/ 150 (expt 2 15))
						   scaley2 (/ 1 (expt 2 15)))
					 (setf source (generate-cos-wave f 1 100)
						   dft (dft source)))

				 (loop for i from 0 below (length dft)
					   for stepx = (/ 500 (length dft))
					   with x = 0
					   for xx = (truncate x)
					   for yy = (truncate (* scaley (aref source i)))
					   for yyr = (truncate (* scaley2 (realpart (aref dft i))))
					   for yyi = (truncate (* scaley2 (imagpart (aref dft i))))
					   for yy-mag = (truncate (sqrt (+ (expt yyr 2) (expt yyi 2)))) do
						 (sdl:draw-line-* xx 150 xx (- 150 yy))
						 (sdl:draw-line-* xx 455 xx (- 455 yy-mag))
						 (sdl:draw-line-* (1+ xx) 455 (1+ xx) (- 455 yyr) :color sdl:*green*)
						 (sdl:draw-line-* (1- xx) 455 (1- xx) (- 455 yyi) :color sdl:*red*)
						 (sdl:draw-line-* xx 455 xx (- 455 yy-mag))
						 (when (> yyr 10) (sdl:draw-string-solid-* (format nil "~d" i) xx 455))
						 (incf x stepx))

				 (sdl:draw-string-solid-* "Source" 240 250)
				 (sdl:draw-string-solid-* "Spectrum" 240 550)
				 (sdl:update-display)
				 (setf updated? nil))
			   (when scroll? 
				 (incf f)
				 (setf updated? t)))))))


;;
;;; TESTS 
;;

(defun read-wav-if-not ()
  (unless *data*
	(setf *data* (read-wav))))

(defun test (n) 
  (case n
	(0
	 (print "Remember to set the *file* var to proper wav file pathname")
	 (force-output)
	 (describe (read-wav)))
	(1
	 (print "use a and d keys to change frequencies")
	 (force-output)
	 (display-source-and-spectrum))
	(2 
	 (display-source-and-spectrum (select-channel (slot-value (read-wav) 'data) 0) t))
	(3 
	 (read-wav-if-not)
	 (display *data*))))


