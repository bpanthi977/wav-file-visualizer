;;;; wav-file-reader.asd

(asdf:defsystem #:wav-file-reader
  :description "Describe wav-file-reader here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:com.gigamonkeys.binary-data #:lispbuilder-sdl)
  :components ((:file "package")
               (:file "wav-file-reader")))
