;;;; brainfuck.asd

(asdf:defsystem #:cl-brainfuck
  :description "A Brainfuck interpreter"
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "brainfuck")))

