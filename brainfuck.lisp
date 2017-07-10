;;;; brainfuck.lisp

(in-package #:cl-brainfuck)

;;; "cl-brainfuck" goes here. Hacks and glory await!

;; Specification: http://esolangs.org/wiki/Brainfuck

(defparameter *pointer* nil "Pointer to a cell in the memory.")
(defparameter *ip* nil "Pointer to the current instruction in the program.")

(defparameter *memory* (make-array 30000
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)
  "The memory array for the VM.")

(defparameter *program* nil "The current program's source code.")

(defun bf> ()
  "Move the pointer to the right."
  (incf *pointer*))

(defun bf< ()
  "Move the pointer to the left."
  (decf *pointer*))

(defun bf+ ()
  "Increment the memory cell under the pointer."
  (setf (elt *memory* *pointer*)
        (mod (1+ (elt *memory* *pointer*)) 256)))

(defun bf- ()
  "Decrement the memory cell under the pointer."
  (setf (elt *memory* *pointer*)
        (mod (1- (elt *memory* *pointer*)) 256)))

(defun bf. ()
  "Output the character signified by the cell at the pointer."
  (format t "~A" (code-char (elt *memory* *pointer*))))

(defun bf\, ()
  "Input a character and store it in the cell at the pointer."
  (setf (elt *memory* *pointer*) (char-code (read-char))))

(defun bf[ ()
  "Jump past the matching ] if the cell under the pointer is 0."
  (when (zerop (elt *memory* *pointer*))
    (loop :with depth := 1
       :while (not (zerop depth))
       :do (incf *ip*)
       :when (equal (elt *program* *ip*) #\[)
       :do (incf depth)
       :when (equal (elt *program* *ip*) #\])
       :do (decf depth))))

(defun bf] ()
  "Jump back to the matching [ if the cell under the pointer is nonzero."
  (when (not (zerop (elt *memory* *pointer*)))
    (loop :with depth := 1
       :while (not (zerop depth))
       :do (decf *ip*)
       :when (equal (elt *program* *ip*) #\])
       :do (incf depth)
       :when (equal (elt *program* *ip*) #\[)
       :do (decf depth))))

(defparameter *opcodes* `((#\> ,'bf>)
                          (#\< ,'bf<)
                          (#\+ ,'bf+)
                          (#\- ,'bf-)
                          (#\. ,'bf.)
                          (#\, ,'bf\,)
                          (#\] ,'bf])
                          (#\[ ,'bf[))
  "Map of characters to opcodes")

(defun parse (program)
  "Run the passed string through the BF interpreter."
  (let ((*memory* (make-array 30000
                               :element-type '(unsigned-byte 8)
                               :initial-element 0))
        (*pointer* 0)
        (*ip* 0)
        (*program* program))
    (loop :while (< *ip* (length *program*))
       :for c := (elt program *ip*)
       :for opcode := (cadr (find c *opcodes* :key 'car))
       :when opcode
       :do (funcall opcode)
       :do (incf *ip*))))
