(defparameter *memory* (make-array #xFFFF :element-type '(unsigned-byte 16)))
(defparameter *registers* (make-array 10 :element-type '(unsigned-byte 16)))
(defparameter *flags* '(pos zro neg))
(defparameter *operations* '(br add ld st jsr and ldr str
                             rti not ldi sti jmp res lea trap ret))
(defparameter *spec*
  '((br (n 1 11) (z 1 10) (p 1 9) (pcoffset9 9 0))
    (add (dr 3 9) (sr1 3 6) (mode 1 5) (sr2 3 0) (imm5 5 0))
    (ld (dr 3 9) (pcoffset9 9 0))
    (st (sr 3 9) (pcoffset9 9 0))
    (jsr (mode 1 11) (pcoffset9 11 0) (baser 3 6))
    (and (dr 3 9) (sr1 3 6) (mode 1 5) (sr2 3 0) (imm5 5 0))
    (ldr (dr 3 9) (baser 3 6) (offset6 6 0))
    (str (sr 3 9) (offset6 6 0))
    (rti)
    (not (dr 3 9) (sr 3 6))
    (ldi (dr 3 9) (pcoffset9 9 0))
    (sti (sr 3 9) (pcoffset9 9 0))
    (jmp (baser 3 6))
    (res)
    (lea (dr 3 9) (pcoffset9 9 0))
    (trap (trapvect8 8 0))
    (ret))
  "Specification of an operation (name size position)")

(defun wrap (bits op)
  "Handle overflowing registers"
  (ldb (byte bits 0) op))

(defmacro with-spec (spec instr &body body)
  "Load the operation specification on the instruction"
  `(let ,(loop for s in spec
               for name = (first s)
               for byte = (apply #'byte (rest s))
               collect (list name (ldb byte instr)))
     ,@body))

(with-spec ((dr 3 9) (sr1 3 6) (mode 1 5) (sr2 3 0) (imm5 5 0)) #x1f3F
  (wrap 16 (+ (aref *registers* sr1) (aref *registers* sr2))))
