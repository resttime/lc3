(defparameter *memory* (make-array #xFFFF :element-type '(unsigned-byte 16)))
(defparameter *registers* (make-array 10 :element-type '(unsigned-byte 16))
  "R0-R7 are for general purpose
   R8 is the program counter
   R9 is the conditional")
(defparameter *flags* '(1 0 -1)
  "Sign of u16 written to the conditional register")
(defparameter *operations* '(br add ld st jsr and ldr str
                             rti not ldi sti jmp res lea trap
                             ret)
  "List of the operations")
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

(defun wrap (op)
  "Handle overflowing registers"
  (ldb (byte 16 0) op))

(defun u16->int (u16)
  "Convert uint16 to an integer"
  (if (= (ldb (byte 1 15) u16) 0)
      u16
      (1+ (logxor (- #xFFFF) u16))))

(defun update-conditional-register (u16)
  "LC3 sets the conditional register with u16's sign when it writes a
   register"
  (setf (aref *registers* 9) (position (signum (u16->int u16)) *flags*)))

(defun reg (idx)
  "Get the register at the index"
  (aref *registers* idx))

(defun (setf reg) (u16 idx)
  "Write to registers with SETF form"
  (update-conditional-register u16)
  (setf (aref *registers* idx) u16))

(defmacro with-spec (spec instr &body body)
  "Bind specification to instruction"
  `(let ,(loop for s in spec
               for name = (first s)
               for byte = (apply #'byte (rest s))
               collect (list name (ldb byte instr)))
     ,@body))

(with-spec ((dr 3 9) (sr1 3 6) (mode 1 5) (sr2 3 0) (imm5 5 0)) #x1fff
  (setf (reg dr)
        (wrap (+ (aref *registers* sr1) (if (= mode 0) (aref *registers* sr2) imm5))))
  (print (map 'vector #'u16->int *registers*)))
