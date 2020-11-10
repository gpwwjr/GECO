;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GECO -*-

(in-package :GECO)
#|
Genetic Evolution through Combination of Objects (GECO)

Copyright (C) 1992,1993,2020  George P. W. Williams, Jr.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(defmethod SHARED-INITIALIZE :AFTER ((self chromosome) slot-names &REST initargs
                                     &KEY size organism random)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp self 'organism)
    (setf (organism self) organism))
  (unless (slot-boundp self 'loci)
    (make-loci-vector self (if size size (size self))
                      :random random)))

(defmethod MAKE-LOCI-VECTOR ((self chromosome) size &KEY &ALLOW-OTHER-KEYS)
  (setf (loci self) (make-array size :element-type 'fixnum
                                :initial-element 0)))

(defmethod MAKE-LOCI-VECTOR :AROUND ((self chromosome) size &KEY random)
  (declare (ignore size))
  (prog1 (call-next-method)
    (when random
      (pick-random-alleles self))))

(defmethod PICK-RANDOM-ALLELES ((self chromosome))
  (dotimes (i (size self))
    (setf (locus self i) (pick-random-allele self i))))

(defmethod PICK-RANDOM-ALLELE ((self chromosome) locus-index)
  "Returns a random allele for the locus."
  (geco-random-integer (locus-arity self locus-index)))

(defmethod ALLELE-CODE-TO-VALUE ((self chromosome) locus-index allele-code)
  (aref (allele-values self locus-index) allele-code))

(defmethod LOCI-PRINTABLE-FORM ((self chromosome)
                                &AUX (s (make-string (size self))))
  (dotimes (i (size self))
    (setf (aref s i) (locus-printable-form self i)))
  s)

(defmethod LOCUS-PRINTABLE-FORM ((self chromosome) locus-index)
  "Returns a character which represents the value of the allele at the indicated locus of the chromosome."
  (let ((allele-code (locus self locus-index))
        (printable-allele-values (printable-allele-values self locus-index)))
    ;; it's especially important to be robust in a method called by the lisp print routines
    ;; the print-object method for organism indirectly calls this function
    (if (< allele-code (length printable-allele-values))
        (aref printable-allele-values allele-code)
      ;; else, indicate we don't know how to print this code
      #\?)))

(defmethod COPY-CHROMOSOME ((self chromosome) owner-organism)
  (let ((new-self (make-chromosome owner-organism (class-of self))))
    (dotimes (i (size self))
      (setf (locus new-self i) (locus self i)))
    new-self))

(defmethod PRINT-OBJECT ((self chromosome) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (loci-printable-form self) stream)))

(defmethod EIDETIC ((chr1 chromosome) (chr2 chromosome)
                    &AUX (size1 (size chr1)))
  "Predicate, true if the chromosomes are of the same class and have the same alleles."
  (and (eq (class-of chr1) (class-of chr2))
       (= size1 (size chr2))      ; some day this may be important
       (do ((locus# 0 (1+ locus#))
            (same t))
           ((or (not same)
                (>= locus# size1))
            same)
         (setq same (= (locus chr1 locus#)
                       (locus chr2 locus#))))))

(defmethod SIZE ((self chromosome))
  (length (loci self)))

(defmethod LOCUS ((self chromosome) index)
  (aref (loci self) index))

(defmethod (SETF LOCUS) (new-value (self chromosome) index)
  (setf (aref (loci self) index) new-value))

(defmethod PICK-RANDOM-LOCUS-INDEX ((self chromosome))
  "Returns a random locus index into the chromosome."
  (geco-random-integer (size self)))

(defmethod COUNT-ALLELE-CODES
           ((chromosome chromosome) from-index loci-to-count allele-code)
  "Returns the number of loci in the loci-to-count loci of chromosome, starting at from-index,
wich have allele-code"
  (do* ((locus# from-index (1+ locus#))
        (counter 1 (1+ counter))
        (result (if (= allele-code (locus chromosome locus#)) 1 0)
                (+ result (if (= allele-code (locus chromosome locus#)) 1 0))))
      ((>= counter loci-to-count)
       result)))

(defmethod HAMMING-DISTANCE ((c1 chromosome) (c2 chromosome)
                             &AUX (distance 0))
  (dotimes (i (size c1))
    (if (/= (locus c1 i) (locus c2 i))
        (incf distance)))
  distance)

;;; ========= Genetic Operators =========

(defmethod MUTATE-CHROMOSOME ((self chromosome) locus-index)
  (setf (locus self locus-index)
        (pick-random-allele self locus-index)))

(defmethod CROSS-CHROMOSOMES ((parent1 chromosome) (parent2 chromosome)
                              (child1 chromosome) (child2 chromosome)
                              locus-index)
  "[One-point crossover] Cross a chromosome segment between two parents at a specified location in the children.
Both chromosomes must be the same size."
  ;; Assume all chromosomes are the same size.
  ;; First, move the unswapped portions
  (dotimes (i locus-index)
    (setf (locus child1 i) (locus parent1 i))
    (setf (locus child2 i) (locus parent2 i)))
  ;; Then, move the swapped portions
  (do ((i (1+ locus-index) (1+ i)))
      ((>= i (size parent1)))
    (setf (locus child1 i) (locus parent2 i))
    (setf (locus child2 i) (locus parent1 i))))

(defmethod 2X-CROSS-CHROMOSOMES ((parent1 chromosome) (parent2 chromosome)
                                 (child1 chromosome) (child2 chromosome)
                                 locus-index1 locus-index2
                                  &AUX
                                  (n (size parent1))
                                  (test (mod (1+ locus-index2) n)))
  "[Two-point crossover] Cross a chromosome segment between two parents two specified locations in the children.
Both chromosomes must be the same size.  When locus-index1 > locus-index2, the copied segment wraps
around the end of the chromosomes; When the designated segment indicates the entire chromosome, nothing is copied."
  ;; Copy the original parents
  (dotimes (i n)
    (setf (locus child1 i) (locus parent1 i)
          (locus child2 i) (locus parent2 i)))
  ;; Copy the indicated segment
  (do* ((i locus-index1 (mod (1+ i) n)))
       ((= i test))
    (setf (locus child2 i) (locus parent1 i))
    (setf (locus child1 i) (locus parent2 i))))

(defmethod UNIFORM-CROSS-CHROMOSOMES ((parent1 chromosome) (parent2 chromosome)
                                      (child1 chromosome) (child2 chromosome)
                                      &KEY (bias 0.5))
  "Uniform crossover of chromosomes between two parents, with optional bias in (0.0:1.0].
Default bias is 0.5; larger bias indicates larger number of crossed alleles."
  ;; Assume all chromosomes are the same size.
  (dotimes (i (size parent1))
    (if (< (geco-random-float 1.0) bias)
        (progn (setf (locus child1 i) (locus parent1 i))
               (setf (locus child2 i) (locus parent2 i)))
      (progn (setf (locus child1 i) (locus parent2 i))
             (setf (locus child2 i) (locus parent1 i))))))

(defmethod SWAP-ALLELES ((self chromosome) &KEY
                         (locus-index (pick-random-locus-index self))
                         (locus-index2 (mod (1+ locus-index)
                                            (size self))))
  (rotatef (locus self locus-index)
           (locus self locus-index2)))

(defmethod SCRAMBLE-ALLELES ((self chromosome) &AUX
                             (loci (loci self))
                             (alleles (copy-seq loci)))
  (do ((s (length alleles))
       locus#)
      ((<= s 0))
    (setq locus# (if (= 0 s) 0 (geco-random-integer s)))
    (decf s)
    (setf (aref loci s) (aref alleles locus#))
    (when (< locus# s)
      (setf (aref alleles locus#) (aref alleles s)))))


;;; ========= Methods for class BINARY-CHROMOSOME =========

(defmethod LOCUS-ARITY ((self binary-chromosome) locus-index)
  (declare (ignore locus-index))
  2)

(defmethod ALLELE-CODE-TO-VALUE ((self binary-chromosome) locus-index allele-code)
  "A much more efficient implementation than the general case of indexing into the vector returned by allele-values."
  (declare (ignore locus-index))
  allele-code)

(defmethod ALLELE-VALUES ((self binary-chromosome) locus-index)
  (declare (ignore locus-index))
  #(0 1))

(defmethod PRINTABLE-ALLELE-VALUES ((self binary-chromosome) locus-index)
  (declare (ignore locus-index))
  #(#\0 #\1))

(defmethod MAKE-LOCI-VECTOR ((self binary-chromosome) size &KEY &ALLOW-OTHER-KEYS)
  (setf (loci self) (make-array size :element-type 'bit
                                :initial-element 0)))


;;; ========= Binary-Chromosome Decoding Routines =========

(defmethod DECODE-BINARY-LOCI-VALUE
           ((chromosome binary-chromosome) from-index loci-to-decode)
  "Returns the binary value represented by the loci-to-decode loci of chromosome, starting at from-index.
chromosome is assumed to be an array of bit."
  (do* ((locus# from-index (1+ locus#))
        (counter 1 (1+ counter))
        (result (locus chromosome locus#)
                (+ (* 2 result) (locus chromosome locus#))))
       ((>= counter loci-to-decode)
        result)))

;;; ========= Methods for class GRAY-CODE-TRANSLATION =========
;; The following gray-code translation code is based on an implementation in C by Larry Yaeger
;; <larryy@apple.com>, which was published in the GA-List v6n5 (GA-List@AIC.NRL.Navy.Mil).

(defmethod SHARED-INITIALIZE :AFTER ((self gray-code-translation) slot-names &REST initargs
                                     &KEY number-of-bits number-of-bits-supplied-p)
  (declare (ignore slot-names initargs))
  (if (or number-of-bits-supplied-p
          (and (slot-boundp self 'number-of-bits)
               (numberp (number-of-bits self))
               (or (not (slot-boundp self 'b2g-map))
                   (not (slot-boundp self 'g2b-map)))))
      (progn
        (let ((b2g (make-array (list number-of-bits number-of-bits)
                               :element-type 'bit))
              (g2b (make-array (list number-of-bits number-of-bits)
                               :element-type 'bit)))
          (dotimes (i number-of-bits)
            (dotimes (j number-of-bits)
              (setf (aref b2g i j) 0)
              (setf (aref g2b i j)
                    (if (<= j i) 1 0)))
            (setf (aref b2g i i) 1)
            (if (> i 0)
                (setf (aref b2g i (1- i) )1)))
          (setf (b2g-map self) b2g
                (g2b-map self) g2b
                (number-of-bits self) number-of-bits)))
      (progn
        (slot-makunbound self 'number-of-bits)
        (slot-makunbound self 'b2g-map)
        (slot-makunbound self 'g2b-map))))

;;(defvar *gct*)
;;(setq *gct* (make-instance 'gray-code-translation :number-of-bits 5))

(defmethod GRAY2BIN ((self gray-code-translation) gray &AUX
                     (bin 0) bit
                     (g2b (g2b-map self))
                     (numbits (number-of-bits self))
                     (numbits-1 (1- numbits))
                     (ii numbits-1)
                     jj)
  (dotimes (i numbits)
    (setq bit 0)
    (setq jj numbits-1)
    (dotimes (j numbits)
      (setq bit (boole boole-xor bit
                       (if (logbitp jj gray)
                           (aref g2b i j)
                         0)))
      ;;(dbgo "~&~5Tjj=~D, (logbitp jj gray)=~S, (aref g2b i j)=~D"
      ;;      jj (logbitp jj gray) (aref g2b i j))
      (decf jj))
    ;;(dbgo "~&bin=~D, bit=~D, ii=~D" bin bit ii)
    (setq bin (boole boole-ior bin (ash bit ii)))
    (decf ii))
  bin)

(defmethod BIN2GRAY ((self gray-code-translation) bin &AUX
                     (gray 0) bit
                     (b2g (b2g-map self))
                     (numbits (number-of-bits self))
                     (numbits-1 (1- numbits))
                     (ii numbits-1)
                     jj)
  (dotimes (i numbits)
    (setq bit 0)
    (setq jj numbits-1)
    (dotimes (j numbits)
      (setq bit (boole boole-xor bit
                       (if (logbitp jj bin)
                           (aref b2g i j)
                         0)))
      ;;(dbgo "~&~5Tjj=~D, (logbitp jj bin)=~S, (aref g2b i j)=~D"
      ;;      jj (logbitp jj bin) (aref b2g i j))
      (decf jj))
    ;;(dbgo "~&gray=~D, bit=~D, ii=~D" gray bit ii)
    (setq gray (boole boole-ior gray (ash bit ii)))
    (decf ii))
  gray)

#|
(let ((gct (make-instance 'gray-code-translation
             :number-of-bits 5)))
  (format t "~&Int ~7TBinary ~19TGray ~23TGrayInt  RecoveredInt")
  (dotimes (i (expt 2 (number-of-bits gct)))
    (let ((g (bin2gray gct i)))
      (format t "~%~3D  ~8B  ~8B  ~4D  ~8D"
              i i g g (gray2bin gct g))))
  (format t "~2%GrayInt Int")
  (dotimes (i (expt 2 (number-of-bits gct)))
    (format t "~% ~6D ~3D" i (gray2bin gct i))))
|#


;;; ========= Methods for class SEQUENCE-CHROMOSOME =========

(defmethod PICK-RANDOM-ALLELES ((self sequence-chromosome))
  (dotimes (i (size self))
    (setf (locus self i) i))
  (scramble-alleles self))

(defmethod PMX-CROSS-CHROMOSOMES ((parent1 sequence-chromosome)
                                  (parent2 sequence-chromosome)
                                  (child1 sequence-chromosome)
                                  (child2 sequence-chromosome)
                                  &KEY
                                  (locus-index1 (pick-random-locus-index parent1))
                                  (locus-index2 (pick-random-locus-index parent1))
                                  (allele-test #'eql)
                                  &AUX
                                  (n (size parent1))
                                  (test-index2 (mod (1+ locus-index2) n)))
  (if (dbg-p :pmx)
      (dbgo "~&PMX: i1,i2,t2= ~S ~S ~S~%~5Tp1,p2= ~S~%~12T~S"
            locus-index1 locus-index2 test-index2 parent1 parent2))
  ;; Copy the original parents
  (dotimes (i n)
    (setf (locus child1 i) (locus parent1 i)
          (locus child2 i) (locus parent2 i)))
  ;; Perform the crossover
  (when (and (/= locus-index1 locus-index2)
             (/= locus-index1 test-index2))
    (do* ((i locus-index1 (mod (1+ i) n)))
         ((= i test-index2))
      (rotatef (locus child2 i)
               (locus child2 (position (locus parent1 i) (loci child2)
                                       :test allele-test)))
      (rotatef (locus child1 i)
               (locus child1 (position (locus parent2 i) (loci child1)
                                       :test allele-test)))))
  (if (dbg-p :pmx)
      (dbgo "~%~5Tc1,c2= ~S --after~%~12T~S"
            child1 child2)))

(defmethod R3-CROSS-CHROMOSOMES ((parent1 sequence-chromosome)
                                 (parent2 sequence-chromosome)
                                 (child1 sequence-chromosome)
                                 (child2 sequence-chromosome)
                                 &KEY
                                 (allele-test #'eql)
                                 &AUX
                                 (n (size parent1))
                                 (mismatched-alleles1 (make-array n))
                                 (mismatched-alleles2 (make-array n))
                                 (mismatched-loci (make-array n))
                                 (mismatched-count 0))
  "Random, Respectful Recombination (R3) crossover operator.  Based on forma analysis work of Nicholas J. Radcliffe."
  (if (dbg-p :r3)
      (dbgo "~&R3: p1,p2= ~S~%~11T~S"
            parent1 parent2))
  ;; Assume all chromosomes are the same size.
  (dotimes (i n)
    (if (funcall allele-test (locus parent1 i) (locus parent2 i))
            (progn ;; copy the common alleles
                              (setf (locus child1 i) (locus parent1 i))
          (setf (locus child2 i) (locus parent2 i)))
      (progn ;; else, save mismatched alleles for randomizing
        (setf (aref mismatched-alleles1 mismatched-count)
              (locus parent1 i))
        (setf (aref mismatched-alleles2 mismatched-count)
              (locus parent2 i))
        (setf (aref mismatched-loci mismatched-count) i)
        (incf mismatched-count))))
  ;; randomly reinsert the mismatched alleles
  (dotimes (i mismatched-count)
    (let ((m (geco-random-integer mismatched-count))
          (locus# (aref mismatched-loci i)))
      (setf (locus child1 locus#)
            (aref mismatched-alleles1 m))
      (setf (locus child2 locus#)
            (aref mismatched-alleles2 m))
      (let ((m-1 (1- mismatched-count)))
        (unless (= m m-1)
          (setf (aref mismatched-alleles1 m)
                (aref mismatched-alleles1 m-1))
          (setf (aref mismatched-alleles2 m)
                (aref mismatched-alleles2 m-1)))
        (decf mismatched-count))))
  (if (dbg-p :r3)
      (dbgo "~%~4Tc1,c2= ~S --after~%~11T~S"
            child1 child2)))

