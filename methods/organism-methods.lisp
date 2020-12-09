;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GECO -*-

(in-package :GECO)
#|
Genetic Evolution through Combination of Objects (GECO)

Copyright (C) 1992,1993  George P. W. Williams, Jr.

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

(defmethod SHARED-INITIALIZE :AFTER ((organism organism) slot-names &REST initargs
                                     &KEY random no-chromosomes)
  (declare (ignore slot-names initargs))
  (unless (or (genotype organism)
              no-chromosomes)
    (make-chromosomes organism :random random)))

(defmethod PRINT-OBJECT ((self organism) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~@[Score=~F~]~@[, Norm=~,2F~] ~A"
            (score self)
            (normalized-score self)
            (genotype-printable-form self))))

(defmethod COPY-ORGANISM ((self organism) &KEY (new-population (population self))
                          &AUX (new-self (make-organism new-population :no-chromosomes t))
                          result)
  (setf (genotype new-self)
        (dolist (chromosome (genotype self)
                            (nreverse result))
          (push (copy-chromosome chromosome new-self)
                result)))
  new-self)

(defmethod COPY-ORGANISM-WITH-SCORE ((self organism)
                                       &KEY (new-population (population self)) &AUX
                                       (new-self (copy-organism
                                                  self :new-population new-population)))
  (setf (score new-self) (score self))
  new-self)

(defmethod MAKE-CHROMOSOME ((org organism) chromosome-class &KEY
                            size random)
  (make-instance chromosome-class
    :organism org
    :size size
    :random random))

(defmethod MAKE-CHROMOSOMES ((self organism) &KEY random
                             &AUX result)
  (dolist (chromosome-class (chromosome-classes self)
                            (setf (genotype self) (nreverse result))) ; returned from dolist
    (push (make-chromosome self chromosome-class
                           :random random)
          result)))

(defmethod RANDOMIZE-CHROMOSOMES ((self organism))
  (dolist (chromosome (genotype self))
    (pick-random-alleles chromosome)))

(defmethod SWAP-ALLELES ((self organism) &KEY
                         (chromosome (nth (pick-random-chromosome-index self)
                                             (genotype self)))
                         (locus-index (pick-random-locus-index chromosome))
                         (locus-index2 (mod (1+ locus-index)
                                            (size chromosome))))
  (swap-alleles chromosome
                :locus-index locus-index
                :locus-index2 locus-index2))

(defmethod GENOTYPE-PRINTABLE-FORM ((self organism))
  (let ((chromosomes (when (genotype self)
                       (genotype self))))
    (format nil "[~A~{ ~A~}]" (first chromosomes) (rest chromosomes))))

#| Since the target implementor will be defining a primary method for evaluate, perform the decoding
automatically via a :before method, and similarly increment the evaluation-number vai an :after method. |#

(defmethod EVALUATE :BEFORE ((self organism-phenotype-mixin) (plan genetic-plan))
  #+:mcl (declare (ignore plan))
  (decode self))

(defmethod EVALUATE :AFTER ((self organism) (plan genetic-plan))
  #+:mcl (declare (ignore plan))
  (declare (ignore plan))
  (incf (evaluation-number (ecosystem (population self)))))

(defmethod NORMALIZE-SCORE ((self organism)
                            (plan genetic-plan)
                            &AUX (stats (statistics (population self))))
  (let ((delta (- (max-score stats) (min-score stats)))
        (range (- (score self) (min-score stats))))
    (setf (normalized-score self)
          (the short-float (float (/ range delta))))))

(defmethod EIDETIC ((org1 organism) (org2 organism) &AUX
                    (genes-1 (genotype org1))
                    (genes-2 (genotype org2)))
  "Predicate, true if the organisms are of the same class and have the same chromosomes."
  (and (eq (class-of org1) (class-of org2))
       (or (and (null genes-1) (null genes-2))
           (do* ((g1-ptr genes-1 (rest g1-ptr))
                 (g2-ptr genes-2 (rest g2-ptr))
                 (chromosome-1 (first g1-ptr) (first g1-ptr))
                 (chromosome-2 (first g2-ptr) (first g2-ptr))
                 (same (eidetic chromosome-1 chromosome-2)
                       (if chromosome-1
			   (eidetic chromosome-1 chromosome-2)
			 t)))		; have processed all chromosomes
                ((or (null g1-ptr) (not same))
                 same)))))

(defmethod PICK-RANDOM-CHROMOSOME-INDEX ((self organism))
  (do* ((chromosome-sizes (mapcar #'size (genotype self)))
        (n-loci (reduce #'+ chromosome-sizes))
        (current-chromosome-index 0 
                                  (+ current-chromosome-index 1))
        (size-list chromosome-sizes 
                   (rest size-list))
        ; order is important here; update locus-index before current-chromosome-size
        (locus-index (geco-random-integer n-loci)
                     (- locus-index current-chromosome-size))
        (current-chromosome-size (first size-list) 
                                 (first size-list)))
       ((or (null size-list)
            (< locus-index current-chromosome-size))
        current-chromosome-index)))

(defmethod PICK-RANDOM-CHROMOSOME ((self organism))
  (nth (pick-random-chromosome-index self) (genotype self)))

;;; ========= Genetic Operators =========

(defmethod MUTATE-ORGANISM ((self organism) &KEY
                            (chromosome-index (pick-random-chromosome-index self))
                            (chromosome (nth chromosome-index
                                             (genotype self)))
                            (locus-index (pick-random-locus-index chromosome)))
  "Mutate a random locus on an organism"
  (mutate-chromosome chromosome locus-index))

(defmethod CROSS-ORGANISMS ((parent1 organism) (parent2 organism)
                            (child1 organism) (child2 organism)
                            &KEY
                            (chromosome-index (pick-random-chromosome-index parent1))
                            (parent1-chromosome (nth chromosome-index
                                                     (genotype parent1)))
                            (locus-index (pick-random-locus-index
                                          parent1-chromosome)))
  "Crossover random chromosomes between two organisms."
  (cross-chromosomes parent1-chromosome
                     (nth chromosome-index (genotype parent2))
                     (nth chromosome-index (genotype child1))
                     (nth chromosome-index (genotype child2))
                     locus-index))

(defmethod 2X-CROSS-ORGANISMS ((parent1 organism) (parent2 organism)
                               (child1 organism) (child2 organism)
                               &KEY
                               (chromosome-index (pick-random-chromosome-index parent1))
                               (parent1-chromosome (nth chromosome-index
                                                        (genotype parent1)))
                               (locus-index1 (pick-random-locus-index
                                              parent1-chromosome))
                               (locus-index2 (pick-random-locus-index
                                              parent1-chromosome)))
  "Two-point crossover of random chromosomes between two organisms."
  (2x-cross-chromosomes parent1-chromosome
                        (nth chromosome-index (genotype parent2))
                        (nth chromosome-index (genotype child1))
                        (nth chromosome-index (genotype child2))
                        locus-index1 locus-index2))

(defmethod UNIFORM-CROSS-ORGANISMS ((parent1 organism) (parent2 organism)
                                    (child1 organism) (child2 organism)
                                    &KEY
                                    (chromosome-index (pick-random-chromosome-index
                                                       parent1))
                                    (bias 0.5)
                                    &AUX
                                    (parent1-chromosome (nth chromosome-index
                                                             (genotype parent1))))
  "Uniform crossover of random chromosomes between two organisms."
  (uniform-cross-chromosomes parent1-chromosome
                             (nth chromosome-index (genotype parent2))
                             (nth chromosome-index (genotype child1))
                             (nth chromosome-index (genotype child2))
                             :bias bias))

(defmethod R3-CROSS-ORGANISMS ((parent1 organism)
                               (parent2 organism)
                               (child1 organism)
                               (child2 organism)
                               &KEY
                               (allele-test #'eql)
                               (chromosome-index (pick-random-chromosome-index parent1))
                               &AUX
                               (parent1-chromosome (nth chromosome-index
                                                        (genotype parent1))))
  (r3-cross-chromosomes parent1-chromosome
                        (nth chromosome-index (genotype parent2))
                        (nth chromosome-index (genotype child1))
                        (nth chromosome-index (genotype child2))
                        :allele-test allele-test))

(defmethod PMX-CROSS-ORGANISMS ((parent1 organism) (parent2 organism)
                                 (child1 organism) (child2 organism)
                                 &KEY
                                 (allele-test #'eql)
                                 (chromosome-index (pick-random-chromosome-index parent1))
                                 (parent1-chromosome (nth chromosome-index
                                                          (genotype parent1)))
                                 (locus-index1 (pick-random-locus-index parent1-chromosome))
                                 (locus-index2 (pick-random-locus-index parent1-chromosome)))
  (pmx-cross-chromosomes parent1-chromosome
                         (nth chromosome-index (genotype parent2))
                         (nth chromosome-index (genotype child1))
                         (nth chromosome-index (genotype child2))
                         :locus-index1 locus-index1
                         :locus-index2 locus-index2
                         :allele-test allele-test))

