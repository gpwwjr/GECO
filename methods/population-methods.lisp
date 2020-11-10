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

;;; population methods

(defmethod SHARED-INITIALIZE :AFTER
           ((pop population) slot-names &REST initargs
            &KEY random)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp pop 'organisms)
    (when (size pop)
      (setf (organisms pop) (make-organisms-vector pop (size pop)))
      (when random
        (make-organisms pop :random random)))))

(defmethod MAKE-ORGANISMS-VECTOR ((self population) size)
  (make-array size))

(defmethod MAKE-ORGANISM ((pop population) &KEY random no-chromosomes)
  (make-instance (organism-class pop)
    :population pop
    :random random
    :no-chromosomes no-chromosomes))

(defmethod MAKE-ORGANISMS ((pop population) &KEY random &AUX
                             (organisms (organisms pop)))
  (dotimes (i (size pop))
    (setf (aref organisms i)
          (make-organism pop :random random))))

(defmethod EVALUATE ((pop population) (plan genetic-plan) &AUX
                     (orgs (organisms pop)))
  (dotimes (i (length orgs))
    (let ((org (aref orgs i)))
      (if (dbg-p :score)
          (if (score org)
              (let ((orig-score (score org)))
                (evaluate org plan)
                (incf (evaluation-number (ecosystem pop)))
                (when (/= orig-score (score org))
                  (dbgo "~%**** bad score: orig=~F, now=~F"
                        orig-score (score org))
                  (break)))))
      (unless (score org)
        (evaluate org plan))))
  (setf (statistics pop)
        (make-population-statistics pop))
  (normalize-score pop plan))

(defmethod MAKE-POPULATION-STATISTICS ((pop population))
  (make-instance (population-statistics-class pop)
    :population pop))

(defmethod COMPUTE-STATISTICS ((pop population))
  "This method should only be used if the statistics need to be recomputed.
They are initially computed when the population-statistics instance is first created for the population."
  (compute-statistics (statistics pop)))

(defmethod COMPUTE-BINARY-ALLELE-STATISTICS ((population population))
  "Returns a list of vectors (one per binary chromosome in the organisms of the population) of counts by locus of non-zero alleles."
  (with-accessors ((orgs organisms))
                  population
    (let ((counts-list (mapcar #'(lambda (chr)
                                   (make-array (size chr)
                                               :element-type 'fixnum
                                               :initial-element 0))
                               ;; assume chromosomes of all organisms are the same size
                               (genotype (aref orgs 0)))))
      (dotimes (org# (size population))
        (do* ((chromosome-ptr (genotype (aref orgs org#))
                              (rest chromosome-ptr))
              (chromosome (first chromosome-ptr)
                          (first chromosome-ptr))
              (counts-ptr counts-list (rest counts-ptr))
              (counts (first counts-ptr)
                      (first counts-ptr)))
             ((null chromosome-ptr))
          (when (typep chromosome 'binary-chromosome)
            (dotimes (locus# (size chromosome))
              (if (/= 0 (locus chromosome locus#))
                  (incf (aref counts locus#)))))))
      counts-list)))

(defmethod NORMALIZE-SCORE ((pop population) (plan genetic-plan)
                            &AUX
                            (orgs (organisms pop))
                            (stats (statistics pop)))
  "Normalize all the score values for each organism in the population, according to the
genetic plan, and update the population-statistics with normalized values
(using compute-normalized-statistics)."
  (unless (= (max-score stats) (min-score stats))
    ;; don't normalize if the population is completely converged, or we'll get arithmetic exceptions
    (dotimes (i (length orgs))
      (normalize-score (aref orgs i) plan))
    (compute-normalized-statistics stats)))

#| There are a number of different ways to normalize the score values. With some plans and evaluation
functions, it may not even be necessary, though beware that the score should always be >= 0. See Chapter
4 of Goldberg's book, under the sections on Scaling Mechanisms and Ranking Procedures. Note that some
selection procedures are also based on ranking. |#

(defmethod CONVERGED-P ((pop population) &AUX
                        (pop-size (size pop))
                        (organisms (organisms pop))
                        (stats (statistics pop))
                        threshold
                        (passing-count 0)
                        (as-good-as-test (as-good-as-test pop)))
  "A predicate which returns true (non-NIL) when the population has converged.
This method defines convergence as either of the following:
1. The entire population is converged to a single score value; or
2. At least convergence-fraction of the current population has a normalized score value which is
   as good as convergence-threshold-margin of the population.
Here, as good as is determined using the function returned by the as-good-as-test generic function of
the population."
  (if (dbg-p :converge)
      (dbgo "~&---CONVERGE-P: generation=~4D, threshold=~,3F"
            (generation-number (ecosystem pop))
            (convergence-threshold-margin pop)))
  (if (= (max-score stats) (min-score stats))
      (progn (if (dbg-p :converge)
                 (dbgo "~&   population is completely converged, score=~F"
                       (max-score stats)))
             T)
    (progn 
      (setq threshold (convergence-threshold-margin pop))
      (dotimes (i pop-size)
        (if (funcall as-good-as-test (normalized-score (aref organisms i)) threshold)
            (incf passing-count)))
      (if (dbg-p :converge)
          (dbgo "~&   passing-count=~3D and passing-fraction=~,3F"
                passing-count (float (/ passing-count pop-size))))
      (>= (float (/ passing-count pop-size))
          (convergence-fraction pop)))))

;;; The following could be :allocation :per-class slots if/when this can be implemented efficiently & portably:

(defmethod POPULATION-STATISTICS-CLASS ((pop population))
  'population-statistics)


;;; Methods for maximizing-score-mixin

(defmethod MAXIMIZING-P ((pop maximizing-score-mixin))
  t)

(defmethod MINIMIZING-P ((pop maximizing-score-mixin))
  nil)

(defmethod BETTER-THAN-TEST ((pop maximizing-score-mixin))
  #'>)

(defmethod AS-GOOD-AS-TEST ((pop maximizing-score-mixin))
  #'>=)

(defmethod CONVERGENCE-FRACTION ((pop maximizing-score-mixin))
  0.95)

(defmethod CONVERGENCE-THRESHOLD-MARGIN ((pop maximizing-score-mixin))
  0.95)

(defmethod BEST-ORGANISM-ACCESSOR ((pop maximizing-score-mixin))
  #'max-organism)

(defmethod WORST-ORGANISM-ACCESSOR ((pop maximizing-score-mixin))
  #'min-organism)

(defmethod BEST-ORGANISM ((pop maximizing-score-mixin))
  (max-organism (statistics pop)))

(defmethod WORST-ORGANISM ((pop maximizing-score-mixin))
  (min-organism (statistics pop)))


;;; Methods for minimizing-score-mixin

(defmethod MAXIMIZING-P ((pop minimizing-score-mixin))
  nil)

(defmethod MINIMIZING-P ((pop minimizing-score-mixin))
  t)

(defmethod BETTER-THAN-TEST ((pop minimizing-score-mixin))
  #'<)

(defmethod AS-GOOD-AS-TEST ((pop minimizing-score-mixin))
  #'<=)

(defmethod CONVERGENCE-FRACTION ((pop minimizing-score-mixin))
  0.95)

(defmethod CONVERGENCE-THRESHOLD-MARGIN ((pop minimizing-score-mixin))
  0.05)

(defmethod BEST-ORGANISM ((pop minimizing-score-mixin))
  #'min-organism)

(defmethod WORST-ORGANISM ((pop minimizing-score-mixin))
  #'max-organism)

