;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GECO-USER -*-

(in-package :GECO-USER)
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
|# #|
This file illustrates use of GECO to implement two different genetic algorithms to solve a simple problem.
The problem is often called 'count ones,' it simply maximizes the one-bits in the chromosome.
The two genetic algorithms (genetic plans in Holland's terminology) are similar to Holland's classic algorithm
(though they both use uniform crossover), and a variation on Holland's algorithm which uses a different selection
procedure discussed by Shaffer in his paper "Some Effects of Selection Procedures on Hyperplane Sampling by
Genetic Algorithms"  (in Davis' 1987 collection Genetic Algorithms and Simulated Annealing). 
|#

(defclass BINARY-CHROMOSOME-10 (binary-chromosome)
  ()
  (:documentation
   "A 10-bit binary chromosome."))

(defmethod SIZE ((self binary-chromosome-10))
  "So GECO will know how large to make the chromosome."
  10)

(defclass SIMPLE-BINARY-10-ORGANISM (organism)
  ()
  (:documentation
   "An organism with only a 10-bit binary chromosome."))

(defmethod CHROMOSOME-CLASSES ((self simple-binary-10-organism))
  "So GECO will know what chromosomes to make."
  '(binary-chromosome-10))

;; requires allele-counts.lisp:
(defclass BINARY-POPULATION-STATISTICS (population-statistics
                                        allele-statistics-mixin)
  ()
  (:documentation
   "The allele-statistics-mixin adds several methods, plus a slot: allele-counts."))

(defmethod COMPUTE-STATISTICS :AFTER ((pop-stats binary-population-statistics))
  "Compute the allele statistics for the population and save them."
  (setf (allele-counts pop-stats)
        (compute-binary-allele-statistics (population pop-stats))))

(defclass SIMPLE-BINARY-POPULATION (generational-population maximizing-score-mixin)
  ()
  (:documentation
   "Our populations are generational, and the scores are maximized."))

(defmethod ORGANISM-CLASS ((self simple-binary-population))
  "So GECO knows how to make the organisms in our population."
  'simple-binary-10-organism)

(defmethod POPULATION-STATISTICS-CLASS ((self simple-binary-population))
  "So GECO knows how to make our population statistics instances."
  'binary-population-statistics)

#||
;; (inspect *sbe*)
;; (test-plan 1 'simple-plan-2)
;; (test-plan 10 'simple-plan-1)
;; (test-plan 10 'simple-plan-2)

(defmethod SHARED-INITIALIZE :AFTER
           ((pop simple-binary-population) slot-names &REST initargs
            &KEY random &AUX (pop-size (size pop)))
  (declare (ignore slot-names initargs))
  ;; (unless (slot-boundp pop 'organisms)
  (when pop-size
    ;; (setf (organisms pop) (make-organisms-vector pop pop-size))
    (when random
      ;; here's where we begin to differ from the default method
      (let ((allele-stats (compute-allele-statistics pop))
            (chroms0 (genotype (aref (organisms pop) 0))))
        (dotimes (chrom# (length chroms0))
          (let ((chrom-counts (nth chrom# allele-stats))
                (starting-org# (pick-random-organism-index pop)))
            (when (subtypep (class-of (elt chroms0 chrom#)) 'binary-chromosome)
              (normalize-nary-allele-distribution
               pop chrom# chrom-counts starting-org#)
              #|| (normalize-binary-allele-distribution
               pop chrom# chrom-counts starting-org#) ||#))))))
  ;;)
  )
||#

(defclass SIMPLE-PLAN (genetic-plan)
  ((STATISTICS
    :accessor statistics
    :initarg :statisics
    :initform nil     ; so we can push instances
    :documentation "A stack of population-statistics for all past populations")) 
  (:documentation
   "Abstract class to allow method sharing for initialization & regeneration."))

(defmethod EVALUATE ((self simple-binary-10-organism) (plan simple-plan)
                     &AUX (chromosome (first (genotype self))))
  "The score for our organisms is the number of non-zero alleles."
  #+:mcl (declare (ignore plan))
  (setf (score self)
        (count-allele-codes chromosome 0 (size chromosome) 1)))

(defmethod REGENERATE ((plan simple-plan) (old-pop simple-binary-population) &AUX
                       (new-pop (make-population (ecosystem old-pop)
                                                 (class-of old-pop)
                                                 :size (size old-pop))))
  "Create a new generation from the previous one, and record statistics."
  (setf (ecosystem new-pop) (ecosystem old-pop))
  ;; selectively reproduce, crossover, and mutate
  (operate-on-population plan old-pop new-pop)
  ;; record old-pop's statistics
  (push (statistics old-pop)     ; impractical for real-world problems
        (statistics plan))
  new-pop)

(defclass SIMPLE-PLAN-1 (simple-plan)
  ())

(defmethod PROB-MUTATE ((self SIMPLE-PLAN-1))
  "This is the probability of mutating an organism, not a single locus as is often used."
  0.03)

(defmethod PROB-CROSS ((self SIMPLE-PLAN-1))
  "The probability of crossover for an organism."
  0.7)

(defmethod OPERATE-ON-POPULATION
           ((plan simple-plan-1) old-population new-population
            &AUX
            (new-organisms (organisms new-population))
            (p-cross (prob-cross plan))
            (p-mutate (+ p-cross (prob-mutate plan)))
            (orphan (make-instance (organism-class old-population)))) ; not in any population
  "Apply the genetic operators on selected organisms from the old population to create a new one."
  (let ((selector (stochastic-remainder-preselect old-population)))
    (do ((org1 (funcall selector) (funcall selector))
         org2
         (random# (geco-random-float 1.0) (geco-random-float 1.0))
         (i 0 (1+ i)))
        ((null org1))
      (cond
       ((> p-cross random#)
        (if (< 1 (hamming-distance (first (genotype org1))
                                   (first (genotype (setf org2 (pick-random-organism
                                                                old-population))))))
            (uniform-cross-organisms
             org1 org2
             (setf (aref new-organisms i)
                   (copy-organism org1 :new-population new-population))
             orphan) ;a throw-away, not in any population so it can be GC'd
          ;; hamming distances < 2 will produce eidetic offspring anyway, so bypass crossover & evaluation
          (setf (aref new-organisms i)
                (copy-organism-with-score org1 :new-population new-population))))
       ((> p-mutate random#)
        (mutate-organism
         (setf (aref new-organisms i)
               (copy-organism org1 :new-population new-population))))
       (T ;; copying the score bypasses the need for a redundant evaluate
        (setf (aref new-organisms i)
              (copy-organism-with-score org1 :new-population new-population)))))))

(defclass SIMPLE-PLAN-2 (simple-plan)
  ())

(defmethod PROB-MUTATE ((self SIMPLE-PLAN-2))
  "This is the probability of mutating an organism, not a single locus as is often used."
  0.03)

(defmethod PROB-CROSS ((self SIMPLE-PLAN-2))
  0.7)

(defmethod OPERATE-ON-POPULATION
           ((plan simple-plan-2) old-population new-population
            &AUX
            (new-organisms (organisms new-population))
            (p-cross (prob-cross plan))
            (p-mutate (+ p-cross (prob-mutate plan))))
  "Apply the genetic operators on selected organisms from the old population to create a new one."
  (let ((selector (stochastic-remainder-preselect old-population)))
    (do* ((org1 (funcall selector) (funcall selector))
          org2
          (random# (geco-random-float 1.0) (geco-random-float 1.0))
          (i 0 (1+ i)))
        ((null org1))
      (cond
       ((> p-cross random#)
        (if (and (setq org2 (funcall selector))
                 (< 1 (hamming-distance (first (genotype org1))
                                        (first (genotype org2)))))
            (uniform-cross-organisms
             org1 org2
             (setf (aref new-organisms i)
                   (copy-organism org1 :new-population new-population))
             (setf (aref new-organisms (1+ i))
                   (copy-organism org2 :new-population new-population)))
          (progn ;; hamming distances < 2 will produce eidetic offspring anyway, so bypass crossover & evaluation
            (setf (aref new-organisms i)
                  (copy-organism-with-score org1 :new-population new-population))
            (when org2
              (setf (aref new-organisms (1+ i))
                    (copy-organism-with-score org2 :new-population new-population)))))
        (incf i))                       ;  because we just added an extra to the new population
       ((> p-mutate random#)
        (setf (aref new-organisms i)
              (copy-organism org1 :new-population new-population)))
       (T ;; copying the score bypasses the need for a redundant evaluate
        (setf (aref new-organisms i)
              (copy-organism-with-score org1 :new-population new-population)))))))

;;; Some stuff to test the algorithms:

(defvar *SBE* nil "a simple binary ecosystem")

(defun TEST-PLAN (times plan &KEY
                        (stream t)
                        (pop-size 20)
                        (evaluation-limit 400))
  (let (maxs avgs gens evals)
    (flet ((test ()
             (dotimes (i times)
               (setq *sbe* (make-instance 'ecosystem
                             :pop-class 'simple-binary-population
                             :pop-size pop-size
                             :plan-class plan
                             :evaluation-limit evaluation-limit))
               (evolve *sbe*)
               (format t "~& -- Max=~F, Avg=~F, #gens=~D, #evals=~D."
                       (max-score (statistics (population *sbe*)))
                       (avg-score (statistics (population *sbe*)))
                       (generation-number *sbe*)
                       (evaluation-number *sbe*))
               (push (max-score (statistics (population *sbe*))) maxs)
               (push (avg-score (statistics (population *sbe*))) avgs)
               (push (generation-number *sbe*) gens)
               (push (evaluation-number *sbe*) evals))))
      (format stream "~&~A:" plan)
      (time (test)))
    (format stream "~& ==> Avg max=~F, Avg avg=~,3F, Avg #gens=~F, Avg #evals=~F"
            (/ (float (reduce #'+ maxs)) times)
            (/ (float (reduce #'+ avgs)) times)
            (/ (float (reduce #'+ gens)) times)
            (/ (float (reduce #'+ evals)) times))))

#||   some code snippets that can be copied & pasted into the listener
(in-package :geco-user)
(progn (test-plan 1 'simple-plan-1)
       (inspect *sbe*))
(progn (test-plan 1 'simple-plan-2)
       (inspect *sbe*))
(test-plan 10 'simple-plan-1)
(test-plan 10 'simple-plan-2)

(evaluate (population *sbe*) (plan *sbe*))
(inspect *sbe*)
(evolve *sbe*)
(dbg :converge)
(undbg)
||#

