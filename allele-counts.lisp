;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GECO-USER -*-

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

(in-package :GECO-USER)

(defclass ALLELE-STATISTICS-MIXIN ()
  ((ALLELE-COUNTS
    :accessor allele-counts
    :initform nil
    :documentation
    "The number of non-zero alleles, by locus, for our population."))
  (:documentation
   "Mixin for use with population-statistics classes to use allele counts."))

#|  Allele Statistics:
list[#chromosomes per each organism]:
    each list element=
     vector[#loci for that chromosome]:
           each vector element=
            vector[#alleles for that locus]:
                  each vector element=
                   fixnum: count of indexed allele in all organisms (@that locus/chromosome)
|#

(defmethod COMPUTE-ALLELE-STATISTICS ((population population))
  "Returns a list of vectors (one per chromosome in the organisms of the population) of vectors of counts by locus for each allele.
The type of the result is (list [length = #chromosomes] of (vector [size = #loci] of (vector [size = locus arity] of fixnum)))."
  (with-accessors ((orgs organisms))
                  population
    (let ((counts-list (mapcar #'(lambda (chr)
                                   (make-array (size chr)
                                               :element-type 'vector
                                               :initial-contents
                                               (let ((allele-vectors-list nil))
                                                 (dotimes (locus# (size chr))
                                                   (push (make-array (locus-arity chr locus#)
                                                                     :element-type 'fixnum
                                                                     :initial-element 0)
                                                         allele-vectors-list))
                                                 (nreverse allele-vectors-list))))
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
          (dotimes (locus# (size chromosome))
            (incf (aref (aref counts locus#)
                        (locus chromosome locus#))))))
      counts-list)))

(defun NORMALIZE-BINARY-ALLELE-DISTRIBUTION (pop chrom# chrom-counts
                                                 &OPTIONAL
                                                 (starting-org#
                                                  (pick-random-organism-index pop))
                                                 &AUX
                                                 (organisms (organisms pop))
                                                 (pop-size (size pop))
                                                 (pop-size/2 (round pop-size 2)))
  (dotimes (locus# (size (nth chrom# (genotype (aref organisms 0)))))
    (do* ((locus-counts (aref chrom-counts locus#))
          (org# starting-org#
                (mod (1+ org#) pop-size))
          (chrom (nth chrom# (genotype (aref organisms org#)))
                 (nth chrom# (genotype (aref organisms org#))))
          (allele (locus chrom locus#)
                  (locus chrom locus#)))
         ((= (aref locus-counts 0) pop-size/2)
          (setq starting-org# org#))
      (if (< (aref locus-counts 0) pop-size/2)
          ;; need more 0's
          (unless (zerop allele) ;allele=1
            (setf (locus chrom locus#) 0)
            (incf (aref locus-counts 0))
            (decf (aref locus-counts 1)))
        ;; need more 1's
        (when (zerop allele) ;allele=0
          (setf (locus chrom locus#) 1)
          (incf (aref locus-counts 1))
          (decf (aref locus-counts 0)))))))

(defun NORMALIZE-NARY-ALLELE-DISTRIBUTION (pop chrom# chrom-counts
                                               &OPTIONAL
                                               (starting-org#
                                                (pick-random-organism-index pop))
                                               &AUX
                                               (organisms (organisms pop))
                                               (pop-size (size pop))
                                               (chrom0
                                                (nth chrom# (genotype (aref organisms 0)))))
  (dotimes (locus# (size chrom0))
    (do* ((locus-counts (aref chrom-counts locus#))
          (arity (locus-arity chrom0 locus#))
          (pop-size/arity (round pop-size arity))
          (org# starting-org#
                (mod (1+ org#) pop-size))
          (chrom (nth chrom# (genotype (aref organisms org#)))
                 (nth chrom# (genotype (aref organisms org#))))
          (allele (locus chrom locus#)
                  (locus chrom locus#))
          (or-alleles (let (result) ; over-represented alleles
                        (dotimes (a arity result)
                          (when (> (aref locus-counts a) pop-size/arity)
                            (push a result)))))
          (ur-alleles (let (result) ; under-represented alleles
                        (dotimes (a arity result)
                          (when (< (aref locus-counts a) pop-size/arity)
                            (push a result))))))
         ((or (null ur-alleles)
              (null or-alleles))
          (setq starting-org# org#))
      (when (member allele or-alleles)
        ;; allele is over-represented in this locus of the population, replace it with one which is under-represented
        (let ((ura (first ur-alleles)))
          (setf (locus chrom locus#) ura)
          (when (>= (incf (aref locus-counts ura)) pop-size/arity)
            (pop ur-alleles))
          (when (<= (decf (aref locus-counts allele)) pop-size/arity)
            (setq or-alleles (delete allele or-alleles))))))))


;; not done  (trace (NORMALIZE-SEQUENCE-ALLELE-DISTRIBUTION :step t))
;; when stepping, be sure to compile with *save-definitions* t

(defun NORMALIZE-SEQUENCE-ALLELE-DISTRIBUTION
       (pop chrom# chrom-counts
            &OPTIONAL (starting-org# (pick-random-organism-index pop))
            &AUX
            (organisms (organisms pop))
            (pop-size (size pop))
            (chrom0
             (nth chrom# (genotype (aref organisms 0))))
            (chrom-size (size chrom0)))
  (dotimes (locus# (1- chrom-size)) ; if rest are balanced, the last locus must be balanced
    (dbgo "~%| Locus#=~D" locus#)
    (do* ((locus-counts (aref chrom-counts locus#))
          (arity (locus-arity chrom0 locus#))
          (pop-size/arity (round pop-size arity))
          (org# starting-org#
                (mod (1+ org#) pop-size))
          (loop-stop-org# (mod (+ org# (1- pop-size)) pop-size))
          (chrom (nth chrom# (genotype (aref organisms org#)))
                 (nth chrom# (genotype (aref organisms org#))))
          (allele (locus chrom locus#)
                  (locus chrom locus#))
          (or-alleles (let (result) ; over-represented alleles at this locus
                        (dotimes (a arity result)
                          (when (> (aref locus-counts a) pop-size/arity)
                            (push a result)))))
          (ur-alleles (let (result) ; under-represented alleles at this locus
                        (dotimes (a arity result)
                          (when (< (aref locus-counts a) pop-size/arity)
                            (push a result))))))
         ((or (null ur-alleles)
              (null or-alleles)
              (= org# loop-stop-org#))
          (dbgo "~%   org#=~D, loop-stop-org#=~D, locus-counts=~S"
                org# loop-stop-org# locus-counts)
          (setq starting-org# org#))
      (dbgo "~%| | Allele=~D~%    Ur-alleles=~S~%    Or-alleles=~S"
              allele ur-alleles or-alleles)
      (cond ((member allele or-alleles)
             ;; allele is over-represented in this locus of the population
             ;; swap it with another locus which is under-represented here, and is also over-represented later in the chromosome
             (dbgo "~%| | | allele=~S is over-rep'd" allele)
             (let (ora  ; other over-represented allele
                   orl  ; other over-represented locus
                   orl-locus-counts)
               (do ((locus (1+ locus#)
                           (1+ locus)))
                   ((or orl (>= locus chrom-size)))
                                                   (dbgo "~%| | | | Locus=~D=~D"
                       locus (locus chrom locus))
                 (if (and (member (setq ora (locus chrom locus)) ; it's under-represented here
                                  ur-alleles)
                          (> (aref (aref chrom-counts locus) ora) ; and over-represented there
                             pop-size/arity))
                     (setq orl locus
                           orl-locus-counts (aref chrom-counts locus))
                   (dbgo "...fails: far count=~D" (aref (aref chrom-counts locus) ora))))
               (if orl
                   (progn
                     (setq loop-stop-org# org#)
                     (rotatef (locus chrom locus#) (locus chrom orl)) ; swap them
                     ;; update locus-counts and lists of over/under-represented alleles
                     ;; first the current locus
                     (when (>= (incf (aref locus-counts ora)) pop-size/arity)
                       (setq or-alleles (delete ora or-alleles)))
                     (when (<= (decf (aref locus-counts allele)) pop-size/arity)
                       (setq or-alleles (delete allele or-alleles)))
                     ;; then the other locus
                     (decf (aref orl-locus-counts ora))
                     (incf (aref orl-locus-counts allele)))
                 (dbgo "~%---> couldn't find a swap"))))
            ((member allele ur-alleles)
             ;; allele is under-represented in this locus of the population
             ;; swap it with another locus which is over-represented here, and is also under-represented later in the chromosome
             (dbgo "~%| | | allele=~S is under-rep'd" allele)
             (let (ura  ; other under-represented allele
                   url  ; other under-represented locus
                   url-locus-counts)
               (do ((locus (1+ locus#)
                           (1+ locus)))
                   ((or url (>= locus chrom-size)))
                                                   (dbgo "~%| | | | Locus=~D=~D"
                       locus (locus chrom locus))
                 (if (and (member (setq ura (locus chrom locus)) ; it's over-represented here
                                  or-alleles)
                          (< (aref (aref chrom-counts locus) ura) ; and under-represented there
                             pop-size/arity))
                     (setq url locus
                           url-locus-counts (aref chrom-counts locus))
                   (dbgo "...fails: far count=~D" (aref (aref chrom-counts locus) ura))))
               (if url
                   (progn
                     (setq loop-stop-org# org#)
                     (rotatef (locus chrom locus#) (locus chrom url)) ; swap them
                     ;; update locus-counts and lists of over/under-represented alleles
                     ;; first the current locus
                     (when (>= (incf (aref locus-counts ura)) pop-size/arity)
                       (setq ur-alleles (delete ura ur-alleles)))
                     (when (<= (decf (aref locus-counts allele)) pop-size/arity)
                       (setq ur-alleles (delete allele ur-alleles)))
                     ;; then the other locus
                     (decf (aref url-locus-counts ura))
                     (incf (aref url-locus-counts allele)))
                 (dbgo "~%---> couldn't find a swap")))))))
  ; (inspect ,chrom-counts)
  ; (inspect ,pop)
  )


