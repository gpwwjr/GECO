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

(defclass ECOSYSTEM ()
  ((PLAN
    :accessor plan
    :type genetic-plan)
   (POPULATION                         ;all the organisms
    :accessor population
    :type population)
   ;; see evolution-termination-p for the usage of the following two slots
   (GENERATION-NUMBER
    :accessor generation-number
    :initform 0
    :type integer)
   (EVALUATION-NUMBER
    :accessor evaluation-number
    :initform 0
    :type integer)))

(defclass GENETIC-PLAN ()
  ((ECOSYSTEM
    :accessor ecosystem
    :initarg :ecosystem
    :type ecosystem)
   ;; see evolution-termination-p for the usage of the following two slots
   (GENERATION-LIMIT
    :accessor generation-limit
    :initarg :generation-limit
    :initform nil
    :type (or integer null)
    :documentation
    "If non-nil, it should be an integer, the final evaluation number.")
   (EVALUATION-LIMIT
    :accessor evaluation-limit
    :initarg :evaluation-limit
    :initform nil
    :type (or integer null)
    :documentation
    "If non-nil, it should be an integer, the final generation number.")))

(defclass POPULATION ()
  ((ECOSYSTEM
    :accessor ecosystem
    :initarg :ecosystem
    :type ecosystem)
   (ORGANISMS
    :accessor organisms
    :type vector)
   (SIZE
    :accessor size
    :initarg :size
    :initform nil
    :type (or fixnum null))
   (STATISTICS
    :accessor statistics
    :initarg :statistics
    :type population-statistics)))

(defclass GENERATIONAL-POPULATION (population)
  ())

;; The converged-p test in population_methods requires that one of the next two mixins be
;; combined with the class of population being used.  The functions are also useful in selection methods.
(defclass MAXIMIZING-SCORE-MIXIN ()
  ()
  (:documentation
   "Class to mix with population classes whose organism's score is better when larger.
An organisms score represents fitness or benefit."))

(defclass MINIMIZING-SCORE-MIXIN ()
  ()
  (:documentation
   "Class to mix with population classes whose organism's score is better when smaller.
An organisms score represents a cost or penalty."))

(defclass POPULATION-STATISTICS ()
  ((POPULATION
    :accessor population
    :initarg :population
    :type population)
   ;; the following slots are set by compute-statistics
   (SUM-SCORE
    :accessor sum-score
    :initarg :sum-score
    :type number)
   (AVG-SCORE
    :accessor avg-score
    :initarg :avg-score
    :type number)
   (MAX-SCORE
    :accessor max-score
    :initarg :max-score
    :type number)
   (MIN-SCORE
    :accessor min-score
    :initarg :min-score
    :type number)
   (MAX-ORGANISM
    :accessor max-organism
    :initarg :max-organism
    :type organism)
   (MIN-ORGANISM
    :accessor min-organism
    :initarg :min-organism
    :type organism)
   ;; the following slots are set by compute-normalized-statistics
   (SUM-NORMALIZED-SCORE
    :accessor sum-normalized-score
    :initarg :sum-normalized-score)
   (AVG-NORMALIZED-SCORE
    :accessor avg-normalized-score
    :initarg :avg-normalized-score)))

(defclass ORGANISM ()
  ((POPULATION
    :accessor population
    :initarg :population
    :initform 'nil
    :type (or population null))
   (GENOTYPE                            ;one or more chromosomes
    :accessor genotype
    :initarg :genotype
    :initform 'nil
    :type list)
   (SCORE
    :accessor score
    :initarg :score
    :initform 'nil)
   (NORMALIZED-SCORE
    :accessor normalized-score
    :initarg :normalized-score
    :initform 'nil)))

(defclass ORGANISM-PHENOTYPE-MIXIN ()
  ((PHENOTYPE                           ;the actual resulting organism
    :accessor phenotype
    :initarg :phenotype)))

(defclass CHROMOSOME ()
  ((ORGANISM
    :accessor organism
    :initarg :organism
    :initform 'nil
    :type (or organism null))
   (LOCI
    :accessor loci
    :initarg :loci
    :type array)))

(defclass BINARY-CHROMOSOME (chromosome)
  ())

(defclass SEQUENCE-CHROMOSOME (chromosome)
  ())

#|
(defclass MULTI-STRAND-CHROMOSOME (chromosome)
  ((CHROMPACK
    :accessor loci
    :initarg :loci
    :type (array chromosome *))))
|#

(defclass GRAY-CODE-TRANSLATION ()
  ((NUMBER-OF-BITS
    :accessor number-of-bits
    :initarg :number-of-bits  ; must be supplied either at instantiation or instance [re-]initialization time
    :type fixnum)
   (B2G-MAP
    :accessor b2g-map
    :type (array bit (* *)))
   (G2B-MAP
    :accessor g2b-map
    :type (array bit (* *)))))

