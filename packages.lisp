;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;; This is a work in progress- converting MK's defsystem definition to ASDF

(in-package "COMMON-LISP-USER")
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

;;; There are a bunch of CCL-2 conditionalizations in this file. I don't know if they still make sense because
;;; I don't have a working MCL/CCL-2 system any more. But I left them in, just in case they might help.

#+:ccl-2 (setq ccl:*fasl-save-definitions* t)

;; define the package for all GECO code:

(defpackage GECO
  (:use "COMMON-LISP"
        #+:ccl-2 "CCL")
  (:nicknames "GA")
  (:EXPORT "2X-CROSS-CHROMOSOMES" "2X-CROSS-ORGANISMS" "ALLELE-CODE-TO-VALUE" "ALLELE-VALUES"
           "AS-GOOD-AS-TEST" "AVG-NORMALIZED-SCORE" "AVG-SCORE" "B2G-MAP"
           "BEST-ORGANISM" "BEST-ORGANISM-ACCESSOR"
           "BETTER-THAN-TEST" "BIN2GRAY" "BINARY-CHROMOSOME" "CHROMOSOME"
           "CHROMOSOME-CLASSES" "COMPILE-GECO"
           "COMPUTE-BINARY-ALLELE-STATISTICS"
           "COMPUTE-NORMALIZED-STATISTICS" "COMPUTE-STATISTICS"
           "CONVERGED-P" "CONVERGENCE-FRACTION"
           "CONVERGENCE-THRESHOLD-MARGIN" "COPY-CHROMOSOME"
           "COPY-CHROMOSOME-WITH-SCORE" "COPY-ORGANISM"
           "COPY-ORGANISM-WITH-SCORE" "COUNT-ALLELE-CODES"
           "CROSS-CHROMOSOMES" "CROSS-ORGANISMS" "DBG" "DBG-P" "DBG?"
           "DBGO" "DECODE" "DECODE-BINARY-LOCI-VALUE" "ECOSYSTEM"
           "EDIT-GECO" "EIDETIC" "EVALUATE" "EVALUATION-LIMIT"
           "EVALUATION-NUMBER" "EVOLUTION-TERMINATION-P" "EVOLVE" "G2B-MAP"
           "GECO-RANDOM-FLOAT" "GECO-RANDOM-INTEGER" "GENERATION-LIMIT"
           "GENERATION-NUMBER" "GENERATIONAL-POPULATION" "GENETIC-PLAN"
           "GENOTYPE" "GENOTYPE-PRINTABLE-FORM" "GRAY-CODE-TRANSLATION"
           "GRAY2BIN" "HAMMING-DISTANCE" "LOCI" "LOCI-PRINTABLE-FORM"
           "LOCUS" "LOCUS-ARITY" "LOCUS-PRINTABLE-FORM" "MAKE-CHROMOSOME"
           "MAKE-CHROMOSOMES" "MAKE-GENETIC-PLAN" "MAKE-LOCI-VECTOR"
           "MAKE-ORGANISM" "MAKE-ORGANISMS" "MAKE-POPULATION"
           "MAKE-POPULATION-STATISTICS" "MAKE-ORGANISMS-VECTOR"
           "MAX-ORGANISM"
           "MAX-SCORE" "MAXIMIZING-P" "MAXIMIZING-SCORE-MIXIN"
           "MIN-ORGANISM" "MIN-SCORE"
           "MINIMIZING-P" "MINIMIZING-SCORE-MIXIN" "MUTATE-CHROMOSOME"
           "MUTATE-ORGANISM" "NORMALIZE-SCORE" "NORMALIZED-SCORE"
           "NUMBER-OF-BITS" "ORGANISM" "ORGANISM-CLASS"
           "ORGANISM-PHENOTYPE-MIXIN" "ORGANISMS" "PARK-MILLER-RANDOMIZER"
           "PHENOTYPE" "PICK-RANDOM-ALLELE"
           "PICK-RANDOM-ALLELES" "PICK-RANDOM-CHROMOSOME"
           "PICK-RANDOM-CHROMOSOME-INDEX" "PICK-RANDOM-LOCUS-INDEX" "PICK-RANDOM-ORGANISM"
           "PICK-RANDOM-ORGANISM-INDEX" "PICK-SOME-RANDOM-ORGANISM-INDICES"
           "PLAN" "PMX-CROSS-CHROMOSOMES" "PMX-CROSS-ORGANISMS"
           "POPULATION" "POPULATION-STATISTICS"
           "POPULATION-STATISTICS-CLASS" "PRINTABLE-ALLELE-VALUES"
           "R3-CROSS-CHROMOSOMES" "R3-CROSS-ORGANISMS"
           "RANDOM-FLOATING-POINT-NUMBER" "RANDOM-INTEGER"
           "RANDOMIZE-CHROMOSOMES" "RANKING-PRESELECT" "REGENERATE"
           "ROULETTE-PICK-RANDOM-ORGANISM"
           "ROULETTE-PICK-RANDOM-ORGANISM-INDEX"
           "ROULETTE-PICK-RANDOM-WEIGHT-INDEX" "SCORE" "SCRAMBLE-ALLELES"
           "SEQUENCE-CHROMOSOME" "SIZE" "STATISTICS"
           "STOCHASTIC-REMAINDER-PRESELECT" "SUM-NORMALIZED-SCORE"
           "SUM-SCORE" "SWAP-ALLELES" "TOURNAMENT-SELECT-ORGANISM" "UNDBG"
           "UNIFORM-CROSS-CHROMOSOMES" "UNIFORM-CROSS-ORGANISMS"
           "WORST-ORGANISM" "WORST-ORGANISM-ACCESSOR")
  )

;; and a package for applications:

(defpackage GECO-USER
    (:use "COMMON-LISP"
          #+:ccl-2 "CCL"
          "GECO")
    (:nicknames "GU"))

;;; Configure conditional compilation options:

;; If your Lisp compiler does not support tail-recursion optimization,
;; you should enable the following feature:
;; (pushnew :NO-TAIL-RECURSION-OPTIMIZATION *features*)

;; Comment out one of the two following lines to select which random
;; number generator to use (see random.lisp).
;; (pushnew :GECO-PARK-MILLER-RANDOM *features*)
(pushnew :GECO-STANDARD-CL-RANDOM *features*)

;; The following two forms assure that only one random number generator is compiled.
;; See random.lisp for details.
#+:geco-standard-cl-random
(setq *features* (remove :geco-park-miller-random *features*))
#+:geco-park-miller-random
(setq *features* (remove :geco-standard-cl-random *features*))


#||

#+:ccl-2 (eval-enqueue '(in-package :geco))

;;; code to construct the :export clause of the GECO defpackage form

;; Display the info
(progn
  (format t "~%Package ~10TSymbol ~60TAccess'ty  FuncInfo  VarInfo  Class")
  (with-package-iterator (generator 'geco :internal :external)
    (do ((mv-list (multiple-value-list (generator))
                  (multiple-value-list (generator))))
        ((not (first mv-list)))
      (format t "~%~A ~10T~A, ~60T~S ~S  ~S  ~S"
              (package-name (fourth mv-list))
              (second mv-list)
              (third mv-list)
              (function-information (second mv-list))
              (variable-information (second mv-list))
              (find-class (second mv-list) nil))))
  (values))

;; Return the info

(with-package-iterator (generator 'geco :internal :external)
  (do ((mv-list (multiple-value-list (generator))
                (multiple-value-list (generator)))
       symbols)
      ((not (first mv-list))
       (cons :export (sort (mapcar #'string symbols) #'string-lessp)))
    (when (or (function-information (second mv-list))
              (find-class (second mv-list) nil))
      (push (second mv-list) symbols))))
||#
