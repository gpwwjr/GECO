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

;;; Just because the documentation describes a generic function doesn't mean there's a defgeneric.

;;; A method for the following generic function must be defined for instantiable subclasses of POPULATION:
(defgeneric ORGANISM-CLASS (population)
  (:documentation
   "Returns the class of organisms which are expected in the population.
Typically this is a class allocated slot on the instantiated class."))

;;; A method for the following generic function should be redefined for instantiable subclasses of GENETIC-PLAN:
(defgeneric REGENERATE (genetic-plan thing)
  (:documentation
   "Returns a new, regenerated thing.  Default methods are provided for ecosystems and populations."))

;;; A method for the following generic function must be defined for instantiable subclasses of ORGANISM:
(defgeneric CHROMOSOME-CLASSES (organism)
  (:documentation
   "Returns a list of classes of chromosomes for the organism.  The size of this list determines the
number of chromosomes.  Typically this is a class allocated slot on the instantiated class."))

;;; A method for the following generic function must be defined for instantiable subclasses of CHROMOSOME:
(defgeneric EVALUATE (thing plan)
  (:documentation
   "Determine the score of the thing, be it ecosystem, population, or organism,
with respect to plan."))
;; Default methods are predefined for ecosystem & population, but the
;; method for subclasses organism is necessarily application specific.

;;; A method for the following generic function must be defined for instantiable subclasses of CHROMOSOME:
(defgeneric LOCUS-ARITY (chromosome locus-index)
  (:documentation
   "Returns the number of allele values which may be used at this locus of this chromosome."))
;; Suitable methods are provided for the common class BINARY-CHROMOSOME.
;; Values in actual loci are generally non-negative integers.  Most frequently they are 0 and 1.

;;; A method for the following generic function must be defined for instantiable subclasses of CHROMOSOME:
(defgeneric PRINTABLE-ALLELE-VALUES (chromosome locus-index)
  (:documentation
   "Returns a vector of characters which represents the possible values
of the allele at the indicated locus of the chromosome."))

;;; A method for the following generic function must be defined for instantiable subclasses of CHROMOSOME:
(defgeneric ALLELE-VALUES (chromosome locus-index)
  (:documentation
   "Returns a vector of allele values which represents the possible values of the allele at the indicated locus of the chromosome."))

;;; A method for the following generic function must be defined for instantiable subclasses of CHROMOSOME:
(defgeneric SIZE (thing)
  (:documentation
   "Returns the size the thing.  Typically this is a class allocated slot on the instantiated class."))
;; There are methods predefined on some classes.

;;; A method for the following generic function must be defined for instantiable subclasses of ORGANISM-PHENOTYPE-MIXIN:
(defgeneric DECODE (organism)
  (:documentation
   "Convert an organism's chromosomal representation (it's genotype) into it's phenotype,
i.e., what the genotype is intended to represent, and save it in the phenotype slot of the organism.
This may or may not be of practical value to a particular implementation.  The function is invoked immediately
before the organism is evaluated."))

;;; End of generic functions for which methods must/should be defined for most application GAs.


;;; The following generic functions are intended for storage management.
#|
Storage management can be very important for good performance of genetic algorithms. Default methods are
defined which simply create the necessary objects, but it is probably worth considering providing specialized
methods for application classes which use some kind of resource management strategy. Look at the tools
provided in the files resources.lisp and resource-mgt.lisp
|#

(defgeneric MAKE-GENETIC-PLAN (ecosystem genetic-plan-class)
  (:documentation
   "Returns an instance of genetic-plan-class which will be used by ecosystem."))

(defgeneric MAKE-POPULATION (ecosystem population-class &KEY size)
  (:documentation
   "Returns an instance of population-class which will be a part of ecosystem.  If size is specified,
the population's organism vector and organisms will be created."))

(defgeneric MAKE-ORGANISMS-VECTOR (population size)
  (:documentation
   "Returns the object (usually a vector) which will contain the organisms for the population."))

(defgeneric MAKE-POPULATION-STATISTICS (population)
  (:documentation
   "Returns an instance of (possibly a subclass of) population-statistics which will be
used for population."))

(defgeneric MAKE-ORGANISM (population &KEY random no-chromosomes)
  (:documentation
   "Returns an instance of organism-class, or the appropriate subclass, for population.
The organisms chromosomes will be initialized to random values if the keyword random has a non-nil value."))

(defgeneric MAKE-ORGANISMS (population &KEY random)
  (:documentation
   "Creates all the organisms for the specified population, storing them in the organisms vector.
The organism instances are created using make-organism, using the value of the random keyword argument.
The organisms chromosomes will be initialized to random values if the keyword random has a non-nil value.
The value returned by this function is not defined."))

(defgeneric MAKE-CHROMOSOMES (organism &KEY random)
  (:documentation
   "Returns a list instances of the appropriate subclasses of chromosome-class for organsm. The
chromosome's sizes are the default size for each instance, and the chromosome's allele's will be initialized to
random values if the keyword random has a non-nil value."))

(defgeneric MAKE-CHROMOSOME (organism chromosome-class &KEY size random)
  (:documentation
   "Returns an instance of chromosome-class which will be a part of organsm.  The chromosome's size
is specified by the keyword size, and the chromosome's allele's will be initialized to random values if the keyword
random has a non-nil value."))

(defgeneric MAKE-LOCI-VECTOR (chromosome size &KEY random)
  (:documentation
   "Returns the object (usually a vector) which will contain the loci
for the chromosome.
If random is non-nil, the loci are initialized to random alleles."))

;;; I still need to create some generics to release the storage.


;;; Other generic functions

(defgeneric NORMALIZE-SCORE (thing plan)
  (:documentation
   "Normalize the score for a population or organism, according to the
genetic plan plan, and revise the population statistics if necessary."))
;; Default methods are provided on the classes population which normalizes each organism,
;; and on organism, which normazlizes it based on the statistics.

(defgeneric COPY-CHROMOSOME (chromosome owner-organism)
  (:documentation
   "Creates a copy of chromosome, which will belong to owner-organism.  The score
is not copied, forcing the organism to be EVALUATEd."))

(defgeneric COPY-CHROMOSOME-WITH-SCORE (chromosome owner-organism)
  (:documentation
   "Creates a copy of chromosome, including its score, which will belong to owner-organism."))

(defgeneric COPY-ORGANISM (organism &KEY new-population)
  (:documentation
   "Creates a copy of organism, which will optionally belong to new-population."))

(defgeneric EVOLVE (thing)
  (:documentation
   "Create new generations until a termination condition is reached."))

(defgeneric COMPUTE-STATISTICS (thing)
  (:documentation
   "Compute the statistics for the relevent thing."))
;; Default methods are provided for population and population-statistics.

(defgeneric EVOLUTION-TERMINATION-P (plan)
  (:documentation
   "A predicate which returns true (non-NIL) when evolution should terminate.
A default method is provided on the class genetic-plan which returns T when the
generation-number of the ecosystem reaches the generation-limit of the plan,
or when the evaluation-number of the ecosystem reaches the evaluation-limit of the plan,
or when the population has converged, based on the converged-p predicate."))

(defgeneric CONVERGED-P (population)
  (:documentation
   "A predicate which returns true (non-NIL) when the population is converged."))
#|
A default method is provided on the class population which defines convergence as either of the following:
 1. The entire population is converged to a single score; or
 2. At least convergence-fraction of the current population has a normalized score which is
    as good as convergence-threshold-margin of the population.
 Here, "as good as" is determined using the function returned by the as-good-as-test generic function of
 an organism (the 0th one) in the population.
|#

(defgeneric CONVERGENCE-FRACTION (population)
  (:documentation
   "Returns a number in the range (0,1] which indicates the portion of the population which has converged
to the current best score in the population."))

(defgeneric CONVERGENCE-THRESHOLD-MARGIN (population)
  (:documentation
   "Returns a number in the range (0,1] which indicates the fraction of the current population's best score
which the convergence-fraction of the population must be as good as to be converged."))

(defgeneric POPULATION-STATISTICS-CLASS (population)
  (:documentation
   "Returns the class of the population-statistics instance to be used with the population."))
;; A default method is provided which returns population-statistics.

(defgeneric EIDETIC (thing1 thing2)
  (:documentation
   "Predicate, true if both things are the same class and have the same genetic makeup."))
#|
I have been questioned regarding the use of the term eidetic, above.
From Webster's Third New International Dictionary of the English Language, Unabridged:
eidetic: of, relating to, or having the characteristics of eide, essences, forms, or images.
Further: eide: plural of eidos, and eidos: something that is seen or intuited: a) in Platonism: idea,
b) in Aristotelianism (1): form, essence (2): species.
Thus, eidetic can be used to indicate 'of the same species,' which is
the essence of my original intent.
|#

#|
Internal to GECO, chromosomes contain allele codes at each locus. These codes are (typically) integers in the
range [0..<locus-arity>), which is often simply a single bit, i.e., [0..1]. When printing the chromosome, it is
desirable to convert these codes to what I'm calling allele values, which are single characters which may be used
to construct a string which will (hopefully) be more meaningful to the user.
|#

(defgeneric ALLELE-CODE-TO-VALUE (chromosome locus-index allele-code)
  (:documentation
   "Returns the allele which would be represented by allele-code for the given locus-index on chromosome."))

