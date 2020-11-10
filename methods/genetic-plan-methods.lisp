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

;; regeneration for standard generational plans

(defmethod REGENERATE ((plan genetic-plan) (ecosys ecosystem))
  (setf (population ecosys)
        (regenerate plan (population ecosys))))

(defmethod REGENERATE ((plan genetic-plan) (old-pop generational-population)
                       &AUX (pop-size (size old-pop))
                       (new-pop (make-population (ecosystem old-pop)
                                                 (class-of old-pop)
                                                 :size pop-size))
                       (new-organisms (organisms new-pop)))
  "A rather dumb default method, which should be replaced by one that
creates a new population from the previous one, using genetic operators,
e.g., selection, mutation, and crossover."
  (setf (ecosystem new-pop) (ecosystem old-pop))
  (dotimes (i pop-size)
    (setf (aref new-organisms i) (pick-random-organism old-pop)))
  new-pop)

(defmethod EVOLUTION-TERMINATION-P ((self genetic-plan))
  (or (and (evaluation-limit self)
           (>= (evaluation-number (ecosystem self))
               (evaluation-limit self)))
      (and (generation-limit self)
           (>= (generation-number (ecosystem self))
               (generation-limit self)))
      (converged-p (population (ecosystem self)))))

;; methods to handle weighted selection are in the file selection-methods

