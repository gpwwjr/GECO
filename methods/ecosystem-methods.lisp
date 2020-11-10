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

(defmethod SHARED-INITIALIZE :AFTER ((self ecosystem) slot-names &REST initargs
                                     &KEY
                                     plan-class pop-class pop-size
                                     generation-limit evaluation-limit)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp self 'population)
    (setf (population self)
          (make-population self pop-class
                           :size pop-size
                           :random t)))
  (unless (slot-boundp self 'plan)
    (setf (plan self) (make-genetic-plan self plan-class))
    (setf (generation-limit (plan self)) generation-limit)
    (setf (evaluation-limit (plan self)) evaluation-limit)))

(defmethod MAKE-POPULATION ((ecosystem ecosystem) pop-class &KEY size random)
  (make-instance pop-class
    :ecosystem ecosystem
    :size size
    :random random))

(defmethod MAKE-GENETIC-PLAN ((ecosystem ecosystem) plan-class)
  (make-instance plan-class :ecosystem ecosystem))

#-:no-tail-recursion-optimization
(defmethod EVOLVE ((self ecosystem))
  (evaluate self (plan self))
  (unless (evolution-termination-p (plan self))
    (incf (generation-number self))
    (regenerate (plan self) self)
    (evolve self)))

;; Here's an alternate definition of evolve for lisps which don't eliminate tail-recursion
#+:no-tail-recursion-optimization
(defmethod EVOLVE ((self ecosystem))
  (evaluate self (plan self))
  (do ()
      ((evolution-termination-p (plan self)))
    (incf (generation-number self))
    (regenerate (plan self) self)
    (evaluate self (plan self))))

(defmethod EVALUATE ((self ecosystem) (plan genetic-plan))
  (evaluate (population self) plan))

