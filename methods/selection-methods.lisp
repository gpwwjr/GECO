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

;;; Selection methods
;;; This file contains a sampling of selection methods.  None of them are guaranteed to be the best in the world,
;;; but some of them may prove useful as examples, or as a base upon which to build your own.

;; This first routine doesn't even pick based on score!!

(defmethod PICK-RANDOM-ORGANISM-INDEX ((p population))
  "Returns the index number of a random organism from the population."
  (geco-random-integer (size p)))

(defmethod PICK-RANDOM-ORGANISM ((p population))
  "Returns a random organism from the population."
  (aref (organisms p) (pick-random-organism-index p)))

;;=======================================================

(defun ROULETTE-PICK-RANDOM-WEIGHT-INDEX (weights-table &KEY invert-p
                                          &AUX (size (length weights-table)))
  "Select a random index into a table of weights, using the roulette wheel approach.
Assumes that the weights-table has been normalized to sum to 1.0"
  (do ((rand (geco-random-integer size))
       (partial-sum (if invert-p 1.0 0.0))
       (i 0 (1+ i)))
      ((or (>= i (1- size))
           (if invert-p
               (<= partial-sum rand)
             (>= partial-sum rand)))
       i)
    (if invert-p
        (decf partial-sum (aref weights-table i))
      (incf partial-sum (aref weights-table i)))))

(defmethod ROULETTE-PICK-RANDOM-ORGANISM-INDEX ((pop population) &AUX
                                                (invert-p (minimizing-p pop)))
  "Select a random organism number from the population, weighted by score, using the roulette wheel approach,
as used in DeJong's R1; also known as Brindle's stochastic sampling with replacement."
  ;; assumes that the population has been initialized,
  ;; and that (sum-normalized-score (statistics pop)) is valid [and all that implies]
  (do ((rand (geco-random-integer (size pop)))
       (partial-sum (if invert-p 1.0 0.0))
       (i 0 (1+ i)))
      ((or (>= i (1- (size pop)))
           (if invert-p
               (<= partial-sum rand)
             (>= partial-sum rand)))
       i)
    (if invert-p
        (decf partial-sum (normalized-score (aref (organisms pop) i)))
      (incf partial-sum (normalized-score (aref (organisms pop) i))))))

(defmethod ROULETTE-PICK-RANDOM-ORGANISM ((pop population))
  "Select a random organism from the population, weighted by score, using the roulette wheel approach,
as used in DeJong's R1; also known as Brindle's stochastic sampling with replacement."
  (aref (organisms pop)
        (roulette-pick-random-organism-index pop)))

;;=======================================================

#| The following technique is one of the most error-free selection techniques;
   however that doesn't mean it's one of the best. |#

(defmethod STOCHASTIC-REMAINDER-PRESELECT ((pop population) &KEY (multiplier 1) &AUX
                                           (pop-size (size pop))
                                                                                                                                 (invert-p (minimizing-p pop)))
  "Prepare and return a function of no arguments which will select random organisms
from the population, weighted by score, using a technique referred to by Brindle as
stochastic remainder selection without replacement."
  (let* ((num-choices (round (* pop-size multiplier)))
         (choices (make-array num-choices :element-type 'fixnum))
         (choice# 0)
         (fractions (make-array pop-size :element-type 'short-float))
         (organisms (organisms pop))
         (avg-normalized-score (avg-normalized-score (statistics pop))))
    (declare (dynamic-extent fractions))
    ;; First, assign whole numbers:
    (dotimes (organism# pop-size)
      (multiple-value-bind (expected frac)
                           (if invert-p
                               (truncate
                                (- 1.0 (* multiplier
                                          (normalized-score
                                           (aref organisms organism#))))
                                (- 1.0 avg-normalized-score))
                             (truncate (* multiplier
                                          (normalized-score
                                           (aref organisms organism#)))
                                       avg-normalized-score))
        (setf (aref fractions organism#) (float frac))
        (dotimes (i expected)
          (setf (aref choices choice#) organism#)
          (incf choice#))))
    ;; then assign fractional parts:
    (do ((organism# 0 (mod (1+ organism#) pop-size))
         (mult (/ 1.0 multiplier)))     ; used when the user consumes more organisms per use than it produces
        ((>= choice# num-choices))
      (when (and (> (aref fractions organism#) 0.0)
                 (< (geco-random-float 1.0) (aref fractions organism#)))
        (setf (aref choices choice#) organism#)
        (incf choice#)
        (decf (aref fractions organism#) mult)))
    ;; Return a function which will return organisms from the chosen, one at a time, until they are all gone
    ;; and will return NIL thereafter.
    #'(lambda (&AUX (choice# (if (> num-choices 0) (geco-random-integer num-choices))))
        (when choice#
          (decf num-choices)
          (prog1
            (aref organisms (aref choices choice#))   ; the function's result
            (setf (aref choices choice#) (aref choices num-choices)))))))

#| -- an example of using this function:
(let ((selector (stochastic-remainder-preselect some-population)))
  (do ((organism (funcall selector) (funcall selector)))
      ((null organism))
    (do-something-with organism)))

-- Note that all the storage held by the closure can be reclaimed as garbage
when the scope of the let binding the selector function is exited. 
|#

;;=======================================================

(defmethod RANKING-PRESELECT ((pop population) &KEY (multiplier 1) (max 2.0) &AUX
                              (min (- max (* 2.0 (- max 1.0))))
                              (pop-size (size pop))
                              (invert-p (minimizing-p pop)))
  "Prepare and return a function of no arguments which will select random organisms
from the population, weighted by score, using a ranking technique developed by Baker."
  (let* ((num-choices (round (* pop-size multiplier)))
         (choices (make-array num-choices))
         (choice# 0)
         (fractions (make-array pop-size :element-type 'short-float))
         (organisms (organisms pop))
         (sorted-organisms (sort (make-array pop-size :initial-contents organisms)
                                 #'> :key #'score))
         (delta (/ (- max min) (float (1- pop-size))))
         (allocation (if invert-p min max)))
    (declare (dynamic-extent fractions sorted-organisms))
    (block ASSIGNING
      ;; First, assign whole numbers:
      (dotimes (organism# pop-size)
        (multiple-value-bind (expected frac)
                             (truncate (* multiplier allocation))
          (setf (aref fractions organism#) (float frac))
          (dotimes (i expected)
            (setf (aref choices choice#) (aref sorted-organisms organism#))
            (incf choice#)
            (when (>= choice# num-choices)
              (return-from ASSIGNING))))
        (if invert-p
            (incf allocation delta)
          (decf allocation delta)))
      ;; then assign fractional parts:
      (do ((organism# 0 (mod (1+ organism#) pop-size))
           (mult (/ 1.0 multiplier)))     ; used when the user consumes more organisms per use than it produces
          ((>= choice# num-choices))
        (when (and (> (aref fractions organism#) 0.0)
                   (< (geco-random-float 1.0) (aref fractions organism#)))
          (setf (aref choices choice#) (aref sorted-organisms organism#))
          (incf choice#)
          (decf (aref fractions organism#) mult))))
    ;; Return a function which will return organisms from the chosen, one at a time, until they are all gone
    ;; and will return NIL thereafter.
    #'(lambda (&AUX (choice# (if (> num-choices 0) (geco-random-integer num-choices))))
        (when choice#
          (decf num-choices)
          (prog1
            (aref choices choice#)   ; the function's result
            (setf (aref choices choice#) (aref choices num-choices)))))))

;;=======================================================

(defmethod PICK-SOME-RANDOM-ORGANISM-INDICES ((p population) number-to-pick
                                                                                                                                          &AUX (pop-size (size p)))
  "Returns number-to-pick random organism indices for the population."
  (do ((indices nil)
       index)
      ((= number-to-pick (length indices))
       indices)
    (unless (member (setq index (geco-random-integer pop-size))
                    indices)
      (push index indices))))

(defmethod TOURNAMENT-SELECT-ORGANISM ((p population) tournament-size)
  "Pick tournament-size organisms from the population at random, and returns the best one."
  (do* ((indices (pick-some-random-organism-indices p tournament-size))
        (organisms (organisms p))
        (best-organism (aref organisms (pop indices)))
        (best-score (normalized-score best-organism))
        (better-than-test (better-than-test p))
        (organism (when indices ; allow tournament-size to be >= 1
                    (aref organisms (pop indices)))
                  (when indices
                    (aref organisms (pop indices))))
        (score (when organism ; allow tournament-size to be >= 1
                 (normalized-score organism))
               (when organism
                 (normalized-score organism))))
       ((null organism) best-organism)
    (when (funcall better-than-test score best-score)
      (setq best-organism organism
            best-score score))))

