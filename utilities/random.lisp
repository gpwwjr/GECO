;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: GECO; Base: 10 -*-

(in-package :GECO)
#|
Genetic Evolution through Combination of Objects (GECO)

Copyright (C) 1993,2020  George P. W. Williams, Jr.

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

#+:GECO-STANDARD-CL-RANDOM
(let ((state *random-state*))

  (defun GECO-RANDOM-FLOAT (number)
    "Accept a positive floating-point number and returns a number of the same kind between zero (inclusive)
and the number (exclusive)."
    (random (float number) state))
  
  (defun GECO-RANDOM-INTEGER (number)
    "Accept a positive integer and returns a number of the same kind between zero (inclusive) and
the number (exclusive)."
    (declare (integer number))
    (random number state))
  
  ;; The following two functions allow external access to GECO's random state.
  ;; This makes it possible for the random state to be recorded/initialized to reproduce runs.
  
  (defun SET-GECO-RANDOM-STATE (new-state)
    (setq state new-state))
  
  (defun GECO-RANDOM-STATE ()
    state)

  )

;;; The following implementation of the Park-Miller multiplicative congruential randomizer
;;; is adapted with permission from John Koza's simple genetic programming example.
;;; Copyright (c) John Koza, All rights reserved.

#+:GECO-PARK-MILLER-RANDOM
(let ((seed (coerce (random 1.0) 'double-float)))
  ;; The seed for the Park-Miller congruential randomizer.
  
  (defun park-miller-randomizer ()
    "The Park-Miller multiplicative congruential randomizer
   (CACM, October 88, Page 1195).  Creates pseudo random floating
   point numbers in the range 0.0 < x <= 1.0.  The seed value
   for this randomizer is called seed, so you should
   record/set this if you want to make your runs reproducible."
    (let ((multiplier 16807.0d0);16807 is (expt 7 5)
          (modulus 2147483647.0d0))
      ;2147483647 is (- (expt 2 31) 1)
      (let ((temp (* multiplier seed)))
        (setf seed (mod temp modulus))
        ;;Produces floating-point number in the range
        ;;  0.0 < x <= 1.0
        (/ seed modulus))))
  
  (defun park-miller-random-floating-point-number (n)
    "Returns a pseudo random floating-point number
                in range 0.0 <= number < n"
    (let ((random-number (park-miller-randomizer)))
      ;; We subtract the randomly generated number from 1.0
      ;; before scaling so that we end up in the range
      ;; 0.0 <= x < 1.0, not 0.0 < x <= 1.0
      (* n (- 1.0d0 random-number))))
  
  (defun park-miller-random-integer (n)
    "Returns a pseudo-random integer in the range 0 ---> n-1."
    (let ((random-number (park-miller-random-floating-point-number 1.0)))
      (floor (* n random-number))))
  
  (defun GECO-RANDOM-FLOAT (number)
    "Accept a positive floating-point number and returns a number of the same kind between zero (inclusive)
and the number (exclusive)."
    (declare (float number))
    (park-miller-random-floating-point-number number))
  
  (defun GECO-RANDOM-INTEGER (number)
    "Accept a positive integer and returns a number of the same kind between zero (inclusive) and
the number (exclusive)."
    (declare (integer number))
    (park-miller-random-integer number))
  
  ;; The following two functions allow external access to GECO's random state.
  ;; This makes it possible for the random state to be recorded/initialized to reproduce runs.
  
  (defun SET-GECO-RANDOM-SEED (new-seed)
    (setq seed new-seed))
  
  (defun GECO-RANDOM-SEED ()
    seed)
  
  )
