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

(defmethod SHARED-INITIALIZE :AFTER
           ((self population-statistics) slot-names &REST initargs)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp self 'sum-score)
    (compute-statistics self)))

(defmethod PRINT-OBJECT ((self population-statistics) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (if (slot-boundp self 'avg-normalized-score)
               (format nil "[avg-norm-score: ~,2F, avg-score: ~,2F]"
                       (avg-normalized-score self)
                       (avg-score self))
             (format nil "[converged: ~F]" (avg-score self)))
           stream)))

(defmethod COMPUTE-STATISTICS ((self population-statistics))
  (with-accessors ((size size)
                   (orgs organisms))
                  (population self)
    (with-accessors ((sum sum-score)
                     (max max-score)
                     (min min-score)
                     (max-org max-organism)
                     (min-org min-organism)) self
      (let* ((org0 (aref orgs 0))
             (score0 (score org0)))
        (setf sum (float score0)
              max score0
              min score0
              max-org org0
              min-org org0))
      (do* ((i 1 (1+ i))
            score-i)
           ((>= i size))
        (setq score-i (score (aref orgs i)))
        (incf sum score-i)
        (if (< max score-i) (setq max score-i
                                  max-org (aref orgs i)))
        (if (> min score-i) (setq min score-i
                                  min-org (aref orgs i))))
      (setf (avg-score self) (/ sum size)))))

(defmethod COMPUTE-NORMALIZED-STATISTICS ((self population-statistics)
                                          &AUX min max)
  (with-accessors ((size size)
                   (orgs organisms))
                  (population self)
    (with-accessors ((sum sum-normalized-score)) self
      (let ((score0 (normalized-score (aref orgs 0))))
        (setf sum (float score0)
              max score0
              min score0))
      (do* ((i 1 (1+ i))
            score-i)
           ((>= i size))
        (setq score-i (normalized-score (aref orgs i)))
        (incf sum score-i)
        (if (< max score-i) (setq max score-i))
        (if (> min score-i) (setq min score-i)))
      (setf (avg-normalized-score self) (/ sum size)))))

