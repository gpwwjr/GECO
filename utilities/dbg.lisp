;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: GECO; Base: 10 -*-

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

(defvar *DBG-FLAGS* nil
  "Either a list of flags controlling debugging code, or T for globally on, or NIL for globally off.")

;;; Currently defined flags are documented in the file dbg-flags
;;; so modifying the documentation won't force recompilation.

;; This is the routine used in code to test for debugging options:

(defun DBG-P (&REST flags)
  "If *DBG-FLAGS* is not a list, return its value, else test for presence of any of FLAGS on the *DBG-FLAGS* list."
  (and *DBG-FLAGS*
       (if (listp *DBG-FLAGS*)
	   ;; *DBG-FLAGS* is a list, search it for any of the FLAGS
	   (do* ((flag-list flags (rest flag-list))
		 (f (first flag-list) (first flag-list)))
	       ;; termination condition:
	       ((or (null flag-list)
		    (member f *DBG-FLAGS* :test #'eq))
		;; if done & flag-list isn't null, we found something
		flag-list))		;returned value
	   ;; *DBG-FLAGS* is non-null, but not a list; might as well return it.
	   *DBG-FLAGS*)))

;; Make adding debugging output more concise (and flexible).

(defun DBGO (fmt-string &REST fmt-args)
  (apply #'format *debug-io* fmt-string fmt-args))

;; The rest of these routines are (primarily) for interactive use.

(defun DBG (&REST flags)
  "If there are no FLAGS, then turn on all debug code, else add each FLAG to the *DBG-FLAGS* list."
  (if flags
      (if (eq *dbg-flags* t)
        (format t "~&  >> *dbg-flags* = T")
        (dolist (flag flags)
	  (setq *DBG-FLAGS* (adjoin flag *DBG-FLAGS* :test #'eq))))
    (setq *DBG-FLAGS* t))
  (format *debug-io* "~&Debug flags: ~S" *DBG-FLAGS*)
  (values))

(defun UNDBG (&REST flags)
  "If there are no FLAGS, then turn off all debug code, else remove each FLAG from the *DBG-FLAGS* list."
  (if flags
      (dolist (flag flags)
	(setq *DBG-FLAGS* (delete flag *DBG-FLAGS* :test #'eq)))
    (setq *DBG-FLAGS* nil))
  (format *debug-io* "~&Debug flags: ~S" *DBG-FLAGS*)
  (values))

(defun DBG? ()
  "Tell me what debugging flags are active."
  (format *debug-io* "~&Debug flags: ~S" *DBG-FLAGS*)
  (values))

