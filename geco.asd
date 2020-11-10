;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

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

;;; to load GECO:
;; Make sure ASDF is installed on your system. See https://common-lisp.net/project/asdf/
;; (require :asdf)
;; Make sure that the directory containing the GECO files can be found by ASDF
;; (asdf:load-system :geco)  ; CLISP apparently requires using "geco" instead, but that's ugly


(defsystem "geco"
           :description "GECO: Genetic Evolution through Combination of Objects
A CLOS-based Framework for Prototyping Genetic Algorithms"
  :version "2.1"
  :author "George P. W. Williams, Jr"
  :mailto "george.p.williams@pobox.com"
  :licence "GPL 2.0"
  :components ((:file "packages")
               (:module "definitions"
                        :depends-on ("packages")
                        :components
                        ((:file "generics")
                         (:file "classes")))
               (:module "utilities"
                        :depends-on ("packages")
                        :components
                        ((:file "dbg")
                         (:file "random")))
               (:module "methods"
                        :depends-on ("packages" "definitions" "utilities")
                        :components
                        ((:file "chromosome-methods")
                         (:file "organism-methods")
                         (:file "selection-methods")
                         (:file "pop-stats-methods")
                         (:file "population-methods")
                         (:file "genetic-plan-methods")
                         (:file "ecosystem-methods")))
               #||
               (:static-file "allele-counts"
                      :depends-on ("packages" "definitions" "utilities"))
               (:module "sb"
                        :depends-on ("packages" "methods" "allele-counts")
                        (:static-file "sb-test"))
               (:module "ss"
                        :depends-on ("packages" "methods")
                        (:static-file "ss-test"))
               (:static-file "COPYING.LIB-2.0")
               (:static-file "geco.pdf")
               (:static-file "geco.asdf")   ; does this belong here?
               ||#
               ))
