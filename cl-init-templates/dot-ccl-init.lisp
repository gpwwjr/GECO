;;; .cl-init.lisp (could also be named cl-init.lisp)
;;; initialization file for Clozure Common Lisp
;;; George Williams

(format t "home:ccl-init loading ... ")

;;; Logical Hosts
(format t "defining logical hosts ... ")
;; Clozure CL sets up logical pathname translations for two logical hosts: ccl and home
;;  ccl  is set to the value of the environment variable
;;       CCL_DEFAULT_DIRECTORY, if that variable exists. Otherwise, it is set
;;       to the directory containing the heap image file.
;;  home refers to the user's home directory.
(setf (logical-pathname-translations "GECO")
      '(("**;*.*.*" "HOME:lisp;geco;**;")))  ;; <---- customize this as necessary
;(setf (logical-pathname-translations "QUICKLISP")
;      '(("**;*.*.*" "HOME:Quicklisp;**;")))

;; to find the physical pathname eqiuvalent, use something like:
;;    (cl:translate-logical-pathname "GECO:") 


;;; ASDF (pre-built into CCL)
(format t "setting up ASDF ... ")
(require :asdf)
(setf asdf:*central-registry* nil) ;; so this file can be reloaded
(setf asdf:*central-registry*      ;; NIL by default
      ;; Default directories, usually just the ``current directory''
      '(*default-pathname-defaults*  ;; #P"" by default

	;; Additional places where ASDF can find
	;; system definition files
	;; #p"/home/foo/lisp/systems/"
	;; #p"/usr/share/common-lisp/systems/"
	))

;; Quicklisp
;(format t "setting up Quicklisp ... ")
;(load "QUICKLISP:setup.lisp")


;;; GECO development related
(format t "setting up GECO ... ")
;; ASDF
(push #P"/Users/george/lisp/geco/" asdf:*central-registry*)  ;; <---- customize this as necessary
;; geco-setup- convenience function to put ccl in geco-dev/ and load geco
(progn
  (in-package :cl-user)
  (defun geco-setup ()
    (ccl::cd "~/lisp/geco")  ;; <---- customize this as necessary
    (asdf:load-system :geco)
    (ccl:current-directory)
    (in-package :GECO-USER)
    (format t "~%in package GECO-USER"))
  (format t "~%... (geco-setup) is defined"))

(format t "~%... home:ccl-init loaded~%")
