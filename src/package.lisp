;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; package.lisp
;;;
;;; NAME
;;; package
;;;
;;; DESCRIPTION
;;; Package definition of BP1.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-01
;;;
;;; $$ Last modified:  00:05:12 Sun Mar  2 2025 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :bp1
  (:use :common-lisp)
  (:nicknames :bp)
  (:import-from
   :alexandria
   :assoc-value
   :alist-hash-table
   :hash-table-alist
   :hash-table-keys
   :read-file-into-string)
  (:import-from
   :cl-ppcre
   :split))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
