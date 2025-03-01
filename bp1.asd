;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; bp1.asd
;;;
;;; NAME
;;; system
;;;
;;; DESCRIPTION
;;; System definition of BP1.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-01
;;;
;;; $$ Last modified:  00:06:21 Sun Mar  2 2025 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defsystem "bp1"
  :description "Common Lisp algorithmic text generation tools."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>, Fabian Bentrup"
  :license "GPL Version 2.0 or later"
  :serial t
  ;; :in-order-to ((test-op (test-op "colporter/tests")))
  :depends-on ("alexandria"
               "cl-ppcre"
               "cologne-phonetics"
               "soundex"
               ;; "cl-yaml"
               )
  :pathname "src/"
  :components ((:file "package")
               ;; to be cont'd...
               ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF bp1.asd
