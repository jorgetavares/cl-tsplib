(defpackage #:cl-tsplib-system 
  (:use #:cl #:asdf))  
 
(in-package #:cl-tsplib-system)  
 
(defsystem :cl-tsplib
  :description "cl-tsplib: a parser for the TSPlib problem instances."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :components ((:file "package")  
               (:file "tsplib" :depends-on ("package"))))
