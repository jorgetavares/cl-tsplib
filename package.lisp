;;;;
;;;; TSPlib parser package definition
;;;;

(defpackage #:cl-tsplib
  (:use #:cl)
  (:export #:print-problem-instance 
	   #:parse-problem-instance
	   #:euclidian-distance
	   #:pseudo-euclidian-distance
	   #:geo-distance
	   #:problem-instance
	   #:make-problem-instance
	   #:problem-instance-name
	   #:problem-instance-comment
	   #:problem-instance-type
	   #:problem-instance-dimension
	   #:problem-instance-edge-weight-type
	   #:problem-instance-nodes-coordinates
	   #:problem-instance-distance-matrix
	   #:list-nearest-neighbors
	   #:*eil51*
	   #:*burma14*
	   #:*kroA100*
	   #:*d198*
	   #:*lin318*
	   #:*pcb442*
	   #:*att532*))


