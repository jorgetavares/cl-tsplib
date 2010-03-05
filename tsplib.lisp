;;;;
;;;; TSP parser
;;;;

(in-package #:cl-tsplib)

;;;
;;; definitions
;;;

(defstruct problem-instance
  name
  comment
  type
  dimension
  edge-weight-type
  edge-weight-format
  display-data-type
  nodes-coordinates
  distance-matrix
  ) 

;;;
;;; parse functions
;;;

(defun print-problem-instance (file)
  "Prints the TSPlib problem instance."
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
       while line do (format t "~a~%" line))))

(defun parse-problem-instance (file)
  "Parses a TSPlib problem instance."
  (let ((instance (make-problem-instance)))
    (with-open-file (stream file :direction :input)
      (loop 
	 for line = (trim (read-line stream nil nil))
	 until (node-coordinates-p line)
	 do (cond ((name-p         line) (parse-name         instance line))
		  ((comment-p      line) (parse-comment      instance line))
		  ((type-p         line) (parse-type         instance line))
		  ((dimension-p    line) (parse-dimension    instance line))
		  ((edge-type-p    line) (parse-edge-type    instance line))
		  ((edge-format-p  line) (parse-edge-format  instance line))
		  ((display-data-p line) (parse-display-data instance line))))
      (parse-node-coordinates instance stream)
      (setf (problem-instance-distance-matrix instance)
	    (compute-distance-matrix
	     (problem-instance-nodes-coordinates instance)
	     (problem-instance-dimension instance)
	     (get-distance-function 
	      (problem-instance-edge-weight-type instance)))))
    instance))

(defun trim (string)
  (let ((size (length string)))
    (if (char= #\  (aref string  (1- size)))
	(subseq  string 0 (1- size))
	string)))


;;
;; parse hearder
;;

(defun filter-start (line)
  "Remove the leadings _:_ or :_ (where _ is the white space) from a parsed line."
  (if (char= #\: (aref line 0))
      (subseq line 2)
      (subseq line 3)))

(defparameter *name* "NAME")

(defun name-p (line)
  (string= *name* line :end2 4))

(defun parse-name (instance line)
  "Checks if is the instance name and stores it."
  (setf (problem-instance-name instance) 
	(filter-start (subseq line (mismatch *name* line)))))

(defparameter *comment* "COMMENT")

(defun comment-p (line)
  (string= *comment* line :end2 7))

(defun parse-comment (instance line)
  "Checks if is the instance commment and stores it."
  (setf (problem-instance-comment instance) 
	(filter-start (subseq line (mismatch *comment* line)))))

(defparameter *type* "TYPE")
(defparameter *instance-types* (cons (cons "TSP" :tsp) '()))

(defun type-p (line)
  (string= *type* line :end2 4))

(defun parse-type (instance line)
  "Checks if is the instance type and stores it."
  (setf (problem-instance-type instance) 
	(cdr (assoc (filter-start (subseq line (mismatch *type* line)))
		    *instance-types* :test #'string=))))

(defparameter *dimension* "DIMENSION")

(defun dimension-p (line)
  (string= *dimension* line :end2 9))

(defun parse-dimension (instance line)
  "Checks if is the instance dimension and stores it."
  (setf (problem-instance-dimension instance)
	(read-from-string (filter-start (subseq line (mismatch *dimension* line))))))

(defparameter *edge-type* "EDGE_WEIGHT_TYPE")
(defparameter *edge-weight-types* (list (cons "EUC_2D" :euc-2d) 
					(cons "GEO" :geo)))

(defun edge-type-p (line)
  (string= *edge-type* line :end2 16))

(defun parse-edge-type (instance line)
  "Checks if is the instance edge weight type and stores it."
  (setf (problem-instance-edge-weight-type instance) 
	(cdr (assoc (filter-start (subseq line (mismatch *edge-type* line)))
		    *edge-weight-types* :test #'string=))))


(defparameter *edge-format* "EDGE_WEIGHT_FORMAT")
(defparameter *edge-weight-formats* (list (cons "FUNCTION" :function)))

(defun edge-format-p (line)
  (string= *edge-format* line :end2 18))

(defun parse-edge-format (instance line)
  "Checks if is the instance edge weight format and stores it."
  (setf (problem-instance-edge-weight-format instance) 
	(cdr (assoc (filter-start (subseq line (mismatch *edge-format* line)))
		    *edge-weight-formats* :test #'string=))))

(defparameter *display-data* "DISPLAY_DATA_TYPE")
(defparameter *display-data-types* (cons (cons "COORD_DISPLAY" :coord) '()))

(defun display-data-p (line)
  (string= *display-data* line :end2 17))

(defun parse-display-data (instance line)
  "Checks if is the instance display data type and stores it."
  (setf (problem-instance-display-data-type instance) 
	(cdr (assoc (filter-start (subseq line (mismatch *display-data* line)))
		    *display-data-types* :test #'string=))))

(defparameter *nodes-coordinates* "NODE_COORD_SECTION")

(defun parse-coordinate (line)
  "Returns the node, x and y coordinates."
  (let ((string line) (position 0))
    (loop repeat 3 collect 
	 (multiple-value-bind (value start)
	     (read-from-string string)
	   (setq string (subseq string start))
	   (setq position start) value))))

(defun node-coordinates-p (line)
  "Chaeck if is about to enter the nodes coordinates section."
  (string= *nodes-coordinates* line))

(defun parse-node-coordinates (instance stream)
  "Checks if is the coordinates section and reads them."
  (let ((coordinates (make-array 
		      `(,(+ 1 (problem-instance-dimension instance)) 2)
		      :initial-element 0)))
    (loop 
       for line = (read-line stream nil nil)
       until (string= line "EOF")
       while line do (let* ((values (parse-coordinate line))
			    (node (first values)) 
			    (x (second values)) (y (third values)))
		       (setf (aref coordinates node 0) x
			     (aref coordinates node 1) y)))
    (setf (problem-instance-nodes-coordinates instance) coordinates)))


;;;
;;; distance functions
;;;  

(defun print-distances (distances dimension)
  (loop for i from 0 to dimension
     do (loop for j from 0 to dimension
	   do (format t "~a " (aref distances i j))
	   finally (format t "~%"))))

(defun compute-distance-matrix (coordinates dimension fn-distance)
  "Computes the distances given the coordinates matrix and a distance function."
  (let ((distances (make-array `(,(+ 1 dimension) ,(+ 1 dimension)) 
			       :initial-element 0)))
    (loop for i from 1 to dimension 
       do (let ((X (node-coordinates i coordinates))) 
	    (loop for j from i to dimension 
	       do (let* ((Y (node-coordinates j coordinates))
			 (distance (funcall fn-distance 
					    (first X) (first Y)
					    (second X) (second Y)))) 
		    (setf (aref distances i j) distance
			  (aref distances j i) distance))))
       finally (return distances))))

(defun node-coordinates (node coordinates)
  "Return the x and y of a node."
  (list (aref coordinates node 0)
	(aref coordinates node 1)))

(defun get-distance-function (type)
  "Returns the correct distance function according to type."
  (case type
    (:euc-2d #'euclidian-distance)
    (:geo #'geo-distance)))

(defun nearest-integer (n)
  "Rouding function. Returns the nearest integer."
  (round n)) ; warning: might be a floor instead of round!

(defun euclidian-distance (xi xj yi yj &optional (zi 0) (zj 0))
  "Euclidian 2D/3D distance function."
  (let ((xd (- xi xj)) (yd (- yi yj)) (zd (- zi zj)))
    (nearest-integer (sqrt (+ (* xd xd) (* yd yd) (* zd zd))))))

(defun geo-distance (xi xj yi yj)
  "Geographical distance."
  (let ((earth-radius 6378.388))
    (multiple-value-bind (lati longi)
	(convert-coordinates xi yi)
      (multiple-value-bind (latj longj)
	  (convert-coordinates xj yj)
	(let ((q1 (cos (- longi longj)))
	      (q2 (cos (- lati latj)))
	      (q3 (cos (+ lati latj))))
	  (floor (1+ (* earth-radius 
			(acos (* 1/2
				 (- (* q2 (+ 1 q1))
				    (* q3 (- 1 q1)))))))))))))

(defun convert-coordinates (x y)
  "Convert to geographical latitude and longitude given in radians."
  (let* ((tsplib-pi 3.141592)
	 (degx (floor x))
	 (degy (floor y))
	 (minx (- x degx))
	 (miny (- y degy)))
    (values (/ (* tsplib-pi (+ degx (/ (* 5 minx) 3))) 180)    ; latitude
	    (/ (* tsplib-pi (+ degy (/ (* 5 miny) 3))) 180)))) ; longitude








