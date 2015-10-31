;;; https://github.com/mtravers/cl-mql

(ql:quickload :cl-mql)

(defun uncounty (s)
  (if (search "County" s)
      (subseq s 0 (- (length s) 7))
      s))

(defun state-counties-raw-mql (state)
	 (mql-read
	  `((("/location/location/area")
	    ("/location/location/containedby"
	     (("id") ("name" . ,state)))
	    ("/location/statistical_region/population"
	     (  ("number") ("optional" . T)
		("type" . "/measurement_unit/dated_integer")
		("year" . (("value" . "2010")))
		))
	    ("id")
	    ("limit" . 500)
	    ("name")
	    ("type" . "/location/us_county")))))  

(defun state-counties (state)
  (let ((raw (state-counties-raw-mql state)))
    (print `(,(length raw) counties in ,state))
    (mapcar #'(lambda (raw-c)
		`(,(identity (mql-assocdr "name" raw-c));uncounty
		   ,(mql-assocdr "/location/location/area" raw-c)
		   ,(mql-assocdr "number" (car (mql-assocdr "/location/statistical_region/population" raw-c)))))
	    raw)))

(defun mql-get-all ()
  (mt:collecting
    (dolist (s *states*)
      (mt:collect (list s (state-counties s))))))
