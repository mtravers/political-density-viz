#| 
- Read in us-state.json
- Calculate centers of each state
- Read in us-counties.json
- match each county to older county db
- if multiple matches, disambiguate using geo

|#


(defvar *x-states
      (mql-assocdr :features (json:decode-json-from-source #P"/misc/working/election/data/us-states.json")))

;;; List of ((:TYPE . "Feature") (:ID . "01001") (:PROPERTIES (:NAME . "Autauga")) (:GEOMETRY (:TYPE . "Polygon") (:COORDINATES ((-86.41178 32.70634) (-86.41178 32.410587) (-86.49941 32.344864) (-86.81708 32.33939) (-86.915665 32.662525) (-86.41178 32.70634)))))
(defvar *x-counties
  (mql-assocdr :features (json:decode-json-from-source #P"/misc/working/election/data/us-counties.json")))


(defun +get (prop tuple)
  (mql-assocdr prop tuple))

(defun +put (prop tuple value)
  (mt:aif (find prop tuple :key #'car :test #'mql-prop-eql)
	  (rplacd mt:it value)
	  (mt:push-end (cons prop value) tuple)))

(defun compute-center (+state)
  (let ((coordinates (+get :coordinates (+get :geometry +state))))
    (setq coordinates
	  (if (equal "MultiPolygon" (+get :type (+get :geometry +state)))
	      (apply #'append (mapcar #'car coordinates))
	      (car coordinates)))
    (+put :x-center +state (mt:average (mapcar #'car coordinates)))
    (+put :y-center +state (mt:average (mapcar #'cadr coordinates)))))
	   
(length (mapcar #'compute-center *x-states))

(length (mapcar #'compute-center *x-counties))

(defun state-counties (state-name)
  (let ((c (find state-name *by-county-electoral-results* :key #'car :test #'equal)))
    (mapcar #'car (cadr c))))

(defun find-state (+county)
  (let ((candidates (mt:filter #'(lambda (+state) 
				   (some #'(lambda (prospective-county-name)
					     (equal (+get :name (+get :properties +county))
						    prospective-county-name))
					 (state-counties (+get :name (+get :properties +state)))))
			       *x-states)))
    (case (length candidates)
      (1 (car candidates))
      (0 (error "No county matched ~A" +county))
      (otherwise (find-closest-match +county candidates)))))

(defun find-closest-match (county candidates)  
  (mt:minimize candidates :key #'(lambda (candidate) (distance county candidate))))

(defun distance (a b)
  (sqrt (+ (expt (- (+get :x-center a)(+get :x-center b)) 2)
	   (expt (- (+get :y-center a)(+get :y-center b)) 2))))


(mapc #'(lambda (+county)
	  (mt:report-and-ignore-errors 
	    (let ((state (find-state +county)))
	      (+put :state +county (+get :name (+get :properties state))))))
      *x-counties)


(with-open-file (s "/misc/working/election/data/us-counties-plus.json" :direction :output :if-exists :supersede)
  ;; +++ haven't tested this but som ekind of wrapping is necessary
  (json:encode-json `(("type" . "FeatureCollection") 
		      ("features" . ,*x-counties))
		    s))
