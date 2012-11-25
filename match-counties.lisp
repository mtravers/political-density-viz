#| 
- Read in us-state.json
- Calculate centers of each state
- Read in us-counties.json
- match each county to older county db
- if multiple matches, disambiguate using geo

|#


(setq *x-states
      (mql-assoc :features (json:decode-json-from-source #P"/misc/working/election/us-states.json")))

(defun +get (prop tuple)
  (mql-assoc prop tuple))

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
	   
(mapcar #'compute-center *x-states)

(mapcar #'compute-center *x-counties)

(setq *x-counties
      (mql-assoc :features (json:decode-json-from-source #P"/misc/working/election/us-counties.json")))

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
    (pprint candidates)
    (case (length candidates)
      (1 (car candidates))
      (0 (error "No county matched ~A" +county))
      (otherwise (find-closest-match +county candidates)))))

(defun find-closest-match (county candidates)  
  (mt:minimize candidates :key #'(lambda (candidate) (distance county candidate))))

(defun distance (a b)
  (sqrt (+ (expt (- (+get :x-center a)(+get :x-center b)) 2)
	   (expt (- (+get :y-center a)(+get :y-center b)) 2))))
