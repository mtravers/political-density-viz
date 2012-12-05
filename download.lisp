(ql:quickload '(:drakma :cl-json :wuwei :cl-html-parse))


#|
(net.html.parser:parse-html
 (drakma:http-request "http://www.politico.com/2012-election/results/president/alabama/"))
|#

(defun html-find-elements (from pred)
  (mt:collecting 
    (labels ((html-select-elements-1 (from)
	       (if (funcall pred from)
		   (mt:collect from)
		   (mapc #'html-select-elements-1 (html-subelements from)))))
      (html-select-elements-1 from))))

(defun html-find-element (from pred)
  (labels ((html-find-element-1 (from)
	     (if (funcall pred from)
		 (return-from html-find-element from)
		 (mapc #'html-find-element-1 (html-subelements from)))))
    (html-find-element-1 from)))

(defun html-subelements (elt)
  (and (listp elt)
       (cdr elt)))

(defun html-tag (elt)
  (cond ((listp elt)
	 (if (listp (car elt))
	     (caar elt)
	     (car elt)))
	((keywordp elt) elt)
	(t nil)))

(defun html-attribute (elt att)
  (and (listp elt)
       (listp (car elt))
       (cadr (member att (car elt)))))

(defun html-class? (elt class)
  (search class (or (html-attribute elt :class) "")))

(defun retrieve-state (state)
  (net.html.parser:parse-html
   (drakma:http-request
    (format nil "http://www.politico.com/2012-election/results/president/~A/" (mt:string-replace (string-downcase state) " " "-")))))

(defun parse-state (state)
  (let* ((div (html-find-element state (class-selector "state-results-micro")))
	 (counties (html-find-elements div (tag-selector :tbody))))
    counties))
    
#|
;;; Input something like
((:TBODY :ID "county1001")
  ((:TR :CLASS "party-republican race-winner")
   ((:TH :ROWSPAN "5" :CLASS "results-county") "Autauga "
    ((:SPAN :CLASS "precincts-reporting") "100.0% Reporting"))
   ((:TH :SCOPE "row" :CLASS "results-candidate") "M. Romney")
   ((:TD :CLASS "results-party") ((:ABBR :TITLE "Republican") "GOP"))
   ((:TD :CLASS "results-percentage") "72.6%")
   ((:TD :CLASS "results-popular") "    17,366"))
  ((:TR :CLASS "party-democrat")
   ((:TH :SCOPE "row" :CLASS "results-candidate") "B. Obama (i)")
   ((:TD :CLASS "results-party") ((:ABBR :TITLE "Democratic") "Dem"))
   ((:TD :CLASS "results-percentage") "26.6%")
   ((:TD :CLASS "results-popular") "     6,354"))
 ... third parties)
|#

(defun tag-selector (tag)
   #'(lambda (elt)
       (eq tag (html-tag elt))))

(defun class-selector (class)
   #'(lambda (elt)
       (html-class? elt class)))

(defun html-contents (elt)
  (cond ((stringp elt) elt)
	(t (cadr elt))))

(defun parse-dumb-number (num-string)
  (parse-integer (mt:string-replace num-string "," "")))

(defun parse-county (county-html)
  (let ((county (html-contents (html-find-element county-html (class-selector "results-county"))))
	(results
	 (mapcar #'(lambda (result-row)
		     (let ((popular (parse-dumb-number
				     (html-contents (html-find-element result-row (class-selector "results-popular")))))
			   (party (html-contents (cadr
						  (html-find-element result-row (class-selector "results-party"))))))
		       (list party popular)))
		 ;; result rows
		 (html-find-elements county-html (tag-selector :tr)))))
    (list county results)))
  
	 
(defvar *states*
  '("Alabama"
    "Alaska"
    "Arizona"
    "Arkansas"
    "California"
    "Colorado"
    "Connecticut"
    "Delaware"
    "Florida"
    "Georgia"
    "Hawaii"
    "Idaho"
    "Illinois"
    "Indiana"
    "Iowa"
    "Kansas"
    "Kentucky"
    "Louisiana"
    "Maine"
    "Maryland"
    "Massachusetts"
    "Michigan"
    "Minnesota"
    "Mississippi"
    "Missouri"
    "Montana"
    "Nebraska"
    "Nevada"
    "New Hampshire"
    "New Jersey"
    "New Mexico"
    "New York"
    "North Carolina"
    "North Dakota"
    "Ohio"
    "Oklahoma"
    "Oregon"
    "Pennsylvania"
    "Rhode Island"
    "South Carolina"
    "South Dakota"
    "Tennessee"
    "Texas"
    "Utah"
    "Vermont"
    "Virginia"
    "Washington"
    "West Virginia"
    "Wisconsin"
    "Wyoming"
    "Puerto Rico"))	 


(mapcar #'(lambda (s) 
	    (list s
		  (mt::report-and-ignore-errors 
		    (mapcar #'parse-county 
			    (parse-state 
			     (retrieve-state s))))))
	*states*)


;;; Weave the two sets together
(defun weave ()
  (mt:collecting
    (dolist (e-state *by-county-electoral-results*)
      (let ((state (car e-state)))
	(dolist (e-county (cadr e-state))
	  (let* ((county (car e-county))
		 (county-fields (cadr e-county))
		 (m-county (lookup-m-county state county)))
	    (setq county-fields (mapcar #'(lambda (x) (cons (car x) (cadr x))) county-fields))
	    (push (cons :population (third m-county)) county-fields)
	    (push (cons :area (second m-county)) county-fields)
	    (push (cons :state state) county-fields)
	    (push (cons :county county) county-fields)
	    ;; easier to do that here than in js, sorry
	    (ignore-errors 
	      (push (cons :log_density (log (/ (mql-assocdr :population county-fields)
					     (mql-assocdr :area county-fields)))) county-fields))
	    (ignore-errors 
	      (push (cons :dem% (float (* 100 (/ (mql-assocdr "GOP" county-fields)
						 (+ (mql-assocdr "GOP" county-fields)
						    (mql-assocdr "Dem" county-fields))))))
		    county-fields))
	    (mt:collect county-fields)
	    ))))))


(defun add-region-code (+county)
  (+put :id +county (+get :id (lookup-geo-county (+get :state +county) (+get :county +county)))))

(length (mapc #'add-region-code *weave*))

    
(defun lookup-geo-county (state county)
  (some #'(lambda (c) (and (equal state (+get :state c)) (equal county (+get :name (+get :properties c))) c))
	*x-counties))

(defun lookup-m-county (state county)
  (find county (cadr (find state *m-county-info* :key #'car :test #'equal))
	:key #'car :test #'equal))
	
(length (setq *weave* (weave)))
(with-open-file (s "/misc/working/election/data/data.json" :direction :output :if-exists :supersede)
  (json:encode-json *weave* s))


(defmacro blank-on-error (exp)
  `(or (ignore-errors ,exp)
       ""))

(with-open-file (s "/misc/working/election/data.csv" :direction :output :if-exists :supersede)
  (dolist (e *weave*)
    (format s "~%~A,~A" 
	    (blank-on-error (float (/ (mql-assocdr :population e) (mql-assocdr :area e))))
	    (blank-on-error (float (* 100 (/ (mql-assocdr "GOP" e) (+ (mql-assocdr "GOP" e)(mql-assocdr "Dem" e)))))))))


(with-open-file (s "/misc/working/election/ldata.csv" :direction :output :if-exists :supersede)
  (dolist (e *weave*)
    (format s "~%~A,~A,~A,~A" 
	    (blank-on-error (float (log (/ (mql-assocdr :population e) (mql-assocdr :area e)))))
	    (blank-on-error (float (* 100 (/ (mql-assocdr "GOP" e) (+ (mql-assocdr "GOP" e)(mql-assocdr "Dem" e))))))
	    (blank-on-error (mql-assocdr :state e))
	    (blank-on-error (mql-assocdr :county e))
	    )))
