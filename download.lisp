(ql:quickload '(:drakma :cl-json :wuwei :cl-html-parse))

;;; Retrieve electoral data

#|
(net.html.parser:parse-html
 (drakma:http-request "http://www.politico.com/2012-election/results/president/alabama/"))
|#

(defun retrieve-state (state)
  (net.html.parser:parse-html
   (drakma:http-request
    (format nil "http://www.politico.com/2012-election/results/president/~A/" (mt:string-replace (string-downcase state) " " "-")))))

(defun parse-state (state)
  (let* ((div (html-find-element state (class-selector "state-results-micro")))
	 (counties (html-find-elements div (tag-selector :tbody))))
    counties))
    
#|
;;; Results in something like:
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

(defun parse-dumb-number (num-string)
  (parse-integer (mt:string-replace num-string "," "")))

(defun parse-county (county-html)
  (let ((county (mt:string-trim-whitespace  (html-contents (html-find-element county-html (class-selector "results-county")))))
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
	 
(defvar *all-counties* (mt:mapappend  #'cadr *by-county-electoral-results*))

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

;;; produces state/county/electoral data structure
(defvar *by-state-electoral-results*
 (mapcar #'(lambda (s) 
	     (list s
		   (mt::report-and-ignore-errors 
		     (mapcar #'parse-county 
			     (parse-state 
			      (retrieve-state s))))))
	 *states*))


;;; Weave the two sets together
(defun weave ()
  (mt:collecting
    (dolist (e-state *by-state-electoral-results*)
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
(with-open-file (s "/misc/working/election/data/election-data.json" :direction :output :if-exists :supersede)
  (json:encode-json *weave* s))

;;; Fixup
(with-open-file (i "/misc/working/election/data/election-data.json" )
  (with-open-file (o "/misc/working/election/data/election-data-fixed.json" :direction :output :if-exists :supersede)
    (setq *weave* (json:decode-json i))
    (mapc #'(lambda (ent)
	      (setf (mql-assocdr :id ent)
		    (mt:report-and-ignore-errors (parse-integer (mql-assocdr :id ent)))))
	  *weave*)
	  (json:encode-json *weave* o))))

;;; More fixup

(length
 (setq *election-fixed*
       (with-open-file (i "/misc/working/election/data/election-data-fixed.json")
	 (json:decode-json i))))

(length
 (setq *dupes*
       (mt:filter #'(lambda (x) (> (length x) 1)) (mt:group *election-fixed* :key (json-accessor :id)))))

;;; And more

(length
 (setq *new*
       (with-open-file (i "/misc/working/election/data/us-new.json")
	 (json:decode-json i))))

;;; More!

(length
 (setq *broken* 
       (mt:filter #'(lambda (ent) (or (null (mql-assocdr :population ent))(null (mql-assocdr :dem% ent))))
		  *election-fixed*)))

(length
 (setq *unbroken* 
       (with-open-file (i "/misc/working/election/data/unbroken.lisp")
	 (read i))))


;;; weave unbroken back in.


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
