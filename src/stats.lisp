;;; Note: wrong for IND since there may be multiples, but we don't care
(mt:def-cached-function county-party-count (state county party)
  (mt::fassocadr party
	       (mt::fassocadr county
			    (mt::fassocadr state *by-county-electoral-results* :test #'equal)
			    :test #'equal)
	       :test #'equal))

(mt:def-cached-function county-total (state county)
  (apply '+ (mapcar #'cadr
		    (mt::fassocadr county
			    (mt::fassocadr state *by-county-electoral-results* :test #'equal)
			    :test #'equal))))

;;; 
(defun by-county ()
  (mt:collecting
      (dolist (state *by-county-electoral-results*)
	(dolist (county (cadr state))
	  (mt:collect (list
		       (car state) (car county)
		      (county-total (car state) (car county))
		      (county-party-count (car state) (car county) "Dem")
		      (county-party-count (car state) (car county) "GOP")))))))

(setq *by-county-raw (by-county))

(setq *most-dem (sort (copy-list *by-county-raw) #'> :key #'(lambda (c) (/ (nth 3 c) (nth 2 c)))))
