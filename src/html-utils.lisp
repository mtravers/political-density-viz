(in-package :cl-user)
(ql:quickload :cl-html-parse)

#|
These work on aserve parse results, eg:
(net.html.parser:parse-html
 (drakma:http-request <url>))
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

(defun html-find-children (elt pred)
  (mt:filter pred (html-subelements elt)))

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
  (cl-ppcre:scan  class (or (html-attribute elt :class) "")))

(defun tag-selector (tag)
   #'(lambda (elt)
       (eq tag (html-tag elt))))

(defun class-selector (class)
  (let ((scanner (cl-ppcre:create-scanner (format nil "\\b~A\\b" class))))
   #'(lambda (elt)
       (mt:awhen  (html-attribute elt :class)
       (funcall scanner mt:it 0 (length mt:it))))))

(defun html-contents (elt)
  (cond ((stringp elt) elt)
	(t (cadr elt))))
