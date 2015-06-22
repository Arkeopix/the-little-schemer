#! /usr/local/bin/sbcl --noinform

(ql:quickload :cl-ppcre)

(defun read-file (file)
  (with-open-file (in file)
	(let ((data (make-string (file-length in))))
      (read-sequence data in)
      data)))

(defun apply-logic (logic match)
  (cond
	((eql logic 'or)
	 (some #'identity match))
	(t
	 (every #'identity match))))

(defun file-ok (file line)
  (cond
	((cl-ppcre:scan line (concatenate 'string (pathname-name file) (pathname-type file))))
	(t nil)))

(defun search-privileged-files (logic-behaviour words)
  (let ((match-file '()))
	(if (probe-file "~/.logfind")
		(with-open-file (in "~/.logfind")
		  (do ((line (read-line in nil)
					 (read-line in nil)))
			  ((null line))
			(let ((files-in-dir (directory (make-pathname :name :wild :type :wild :defaults (truename ".")))))
			  (loop for file in files-in-dir do
				   (if (file-ok file line)
					   (let ((file-content (read-file file)))
						 (let ((match '()))
						   (loop for word in words do
								(cond
								  ((search word file-content)
								   (push t match))
								  (t (push nil match))))
						   (if (apply-logic logic-behaviour match)
							   (push file match-file)))))))
			(format t "file matched:~{~%~5t- ~a~}" match-file))))))

(defun behaviour (argv)
  "determines the logic behaviour of logfind."
  (cond
	((null argv) 'and)
	((string= "-o" (car argv)) 'or)
	(t (behaviour (cdr argv)))))

(defun sanitize-commandline (argv)
  "cleans the command line arguments"
  (remove "-o" argv :test #'equal))
  
(defun main (argv)
  (let ((logic (behaviour argv)) (words (sanitize-commandline argv)))
	;(format t "~a ~{~a ~}" logic words)
	(search-privileged-files logic words)))

(if (>= (length sb-ext:*posix-argv*) 1)
	(main (cdr (cdr sb-ext:*posix-argv*))))
