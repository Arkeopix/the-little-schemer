#! /usr/local/bin/sbcl --noinform

(defun read-file (file)
  (with-open-file (in file)
	(let ((data (make-string (file-length in))))
      (read-sequence data in)
      data)))

(defun apply-logic (logic match)
  (print match)
  (cond
	((eql logic 'or)
	 (some #'identity match))
	(t
	 (every #'identity match))))

(defun search-privileged-files (logic-behaviour words)
  (let ((match-file '()))
	(if (probe-file "~/.logfind")
		(with-open-file (in "~/.logfind")
		  (do ((line (read-line in nil)
					 (read-line in nil)))
			  ((null line))
			(let ((file-content (read-file line)))
			  (let ((match '()))
				(loop for word in words do
					 (cond
					   ((search word file-content)
						(push t match))
					   (t (push nil match))))
				(if (apply-logic logic-behaviour match)
					(push line match-file)))))))
	(print match-file)))

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
	(format t "~a ~{~a ~}" logic words)
	(search-privileged-files logic words)))

(if (>= (length sb-ext:*posix-argv*) 1)
	(main (cdr (cdr sb-ext:*posix-argv*))))
