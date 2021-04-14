(let a 10) ;; a is 10
(let b 10) ;; b is also 10
(define check (
	(cond ((eq? a b)
	        (print "Equal"))
		  (else(
		    (print "Not Equal"))))))

check ;; Run yo check