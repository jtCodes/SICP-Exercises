;;; 1.
;;;;;;; 1.
;;;;;;; ==>(define acc (make-account-lambda 100))
;;;;;;; ==> ((acc 'withdraw) 50)
;;;;;;; 50
(define make-account-lambda
  (lambda (balance)
    (define withdraw
      (lambda (amount)
	(if (>= balance amount)
	    (begin (set! balance (- balance amount))
		   balance)
	    "Insufficient funds")))
    (define deposit
      (lambda (amount)
	(set! balance (+ balance amount))
	balance))
    (lambda (m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))))

;;;;;;;; 2.
;;;;;;;; ==> (define acc (make-account-inline 100))
;;;;;;;; ==> ((acc 'withdraw) 50)
;;;;;;;; 50
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw)
	     (lambda (amount)
	       (if (>= balance amount)
		   (begin (set! balance (- balance amount))
			  balance)
		   "Insufficient funds")))
	    ((eq? m 'deposit)
	     (lambda (amount)
	       (set! balance (+ balance amount))
	       balance))
	    (else (display "Unknown request -- MAKE-ACCOUNT"
			   m))))
    ))

;;;;;;;; 3.
;;;;;;;; ==> (define acc (make-account-inline-factored 100))
;;;;;;;; ==> ((acc 'withdraw) 50)
;;;;;;;; 50
(define make-account-inline-factored
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
	(cond ((eq? m 'withdraw)
	       (if (>= balance amount)
		   (begin (set! balance (- balance amount))
			  balance)
		   "Insufficient funds"))
	      ((eq? m 'deposit)
	       (set! balance (+ balance amount))
	       balance)
	      (else (display "Unknown request -- MAKE-ACCOUNT"
			     m)))))
    ))

;;; 3. 3.2
;; ==>(define s (make-monitored sqrt))
;; ==>(s 100)
;; 10.0
;; ==>(s 'how-many-calls?)
;; 1
(define make-monitored
  (lambda (f)
    (define count 0)
    (define (mf m)
	(cond ((eq? m 'how-many-calls?)
	       count)
	      ((eq? m 'reset-count)
	       (set! count 0))
	      (else
	       (set! count (+ 1 count))
	       (f m))))
    mf))

;;; 4. 3.3
;; ==>(define acc (make-pw-account 1000 'test))
;; ==> ((acc 'dsfsf 'withdraw) 40)
;; INCORRECT PASSWORD
;; ==> ((acc 'test 'withdraw) 40)
;; 960
(define make-pw-account
  (lambda (balance stored-p)
    (define acc (make-account-inline-factored balance))
    (define (dispatch input-p m)
      (lambda (amount)
	(cond ((eq? input-p stored-p)
	       ((acc m) amount))
	      (else
	       (display "INCORRECT PASSWORD")
	       ))))
    dispatch))
	     
