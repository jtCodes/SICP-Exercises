;;; by Jiaan Tan

;;; 1.
;;
;; ==> (is-list? (list))  
;; #t
;; ==> (is-list? 1)
;; #f

(define (is-list? item)
  (cond ((null? item) #t)
        (else
         (cond ((pair? item) (is-list? (cdr item)))
               (else #f)))))

;;; 2. 2.18
;;
;; ==> (my-reverse (list 'a 1 'b 'c 333))
;; (333 c b 1 a)

(define (my-reverse l)
  (define (iter things result)
    (if (null? things)
	result
	(iter (cdr things)
	      (append (to-list(car things))
		      result))))
  (iter l nil))

;; Helper functions
(define nil (list))          ;nil not pre-defined, used in other exercises too

(define (to-list item)        ;car of a list might not be a list
  (cons item nil))            ;used for append function

;;; 3. 2.20
;;
;; ==> (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)
;; ==> (same-parity 2 3 4 5 6 7)
;; (2 4 6)

(define (same-parity . l)
  (cond ((odd? (car l)) (get-items odd? l))
        ((even? (car l)) (get-items even? l))))

;; Helper function
(define (get-items pred l)
  (define (iter pred things result)
    (cond ((null? things) result)
          ((pred (car things))
           (iter pred (cdr things) (append result (to-list (car things)))))
          (else
           (iter pred (cdr things) (append result nil)))))
   (iter pred l nil))

;;; 4. 2.21
;;
;; ==> (sq-list (list 1 2 3))
;; (1 4 9)
;; ==> (square-list (list 1 2 3))
;; (1 4 9)

(define (sq-list items)
  (if (null? items)
      nil
      (cons (sq (car items)) (sq-list (cdr items)))))

(define (square-list items)
  (map sq items))

;; Helper function
(define (sq x)
  (* x x))

;;; 5. 2.23
;;
;; ==> (my-for-each (lambda (x) (display x))  (list 57 321 88))
;; 5732188

(define (my-for-each func l)
  (cond ((null? (cdr l)) (func (car l)))
        (else (func (car l))
              (my-for-each func (cdr l)))))

;;; 7. 2.54
;;
;; ==> (my-equal? '(this is a list) '(this is a list))
;; #t
;; ==> (my-equal? '(this is a list) '(this (is a) list))
;; #f
;;NEEDS FIX
(define (my-equal? item1 item2)
  (cond ((and (list? item1) (list? item2))
         (if (null? (or (cdr item1) (cdr item2)))         ;end of either list
             (eq? (cdr item1) (cdr item2))
             (and (eq? (car item1) (car item2))           ;first item of list
                  (my-equal? (cdr item1) (cdr item2)))))  ;next
        (else (eq? item1 item2))))                        

;;; 8. a.
;;
;; ==> (every? even? (append (list 2 6 8) (list 2 4 6)))
;; #t
;; ==> (every? even? (append (list 2 6 8) (list 2 4 1)))
;; #f
;; ==> (and (every? odd? (list 1 1 3)) (every? odd? (list 1 1 3)))
;; #t

(define (every? pred l)
  (cond ((null? l) #t)
        (else (and (pred (car l)) (every? pred (cdr l))))))

;;; 8. b.
;;
;; What should happen if the list is empty is that #t should be returned. The
;; reason why is due to the fact that the empty list is a list with no
;; elements and the fact that function every? must return #t or #f if given
;; a list. To say that
;;
;; (every? even? (list))
;;
;; returns #f is saying there is at least one element in the empty list that is
;; not even. But there are no elements in the empty list so
;;
;; (every? even? (list))
;;
;; can't be false. Furthermore,
;;
;; (every? even? (append (list) (list 2)))
;;
;; returns #t. The only way that is possible is that all elements in both lists
;; combined satisfy the predicate even?. Saying that all elements in both the
;; empty list and the non-empty list combined satisfy the predicate means that
;; both lists can satisfy the condition of the predicate on their own so
;;
;; (and (every? even? (list)) (every? even? (list 2)))
;;
;; must return #t too.

;;; 9. 2.59
;;
;; ==> (unordered-union-set (list 92 3 25 7) (list 1 6 4 3 2))
;; (92 25 7 1 6 4 3 2)

(define (unordered-union-set set1 set2)
  (cond ((null? set1) set2)
	((not (unordered-element-of-set? (car set1) set2))  
         (cons (car set1)                                 ;take out duplicates
               (unordered-union-set (cdr set1) set2)))
        (else (unordered-union-set (cdr set1) set2))))

;; Helper function
(define (unordered-element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (unordered-element-of-set? x (cdr set)))))

;;; 10. 2.62
;;
;; ==> (ordered-union-set (list 1 2 90) (list 2 56 100))
;; (1 2 56 90 100)

(define (ordered-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (ordered-union-set (cdr set1)
                                           (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (ordered-union-set (cdr set1)
					   set2)))
                 ((< x2 x1)
                  (cons x2
                        (ordered-union-set set1
					   (cdr set2)))))))))

;;; 11.
;;
;; ==> (remove-val 'b (list 1 2 3 'a 'b 'b))
;; (1 2 3 a)

(define (remove-val val l)
  (cond ((null? l) nil)
        ((not (eq? val (car l)))                              ;if not match
         (append (to-list (car l)) (remove-val val (cdr l)))) ;add to new list
        (else (remove-val val (cdr l)))))                     ;else next item 

