(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map proc (cdr items)))
  )
)

(define (cons-all first rests)
  (map (lambda (lst) (cons first lst)) rests)
)

(define (zip pairs)
  (cons
    (map car pairs)
    (list (map cadr pairs))
  )
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s c)
    (if (null? s)
      nil
      (cons (cons c (cons (car s) nil)) (helper (cdr s) (+ c 1))))
    )
  (helper s 0)
  )
  (helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (define (helper-iterate val times)
    (if (= times 0)
      '()
      (cons val (helper-iterate val (- times 1)))
    )
  )

  (define (helper-change total denoms)
    (cond
      ((= total 0) '(())
      )
      ((null? (cdr denoms)) (list (helper-iterate (car denoms) total))
      )
      ((< total (car denoms)) (helper-change total (cdr denoms))
      )
      (else
        (append
          (cons-all (car denoms) (helper-change (- total (car denoms)) denoms))
          (helper-change total (cdr denoms))
        )
      )
    )
  )
  (helper-change total denoms)
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons
              form
              (cons
                  params
                  (map let-to-lambda body)
              )
            )
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons
              (cons
                'lambda
                (cons
                  (car (zip values))
                  (map let-to-lamda body)
                )
              )
              (map let-to-lamda (cadr (zip values)))
           )
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (cons
            (car expr)
            (map let-to-lamda (cdr expr))
         )
         ; END PROBLEM 19
         )))
         (define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons (cons 'lambda (cons (car (zip values)) (map let-to-lambda body))) (map let-to-lambda (cadr (zip values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         )))
