(define nil '())
(define true #t)
(define false #f)

(define nil? null?)

(define-syntax if
  (syntax-rules ()
    ((_) (syntax-error "Too few arguments to if"))
    ((_ _) (syntax-error "Too few arguments to if"))
    ((_ p t) (if p t '()))
    ((_ p t f) (##core#if p
                          (##core#if (not (null? p))
                                     t
                                     f)
                          f))
    ((_ _ _ _ _ . _) (syntax-error "Too many arguments to if"))))

#;
(define-syntax cond
  (syntax-rules (else)
    ((_ ))))

(define-syntax and
  (syntax-rules ()
    ((_) true)
    ((_ x) x)
    ((_ x1 x2 . xs)
     (if x1
       (and x2 . xs)
       x1))))

(define-syntax or
  (syntax-rules ()
    ((_) nil)
    ((_ x) x)
    ((_ x y . zs)
     (if x
       x
       (or y . zs)))))

(define-syntax begin
  (syntax-rules ()
    ((_) nil)
    ((_ s . ss) (##core#begin s . ss))))

(define-syntax when
  (syntax-rules ()
    ((_) (syntax-error "Too few arguments to when"))
    ((_ p . ss) (if p (begin . ss)))))
