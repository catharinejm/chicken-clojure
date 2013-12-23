(use posix)
(current-directory "/Users/jon/Code/Langs/clojure-scheme")
(load "clojure.scm")
(load "protocols.scm")
(load "types.scm")
(unload-clojure)

(let ((x 1))
  (let-syntax
      ((foo (syntax-rules ()
              ((_ y) (let-syntax
                         ((bar (syntax-rules ()
                                 ((_) (let ((x 2)) y)))))
                       (bar))))))
    (foo x)))

(let ((x 1))
  (let-syntax ((foo (syntax-rules ()
                      ((_ y) (let-syntax
                                 ((bar (syntax-rules ()
                                         ((_ x) y))))
                               (bar 2))))))
    (foo x)))

(define-syntax cc
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((clauses (cdr exp)))
       (if (null? clauses)
         `(,(rename 'quote) unspecified)
         (let* ((first (car clauses))
                (rest (cdr clauses))
                (test (car first)))
           (cond ((and (symbol? test)
                       (compare test (rename 'else)))
                  `(,(rename 'begin) ,@(cdr first)))
                 (else `(,(rename 'if)
                         ,test
                         (,(rename 'begin) ,@(cdr first))
                         (,(rename 'cc) ,@rest))))))))))

(define-syntax cci
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let ((clauses (cdr exp)))
       (if (null? clauses)
         `(quote unspecified)
         (let* ((first (car clauses))
                (rest (cdr clauses))
                (test (car first)))
           (cond ((and (symbol? test)
                       (compare test 'else))
                  `(begin ,@(cdr first)))
                 (else `(if ,test
                          (begin ,@(cdr first))
                          (cci ,@rest))))))))))


(type-case 10
           ((symbol number) #:yerp)
           (else #:poobers))

(define-syntax test-mac
  (syntax-rules ()
    ((_ name) 'name)
    ((_ name (fn (this . args) . body) ...)
     (list 'emit-it 'name '(fn (this . args) . body) ...))))

(define-syntax test2
  (er-macro-transformer
   (lambda (form ren cmp)
     (##sys#check-syntax 'test2 form '(_ symbol . _))
     (let ((pname (cadr form))
           (methods (cddr form)))
       (do ((ms methods (cdr ms)))
           ((null? ms))
         (##sys#check-syntax 'test2 (car ms) '(symbol (symbol . #(symbol 0)) . _)))
       #t))))

(use lolevel)
(define ##sys#procedure->string
  (let ((old-fn ##sys#procedure->string))
    (lambda (proc)
      (if (extended-procedure? proc)
        (call-with-output-string
         (lambda (p) (write (procedure-data proc) p)))
        (old-fn proc)))))

(define foo 10)

(define foo
  (let ((oldfoo foo))
    (list oldfoo 20)))

(define-record-type Type (->Type 'foo))
