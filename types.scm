(load-relative "protocols.scm")
(use lolevel)

(define-record Type name protocols)
(define-record-printer (Type ty port)
  (write (Type-name ty) port))

(define-for-syntax (all-syms? lis)
  (cond
   ((null? lis) #t)
   ((symbol? (car lis)) (all-syms? (cdr lis)))
   (else #f)))

(define-syntax deftype
  (ir-macro-transformer
   (lambda (form r c)
     (if (null? (cdr form)) (syntax-error "type name is required"))
     (if (not (symbol? (cadr form))) (syntax-error "type name must be a symbol"))
     (if (null? (cddr form)) (syntax-error "Type fields required"))
     (if (not (and (list? (caddr form))
                   (all-syms? (caddr form))))
       (syntax-error "Type fields must be a list of symbols"))
     (let* ((name (cadr form))
            (fields (caddr form))
            (protocols (cdddr form)))
       `(let ((new-type (make-Type ',name ',protocols)))
          (##core#set! ,(r name) new-type)
          (define-record ,name ,@fields)
          ,(if (not (null? protocols))
             `(extend-type ,name ,@protocols)
             '(void))
          new-type)))))

(define (instance? ty o)
  (cond
   ((symbol? ty) (record-instance? o ty))
   ((record-instance? ty 'Type) (record-instance? o (Type-name ty)))
   (else
    (abort "First parameter must be a Type record or a symbol representing a Type's name"))))
