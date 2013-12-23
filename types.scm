(load-relative "protocols.scm")
(use lolevel)
(use srfi-1)

(define-record Type name fields protocols)
(define-record-printer (Type ty port)
  (write (Type-name ty) port))

(define-for-syntax (all-syms? lis)
  (cond
   ((null? lis) #t)
   ((symbol? (car lis)) (all-syms? (cdr lis)))
   (else #f)))

(define-for-syntax (inject-fields ren type fields protocols)
  (let loop ((pdefs protocols))
    (cond
     ((null? pdefs) '())
     ((symbol? (car pdefs)) (cons (car pdefs) (loop (cdr pdefs))))
     ((list? (car pdefs))
      (if (not (pair? (car pdefs))) (syntax-error "Invalid method implementation"))
      (let ((impl (car pdefs)))
        (if (not (symbol? (car impl))) (syntax-error "Method name must be a symbol"))
        (if (or (not (pair? (cdr impl)))
                (not (pair? (cadr impl)))) (syntax-error "Method requires a lambda list"))
        (let ((field-shadows (lset-difference eq? fields (cadr impl))))
          (cons (list (car impl)
                      (cadr impl)
                      `(let (,@(map (lambda (f) (list f
                                                      (list (string->symbol (conc (ren type) "-" (ren f)))
                                                            (caadr impl))))
                                    field-shadows))
                         ,@(cddr impl)))
                (loop (cdr pdefs))))))
     (else (syntax-error "Something is wrong :/")))))

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
       `(let ((new-type (make-Type ',name ',fields ',protocols)))
          (##core#set! ,(r name) new-type)
          (define-record ,name ,@fields)
          ,(if (not (null? protocols))
             `(extend-type ,name ,@(inject-fields r name fields protocols))
             '(void))
          new-type)))))

(define (instance? ty o)
  (cond
   ((symbol? ty) (record-instance? o ty))
   ((record-instance? ty 'Type) (record-instance? o (Type-name ty)))
   (else
    (abort "First parameter must be a Type record or a symbol representing a Type's name"))))
