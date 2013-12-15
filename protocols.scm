(use srfi-69)

(define-record protocol name impls sigs)

(define (protocol-dispatch-type obj)
  (cond
   ((and (not (##sys#immediate? obj))
         (##sys#generic-structure? obj))
    (##sys#slot obj 0))
   ((pair? obj) 'pair)
   ((vector? obj) 'vector)
   ((string? obj) 'string)
   (else (abort (conc "Unsupported protocol dispatch type: " obj)))))

(define (protocol-dispatch-fn protocol obj mname)
  (let* ((type (protocol-dispatch-type obj))
         (impls-table (protocol-impls protocol))
         (impl (hash-table-ref impls-table type
                               (lambda ()
                                 (abort
                                  (conc "Protocol " (protocol-name protocol)
                                        " is not implemented for type "
                                        type)))))
         (method (hash-table-ref impl mname
                                 (lambda ()
                                   (abort
                                    (conc (protocol-name protocol) "/" mname
                                          " is not implemented for type " type))))))

    method))

(define-syntax defprotocol
  (ir-macro-transformer
   (lambda (form ren cmp)
     (if (null? (cdr form)) (syntax-error "Protocol must have a name"))
     (let ((pname (cadr form))
           (sigs (cddr form)))
       (if (not (symbol? pname)) (syntax-error "Name must be an unquoted symbol"))       
       (do ((sigs sigs (cdr sigs)))
           ((null? sigs))
         (##sys#check-syntax 'defprotocol (car sigs) '(symbol . #((symbol . lambda-list) 1))))
       `(begin
          (##core#set! ,(ren pname) (make-protocol ',pname (make-hash-table) ',sigs))
          ,@(map (lambda (sig)
                   `(##core#set! ,(ren (car sig))
                                 (case-lambda
                                  ,@(map (lambda (s)
                                           (list s
                                                 `((protocol-dispatch-fn ,(ren pname) ,(car s) ',(car sig))
                                                   ,@s)))
                                         (cdr sig)))))
                 sigs))))))

(define (protocol-add-impl protocol type mname impl)
  (let ((type-impls (hash-table-ref (protocol-impls protocol) type)))
    (hash-table-set! type-impls mname impl)))

(define-syntax extend-type
  (ir-macro-transformer
   (lambda (form ren cmp)
     (if (null? (cdr form)) (syntax-error "Type required"))
     (if (null? (cddr form)) (syntax-error "At least one protocol must be specified"))
     (let* ((type (cadr form))
            (specs (cddr form)))
       (let loop ((prot (car specs))
                  (rest (cdr specs))
                  (emits '()))
         (if (not (symbol? prot)) (syntax-error "Protocol name must be a symbol"))
         (cond
          ((null? rest)
           `(begin
              (hash-table-set! (protocol-impls ,(ren prot)) ',type (make-hash-table))
              ,@emits))
          ((symbol? (car rest))
           (loop (car rest)
                 (cdr rest)
                 emits))
          (else
           (##sys#check-syntax 'extend-type (car rest) '(symbol list . _))
           (let* ((impl (car rest))
                  (mname (car impl))
                  (bodies (cdr impl))
                  (bodies (if (symbol? (caar bodies))
                            (list bodies)
                            bodies)))
             (##sys#check-syntax 'extend-type bodies '#(((symbol . lambda-list) _ . _) 1))
             (loop prot (cdr rest) (cons `(protocol-add-impl ,(ren prot) ',type ',(caar rest)
                                                             (case-lambda ,@bodies))
                                         emits))))))))))
