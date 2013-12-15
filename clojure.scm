(module clojure *
  (import chicken scheme data-structures)
  (set-read-syntax! 'clj (lambda (port) (init-clojure) #:clj))
  (set-read-syntax! 'end-clj (lambda (port) (unload-clojure) #:scm))

  (define *clj-loaded* (make-parameter #f))
  (define scheme-read-table (copy-read-table (current-read-table)))
  (define clojure-read-table (copy-read-table (current-read-table)))
  (define old-keyword-style (keyword-style))

  (define (is-whitespace? chr)
    (or (eq? chr #\space) (eq? chr #\tab) (eq? chr #\newline)))

  (define (read-coll port delim)
    (let ((chr (peek-char port)))
      (cond
       ((eq? chr delim) (read-char port) '())
       ((is-whitespace? chr) (read-char port) (read-coll port delim))
       (else (cons (read port) (read-coll port delim))))))

  (define (contains? lis elm)
    (cond
     ((null? lis) #f)
     ((eqv? elm (car lis)) #t)
     (else (contains? (cdr lis) elm))))

  (define clj-redefines
    (list 'let
          'if
          'and
          'or))

  (define (bind-clojure-literals!)
    (current-read-table clojure-read-table)

    (set-read-syntax! #\( (lambda (port)
                            (let ((lis (read-coll port #\))))
                              (if (and (pair? lis)
                                       (symbol? (car lis))
                                       (contains? clj-redefines (car lis)))
                                (cons (string->symbol (conc "clj-" (car lis)))
                                      (cdr lis))
                                lis))))
    (set-read-syntax! #\[ (lambda (port) `(vector ,@(read-coll port #\])))))

  (define-for-syntax nil '())
  (define-for-syntax true #t)
  (define-for-syntax false #f)

  (define-for-syntax (take n lis)
    (cond
     ((or (null? lis) (< n 1)) '())
     (else (cons (car lis)
                 (take (sub1 n) (cdr lis))))))

  (define-for-syntax (drop n lis)
    (cond
     ((or (null? lis) (< n 1)) lis)
     (else (drop (sub1 n) (cdr lis)))))
  
  (define-for-syntax (partition n lis)
    (let loop ((lis lis))
      (if (null? lis)
        '()
        (cons (take n lis) (loop (drop n lis))))))

  (define-syntax clj-and
    (syntax-rules ()
      ((_) #t)
      ((_ c1) c1)
      ((_ c1 c2 . r)
       (##core#let ((c c1))
                   (clj-if c (clj-and c2 . r) c)))))

  (define-syntax clj-or
    (syntax-rules ()
      ((_) nil)
      ((_ c1 . r)
       (##core#let ((c c1))
                   (clj-if c c (clj-or . r))))))
  
  (define-syntax clj-let
    (ir-macro-transformer
     (lambda (form i c)
       (let* ((bindings (cadr form))
              (bindings (if (and (list? bindings)
                                 (c (car bindings) 'vector))
                            (cdr bindings)
                            (syntax-error "Bindings must be a vector")))
              (body (cddr form)))
         (if (not (even? (length bindings)))
           (syntax-error "Even number of binding forms required"))
         (let ((scm-bindings (partition 2 bindings)))
           `(let* ,scm-bindings ,@body))))))

  (define-syntax clj-if
    (syntax-rules ()
      ((_) (syntax-error "Too few arguments to if"))
      ((_ c) (syntax-error "Too few arguments to if"))
      ((_ cnd then-c)
       (clj-if cnd then-c '()))
      ((_ cnd then-c else-c)
       (if (and cnd (not (null? cnd)))
         then-c else-c))))
  
  (define (init-clojure)
    (if (not (*clj-loaded*))
        (begin
          (keyword-style #:prefix)
          (bind-clojure-literals!)
          (*clj-loaded* #t))))

  (define (unload-clojure)
    (if (*clj-loaded*)
        (begin
          (keyword-style old-keyword-style)
            (current-read-table scheme-read-table)
          (*clj-loaded* #f))))

  )
