(load-relative "protocols.scm")

(use srfi-69)

(defprotocol IMap
  (get (m k) (m k nf))
  (assoc (m k v))
  (keys (m))
  (values (m)))

(extend-type hash-table
             IMap
             (get ((m k)
                   (get m k #f))
                  ((m k nf)
                   (hash-table-ref m k (lambda () nf))))
             (assoc (m k v)
                    (hash-table-set! m k v))
             (keys (m) (hash-table-keys m))
             (values (m) (hash-table-values m)))

(define (fn-map #!optional init)
  (let ((self (if init init (make-hash-table))))
    (extend-procedure
     (lambda (key #!optional not-found)
       (get self key not-found))
     self)))
