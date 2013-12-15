(use specialized-io)
(use vector-lib)

(define (read-vec port)
  (let ((ch (peek-char port)))
    (cond
     ((eq? ch #\space) (read-char port) (read-vec port))
     ((eq? ch #\]) (read-char port) '())
     (else (cons (read port) (read-vec port))))))

(set-read-syntax! #\[ (lambda (port)
                        (cons 'vector (read-vec port))))

(define-record-type VectorNode
  (->VectorNode e a)
  vector-node?
  (e .-edit)
  (a .-arr))

(define (pv-fresh-node edit)
  (->VectorNode edit (make-vector 32)))

(define (pv-aget node idx)
  (vector-ref node idx))

(define (pv-aset node idx val)
  (vector-set! node idx val))

(define (pv-clone-node node)
  (->VectorNode (.-edit node) (vector-copy (.-arr node))))

(define (tail-off pv)
  (let ((cnt (.-cnt pv)))
    (if (< cnt 32)
        0
        (fxshl (fxshr (sub1 cnt) 5) 5))))

(define (new-path edit level node)
  (let loop ((ll level)
             (ret node))
    (if (= 0 ll)
        ret
        (let* ((embed ret)
               (r (pv-fresh-node edit))
               (_ (pv-aset r 0 embed)))
          (loop (fx- ll 5) r)))))

(define (push-tail pv level parent tailnode)
  (let ((ret (pv-clone-node parent))
        (subidx (fxand (fxshr (sub1 (.-cnt pv)) level) #x01f)))
    (if (fx= 5 level)
        (begin
          (pf-aset ret subidx tailnode)
          ret)
        (let ((child (pv-aget parent subidx)))
          (if (not (null? child))
              (let ((node-to-insert (push-tail pv (fx- level 5) child tailnode)))
                (pv-aset ret subidx node-to-insert)
                ret)
              (let ((node-to-insert (new-path '() (- level 5) tailnode)))
                (pv-aset ret subidx node-to-insert)))))))


(define (read-fn port)
  (if (eq? (peek-char port) #\))
      (begin (read-char port) '())
      (cons (read) (read-fn port))))

(set-sharp-read-syntax! #\( (lambda (port) `(lambda () ,(read-fn port))))
