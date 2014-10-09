(load-relative "types.scm")
(load-relative "core.scm")

(defprotocol Fn) 

(defprotocol IFn
  (-invoke
    [this]
    [this a]
    [this a b]
    [this a b c]
    [this a b c d]
    [this a b c d e]
    [this a b c d e f]
    [this a b c d e f g]
    [this a b c d e f g h]
    [this a b c d e f g h i]
    [this a b c d e f g h i j]
    [this a b c d e f g h i j k]
    [this a b c d e f g h i j k l]
    [this a b c d e f g h i j k l m]
    [this a b c d e f g h i j k l m n]
    [this a b c d e f g h i j k l m n o]
    [this a b c d e f g h i j k l m n o p]
    [this a b c d e f g h i j k l m n o p q]
    [this a b c d e f g h i j k l m n o p q s]
    [this a b c d e f g h i j k l m n o p q s t]
    [this a b c d e f g h i j k l m n o p q s t rest]))

(defprotocol ICounted
  (-count [coll]))
(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol ICollection
  (-conj [coll o]))

#;(defprotocol IOrdinal
    (-index [coll]))

(defprotocol IIndexed
  (-nth [coll n] [coll n not-found]))

(defprotocol ASeq)

(defprotocol ISeq
  (-first [coll])
  (-rest [coll]))

(defprotocol INext
  (-next [coll]))

(defprotocol ILookup
  (-lookup [o k] [o k not-found]))

(defprotocol IAssociative
  (-contains-key? [coll k])
  #;(-entry-at [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  #;(-assoc-ex [coll k v])
  (-dissoc [coll k]))

(defprotocol IMapEntry
  (-key [coll])
  (-val [coll]))

(defprotocol ISet
  (-disjoin [coll v]))

(defprotocol IStack
  (-peek [coll])
  (-pop [coll]))

(defprotocol IVector
  (-assoc-n [coll n val]))

(defprotocol IDeref
 (-deref [o]))

(defprotocol IDerefWithTimeout
  (-deref-with-timeout [o msec timeout-val]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o meta]))

(defprotocol IReduce
  (-reduce [coll f] [coll f start]))

(defprotocol IKVReduce
  (-kv-reduce [coll f init]))

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol IHash
  (-hash [o]))

(defprotocol ISeqable
  (-seq [o]))

(defprotocol ISequential)

(defprotocol IList)

(defprotocol IRecord)

(defprotocol IReversible
  (-rseq [coll]))

(defprotocol ISorted
  (-sorted-seq [coll ascending?])
  (-sorted-seq-from [coll k ascending?])
  (-entry-key [coll entry])
  (-comparator [coll]))

(defprotocol IWriter
  (-write [writer s])
  (-flush [writer]))

(defprotocol IPrintWithWriter
  (-pr-writer [o writer opts]))

(defprotocol IPending
  (-realized? [d]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(defprotocol IEditableCollection
  (-as-transient [coll]))

(defprotocol ITransientCollection
  (-conj! [tcoll val])
  (-persistent! [tcoll]))

(defprotocol ITransientAssociative
  (-assoc! [tcoll key val]))

(defprotocol ITransientMap
  (-dissoc! [tcoll key]))

(defprotocol ITransientVector
  (-assoc-n! [tcoll n val])
  (-pop! [tcoll]))

(defprotocol ITransientSet
  (-disjoin! [tcoll v]))

(defprotocol IComparable
  (-compare [x y]))

(defprotocol IChunk
  (-drop-first [coll]))

(defprotocol IChunkedSeq
  (-chunked-first [coll])
  (-chunked-rest [coll]))

(defprotocol IChunkedNext
  (-chunked-next [coll]))

(defprotocol INamed
  (-name [x])
  (-namespace [x]))

(deftype VectorNode (edit arr))

(define aget vector-ref)
(define (aset ary idx val)
  (vector-set! ary idx val)
  val)
(define (aclone ary)
  (vector-resize ary (vector-length ary)))

(define (push ary o)
  (vector-resize ary (add1 (vector-length ary)) o))

(define (pv-fresh-node edit)
  (make-VectorNode edit (make-vector 32)))
(define (pv-aget node idx)
  (aget (VectorNode-arr node) idx))
(define (pv-aset node idx val)
  (aset (VectorNode-arr node) idx val))

(define (pv-clone-node node)
  (make-VectorNode (VectorNode-edit node) (aclone node)))

(define (tail-off pv)
  (let ((cnt (PersistentVector-cnt pv)))
    (if (fx< cnt 32)
      0
      (fxshl (fxshr (sub1 cnt) 5) 5))))

(define (new-path edit level node)
  (let loop ((ll level)
             (ret node))
    (if (zero? ll)
      ret
      (let ((embed ret)
            (r (pv-fresh-node edit)))
        (pv-aset r 0 embed)
        (loop (fx- ll 5) r)))))

(define (push-tail pv level parent tailnode)
  (let ((ret (pv-clone-node parent))
        (subidx (fxand (fxshr (sub1 (PersistentVector-cnt pv)) level) #x01f)))
    (if (fx= 5 level)
      (begin
        (pv-aset ret subidx tailnode)
        ret)
      (let ((child (pv-aget parent subidx)))
        (if (eq? (void) child)
          (let ((node-to-insert (push-tail pv (fx- level 5) child tailnode)))
            (pv-aset ret subidx node-to-insert)
            ret)
          (let ((node-to-insert (new-path nil (fx- level 5) tailnode)))
            (pv-aset ret subidx node-to-insert)
            ret))))))

(define (vector-index-out-of-bounds i cnt)
  (abort (conc "No item " i " in vector of length " cnt)))

(define (array-for pv i)
  (if (and (fx<= 0 i) (fx< i (PersistentVector-cnt pv)))
    (if (fx>= i (tail-off pv))
      (PersistentVector-tail pv)
      (let loop ((node (PersistentVector-root pv))
                 (level (PersistentVector-shift pv)))
        (if (positive? level)
          (loop (pv-aget node (fxand (fxshr i level) #x01f))
                (fx- level 5))
          (VectorNode-arr node))))
    (vector-index-out-of-bounds i (PersistentVector-cnt pv))))

(define (do-assoc pv level node i val)
  (let ((ret (pv-clone-node node)))
    (if (zero? level)
      (begin
        (pv-aset ret (fxand i #x01f) val)
        ret)
      (let ((subidx (fxand (fxshr i level) #x01f)))
        (pv-aset ret subidx (do-assoc pv (fx- level 5) (pv-aget node subidx) i val))
        ret))))

(define (pop-tail pv level node)
  (let ((subidx (fxand (fxshr (fx- (PersistentVector-cnt pv) 2) level) #x01f)))
    (cond
     ((fx> level 5) (let ((new-child (pop-tail pv (fx- level 5) (pv-aget node subidx))))
                      (if (and (null? new-child) (zero? subidx))
                        '()
                        (let ((ret (pv-clone-node node)))
                          (pv-aset ret subidx new-child)
                          ret))))
     ((zero? subidx) '())
     (else (let ((ret (pv-clone-node node)))
             (pv-aset ret subidx '())
             ret)))))


(deftype PersistentVector (meta cnt shift root tail __hash)
  #;Object
  #;(toString (coll)
      ...)

  IWithMeta
  (-with-meta [coll meta] (make-PersistentVector meta cnt shift root tail __hash))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
         (if (fx> cnt 0)
           (-nth coll (sub1 cnt))))
  (-pop [coll]
        (cond
         ((zero? cnt) (abort "Can't pop empty vector"))
         ((fx= 1 cnt) (-with-meta PersistentVector.EMPTY meta))
         ((fx< 1 (fx- cnt (tail-off coll)))
          (make-PersistentVector meta (sub1 cnt) shift root (subvector tail 0 (sub1 (vector-length tail))) '()))
         (else (let* ((new-tail (array-for coll (fx- cnt 2)))
                      (nr (pop-tail coll shift root))
                      (new-root (if (null? nr) PersistentVector.EMPTY_NODE nr))
                      (cnt-1 (sub1 cnt)))
                 (if (and (fx< 5 shift) (null? (pv-aget new-root 1)))
                   (make-PersistentVector meta cnt-1 (fx- shift 5) (pv-aget new-root 0) new-tail '())
                   (make-PersistentVector meta cnt-1 shift new-root new-tail '()))))))

  ICollection
  (-conj [coll o]
         (let ((tail-idx (fx- cnt (tail-off coll))))
           (if (fx< tail-idx 32)
             (make-PersistentVector meta (add1 cnt) shift root (push tail o) '())
             (let* ((root-overflow? (fx> (fxshr cnt 5) (fxshl 1 shift)))
                    (new-shift (if root-overflow? (fx+ shift 5) shift))
                    (new-root (if root-overflow?
                                (let ((n-r (pv-fresh-node nil)))
                                  (pv-aset n-r 0 root)
                                  (pv-aset n-r 1 (new-path '() shift (make-VectorNode nil tail)))
                                  n-r)
                                (push-tail coll shift root (make-VectorNode nil tail)))))
               (make-PersistentVector meta (add1 cnt) new-shift new-root (vector o) '())))))

  IEmptyableCollection
  (-empty [coll] (with-meta PersistentVector.EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ISeqable
  (-seq [coll]
        (cond
         ((zero? cnt) '())
         ((fx< cnt 32) (array-seq tail))
         (else (chunked-seq coll 0 0))))

  ICounted
  (-count [coll] cnt)

  IIndexed
  (-nth
   ([coll n]
    (aget (array-for coll n) (fxand n #x01f)))
   ([coll n not-found]
    (if (and (fx<= 0 n) (< n cnt))
      (-nth coll n)
      not-found)))

  ILookup
  (-lookup
   ([coll k] (-nth coll k nil))
   ([coll k not-found] (-nth coll k not-found)))

  IMapEntry
  (-key [coll]
        (-nth coll 0))
  (-val [coll]
        (-nth coll 1))

  IAssociative
  (-assoc [coll k v]
          (cond
           ((and (<= 0 k) (< k cnt))
            (if (<= (tail-off coll) k)
              (let ((new-tail (aclone tail)))
                (aset new-tail (fxand k #x01f) v)
                (make-PersistentVector meta cnt shift root new-tail nil))
              (make-PersistentVector meta cnt shift (do-assoc shift root k v) tail nil)))
           ((== k cnt) (-conj coll v))
           (else (abort (conc "Index " k " out of bounds  [0," cnt "]")))))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce
   ([v f]
    (ci-reduce v f))
   ([v f start]
    (ci-reduce v f start)))

  #;IKVReduce
  #;  (-kv-reduce [v f init]
              (let ((step-init (vector 0 init)))
                (let loop ((i 0))
                  (if (fx< i cnt)
                    (let* ((arr (array-for v i))
                           (len (vector-length arr)))
                      (let ((init (let loop ((j 0 init (aget step-init 1)))
                                    (if (< j len)
                                      (let ((init (f init (+ j i)) (aget arr j)))
                                        (if (reduced? init)
                                          init
                                          (loop (add1 j) init)))
                                      (begin (aset step-init 0 len)
                                             (aset step-init 1 init)
                                             init)))))
                        (if (reduced? init)
                          (deref init)
                          (loop (+ i (aget step-init 0))))))
                    (aget step-init 1)))))

  IFn
  (-invoke
   ([coll k]
    (-nth coll k))
   ([coll k not-found]
    (-nth coll k not-found)))

  #;IEditableCollection
  #;  (-as-transient [coll]
                 (make-TransientVector cnt shift (tv-editable-root root) (tv-editable-tail tail)))

  #;IReversible
  #;  (-rseq [coll]
         (if (positive? cnt)
           (make-RSeq coll (sub1 cnt) nil)))
  )

(define PersistentVector.EMPTY_NODE (make-VectorNode nil (make-vector 32)))
(define PersistentVector.EMPTY (make-PersistentVector nil 0 5 PersistentVector.EMPTY_NODE (vector) 0))
