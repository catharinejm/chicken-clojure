(load-relative "types.scm")

(deftype VectorNode (edit arr))

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

(define aget vector-ref)
(define (aset ary idx val)
  (vector-set! ary idx val)
  val)
(define (aclone ary)
  (let ((dest (make-vector (vector-length ary))))
    (vector-copy! ary dest)
    dest))

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
        (subidx (fxand (fxshr (sub1 (PersistentVector-cnt pv)) level) 0x01f)))
    (if (fx= 5 level)
      (begin
        (pv-aset ret subidx tailnode)
        ret)
      (let ((child (pv-aget parent subidx)))
        (if (eq? (void) child)
          (let ((node-to-insert (push-tail pv (fx- level 5) child tailnode)))
            (pv-aset ret subidx node-to-insert)
            ret)
          (let ((node-to-insert (new-path #f (fx- level 5) tailnode)))
            (pv-aset ret subidx node-to-insert)
            ret))))))

(define (vector-index-out-of-bounds i cnt)
  (abort (cond "No item " i " in vector of length " cnt)))

(define (array-for pv i)
  (if (and (fx<= 0 i) (fx< i (PersistentVector-cnt pv)))
    (if (fx>= i (tail-off pv))
      (PersistentVector-tail pv)
      (let loop ((node (PersistentVector-root pv))
                 (level (PersistentVector-shift pv)))
        (if (positive? level)
          (loop (pv-aget node (fxand (fxshr i level) 0x01f))
                (fx- level 5))
          (VectorNode-arr node))))
    (vector-index-out-of-bounds i (PersistentVector-cnt pv))))

(define (do-assoc pv level node i val)
  (let ((ret (pv-clone-node node)))
    (if (zero? level)
      (begin
        (pv-aset ret (fxand i 0x01f) val)
        ret)
      (let ((subidx (fxand (fxshr i level) 0x01f)))
        (pv-aset ret subidx (do-assoc pv (fx- level 5) (pv-aget node subidx) i val))
        ret))))

(define (pop-tail pv level node)
  (let ((subidx (fxand (fxshr (fx- (PersistentVector-cnt pv) 2) level) 0x01f)))
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


(deftype PersistentVector (meta cnt shift root tail hash)
  )
