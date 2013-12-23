(load-relative "protocols.scm")

(defprotocol IFn
  (invoke (x #!rest args)))

