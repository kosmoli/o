;;; std/net/msgpack.ss - MessagePack stub implementation
;;;
;;; This is a placeholder stub for MessagePack functionality.
;;; For production use, install a proper MessagePack library.

(export #t)

(import :std/sugar
        :std/text/json)

;;; Stub implementations using JSON as fallback
(def (msgpack-encode obj)
  "Encode object to MessagePack (stub: uses JSON)"
  (string->bytes (json-object->string obj)))

(def (msgpack-decode bytes)
  "Decode MessagePack to object (stub: uses JSON)"
  (string->json-object (bytes->string bytes)))

(def (bytes->string bytes)
  "Convert bytes to string"
  (utf8->string bytes))

(def (string->bytes str)
  "Convert string to bytes"
  (string->utf8 str))
