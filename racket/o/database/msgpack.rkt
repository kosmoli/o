#lang racket

;;; database/msgpack.rkt - MessagePack Serialization
;;;
;;; This module provides MessagePack encoding/decoding for database communication.

(provide msgpack-encode
         msgpack-decode
         jsexpr->msgpack
         msgpack->jsexpr)

(require racket/bytes
        racket/port
        racket/match)

;;; ============================================================================
;;; MessagePack Encoding
;;; ============================================================================

(define (msgpack-encode data)
  "Encode Racket data to MessagePack format

   Args:
     data: Data to encode (hash, list, string, number, boolean, #f)

   Returns:
     Bytes containing MessagePack encoded data"
  (let ([out (open-output-bytes)])
    (encode-value out data)
    (get-output-bytes out)))

(define (encode-value out value)
  "Encode a single value to output port"
  (cond
   [(false? value)  ; #f or '()
    (write-byte #xc0 out)]

   [(null? value)
    (write-byte #xc0 out)]

   [(eq? value #t)
    (write-byte #xc3 out)]

   [(exact-nonnegative-integer? value)
   (encode-uint out value)]

   [(exact-integer? value)
   (encode-int out value)]

   [(real? value)
   (encode-float out value)]

   [(string? value)
   (encode-string out value)]

   [(bytes? value)
   (encode-bytes-val out value)]

   [(list? value)
   (encode-array out value)]

   [(hash? value)
   (encode-map out value)]

   [else
    (error 'encode-value "Unsupported type: ~a" value)]))

(define (write-uint16-be out val)
  "Write 16-bit unsigned integer in big-endian order"
  (write-byte (bitwise-and (arithmetic-shift val -8) #xff) out)
  (write-byte (bitwise-and val #xff) out))

(define (write-uint32-be out val)
  "Write 32-bit unsigned integer in big-endian order"
  (write-byte (bitwise-and (arithmetic-shift val -24) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -16) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -8) #xff) out)
  (write-byte (bitwise-and val #xff) out))

(define (write-uint64-be out val)
  "Write 64-bit unsigned integer in big-endian order"
  (write-byte (bitwise-and (arithmetic-shift val -56) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -48) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -40) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -32) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -24) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -16) #xff) out)
  (write-byte (bitwise-and (arithmetic-shift val -8) #xff) out)
  (write-byte (bitwise-and val #xff) out))

(define (write-int16-be out val)
  "Write 16-bit signed integer in big-endian order"
  (write-uint16-be out val))

(define (write-int32-be out val)
  "Write 32-bit signed integer in big-endian order"
  (write-uint32-be val))

(define (write-int64-be out val)
  "Write 64-bit signed integer in big-endian order"
  (write-uint64-be val))

(define (encode-uint out value)
  "Encode unsigned integer"
  (cond
   [(<= value 127)
    (write-byte value out)]

   [(<= value #xff)
    (write-byte #xcc out)
    (write-byte value out)]

   [(<= value #xffff)
    (write-byte #xcd out)
    (write-uint16-be out value)]

   [(<= value #xffffffff)
    (write-byte #xce out)
    (write-uint32-be out value)]

   [else
    (write-byte #xcf out)
    (write-uint64-be out value)]))

(define (encode-int out value)
  "Encode signed integer"
  (cond
   [(>= value -32)
    (write-byte (bitwise-ior #xe0 (bitwise-and value #x1f)) out)]

   [(>= value -128)
    (write-byte #xd0 out)
    (write-byte (bitwise-and value #xff) out)]

   [(>= value -32768)
    (write-byte #xd1 out)
    (write-int16-be out value)]

   [(>= value -2147483648)
    (write-byte #xd2 out)
    (write-int32-be out value)]

   [else
    (write-byte #xd3 out)
    (write-int64-be out value)]))

(define (encode-float out value)
  "Encode floating point number as float64"
  (write-byte #xcb out)
  (define bytes (real->floating-point-bytes value 'double))
  (write-bytes bytes out))

(define (encode-string out value)
  "Encode string"
  (define bytes (string->bytes/utf-8 value))
  (define len (bytes-length bytes))

  (cond
   [(< len 32)
    (write-byte (bitwise-ior #xa0 len)) out]

   [(<= len #xff)
    (write-byte #xd9 out)
    (write-byte len out)]

   [(<= len #xffff)
    (write-byte #xda out)
    (write-uint16-be out len)]

   [else
    (write-byte #xdb out)
    (write-uint32-be out len)])

  (write-bytes bytes out))

(define (encode-bytes-val out value)
  "Encode bytes"
  (define len (bytes-length value))

  (cond
   [(<= len #xff)
    (write-byte #xc4 out)
    (write-byte len out)]

   [(<= len #xffff)
    (write-byte #xc5 out)
    (write-uint16-be out len)]

   [else
    (write-byte #xc6 out)
    (write-uint32-be out len)])

  (write-bytes value out))

(define (encode-array out value)
  "Encode array/list"
  (define len (length value))

  (cond
   [(< len 16)
    (write-byte (bitwise-ior #x90 len)) out]

   [(<= len #xffff)
    (write-byte #xdc out)
    (write-uint16-be out len)]

   [else
    (write-byte #xdd out)
    (write-uint32-be out len)])

  (for-each (lambda (v) (encode-value out v)) value))

(define (encode-map out value)
  "Encode hash/map"
  (define len (hash-count value))

  (cond
   [(< len 16)
    (write-byte (bitwise-ior #x80 len)) out]

   [(<= len #xffff)
    (write-byte #xde out)
    (write-uint16-be out len)]

   [else
    (write-byte #xdf out)
    (write-uint32-be out len)])

  (for ([(k v) (in-hash value)])
    ;; Keys must be strings
    (encode-string out (if (symbol? k) (symbol->string k) k))
    (encode-value out v)))

;;; ============================================================================
;;; MessagePack Decoding
;;; ============================================================================

(define (msgpack-decode bytes-in)
  "Decode MessagePack bytes to Racket data

   Args:
     bytes-in: Bytes containing MessagePack data

   Returns:
     Decoded Racket data"
  (define in (open-input-bytes bytes-in))
  (define result (decode-value in))
  (close-input-port in)
  result)

(define (read-uint16-be in)
  "Read 16-bit unsigned integer in big-endian order"
  (define b0 (read-byte in))
  (define b1 (read-byte in))
  (bitwise-ior (arithmetic-shift b1 8) b0))

(define (read-uint32-be in)
  "Read 32-bit unsigned integer in big-endian order"
  (define b0 (read-byte in))
  (define b1 (read-byte in))
  (define b2 (read-byte in))
  (define b3 (read-byte in))
  (bitwise-ior (arithmetic-shift b3 24)
               (arithmetic-shift b2 16)
               (arithmetic-shift b1 8)
               b0))

(define (read-uint64-be in)
  "Read 64-bit unsigned integer in big-endian order"
  (define b0 (read-byte in))
  (define b1 (read-byte in))
  (define b2 (read-byte in))
  (define b3 (read-byte in))
  (define b4 (read-byte in))
  (define b5 (read-byte in))
  (define b6 (read-byte in))
  (define b7 (read-byte in))
  (bitwise-ior (arithmetic-shift b7 56)
               (arithmetic-shift b6 48)
               (arithmetic-shift b5 40)
               (arithmetic-shift b4 32)
               (arithmetic-shift b3 24)
               (arithmetic-shift b2 16)
               (arithmetic-shift b1 8)
               b0))

(define (read-int8 in)
  (define b (read-byte in))
  (if (> b 127) (- b 256) b))

(define (read-int16-be in)
  (define b0 (read-byte in))
  (define b1 (read-byte in))
  (define val (bitwise-ior (arithmetic-shift b1 8) b0))
  (if (> val 32767) (- val 65536) val))

(define (read-int32-be in)
  (define b0 (read-byte in))
  (define b1 (read-byte in))
  (define b2 (read-byte in))
  (define b3 (read-byte in))
  (define val (bitwise-ior (arithmetic-shift b3 24)
                           (arithmetic-shift b2 16)
                           (arithmetic-shift b1 8)
                           b0))
  (if (> val 2147483647)
      (- val 4294967296)
      val))

(define (read-int64-be in)
  (define b0 (read-byte in))
  (define b1 (read-byte in))
  (define b2 (read-byte in))
  (define b3 (read-byte in))
  (define b4 (read-byte in))
  (define b5 (read-byte in))
  (define b6 (read-byte in))
  (define b7 (read-byte in))
  (define val (bitwise-ior (arithmetic-shift b7 56)
                           (arithmetic-shift b6 48)
                           (arithmetic-shift b5 40)
                           (arithmetic-shift b4 32)
                           (arithmetic-shift b3 24)
                           (arithmetic-shift b2 16)
                           (arithmetic-shift b1 8)
                           b0))
  (if (> val 9223372036854775807)
      (- val 18446744073709551616)
      val))

(define (decode-value in)
  "Decode a single value from input port"
  (define byte (read-byte in))
  (cond
   [(eof-object? byte)
    (error 'decode-value "Unexpected EOF")]

   [(<= byte 127)
    ;; FixInt positive
    byte]

   [(<= #xe0 byte 255)
    ;; FixInt negative
    (- byte 256)]

   [(= byte #xc0)
    ;; nil
    #f]

   [(= byte #xc2)
    ;; false
    #f]

   [(= byte #xc3)
    ;; true
    #t]

   [(= byte #xc4)
    ;; bin8
    (decode-bytes-val in (read-byte in))]

   [(= byte #xc5)
    ;; bin16
    (decode-bytes-val in (read-uint16-be in))]

   [(= byte #xc6)
    ;; bin32
    (decode-bytes-val in (read-uint32-be in))]

   [(= byte #xcc)
    ;; uint8
    (read-byte in)]

   [(= byte #xcd)
    ;; uint16
    (read-uint16-be in)]

   [(= byte #xce)
    ;; uint32
    (read-uint32-be in)]

   [(= byte #xcf)
    ;; uint64
    (read-uint64-be in)]

   [(= byte #xd0)
    ;; int8
    (read-int8 in)]

   [(= byte #xd1)
    ;; int16
    (read-int16-be in)]

   [(= byte #xd2)
    ;; int32
    (read-int32-be in)]

   [(= byte #xd3)
    ;; int64
    (read-int64-be in)]

   [(= byte #xd9)
    ;; str8
    (decode-string in (read-byte in))]

   [(= byte #xda)
    ;; str16
    (decode-string in (read-uint16-be in))]

   [(= byte #xdb)
    ;; str32
    (decode-string in (read-uint32-be in))]

   [(= byte #xdc)
    ;; array16
    (decode-array in (read-uint16-be in))]

   [(= byte #xdd)
    ;; array32
    (decode-array in (read-uint32-be in))]

   [(= byte #xde)
    ;; map16
    (decode-map in (read-uint16-be in))]

   [(= byte #xdf)
    ;; map32
    (decode-map in (read-uint32-be in))]

   [(= byte #xcb)
    ;; float64
    (define bytes (read-bytes 8 in))
    (floating-point-bytes->real bytes 'double)]

   [(>= byte #xa0)
    ;; FixStr
    (define len (- byte #xa0))
    (decode-string in len)]

   [(>= byte #x90)
    ;; FixArray
    (define len (- byte #x90))
    (decode-array in len)]

   [(>= byte #x80)
    ;; FixMap
    (define len (- byte #x80))
    (decode-map in len)]

   [else
    (error 'decode-value "Unknown marker: 0x~x" byte)]))

(define (decode-string in len)
  (define bytes (read-bytes len in))
  (bytes->string/utf-8 bytes))

(define (decode-bytes-val in len)
  (read-bytes len in))

(define (decode-array in len)
  (for/list ([i (in-range len)])
    (decode-value in)))

(define (decode-map in len)
  (for/fold ([h (hash)])
            ([i (in-range len)])
    (define key (decode-value in))
    (define value (decode-value in))
    (hash-set h (if (string? key) key (format "~a" key)) value)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (jsexpr->msgpack data)
  "Convert jsexpr to MessagePack bytes (alias for msgpack-encode)"
  (msgpack-encode data))

(define (msgpack->jsexpr bytes)
  "Convert MessagePack bytes to jsexpr (alias for msgpack-decode)"
  (msgpack-decode bytes))
