;;; std/kwargs.ss - Keyword argument utilities for Gerbil
;;;
;;; Gerbil doesn't support . rest keyword arguments directly.
;;; This module provides utilities for handling keyword arguments via rest parameters.

(export #t)

(import :std/misc/hash)

(def (get-kw args key default)
  "Get a keyword argument value from rest args

   Args:
     args: Rest arguments list (e.g., from (lambda (x . rest))
     key: Keyword symbol (e.g., 'timeout)
     default: Default value if key not found

   Returns:
     The value if key found, otherwise default

   Examples:
     (def (f x . rest)
       (let ((timeout (get-kw rest 'timeout 5000))
             (verbose (get-kw rest 'verbose #f)))
         ...))

     (f 1 'timeout 1000 'verbose #t)
  "

  (let loop ((rest args))
    (cond
     ((null? rest) default)
     ((null? (cdr rest)) default)
     ((eq? (car rest) key)
      (cadr rest))
     (else
      (loop (cddr rest))))))

(def (kwargs->hash args)
  "Convert rest keyword arguments to a hash table

   Args:
     args: Rest arguments list

   Returns:
     Hash table of keyword arguments

   Example:
     (kwargs->hash '(\"value\" 100 'timeout 5000))
     => #hash((\"value\" . 100) (\"timeout\" . 5000))
  "

  (def ht (make-hash-table))
  (let loop ((rest args))
    (unless (null? rest)
      (when (and (not (null? (cdr rest)))
                 (symbol? (car rest)))
        (hash-put! ht (car rest) (cadr rest))
        (loop (cddr rest)))))
  ht)
