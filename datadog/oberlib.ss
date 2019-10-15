;;; -*- Gerbil -*-
;;; © ober
;;; my utils

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/list
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/1
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/text/zlib
  :std/xml/ssax)

(export #t)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time mytime)))

(def (format-string-size string size)
  (unless (string? string)
    (set! string (format "~a" string)))
  (let* ((string (string-trim-both string))
         (our-size (string-length string))
         (delta (if (> size our-size)
                  (- size our-size)
                  0)))
    ;;    (displayln "fss: delta: " delta " string: " string " our-size: " our-size " size: " size)
    (format " ~a~a" string (make-string delta #\space))))

(def (style-output infos)
  (let-hash (load-config)
    (when (list? infos)
      (let* ((sizes (hash))
             (data (reverse infos))
             (header (car data))
             (rows (cdr data)))
        (for (head header)
             (unless (string? head) (displayln "head is not string: " head) (exit 2))
             (hash-put! sizes head (string-length head)))
        (for (row rows)
             (let (count 0)
               (for (column row)
                    (let* ((col-name (nth count header))
                           (current-size (hash-ref sizes col-name))
                           (this-size (if (string? column) (string-length column) (string-length (format "~a" column)))))
                      (when (> this-size current-size)
                        (hash-put! sizes col-name this-size))
                      ;;		      (displayln "colname: " col-name " col: " count " current-size: " current-size " this-size: " this-size " column: " column)
                      (set! count (1+ count))))))

        (for (head header)
             (display (format "| ~a" (format-string-size head (hash-get sizes head)))))

        ;; print header
        (displayln "|")
        (let ((count 0))
          (for (head header)
               (let ((sep (if (= count 0) "|" "+")))
                 (display (format "~a~a" sep (make-string (+ 2 (hash-get sizes (nth count header))) #\-))))
               (set! count (1+ count))))
        (displayln "|")

        (for (row rows)
             (let (count 0)
               (for (col row)
                    (display (format "|~a " (format-string-size col (hash-ref sizes (nth count header)))))
                    (set! count (1+ count))))
             (displayln "|"))
        ))))

(def (print-header style header)
  (let-hash (load-config)
    (cond
     ((string=? style "org-mode")
      (displayln "| " (string-join header " | ") " |")
      (displayln "|-|"))
     (else
      (displayln "Unknown format: " style)))))

(def (print-row style data)
  (if (list? data)
    (cond
     ((string=? style "org-mode")
      (org-mode-print-row data))
     (else
      (displayln "Unknown format! " style)))))

(def (org-mode-print-row data)
  (when (list? data)
    (for (datum data)
         (printf "| ~a " datum))
    (displayln "|")))

(def (resolve-ipv4 host)
  (let* ((host-info (host-info-addresses (host-info host))))
    (dp (format "host-info: ~a type:~a" host-info (type-of host-info)))
    (ip4-address->string
     (car host-info))))

(def (dp msg)
  (when DEBUG
    (displayln msg)))
