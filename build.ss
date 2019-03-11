#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script
	:std/make)

(defbuild-script
  `("datadog.ss"))

;; (def build-spec
;;   '("datadog"
;;     (exe: "datadog" "-ld-options" "-lyaml -lssl -lz -L/usr/local/opt/openssl/lib/ -L/usr/local/lib" "-cc-options" "-I/usr/local/opt/openssl/include -I/usr/local/include")
;;     ))

;; (def build-spec-static
;;   '("datadog"
;;     (static-exe: "datadog"
;;                  "-ld-options" "-lyaml -lssl -lz -L/usr/local/opt/openssl/lib -L/usr/local/lib"
;; 		 "-cc-options" "-I/usr/local/opt/openssl/include -I/usr/local/include"
;;                  "-prelude" "(declare (not safe))")))

;; (def srcdir
;;   (path-normalize (path-directory (this-source-file))))

;; (def (main . args)
;;   (match args
;;     (["deps"]
;;      (let (build-deps (make-depgraph/spec build-spec))
;;        (call-with-output-file "build-deps" (cut write build-deps <>))))
;;     (["static"]
;;      (let (depgraph (call-with-input-file "build-deps" read))
;;        (make srcdir: srcdir
;;              bindir: srcdir
;;              optimize: #t
;;              static: #t
;;              depgraph: depgraph
;;              prefix: "datadog"
;;              build-spec-static)))
;;     ([]
;;      (let (depgraph (call-with-input-file "build-deps" read))
;;        (make srcdir: srcdir
;;              bindir: srcdir
;;              optimize: #t
;;              debug: 'env
;;              static: #t
;;              depgraph: depgraph
;;              prefix: "datadog"
;;              build-spec)))))
