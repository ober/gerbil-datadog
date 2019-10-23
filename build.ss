#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("datadog/client"
    (static-exe: "datadog/dda"
                 "-ld-options" "-lyaml -lssl -lz -L/usr/local/opt/openssl/lib/ -L/usr/local/lib" "-cc-options" "-I/usr/local/opt/openssl/include -I/usr/local/include")))
