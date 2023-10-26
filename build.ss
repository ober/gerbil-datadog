#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("datadog/client"
    (static-exe:
     "datadog/datadog"
     "-ld-options"
     "-lyaml")))
