#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("datadog/client"
    (exe:
     "datadog/datadog"
     "-ld-options"
     "-lyaml")))
