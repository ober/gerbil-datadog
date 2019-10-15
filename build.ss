#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("datadog/client"
    "datadog/oberlib"
    (exe: "datadog/dda")))
