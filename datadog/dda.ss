;;; -*- Gerbil -*-
;;; © ober
;;; Datadog client binary

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
  :std/xml/ssax
  :ober/oberlib
  :ober/datadog/client)

(export main)


(declare (not optimize-dead-definitions))

(def program-name "datadog")

(def interactives
  (hash
   ("agents" (hash (description: "List all servers with Datadog Agent running.") (usage: "agents") (count: 0)))
   ("config" (hash (description: "Configure credentials for datadog.") (usage: "config") (count: 0)))
   ("contexts" (hash (description: "List all contexts") (usage: "contexts") (count: 0)))
   ("del-monitor" (hash (description: "Delete monitor.") (usage: "del-monitor <monitor id>") (count: 1)))
   ("dump" (hash (description: "Dump: dump json defintion of tboard ") (usage: "dump <tboard id>") (count: 1)))
   ("dump-monitors" (hash (description: "Dump all monitors to yaml definitions in directory passed.") (usage: "dump-monitors <directory>") (count: 1)))
   ("edit-monitor" (hash (description: "Update a monitor with new values.") (usage: "edit-monitor <id> <new query> <new name> <new message>") (count: 4)))
   ("ems" (hash (description: "Search event for last minute matching tag.") (usage: "ems") (count: 1)))
   ("events-day" (hash (description: "List all events for the past day") (usage: "events-day <tags string>") (count: 1)))
   ("events-hour" (hash (description: "List all events for past hours") (usage: "events-hour <tags string>") (count: 1)))
   ("events-min" (hash (description: "List all events for the past minute") (usage: "events-min <tags string>") (count: 1)))
   ("events-month" (hash (description: "List all events for the past month") (usage: "events-month <tags string>") (count: 1)))
   ("events-raw" (hash (description: "List all events for the past day") (usage: "events-raw <secs>") (count: 1)))
   ("events-week" (hash (description: "List all events for the past week") (usage: "events-week <tags string>") (count: 1)))
   ("find-app" (hash (description: "List all servers with app") (usage: "find-app <application name>") (count: 1)))
   ("graph-min" (hash (description: "Create a graph from query.") (usage: "graph-min <query>") (count: 1)))
   ("host" (hash (description: "host") (usage: "host <host pattern>") (count: 1)))
   ("hosts" (hash (description: "List Datadog Hosts that match argument 1.") (usage: "hosts <pattern of host to search for>") (count: 1)))
   ("indexes" (hash (description: "List Log Indexes.") (usage: "indexes") (count: 0)))
   ("live-metrics" (hash (description: "List Datadog live metrics for host.") (usage: "live-metrics <hostname>") (count: 1)))
   ("livetail" (hash (description: "List Log Indexes.") (usage: "indexes") (count: 0)))
   ("metric-tags" (hash (description: "metric-tags <metric>.") (usage: "metric-tags <metric>") (count: 1)))
   ("metric-tags-from-file" (hash (description: "metric-tags-from-file <file>.") (usage: "metric-tags-from-file <file>") (count: 1)))
   ("metrics" (hash (description: "List Datadog Metrics and search on argument 1.") (usage: "metrics <pattern of metric to search for>") (count: 1)))
   ("monitor" (hash (description: "Describe Monitor.") (usage: "monitor <monitor id>") (count: 1)))
   ("monitors" (hash (description: "List all monitors.") (usage: "monitors") (count: 0)))
   ("new-monitor" (hash (description: "Create new monitor.") (usage: "new-monitor <type> <query> <name> <message> <tags>") (count: 5)))
   ("query-day" (hash (description: "<query>: Query metrics for last day.") (usage: "query-day") (count: 1)))
   ("query-hour" (hash (description: "<query>: Query metrics for last hour.") (usage: "query-hour") (count: 1)))
   ("query-metrics" (hash (description: "<query>: Query metrics for last min.") (usage: "query-metrics") (count: 1)))
   ("query-min" (hash (description: "<query>: Query metrics for last min.") (usage: "query-min") (count: 1)))
   ("screen" (hash (description: "Descript existing screen") (usage: "screen <screen id>") (count: 1)))
   ("screens" (hash (description: "List all Screenboards") (usage: "screens") (count: 0)))
   ("search" (hash (description: "list all metrics or hosts matching pattern") (usage: "search <pattern>") (count: 1)))
   ("sproc" (hash (description: "Hosts Process Search.") (usage: "sproc <process pattern>") (count: 1)))
   ("run-agent-report" (hash (description: "Hosts Process Search.") (usage: "run-agent-report <inventory>") (count: 1)))
   ("status" (hash (description: "Get Datadog Status.") (usage: "status") (count: 0)))
   ("stories" (hash (description: "stories") (usage: "stories") (count: 0)))
   ("tag" (hash (description: "Add a tag to a hostname") (usage: "tag <host> <tag name:value>") (count: 2)))
   ("tags" (hash (description: "list all tags and hosts") (usage: "tags") (count: 0)))
   ("tboard" (hash (description: "Get info on timeboard.") (usage: "timeboards <timeboard id>")(count: 1)))
   ("tboard-add-chart" (hash (description: "Add a chart to a timeboard.") (usage: "tboard-add-chart <timeboard id> <title> <request> <viz>")(count: 4)))
   ("tboard-create" (hash (description: "Create a new timeboard") (usage: "tboard-create <title> <description>") (count: 2)))
   ("tboard-fancy" (hash (description: "Add charts for metrics which apply to tag provided.") (usage: "tboard-fancy <timeboard id> <metric pattern ^$ supported> <tag clause. key:value> <replace all charts? ? t or f>")(count: 4)))
   ("tboard-mass-add" (hash (description: "Add charts matching regexp metrics to a timeboard.") (usage: "tboard-add-chart <timeboard id> <pattern> <host clause> <Group by clause> <replace all charts? ? t or f>")(count: 5)))
   ("tboard-mass-add-many" (hash (description: "Add charts matching regexp metrics to a timeboard. Useful for multiple hosts.") (usage: "tboard-add-chart <timeboard id> <pattern> <host clause 'host1 host2 host3'> <replace all charts? t/f>  ")(count: 4)))
   ("tboards" (hash (description: "List all timeboards") (usage: "timeboards")(count: 0)))
   ("totals" (hash (description: "Host Totals.") (usage: "totals") (count: 0)))
   ("verify-account" (hash (description: "Verify account credentials") (usage: "validate") (count: 0)))
   ("verify-apps" (hash (description: "Verify account credentials") (usage: "verify-apps") (count: 0)))
   ("view-md" (hash (description: "Describe metric metadata") (usage: "view-md") (count: 1)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
	 (verb (car args))
	 (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
	   (count (hash-get info count:)))
      (unless count
	(set! count 0))
      (unless (= (length args2) count)
	(usage-verb verb))
      (apply (eval (string->symbol (string-append "dda#" verb))) args2))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln "Datadog version: " version)
  (displayln "Usage: datadog <verb>")
  (displayln "Verbs:")
  (for (k (sort! (hash-keys interactives) string<?))
       (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
  (exit 2))
