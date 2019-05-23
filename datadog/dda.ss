;; -*- Gerbil -*-
namespace: dda

(export main)
(declare (not optimize-dead-definitions))

(import
  :gerbil/gambit
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/debug/heap
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/xml/ssax
  :std/text/zlib
  :std/error
  :gerbil/gambit/ports
  )

(def config-file "~/.datadog.yaml")

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time builtin-time)))

(def DEBUG (getenv "DEBUG" #f))

(def program-name "datadog")
(def datadog-api-key #f)
(def datadog-app-key #f)
(def datadog-host "app.datadoghq.com")

(def good-ips (hash))

(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def interactives
  (hash
   ("config" (hash (description: "Configure credentials for datadog.") (usage: "config") (count: 0)))
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
   ("graph-min" (hash (description: "Create a graph from query.") (usage: "graph-min <query>") (count: 1)))
   ("metric-tags" (hash (description: "get-tags-for-metric <metric>.") (usage: "get-tags-for-metric <metric>") (count: 1)))
   ("metrics" (hash (description: "List Datadog Metrics and search on argument 1.") (usage: "metrics <pattern of metric to search for>") (count: 1)))
   ("live-metrics" (hash (description: "List Datadog live metrics for host.") (usage: "live-metrics <hostname>") (count: 1)))
   ("totals" (hash (description: "Host Totals.") (usage: "totals") (count: 0)))
   ("indexes" (hash (description: "List Log Indexes.") (usage: "indexes") (count: 0)))
   ("livetail" (hash (description: "List Log Indexes.") (usage: "indexes") (count: 0)))
   ("contexts" (hash (description: "List all contexts") (usage: "contexts") (count: 0)))
   ("procs" (hash (description: "List all processes for host") (usage: "procs <hostname> < How many seconds ago?>") (count: 2)))
   ("stories" (hash (description: "stories") (usage: "stories") (count: 0)))
   ("host" (hash (description: "host") (usage: "host <host pattern>") (count: 1)))
   ("hosts" (hash (description: "List Datadog Hosts that match argument 1.") (usage: "hosts <pattern of host to search for>") (count: 1)))
   ("monitor" (hash (description: "Describe Monitor.") (usage: "monitor <monitor id>") (count: 1)))
   ("monitors" (hash (description: "List all monitors.") (usage: "monitors") (count: 0)))
   ("status" (hash (description: "Get Datadog Status.") (usage: "status") (count: 0)))
   ("new-monitor" (hash (description: "Create new monitor.") (usage: "new-monitor <type> <query> <name> <message> <tags>") (count: 5)))
   ("query-day" (hash (description: "<query>: Query metrics for last day.") (usage: "query-day") (count: 1)))
   ("query-hour" (hash (description: "<query>: Query metrics for last hour.") (usage: "query-hour") (count: 1)))
   ("query-metrics" (hash (description: "<query>: Query metrics for last min.") (usage: "query-metrics") (count: 1)))
   ("query-min" (hash (description: "<query>: Query metrics for last min.") (usage: "query-min") (count: 1)))
   ("screen" (hash (description: "Descript existing screen") (usage: "screen <screen id>") (count: 1)))
   ("screens" (hash (description: "List all Screenboards") (usage: "screens") (count: 0)))
   ("search" (hash (description: "list all metrics or hosts matching pattern") (usage: "search <pattern>") (count: 1)))
   ("tag" (hash (description: "Add a tag to a hostname") (usage: "tag <host> <tag name:value>") (count: 2)))
   ("tags" (hash (description: "list all tags and hosts") (usage: "tags") (count: 0)))
   ("tboard" (hash (description: "Get info on timeboard.") (usage: "timeboards <timeboard id>")(count: 1)))
   ("tboard-add-chart" (hash (description: "Add a chart to a timeboard.") (usage: "tboard-add-chart <timeboard id> <title> <request> <viz>")(count: 4)))
   ("tboard-create" (hash (description: "Create a new timeboard") (usage: "tboard-create <title> <description>") (count: 2)))
   ("tboard-mass-add" (hash (description: "Add charts matching regexp metrics to a timeboard.") (usage: "tboard-add-chart <timeboard id> <pattern> <host clause> <Group by clause> <replace all charts? ? t or f>")(count: 5)))
   ("tboard-mass-add-many" (hash (description: "Add charts matching regexp metrics to a timeboard. Useful for multiple hosts.") (usage: "tboard-add-chart <timeboard id> <pattern> <host clause 'host1 host2 host3'> <replace all charts? t/f>  ")(count: 4)))
   ("tboards" (hash (description: "List all timeboards") (usage: "timeboards")(count: 0)))
   ("verify-account" (hash (description: "Verify account credentials") (usage: "validate") (count: 0)))
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

(def (load-config)
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    (let-hash config
      (when .?secrets
	(let-hash (u8vector->object (base64-decode .secrets))
	  (hash-put! config 'datadog-api-key (decrypt-bundle .api-key))
	  (hash-put! config 'datadog-app-key (decrypt-bundle .app-key))
	  (hash-put! config 'username (decrypt-bundle .username))
	  (hash-put! config 'password (decrypt-bundle .password)))))
    config))

(def (ensure-api-keys)
  (unless (and
	    datadog-app-key
	    datadog-api-key)
    (let-hash (load-config)
      (set! datadog-app-key .datadog-app-key)
      (set! datadog-api-key .datadog-api-key)
      )))

(def (resolve-ipv4 host)
  (if (hash-key? good-ips host)
    (hash-get good-ips host)
    (let* ((host-info (host-info-addresses (host-info host))))
      (dp (format "host-info: ~a type:~a" host-info (type-of host-info)))
      (ip4-address->string
       (car host-info)))))

(def (make-dd-uri ip adds)
  (ensure-api-keys)
  (let* ((datadog-base-url (format "https://~a/api/v1/" ip)))
    (if (string-contains adds "?")
      (string-concatenate [ datadog-base-url adds (format "&api_key=~a&application_key=~a" datadog-api-key datadog-app-key)])
      (string-concatenate [ datadog-base-url adds (format "?api_key=~a&application_key=~a" datadog-api-key datadog-app-key)]))))

(def (make-dd-uri-metric ip adds)
  (ensure-api-keys)
  (let* ((datadog-base-url (format "https://~a/" ip)))
    (if (string-contains adds "?")
      (string-concatenate [ datadog-base-url adds (format "&api_key=~a&application_key=~a" datadog-api-key datadog-app-key)])
      (string-concatenate [ datadog-base-url adds (format "?api_key=~a&application_key=~a" datadog-api-key datadog-app-key)]))))

(def (make-test-uri ip adds)
  (let* ((datadog-base-url (format "http://~a/headers" ip)))
    (if (string-contains adds "?")
      (string-concatenate [ datadog-base-url adds (format "&api_key=~a&application_key=~a" datadog-api-key datadog-app-key)])
      (string-concatenate [ datadog-base-url adds (format "?api_key=~a&application_key=~a" datadog-api-key datadog-app-key)]))))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (verify-account)
  (let* ((ip datadog-host)
	 (req (http-get (make-dd-uri ip "validate")))
	 (status (request-status req))
	 (valid (success? status)))
    (unless valid
      (displayln "Credentials are not valid. got status:" status " with error:"  (request-text req)))))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (try
   (let* ((reply (http-post uri
			    headers: headers
			    data: data))
	  (status (request-status reply))
	  (text (request-text reply)))

     (if (success? status)
       (displayln status text)
       (displayln (format "Failure on post. Status:~a Text:~a~%" status text))))
   (catch (e)
     (begin
       (let ((uri2 (get-new-ip uri datadog-host)))
	 (display-exception e)
	 (do-post uri2 headers data))))))

(def (get-new-ip uri host)
  (pregexp-replace "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" uri (resolve-ipv4 host)))

(def (print-object obj)
  #f)

(def (do-put uri headers data)
  (dp (print-curl "put" uri headers data))
  (let* ((reply (http-put uri
			  headers: headers
			  data: data))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on post. Status:~a Text:~a~%" status text)))))

(def (do-delete uri headers params)
  (dp (print-curl "delete" uri headers params))
  (let* ((reply (http-delete uri
			     headers: headers
			     params: params))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on delete. Status:~a Text:~a~%" status text)))))

(def (print-opts t)
  (let-hash t
    (displayln
     " end: " .end
     " unit: "  (stringify-hash .unit)
     " length: " .length
     " attributes: " (stringify-hash .attributes)
     " metric: " .metric
     " display_name: " .display_name
     " aggr: " .aggr
     " query_index: " .query_index
     " start: " .start
     " expression: " .expression
     " interval: " .interval
     " scope: " .scope)
    (print-points .pointlist)))

(def (print-points points)
  (for-each
    (lambda (point)
      (let ((date (car point))
	    (datum (car (cdr point))))
	(unless (fixnum? datum)
	  (set! datum (float->int datum)))
	(displayln (date->string (epoch->date date) "~s") " " datum)))
    points))

(def (stringify-hash h)
  (let ((results []))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results [ (format " ~a->" k) (format "~a   " v)])))
	 h)
	(append-strings results))
      "N/A")))


(def (make-metric-string str)
  (pregexp-replace* "," (pregexp-replace* " " str "-") ""))

(def (query-metrics from-s to-s query)
  (verify-account)
  (let* ((adds (format "query?from=~a&to=~a&query=~a" from-s to-s query))
	 (ip datadog-host)
	 (uri (make-dd-uri ip adds))
	 (results (do-get uri))
	 (myjson (from-json results))
	 (status (hash-ref myjson 'status)))
    (if (string= "ok" status)
      (begin
	(for-each
	  (lambda (p)
	    (displayln p)
	    (print-opts p))
	  (hash-get myjson 'series)))
      (begin ;; failure to find stuff
	(displayln "Failed with status:" status)))))

(def (query-last-secs secs query)
  (let* ((start (float->int (- (time->seconds (builtin-current-time)) secs)))
	 (end (float->int (time->seconds (builtin-current-time)))))
    (query-metrics start end query)))

(def (query-min query)
  (query-last-secs 60 query))

(def (query-hour query)
  (query-last-secs 3600 query))

(def (query-day query)
  (query-last-secs 86400 query))

(def (print-curl type uri headers data)
  ;;(displayln headers)
  (let ((heads "Content-type: application/json")
	(do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
	(if (string=? "" data)
	  (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
	  (displayln (format "curl -X GET -H \'~a\' -d \'~a\' \'~a\'" heads data uri))))
       ((string=? type "put")
	(displayln (format "curl -X PUT -H \'~a\' -d \'~a\' \'~a\'" heads data uri)))
       ((string=? type "post")
	(displayln (format "curl -X POST -H \'~a\' -d \'~a\' \'~a\'" heads data uri)))
       ((string=? type "delete")
	(displayln (format "curl -X DELETE -H \'~a\' -d \'~a\' \'~a\'" heads data uri)))
       (else
	(displayln "unknown format " type))))))

(def (do-get uri)
  (print-curl "get" uri "" "")
  (let* ((reply (http-get uri))
	 (status (request-status reply))
	 (text (request-text reply)))
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (do-post-generic uri headers data)
  (let* ((reply (http-post uri
			   headers: headers
			   data: data))
	 (status (request-status reply))
	 (text (request-text reply)))
    (dp (print-curl "post" uri headers data))
    (if (success? status)
      text
      (displayln (format "Error: Failure on a post. got ~a text: ~a~%" status text)))))

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
			  headers: headers))
	 (status (request-status reply))
	 (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (metrics pattern)
  (verify-account)
  (let* ((ip datadog-host)
	 (uri
	  (make-dd-uri ip
		       (format "metrics?from=~a"
			       (inexact->exact
				(round
				 (-
				  (time->seconds
				   (builtin-current-time)) 9000))))))
	 (results (do-get uri))
	 (metrics
	  (hash-get
	   (from-json results)
	   'metrics)))
    (for-each
      (lambda (m)
	(if pattern
	  (if (string-contains m pattern)
	    (displayln m))))
      metrics)))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln "Usage: datadog <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(def (view-md metric)
  (verify-account)
  (let* ((adds (format "metrics/~a" metric))
	 (ip datadog-host)
	 (uri (make-dd-uri ip adds))
	 (results (do-get uri))
	 (myjson (from-json results))
	 (description (hash-get myjson 'description))
	 (integration (hash-get myjson 'integration))
	 (statsd_interval (hash-get myjson 'statsd_interval))
	 (type (hash-get myjson 'type))
	 (unit (hash-get myjson 'unit)))
    (displayln (format "description:~a integration:~a statsd_interval:~a type:~a unit:~a"
		       (if (void? description)
			 "N/A"
			 description)
		       (if (void? integration)
			 "N/A"
			 integration)
		       (if (void? statsd_interval)
			 "N/A"
			 statsd_interval)
		       (if (void? type)
			 "N/A"
			 type)
		       (if (void? unit)
			 "N/A"
			 unit)))))

(def (edit-metric-metadata metric description short_name)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "metrics/~a" metric)))
	 (data (json-object->string
		(hash
		 ("description" description)
		 ("short_name" short_name)))))
    (displayln "json is: " data)
    (do-put uri headers data)))

(def (submit-event title text priority tags alert_type)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip "events"))
	 (data (json-object->string
		(hash
		 ("title" title)
		 ("text" text)
		 ("priority" priority)
		 ("tags" tags)
		 ("alert_type" alert_type)))))
    (do-post uri headers data)))

(def (get-events-time start end)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "events?start=~a&end=~a" start end))))
    (do-get uri)))

(def (get-events-tags start end tags)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "events?start=~a&end=~a&tags=~a" start end tags))))
    (do-get uri)))

(def (float->int num)
  (inexact->exact
   (round num)))

(def (get-events-last-secs secs tags)
  (let* ((start (float->int (- (time->seconds (builtin-current-time)) secs)))
	 (end (float->int (time->seconds (builtin-current-time))))
	 (results (get-events-tags start end tags))
	 (events (from-json results))
	 (events2 (hash-get events 'events)))
    (print-events events2)))

(def (get-events-last-secs-raw secs)
  (let* ((start (float->int (- (time->seconds (builtin-current-time)) secs)))
	 (end (float->int (time->seconds (builtin-current-time))))
	 (results (get-events-time start end))
	 (events (from-json results))
	 (events2 (hash-get events 'events)))
    (print-events events2)))

(def (events-raw secs)
  (get-events-last-secs-raw (string->number secs)))

(def (events-hour tags)
  (get-events-last-secs 3600 tags))

(def (events-min tags)
  (get-events-last-secs 60 tags))

(def (events-day tags)
  (get-events-last-secs 86400 tags))

(def (events-week tags)
  (get-events-last-secs (* 7 86400) tags))

(def (events-month tags)
  (get-events-last-secs (* 30 86400) tags))

(def (print-events events)
  (for-each
    (lambda (event)
      (let-hash event
	(displayln
	 " source: " .source
	 " priority: " .priority
	 " -" .date_happened
	 " title: " .title
	 " id: " .id
	 " url: " .url
	 " host: " .host
	 " is_aggregate: " .is_aggregate
	 " device_name: " .device_name
	 " tags: " .tags
	 " resource: " .resource
	 " alert_type: " .alert_type
	 " text: " .text
	 " comments: " .comments
	 (if (hash-key? event 'children)
	   (print-children .children)))))
    events))

(def (print-date date)
  (date->string date "~c"))

(def (print-children children)
  (for-each
    (lambda (child)
      (let-hash child
	(displayln
	 " -: " (print-date (epoch->date .date_happened))
	 " alert_type: " .alert_type
	 " id: " .id)))
    children))

(def (downtimes)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "downtime"))
	 (dts (from-json (do-get uri))))
    (for-each
      (lambda (dt)
	(let
	    ((start (hash-get dt 'start))
	     (parent_id (hash-get dt 'parent_id))
	     (scope (hash-get dt 'scope))
	     (downtime_type (hash-get dt 'downtime_type))
	     (timezone (hash-get dt 'timezone))
	     (id (hash-get dt 'id))
	     (message (hash-get dt 'message))
	     (updater_id (hash-get dt 'updater_id))
	     (recurrence (hash-get dt 'recurrence))
	     (canceled (hash-get dt 'canceled))
	     (disabled (hash-get dt 'disabled))
	     (creator_id (hash-get dt 'creator_id))
	     (end (hash-get dt 'end))
	     (active (hash-get dt 'active))
	     (monitor_id (hash-get dt 'monitor_id)))
	  (displayln
	   " start:" start
	   " parent_id:" parent_id
	   " scope:" scope
	   " downtime_type:" downtime_type
	   " timezone:" timezone
	   " id:" id
	   " message:" message
	   " updater_id:" updater_id
	   " recurrence:" recurrence
	   " canceled:" canceled
	   " disabled:" disabled
	   " creator_id:" creator_id
	   " end:" end
	   " active:" active
	   " monitor_id:" monitor_id
	   )))
      dts)))

(def (screen id)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "screen/~a" id)))
	 (results (from-json (do-get uri))))
    (print-screen results)))

(def (print-screens screen)
  (let-hash
      screen
    (displayln
     " created: "  .created
     " resource: " .resource
     " id: " .id
     " description: " .description
     " modified: " .modified
     " read_only: " .read_only
     " created_by: " (hash-keys .created_by)
     " title: " .title
     )))

(def (print-widgets widgets)
  (for-each
    (lambda (widget)
      (let-hash
	  widget
	(if (string=? .type "query_value")
	  (let-hash
	      .tile_def
	    (print-widget-requests .requests)))))
    widgets))

(def (print-widget-requests requests)
  (for-each
    (lambda (request)
      (let-hash
	  request
	(displayln
	 (when (hash-key? request 'style)" style: " (print-widget-style .style))
	 " aggregator: " .aggregator
	 " q: " .q)
	(print-widget-conditional-formats .conditional_formats)))
    requests))

(def (print-widget-style style)
  (let-hash
      style
    (format " width: ~a type: ~a palette: ~a" .width .type .palette)))

(def (print-widget-conditional-formats formats)
  (for-each
    (lambda (cformat)
      (let-hash
	  cformat
	(displayln (format "	 value: ~a comparator: ~a palette: ~a" .value .comparator .palette))))
    formats))

(def (print-screen screen)
  (let-hash
      screen
    (displayln
     " isShared: " .isShared
     " modified: " .modified
     " disableEditing: " .disableEditing
     ;;    " widgets: " .widgets
     " board_title: " .board_title
     " id: " .id
     " board_bgtype: " .board_bgtype
     " height: " .height
     " width: " .width
     " isIntegration: " .isIntegration
     ;;" templated: " .templated
     " read_only: " .read_only
     " template_variables: " .template_variables
     " created_by: " .created_by
     " disableCog: " .disableCog
     " created: " .created)
    (print-widgets .widgets)))

(def (make-graph title request viz)
  (hash
   ("title" title)
   ("definition"
    (hash
     ("events" [])
     ("requests"
      [
       (hash
	("q" request)
	) ])))
   ("viz" viz)))

(def (tboard-create title description)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri datadog-host "dash"))
	 (data (json-object->string
		(hash
		 ("graphs" [ (make-graph (metric-name-to-title title) "avg:system.mem.free{*}" "timeseries")])
		 ("title" title)
		 ("description" description)
		 ("template_variables"
		  [ (hash
		     ("name" "host1")
		     ("prefix" "host")
		     ("default" "host:my-host")) ])
		 )))
	 (results (do-post uri headers data)))
    (displayln results)))

(def (tboard-mass-add id metric-pattern host-clause groupby replace)
  (let* ((tbinfo (get-tboard id))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "dash/~a" id)))
	 (dash (hash-get tbinfo 'dash))
	 (graphs (hash-get dash 'graphs))
	 (title (hash-get dash 'title))
	 (new-graphs [])
	 (headers [["Content-type" :: "application/json"]]))
    (unless (string=? replace "t")
      (set! new-graphs graphs))
    (for-each
      (lambda (m)
	(let ((new-graph
	       (make-graph
		(metric-name-to-title m)
		(format "avg:~a{~a}by{~a}" m host-clause groupby) "timeseries")))
	  (set! new-graphs (append new-graphs [new-graph]))))
      (sort! (search-metrics metric-pattern) string<?))

    (do-put uri headers
	    (json-object->string
	     (hash
	      ("graphs" new-graphs)
	      ("title" title)
	      ("description" (hash-get dash 'description)))))))

(def (tboard-netdata-dashboard id metric-pattern host-clause groupby replace)
  (let* ((tbinfo (get-tboard id))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "dash/~a" id)))
	 (dash (hash-get tbinfo 'dash))
	 (graphs (hash-get dash 'graphs))
	 (title (hash-get dash 'title))
	 (new-graphs [])
	 (headers [["Content-type" :: "application/json"]]))
    (unless (string=? replace "t")
      (set! new-graphs graphs))
    (for-each
      (lambda (m)
	(let ((new-graph
	       (make-graph
		(metric-name-to-title m)
		(format "avg:~a{~a}by{~a}" m host-clause groupby) "timeseries")))
	  (set! new-graphs (append new-graphs [new-graph]))))
      (sort! (search-metrics metric-pattern) string<?))

    (do-put uri headers
	    (json-object->string
	     (hash
	      ("graphs" new-graphs)
	      ("title" title)
	      ("description" (hash-get dash 'description)))))))

(def (make-query-for-hosts metric hosts)
  (let ((results []))
    (for-each
      (lambda (host)
	(set! results (append results [(format "avg:~a{host:~a}" metric host)]))
	hosts)
      hosts)
    (string-join results ",")))

(def (tboard-mass-add-many id metric-pattern host-pattern replace)
  (let* ((tbinfo (get-tboard id))
	 (hosts (search-hosts host-pattern))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "dash/~a" id)))
	 (dash (hash-get tbinfo 'dash))
	 (graphs (hash-get dash 'graphs))
	 (title (hash-get dash 'title))
	 (new-graphs [])
	 (headers '(("Content-type" . "application/json"))))
    (unless (string=? replace "t")
      (set! new-graphs graphs))
    (for-each
      (lambda (m)
	(let ((new-graph
	       (make-graph
		(metric-name-to-title m)
		(make-query-for-hosts m hosts)
		"timeseries")))
	  (set! new-graphs (append new-graphs [new-graph]))))
      (sort! (search-metrics metric-pattern) string<?))

    (do-put uri headers
	    (json-object->string
	     (hash
	      ("graphs" new-graphs)
	      ("title" title)
	      ("description" (hash-get dash 'description)))))))

(def (metric-name-to-title metric)
  (let* ((no-netdata (pregexp-replace "^netdata." metric ""))
	 (dot-to-space (pregexp-replace "\\." no-netdata " "))
	 (no-es (pregexp-replace "elasticsearch_local\\." no-netdata "")))
    no-es))

(def (tboard-add-chart id title request viz)
  (let* ((tbinfo (get-tboard id))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "dash/~a" id)))
	 (dash (hash-get tbinfo 'dash))
	 (graphs (hash-get dash 'graphs))
	 (new-graph (make-graph (metric-name-to-title title) request viz))
	 (headers '(("Content-type" . "application/json"))))
    (do-put uri headers
	    (json-object->string
	     (hash
	      ("graphs" (append graphs [ new-graph ]))
	      ("title" (hash-get dash 'title))
	      ("description" (hash-get dash 'description)))))))

(def (get-tboard id)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "dash/~a" id))))
    (from-json (do-get uri))))

(def (tboard id)
  (let ((tbinfo (get-tboard id)))
    (let-hash tbinfo
      (let-hash .dash
	(displayln
	 "id: " .?id
	 " title: " .?title
	 " resource: " ..?resource
	 " url: " ..?url
	 " dash: "
	 " created: " .?created
	 " modified: " .?modified
	 " description: " .?description
	 " read_only: " .?read_only
	 " created_by: " (hash-keys .?created_by))
	;;                      " graphs: " (hash-keys (car .graphs))
	(print-graphs .?graphs)
	))))

(def (dump id)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "dash/~a" id))))
    (let-hash (from-json (do-get uri))
      (displayln (json-object->string .dash)))))

(def (print-graphs graphs)
  (let ((results ""))
    (for-each
      (lambda (graph)
	(let-hash
	    graph
	  (let-hash
	      .definition
	    (cond
	     ((string=? .viz "toplist")
	      (displayln "<--- chart:" (format " status:~a requests:~a autoscale:~a viz:~a " .?status .?requests .?autoscale .?viz)))
	     ((string=? .viz "timeseries")
	      (displayln "<--- chart:" (format " requests:~a autoscale:~a viz:~a " (print-requests .requests) .?autoscale .?viz)))
	     (else
	      (format "Unknown chart type viz: ~a. keys: ~a" .viz))))))
      graphs)
    results))

(def (print-requests requests)
  (let ((results ""))
    (for-each
      (lambda (request)
	(hash-for-each
	 (lambda (k v)
	   (if (hash-table? v)
	     (set! results (string-append results (format " ~a:~a " k (stringify-hash v))))
	     (set! results (string-append results (format " ~a:~a " k v)))))
	 request))
      requests)
    results))

(def (tboards)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "dash"))
	 (tboardz (from-json (do-get uri)))
	 (timeboards (hash-get tboardz 'dashes)))
    (displayln "|Description|Id|Resource|Title|Created|Modified|RO?|")
    (displayln "|-|")
    (for-each
      (lambda (timeboard)
	(let-hash timeboard
	  (displayln
	   "|" .description
	   "|" .id
	   "|"  .resource
	   "|" .title
	   "|" .created
	   "|" .modified
	   "|" .read_only "|"
	   )))
      timeboards)))

(def (screens)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "screen"))
	 (screenz (from-json (do-get uri)))
	 (screenboards (hash-get screenz 'screenboards)))
    (for-each
      (lambda (screen)
	(print-screens screen))
      screenboards)))

(def (screen-create board_title widgets width height)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip "screen"))
	 (data (json-object->string
		(hash
		 ("width" width)
		 ("height" height)
		 ("board_title" board_title)
		 ("widgets"
		  [
		   (hash
		    ("type" "image")
		    ("height" 20)
		    ("width" 32)
		    ("y" "7")
		    ("x" "32")
		    ("url" "https://upload.wikimedia.org/wikipedia/commons/b/b4/Kafka.jpg"))])))))
    (displayln (hash-keys (from-json data)))
    (do-post uri headers data)))

(def (screen-update id width height board_title)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "screen/~a" id)))
	 (data (json-object->string
		(hash
		 ("width" width)
		 ("height" height)
		 ("board_title" board_title)
		 ("widgets"
		  [
		   (hash
		    ("type" "image")
		    ("height" 20)
		    ("width" 32)
		    ("y" "7")
		    ("x" "32")
		    ("url" "https://upload.wikimedia.org/wikipedia/commons/b/b4/Kafka.jpg"))])))))
    (displayln (hash-keys (from-json data)))
    (do-put uri headers data)))

(def (search query)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "search?q=~a" query)))
	 (results (hash-get (from-json (do-get uri)) 'results))
	 (metrics (hash-get results 'metrics))
	 (hosts (hash-get results 'hosts)))
    (for-each
      (lambda (m)
	(displayln "metric: " m))
      metrics)
    (for-each
      (lambda (h)
	(displayln "host: " h))
      hosts)))

(def (search-metrics pattern)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "search?q=~a" pattern)))
	 (metrics-matched [])
	 (results (hash-get (from-json (do-get uri)) 'results))
	 (metrics (hash-get results 'metrics)))
    (for-each
      (lambda (m)
	(dp (format "(pregexp-match \"~a\" ~a)" pattern m))
	(when (pregexp-match pattern m)
	  (set! metrics-matched (cons m metrics-matched))))
      metrics)
    metrics-matched))

(def (hosts pattern)
  (let ((results (search-hosts pattern)))
    (when (list? results)
      (for-each
	(lambda (r)
	  (displayln r))
	results))))

(def (search-hosts pattern)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "search?q=~a" pattern)))
	 (hosts-matched [])
	 (results (hash-get (from-json (do-get uri)) 'results))
	 (hosts (hash-get results 'hosts)))
    (dp (format "search-hosts pattern:~a host:~a results:~a" pattern hosts results))
    (for-each
      (lambda (h)
	(if (string-contains h pattern)
	  (begin
	    (dp (format "host: ~a matches ~a)" h pattern))
	    (set! hosts-matched (cons h hosts-matched)))
	  (begin
	    (dp (format "host:~a does NOT match ~a" h pattern)))))
      hosts)
    hosts-matched))

(def (tag host-pattern tag)
  "Tag a given host pattern with a given tag"
  (let* ((headers '(("Content-type" . "application/json")))
	 (hosts (search-hosts host-pattern))
	 (ip datadog-host)
	 (data (json-object->string
		(hash
		 ("tags" [ tag ])))))
    (for-each
      (lambda (h)
	(let ((uri (make-dd-uri ip (format "tags/hosts/~a" h))))
	  (displayln "doing " h)
	  (displayln (do-post uri headers data))
	  (dp (format "tag data is ~a uri: ~a" data uri))))
      hosts)))

(def (tags)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "tags/hosts"))
	 (tags (hash-get (from-json (do-get uri)) 'tags)))
    (for-each
      (lambda (k)
	(displayln k))
      (hash-keys tags))))

(def (tags-for-metric metric)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "tags/metrics"))
	 (tags (hash-get (from-json (do-get uri)) 'tags)))
    (for-each
      (lambda (k)
	(displayln k))
      (hash-keys tags))))

(def (tags-for-source source)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "tags/hosts/~a" source)))
	 (tags (hash-get (from-json (do-get uri)) 'tags)))
    (for-each
      (lambda (k)
	(displayln k))
      (hash-keys tags))))

(def (graph query start end)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "graph/snapshot?metric_query=~a&start=~a&end=~a" query start end)))
	 (results (do-get uri))
	 (url (hash-get (from-json results) 'snapshot_url)))
    (displayln url)))

(def (graph-last-secs secs query)
  (let* ((start (float->int (- (time->seconds (builtin-current-time)) secs)))
	 (end (float->int (time->seconds (builtin-current-time)))))
    (graph query start end)))

(def (graph-min query)
  (query-last-secs 60 query))

(def (graph-hour query)
  (query-last-secs 3600 query))

(def (edit-monitor id query name message)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "montior/~a" id)))
	 (data (json-object->string
		(hash
		 ("query" query)
		 ("name" name)
		 ("message" message)))))
    (displayln "json is:"  data)
    (do-put uri headers data)))

(def (del-monitor id)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip (format "monitor/~a" id))))
    (do-delete uri headers #f)))

(def (new-monitor type query name message tags)
  (let* ((headers '(("Content-type" . "application/json")))
	 (ip datadog-host)
	 (uri (make-dd-uri ip "monitor"))
	 (data (json-object->string
		(hash
		 ("type" type)
		 ("query" query)
		 ("name" name)
		 ("message" message)
		 ("tags" [tags]) ;; this must be a list
		 ("options"
		  (hash
		   ("notify_no_data" #t)
		   ("no_data_timeframe" 20)))))))
    (do-post uri headers data)))

(def (from-json json)
  (try
   (with-input-from-string json read-json)
   (catch (e)
     (displayln "error parsing json " e))))

(def (monitor id)
  (displayln (format "* Datadog Monitor: ~a" id))
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip (format "monitor/~a" id)))
	 (mon (from-json (do-get uri))))
    (print-monitor-long mon)))

(def (print-monitor monitor)
  (let-hash monitor
    (displayln
     "|" .id
     "|" (if (table? .creator) (let-hash .creator .name " " .email " " .handle) "N/A")
     "|" .query
     "|" .message
     "|" .tags
     "|" (stringify-hash .options)
     "|" .org_id
     "|" .type
     "|" .multi
     "|" .created
     "|" .modified "|")))

(def (print-monitor-long monitor)
  (let-hash monitor
    (displayln "** Query: " .query)
    (displayln "*** Id: " .id)
    (displayln "*** Message:")
    (displayln .message)
    (displayln "*** Tags: " .tags)
    (displayln "*** Creator: " (if (table? .creator) (let-hash .creator .name " " .email " " .handle) "N/A"))
    (displayln "*** Options: ")
    (let-hash .options
      (displayln "	- require_full_window: " .?require_full_window)
      (displayln "	- include_tags: " .?include_tags)
      (displayln "	- renotify_interval: " .?renotify_interval)
      (displayln "	- notify_audit: " .?notify_audit)
      (displayln "	- notify_no_data: " .?notify_no_data)
      (displayln "	- no_data_timeframe: " .?no_data_timeframe)
      (displayln "	- locked: " .?locked)
      (displayln "	- silenced: ")
      (when (and .?silenced (table? .silenced))
	(hash-for-each
	 (lambda (k v)
	   (displayln "	- " k ": " v))
	 .silenced))
      (when (and .?thresholds (table? .silenced))
	(hash-for-each
	 (lambda (k v)
	   (displayln "	- " k ": " v))
	 .thresholds))
      (displayln "	- new_host_delay: " .?new_host_delay)
      (displayln "	- timeout_h: " .?timeout_h)
      (displayln "	- escalation_message: " .?escalation_message))
    (displayln "*** Created: " .?created)
    (displayln "*** Modified: " .?modified)))

(def (monitors)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "monitor"))
	 (results (from-json (do-get uri))))
    (displayln "* Datadog Monitors")
    (for-each
      (lambda (monitor)
	(print-monitor-long monitor))
      results)))

(def (dump-monitors dir)
  (let* ((ip datadog-host)
	 (uri (make-dd-uri ip "monitor"))
	 (results (from-json (do-get uri))))
    (for-each
      (lambda (monitor)
	(let-hash monitor
	  (yaml-dump (format "~a/~a.yaml" dir .id) (format-monitor monitor))))
      results)))

(def (format-monitor monitor)
  "Try to order the keys in this hash to consistently represent them in yaml"
  (let-hash monitor
    (hash
     (name .name)
     (query .query)
     (id .id)
     (multi .multi)
     (created .created)
     (created_at .created_at)
     (overall_state .overall_state)
     (creator (let-hash .creator
		(hash
		 (name .name)
		 (email .email)
		 (id .id)
		 (handle .handle))))
     (type .type)
     (org_id .org_id)
     (options (let-hash .options
		(hash
		 (notify_no_data (sis .?notify_no_data))
		 (timeout_h (sis .?timeout_h))
		 (silenced (sis .?silenced))
		 (renotify_interval (sis .?renotify_interval))
		 (notify_audit (sis .?notify_audit))
		 (locked (sis .?locked))
		 (new_host_delay (sis .?new_host_delay))
		 (no_data_timeframe (sis .?no_data_timeframe)))))
     (deleted .deleted)
     (matching_downtimes .matching_downtimes)
     (modified .modified)
     (overall_state_modified .overall_state_modified)
     (tags .tags)
     (message .message))))

(def (sis item)
  (if item
    item
    "N/A"))

(def (epoch->date epoch)
  (cond
   ((string? epoch)
    (time-utc->date (make-time time-utc 0 (string->number epoch))))
   ((flonum? epoch)
    (time-utc->date (make-time time-utc 0 (float->int epoch))))
   ((fixnum? epoch)
    (time-utc->date (make-time time-utc 0 epoch)))))

(def (date->epoch mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~d ~H:~M:~S") "~s")))

(def (flatten x)
  (cond ((null? x) [])
	((pair? x) (append (flatten (car x)) (flatten (cdr x))))
	(else [x])))

(def (data->get uri data)
  (if (table? data)
    (string-append
     uri "?"
     (string-join
      (hash-fold
       (lambda (key val r)
	 (cons
	  (string-append key "=" val) r))
       [] data) "&"))
    (displayln "not a table. got " data)))

(def (config)
  (displayln "Please enter your DataDog API Key:")
  (def api-key (read-line (current-input-port)))
  (displayln "Please enter your DataDog Application Key:")
  (def app-key (read-line (current-input-port)))
  (displayln "Please enter your DataDog Username:")
  (def username (read-line (current-input-port)))
  (displayln "Please enter your DataDog Password:")
  (def password (read-line (current-input-port)))
  (def secrets (base64-encode
		(object->u8vector
		 (hash
		  (api-key (encrypt-string api-key))
		  (app-key (encrypt-string app-key))
		  (username (encrypt-string username))
		  (password (encrypt-string password))))))

  (displayln "Add the following lines to your " config-file)
  (displayln "-----------------------------------------")
  (displayln "secrets: " secrets)
  (displayln "-----------------------------------------"))

(def (encrypt-string str)
  (let* ((cipher (make-aes-256-ctr-cipher))
	 (iv (random-bytes (cipher-iv-length cipher)))
	 (key (random-bytes (cipher-key-length cipher)))
	 (encrypted-password (encrypt cipher key iv str))
	 (enc-pass-store (u8vector->base64-string encrypted-password))
	 (iv-store (u8vector->base64-string iv))
	 (key-store (u8vector->base64-string key)))
    (hash
     (key key-store)
     (iv iv-store)
     (password enc-pass-store))))

(def (decrypt-password key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (decrypt-bundle bundle)
  (let-hash bundle
    (decrypt-password .key .iv .password)))

(def datadog-auth-url "https://app.datadoghq.com/account/login?redirect=f")

(def (metric-tags metric)
  (let-hash (datadog-web-login)
    (let* ((url (format "https://app.datadoghq.com/metric/flat_tags_for_metric?metric=~a&window=86400" metric))
    	   (reply (http-get url headers: .headers))
    	   (tags (let-hash (from-json (request-text reply)) .tags)))
      (for-each
    	(lambda (tag)
    	  (displayln tag))
    	tags))))

(def (datadog-web-login)
  (let-hash (load-config)
    (let* ((dogwebu (datadog-get-dogwebu))
	   (dogweb (datadog-get-dogweb dogwebu))
	   (headers [[ "Cookie:" :: (format "dogwebu=~a; dogweb=~a" dogwebu dogweb) ]]))
      (hash
       (headers headers)
       (dogwebu dogwebu)
       (dogweb dogweb)))))

(def (datadog-get-dogwebu)
  (let-hash (load-config)
    (let* ((uri datadog-auth-url)
	   (data (strip-^m (format "username=~a&password=~a" .username .password)))
	   (reply (http-post uri
			     headers: []
			     data: data))
	   (cookies (request-cookies reply))
	   (dogwebu (find-cookie cookies "^dogwebu=")))
      (strip-^m dogwebu))))

(def (find-cookie cookies pattern)
  (let ((cookie-of-interest ""))
    (when (list? cookies)
      (for-each
	(lambda (c)
      	  (when (pregexp-match pattern c)
      	    (set! cookie-of-interest (car (pregexp-split ";" (cadr (pregexp-split "=" c)))))))
	cookies))
    cookie-of-interest))

(def (datadog-get-dogweb dogwebu)
  (let-hash (load-config)
    (let* ((uri datadog-auth-url)
	   (data (format "username=~a&password=~a&_authentication_token=~a" .username .password dogwebu))
	   (reply (http-post uri
			     headers: [[ "Content-Type" :: "application/x-www-form-urlencoded" ]]
			     cookies: (format "dogwebu=~a; intercom-session=please-add-flat_tags_for_metric-to-your-api-thanks;" dogwebu)
			     data: data))
	   (cookies (request-cookies reply))
	   (dogweb (find-cookie cookies "^dogweb=")))
      (strip-^m dogweb))))

(def (strip-^m str)
  (if (string? str)
    (string-trim-both str)
    str))

(def (live-metrics host)
  (let-hash (load-config)
    (let* ((adds (format "hosts/live_metrics?hosts[]=~a" host))
	   (ip datadog-host)
	   (uri (make-dd-uri ip adds))
	   (results (do-get uri))
	   (hosts (from-json results)))
      (let-hash hosts
	(for-each
      	  (lambda (host)
	    (displayln (hash->list host))
      	    (let-hash host
      	      (display .name)
      	      (let-hash .live_metrics
      		(displayln ": iowait: " .?iowait " load15: " .?load15 " cpu: " .?cpu))))
      	  .host_list)))))

(def (totals)
  (let-hash (load-config)
    (let* ((adds "hosts/totals")
	   (ip datadog-host)
	   (uri (make-dd-uri ip adds))
	   (results (do-get uri))
	   (myjson (from-json results)))
      (let-hash myjson
	(displayln "Total Up: " .total_up " Total Active: " .total_active)))))

(def (stories)
  (let-hash (datadog-web-login)
    (let* ((url "https://app.datadoghq.com/watchdog/stories?page_size=100&stories_api_v2=true")
	   (reply (http-get url headers: .headers)))
      (displayln (request-text reply)))))

(def (livetail)
  (let-hash (datadog-web-login)
    (let* ((url "https://app.datadoghq.com/logs/livetail")
	   (reply (http-get url headers: .headers)))
      (displayln (request-text reply)))))

(def (procs host secs)
  (let-hash (datadog-web-login)
    (let* ((secs 300)
	   (start (float->int (* (- (time->seconds (builtin-current-time)) secs) 1000)))
	   (end (float->int (* (time->seconds (builtin-current-time)) 1000)))
	   (url (format "https://app.datadoghq.com/proc/query?from=~a&to=~a&size_by=pct_mem&group_by=family&color_by=user&q=processes{host:~a}" start end host))
	   (headers [[ "cookie" :: (format "dogweb=~a; intercom-session=please-add-flat_tags_for_metric-to-your-api-thanks" .dogweb) ]
		     [ "authority" :: "app.datadoghq.com" ]
		     ] )
	   (reply (http-get url headers:  headers))
	   (text (request-text reply))
	   (procs (from-json text)))
      (let-hash procs
	(for-each
	  (lambda (snapshot)
	    (format-snapshot snapshot))
	  .snapshots)))))

(def (format-snapshot snapshot)
  "Snapshots are lists of pslists."
  ;; [ 1, "rabbitmq", "0.0", "25.84", 14841081856, 8697987072, 0, 0, "beam.smp", 1 ],
  (when (table? snapshot)
    (let-hash snapshot
      (for-each
	(lambda (proc)
	  (with ([ ppid user pct1 pctmem vsz rss zero1 zero2 name nprocs ] proc)
	    (displayln (format "ppid?:~a user:~a pct1?:~a pctmem:~a vsz:~a rss:~a zero1?:~a zero2?:~a name:~a nprocs:~a" ppid user pct1 pctmem vsz rss zero1 zero2 name nprocs))))
	.pslist))))

(def (indexes)
  (let-hash (datadog-web-login)
    (let* ((url "https://app.datadoghq.com/api/v1/logs/indexes?type=logs")
	   (reply (http-get url headers: .headers))
	   (myjson (from-json (request-text reply))))
      (let-hash myjson
	(for-each
	  (lambda (index)
	    (displayln (hash->list index)))
	  .indexes)))))

(def (status)
  (let* ((url "https://1k6wzpspjf99.statuspage.io/api/v2/status.json")
	(reply (http-get url headers: default-headers))
	(myjson (from-json (request-text reply))))
    (let-hash myjson
      (displayln (let-hash .page .name " Url: " .url " Updated: " .updated_at))
      (displayln "Status: " (let-hash .status " Indicator: " .indicator " Description: " .description)))))

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]])

(def (host hst)
  (let-hash (datadog-web-login)
    (let lp ((start 0))
      (let* ((url (format "https://app.datadoghq.com/api/v1/hosts?filter=~a&group=&start=~a&count=100&sort_field=cpu&sort_dir=desc&discovery=true" hst start))
    	     (reply (http-get url headers: .headers))
	     (myjson (from-json (request-text reply)))
	     (hosts (let-hash myjson .host_list)))
	(let-hash myjson
	  (when (= start 0)
	    (displayln "|Name|host name|Id|Apps|Muted?|Sources|Meta|Tags By Source| aliases| up?|metrics|")
	    (displayln "|-|-|"))
	  (for-each
	    (lambda (host)
	      (format-host host))
	    .host_list)
	  (when (> .total_matching (+ start .total_returned))
	    (lp (+ start .total_returned))))))))

 (def (format-host host)
   (let-hash host
     (displayln "|" .name
		"|" .host_name
		"|" .id
		"|" (jif (sort! .apps string<?) ",")
		"|" (if .is_muted "True" "False")
		"|" (jif .sources ",")
		"|" (hash->str .meta)
		"|" (hash->str .tags_by_source)
		"|" (jif .aliases ",")
		"|" (if .up "True" "False")
		"|" (hash->str .metrics)
		"|" )))

(def (jif lst sep)
  "If we get a list, join it on sep"
  (if (list? lst)
    (string-join lst sep)
    lst))

(def (hash->str h)
  (let ((results []))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
	 h)
	(append-strings results))
      "N/A")))

(def (contexts)
  (let-hash (datadog-web-login)
    (let* ((url "https://app.datadoghq.com/check/contexts?names_only=true")
	   (headers [[ "Cookie" :: (format "dogwebu=~a; dogweb=~a; intercom-session=please-add-flat_tags_for_metric-to-your-api-thanks" .dogwebu .dogweb) ]] )
	   (reply (http-get url headers:  headers))
	   (text (request-text reply))
	   (contexts (from-json text)))
      (for-each
	(lambda (context)
	  (display-context context))
	contexts))))

(def (display-context context)
  (when (table? context)
    (let-hash context
      (displayln "*** " .name)
      (displayln "**** Groups")
      (for-each
	(lambda (group)
	  (let-hash group
	    (displayln "****** " .status " message: " (or .message "None") " " .tags " " .modified)))
	.groups))))
