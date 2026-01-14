;; -*- Gerbil -*-
;; © ober 2021

(import
 :clan/text/yaml
 :gerbil/gambit
 :ober/oberlib
 :std/crypto/cipher
 :std/format
 :std/generic
 :std/iter
 :std/misc/list
 :std/misc/ports
 :std/net/address
 :std/net/httpd
 :std/net/request
 :std/pregexp
 :std/srfi/1
 :std/srfi/13
 (only-in :std/srfi/19 date->string)
 :std/srfi/95
 :std/sugar
 :std/text/base64
 :std/text/json
 )

(export #t)
(def version "0.15")
(declare (not optimize-dead-definitions))
(def datadog-host "app.datadoghq.com")
(def datadog-api-key #f)
(def datadog-app-key #f)
(def config-file "~/.datadog.yaml")
(def keys-dir (path-expand "~/.config/gerbil/keys"))
(def key-file (path-expand "datadog.key" keys-dir))

;; Security: Ensure directory exists with restricted permissions
(def (ensure-keys-dir!)
  (unless (file-exists? keys-dir)
    (create-directory* keys-dir)
    (##shell-command (format "chmod 700 ~a" keys-dir))))

;; Security: Save key to separate file with restricted permissions
(def (save-key-to-file! key-bytes)
  (ensure-keys-dir!)
  (let ((key-b64 (u8vector->base64-string key-bytes)))
    (with-output-to-file [path: key-file create: 'maybe truncate: #t]
      (lambda () (display key-b64)))
    (##shell-command (format "chmod 400 ~a" key-file))
    (displayln (format "Key saved to ~a (mode 0400)" key-file))))

;; Security: Load key from separate file
(def (load-key-from-file)
  (unless (file-exists? key-file)
    (error (format "Key file not found: ~a. Run 'config' command first." key-file)))
  (base64-string->u8vector (read-file-string key-file)))

;; Security: Check file permissions (warn if too permissive)
(def (check-file-permissions! file)
  (when (file-exists? file)
    (let* ((info (file-info file))
           (mode (file-info-mode info)))
      (when (> (bitwise-and mode #o077) 0)
        (displayln (format "WARNING: ~a has insecure permissions. Run: chmod 600 ~a" file file))))))

(def (load-config)
  ;; SECURITY: Check file permissions
  (check-file-permissions! config-file)
  (check-file-permissions! key-file)

  (let ((config (hash))
        (config-data (yaml-load config-file)))
    (unless (and (list? config-data)
                 (length>n? config-data 0)
                 (hash-table? (car config-data)))
      (displayln (format "Could not parse your config ~a" config-file))
      (exit 2))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car config-data))
    (let-hash config
      (when .?secrets
        ;; Use JSON parsing instead of u8vector->object for security
        ;; Prevents arbitrary code execution from malicious config files
        (let ((secrets-json (parameterize ((read-json-key-as-symbol? #t))
                              (with-input-from-string
                                  (bytes->string (base64-decode .secrets))
                                read-json))))
          (let-hash secrets-json
            (hash-put! config 'datadog-api-key (decrypt-bundle .api-key))
            (hash-put! config 'datadog-app-key (decrypt-bundle .app-key))
            (when .?username
              (hash-put! config 'username (decrypt-bundle .username)))
            (when .?password
              (hash-put! config 'password (decrypt-bundle .password)))))))
    config))

(def (ensure-api-keys)
     (unless (and
	      datadog-api-key
	      datadog-app-key)
	 (let-hash (load-config)
		   (set! datadog-app-key .datadog-app-key)
		   (set! datadog-api-key .datadog-api-key))))

(def (make-dd-url adds)
     (ensure-api-keys)
     (let* ((datadog-base-url (format "https://~a/api/v1/" datadog-host)))

       ;; (let ((delim "?"))
       ;;   (when (string-contains adds "?")
       ;;     (set! delim "&"))
       (string-concatenate [ datadog-base-url adds ])))

(def (make-dd-url-metric ip adds)
     (ensure-api-keys)
     (let* ((datadog-base-url (format "https://~a/" ip)))
       (string-concatenate [ datadog-base-url adds ])))

(def (make-test-url ip adds)
     (let* ((datadog-base-url (format "http://~a/headers" ip)))
       (string-concatenate [ datadog-base-url adds ])))

(def (verify-account)
     (let* ((req (http-get (make-dd-url "validate") headers: (default-headers)))
	    (status (request-status req))
	    (valid (success? status)))
       (unless valid
	 (displayln "Credentials are not valid. got status:" status " with error:"  (request-text req)))))

(def (get-new-ip url host)
     (pregexp-replace "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" url (resolve-ipv4 host)))

(def (print-object obj)
     #f)

(def (print-opts t)
     (let-hash t
	       (displayln
		" end: " .end
		" unit: "  (hash->string .unit)
		" length: " .length
		" attributes: " (hash->string .attributes)
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
     (for (point points)
	  (let ((date (car point))
		(datum (car (cdr point))))
	    (unless (fixnum? datum)
              (set! datum (float->int datum)))
	    (displayln (date->string (epoch->date date) "~s") " " datum))))

(def (make-metric-string str)
     (pregexp-replace* "," (pregexp-replace* " " str "-") ""))

(def (query-metrics from-s to-s query)
     "Interactive interface to query-metric"
     (let ((series (query-metric from-s to-s query)))
       (when (list? series)
	 (for (p series)
              (print-opts p)))))

(def (query-metric from-s to-s query)
     (let* ((adds (format "query?from=~a&to=~a&query=~a" from-s to-s query))
	    (url (make-dd-url adds)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when .?series
			   .series))))))

(def (users)
     (let* ((adds "user")
	    (url (make-dd-url adds))
            (outs [[ "Handle" "Admin?" "Disabled?" "Title" "Verified" "Email" "Icon" "Name" "Role" "Access Role" ]]))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when .?users
			   (for (user .users)
				(let-hash user
					  (set! outs (cons [ .?handle .?is_admin .?disabled .?title .?verified .?email .?icon .?name .?role .?access_role ] outs))))))))
       (style-output outs)))

(def (query-last-secs secs query)
     "Interactive version of query-last-sec"
     (let* ((start (float->int (- (time->seconds (current-time)) secs)))
	    (end (float->int (time->seconds (current-time)))))
       (query-metrics start end query)))

(def (query-last-sec secs query)
     "Non-interactive version"
     (let ((start (float->int (- (time->seconds (current-time)) secs)))
           (end (float->int (time->seconds (current-time)))))
       (query-metric start end query)))

(def (query-min query)
     (query-last-secs 60 query))

(def (query-hour query)
     (query-last-secs 3600 query))

(def (query-day query)
     (query-last-secs 86400 query))

(def (metrics pattern)
     (verify-account)
     (let* ((past (inexact->exact (round (- (time->seconds (current-time)) 9000))))
	    (url (make-dd-url (format "metrics?from=~a" past))))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (for (m .metrics)
			      (if pattern
				  (if (string-contains m pattern)
				      (displayln m)))))))))

(def (view-md metric)
     (verify-account)
     (let* ((adds (format "metrics/~a" metric))
	    (url (make-dd-url adds)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (displayln (format "description:~a integration:~a statsd_interval:~a type:~a unit:~a"
					    (if (void? .?description)
						"N/A"
						.?description)
					    (if (void? .?integration)
						"N/A"
						.?integration)
					    (if (void? .?statsd_interval)
						"N/A"
						.?statsd_interval)
					    (if (void? .?type)
						"N/A"
						.?type)
					    (if (void? .unit)
						"N/A"
						.?unit))))))))

(def (edit-metric-metadata metric description short_name)
     (let ((url (make-dd-url (format "metrics/~a" metric)))
           (data (json-object->string
		  (hash
                   ("description" description)
                   ("short_name" short_name)))))
       (with ([status body] (rest-call 'put url (default-headers) data))
	     (unless status
               (error body))
	     (present-item body))))

(def (submit-event title text priority tags alert_type)
     (let ((url (make-dd-url "events"))
           (data (json-object->string
		  (hash
                   ("title" title)
                   ("text" text)
                   ("priority" priority)
                   ("tags" tags)
                   ("alert_type" alert_type)))))
       (with ([status body] (rest-call 'post url (default-headers) data))
	     (present-item body))))

(def (get-events-time start end)
     (let (url (make-dd-url (format "events?start=~a&end=~a" start end)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (present-item body))))

(def (get-events-tags start end tags)
     (let (url (make-dd-url (format "events?start=~a&end=~a&tags=~a" start end tags)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     body)))

(def (get-event event_id)
     "Get a specific event by ID"
     (let ((url (make-dd-url (format "events/~a" event_id))))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (delete-event event_id)
     "Delete an event by ID"
     (let ((url (make-dd-url (format "events/~a" event_id))))
       (with ([status body] (rest-call 'delete url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (get-events-last-secs secs tags)
     (let* ((start (float->int (- (time->seconds (current-time)) secs)))
            (end (float->int (time->seconds (current-time))))
            (evtags (get-events-tags start end tags)))
       (unless (hash-table? evtags)
	 (error (format "evtags is not a table: ~a" evtags)))
       (let-hash evtags
		 (when (list? .?events)
		   (print-events .events)))))

(def (get-events-last-secs-raw secs)
     (let* ((start (float->int (- (time->seconds (current-time)) secs)))
            (end (float->int (time->seconds (current-time))))
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
     (when (list? events)
       (for (event events)
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
			   (print-children .children)))))))

(def (print-children children)
     (for (child children)
	  (let-hash child
		    (displayln
		     " -: " (print-date (epoch->date .date_happened))
		     " alert_type: " .alert_type
		     " id: " .id))))

;; *** COMMENTS ***

(def (create-comment message handle related_event_id)
     "Create a comment on Datadog
  message: The comment text (required)
  handle: The handle of the user making the comment (optional)
  related_event_id: The id of another comment or event to reply to (optional)"
     (let* ((url (make-dd-url "comments"))
            (data-hash (hash ("message" message))))
       (when handle
         (hash-put! data-hash "handle" handle))
       (when related_event_id
         (hash-put! data-hash "related_event_id" related_event_id))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (edit-comment comment_id message handle)
     "Edit an existing comment
  comment_id: The ID of the comment to edit (required)
  message: The new comment text (optional)
  handle: The handle of the user making the edit (optional)"
     (let* ((url (make-dd-url (format "comments/~a" comment_id)))
            (data-hash (hash)))
       (when message
         (hash-put! data-hash "message" message))
       (when handle
         (hash-put! data-hash "handle" handle))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'put url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (delete-comment comment_id)
     "Delete a comment by ID"
     (let ((url (make-dd-url (format "comments/~a" comment_id))))
       (with ([status body] (rest-call 'delete url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (downtimes)
     (let (url (make-dd-url "downtime"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (for (dt body)
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
		     ))))))

;; *** SERVICE CHECKS ***

(def (post-check-run check host_name status timestamp message tags)
     "Post a service check run
  check: The name of the check
  host_name: The name of the host submitting the check
  status: An integer for the status (0=OK, 1=WARNING, 2=CRITICAL, 3=UNKNOWN)
  timestamp: POSIX timestamp (optional, use #f for current time)
  message: Description of why this status occurred (optional)
  tags: List of tags for this check (optional)"
     (let* ((url (make-dd-url "check_run"))
            (data-hash (hash
                        ("check" check)
                        ("host_name" host_name)
                        ("status" status))))
       (when timestamp
         (hash-put! data-hash "timestamp" timestamp))
       (when message
         (hash-put! data-hash "message" message))
       (when tags
         (hash-put! data-hash "tags" tags))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (screen id)
     (let (url (make-dd-url (format "screen/~a" id)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (print-screens body))))

(def (print-screens screen)
     (let-hash screen
	       (displayln
		"|" (format "[[~a][~a]]" (make-dd-url .resource) .id)
		"|" (let-hash .created_by .email)
		"|" .title
		"|" .modified
		"|" .created
		"|")))

(def (print-dash dash)
     (let-hash dash
	       (pi dash)))
;; (displayln
;;  "|" (format "[[~a][~a]]" (make-dd-url .resource) .id)
;;  "|" (let-hash .created_by .email)
;;  "|" .title
;;  "|" .modified
;;  "|" .created
;;  "|")))

(def (print-widgets widgets)
     (for (widget widgets)
	  (let-hash
           widget
	   (if (string=? .type "query_value")
               (let-hash
		.tile_def
		(print-widget-requests .requests))))))

(def (print-widget-requests requests)
     (for (request requests)
	  (let-hash
           request
	   (displayln
	    (when (hash-key? request 'style)" style: " (print-widget-style .style))
	    " aggregator: " .aggregator
	    " q: " .q)
	   (print-widget-conditional-formats .conditional_formats))))

(def (print-widget-style style)
     (let-hash
      style
      (format " width: ~a type: ~a palette: ~a" .width .type .palette)))

(def (print-widget-conditional-formats formats)
     (for (cformat formats)
	  (let-hash
           cformat
	   (displayln (format "	 value: ~a comparator: ~a palette: ~a" .value .comparator .palette)))))

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
     (let ((url (make-dd-url "dash"))
           (data (json-object->string
		  (hash
                   ("graphs" [ (make-graph (metric-name-to-title title) "avg:system.mem.free{*}" "timeseries")])
                   ("title" title)
                   ("description" description)
                   ("template_variables"
                    [ (hash
                       ("name" "host1")
                       ("prefix" "host")
                       ("default" "host:my-host")) ])))))
       (with ([status body] (rest-call 'post url (default-headers) data))
	     (unless status
               (error body))
	     (present-item body))))

(def (tboard-mass-add id metric-pattern host-clause groupby replace)
     (let* ((tbinfo (get-tboard id))
            (new-graphs [])
            (url (make-dd-url (format "dash/~a" id))))
       (when (hash-table? tbinfo)
	 (let-hash tbinfo
		   (when (hash-table? .?dash)
		     (let-hash .dash
			       (unless (string=? replace "t")
				 (set! new-graphs .?graphs))
			       (for (m (sort! (search-metrics metric-pattern) string<?))
				    (let ((new-graph
					   (make-graph
					    (metric-name-to-title m)
					    (format "avg:~a{~a}by{~a}" m host-clause groupby) "timeseries")))
				      (set! new-graphs (append new-graphs [new-graph]))))
			       (let ((data (json-object->string
					    (hash
					     ("graphs" new-graphs)
					     ("title" .?title)
					     ("description" .?description)))))
				 (with ([status body] (rest-call 'put url (default-headers) data))
				       (unless status
					 (error body))
				       (present-item body)))))))))

(def (tboard-netdata-dashboard id metric-pattern host-clause groupby replace)
     (let* ((tbinfo (get-tboard id))
            (url (make-dd-url (format "dash/~a" id)))
            (dash (hash-get tbinfo 'dash))
            (graphs (hash-get dash 'graphs))
            (title (hash-get dash 'title))
            (new-graphs []))
       (unless (string=? replace "t")
	 (set! new-graphs graphs))
       (for (m (sort! (search-metrics metric-pattern) string<?))
	    (let ((new-graph
		   (make-graph
		    (metric-name-to-title m)
		    (format "avg:~a{~a}by{~a}" m host-clause groupby) "timeseries")))
              (set! new-graphs (append new-graphs [new-graph]))))

       (let ((data (json-object->string
                    (hash
                     ("graphs" new-graphs)
                     ("title" title)
                     ("description" (hash-get dash 'description))))))
	 (with ([status body] (rest-call 'put url (default-headers) data))
               (unless status
		 (error body))
               (present-item body)))))

(def (make-query-for-hosts metric hosts)
     (let ((results []))
       (for (host hosts)
	    (set! results (flatten (cons (format "avg:~a{host:~a}" metric host) results))))
       (string-join results ",")))

(def (tboard-mass-add-many id metric-pattern host-pattern replace)
     (let* ((tbinfo (get-tboard id))
            (hosts (search-hosts host-pattern))
            (url (make-dd-url (format "dash/~a" id)))
            (dash (hash-get tbinfo 'dash))
            (graphs (hash-get dash 'graphs))
            (title (hash-get dash 'title))
            (new-graphs []))
       (unless (string=? replace "t")
	 (set! new-graphs graphs))
       (for (m (sort! (search-metrics metric-pattern) string<?))
	    (let ((new-graph
		   (make-graph
		    (metric-name-to-title m)
		    (make-query-for-hosts m hosts)
		    "timeseries")))
              (set! new-graphs (append new-graphs [new-graph]))))
       (let ((data (json-object->string
                    (hash
                     ("graphs" new-graphs)
                     ("title" title)
                     ("description" (hash-get dash 'description))))))
	 (with ([status body] (rest-call 'put url (default-headers) data))
               (unless status
		 (error body))
               (present-item body)))))

;; (def (tboard-fancy id metric-pattern tag replace)
;;   (let ((tboard (get-tboard id)))
;;     (when (hash-table? tboard)
;;       (let-hash tboard
;;         (let ((groupby (if (string-contains tag ":")
;;                          (car (pregexp-split ":" tag))
;;                          tag))
;;               (url (make-dd-url (format "dash/~a" id)))
;;               (new-graphs []))
;;           (when .?dash
;;             (when (hash-table? .dash)
;;               (dp (hash->list .dash))
;;               (let-hash .dash
;;                 (unless (string=? replace "t")
;;                   (set! new-graphs .graphs))
;;                 (for (metric (sort! (metrics-tag-search metric-pattern tag) string<?))
;;                   (dp (format "metric is ~a" metric))
;;                   (let* ((new-graph
;;                           (make-graph
;;                            (metric-name-to-title metric)
;;                            (format "avg:~a{~a}by{~a}" metric tag groupby) "timeseries")))
;;                     (set! new-graphs (flatten (cons new-graph new-graphs)))))
;;                 (if (length>n? new-graphs 0)
;;                   (let ((data (json-object->string
;;                                (hash
;;                                 ("graphs" new-graphs)
;;                                 ("title" (if replace .?title (format "~a for ~a" metric-pattern tag)))
;;                                 ("description" (if replace .?description (format "~a for ~a" metric-pattern tag)))))))
;;                     (with ([status body] (rest-call 'put url (default-headers) data))
;;                       (unless status
;;                         (error body))
;;                       (when (hash-table? body)
;;                         (let-hash body
;;                           (when (hash-table? .?dash)
;;                             (let-hash .dash
;;                               (dp (format "description: ~a title: ~a graphs: ~a" .?description .?title new-graphs))
;;                               (displayln (format "https://~a/dashboard/~a" datadog-host .?new_id)))))
;;                         (displayln (format "No metrics found matching tag ~a for metric ~a" tag metric-pattern))))))))))))))

(def (metric-name-to-title metric)
     (let* ((no-netdata (pregexp-replace "^netdata." metric ""))
            (dot-to-space (pregexp-replace "\\." no-netdata " "))
            (no-es (pregexp-replace "elasticsearch_local\\." no-netdata "")))
       no-es))

(def (tboard-add-chart id title request viz)
     (let* ((tbinfo (get-tboard id))
            (url (make-dd-url (format "dash/~a" id)))
            (dash (hash-get tbinfo 'dash))
            (graphs (hash-get dash 'graphs))
            (new-graph (make-graph (metric-name-to-title title) request viz))
            (data (json-object->string
                   (hash
                    ("graphs" (append graphs [ new-graph ]))
                    ("title" (hash-get dash 'title))
                    ("description" (hash-get dash 'description))))))
       (with ([status body] (rest-call 'post url (default-headers) data))
	     (unless status
               (error body))
	     (present-item body))))

(def (get-tboard id)
     (let (url (make-dd-url (format "dash/~a" id)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     body)))

(def (get-sboard id)
     (let (url (make-dd-url (format "screen/~a" id)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     body)))

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
			   (displayln (format "https://app.datadoghq.com/dashboard/~a" .new_id))
			   ))))

(def (dump-tboards dir)
     (let (url (make-dd-url "dash"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when (and .?dashes (list? .dashes))
			   (for (tboard .dashes)
				(let-hash tboard
					  (try
					   (yaml-dump
					    (format "~a/~a.yaml" dir .?id)
					    (format-tboard (get-tboard .?id)))
					   (catch (e)
						  (raise e)))))))))))

(def (dump-sboards dir)
     (let (url (make-dd-url "screen"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when (and .?screenboards (list? .screenboards))
			   (for (screen .screenboards)
				(let-hash screen
					  (when .?id
					    (try
					     (yaml-dump
					      (format "~a/~a.yaml" dir .?id)
					      (get-sboard .id))
					     (catch (e)
						    (raise e))))))))))))

(def (print-graphs graphs)
     (let ((results ""))
       (for (graph graphs)
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
       results))

(def (print-requests requests)
     (let ((results ""))
       (for (request requests)
	    (hash-for-each
	     (lambda (k v)
               (if (hash-table? v)
		   (set! results (string-append results (format " ~a:~a " k (hash->string v))))
		   (set! results (string-append results (format " ~a:~a " k v)))))
	     request))
       results))

(def (screens)
     (with ([status body] (rest-call 'get (make-dd-url "screen") (default-headers)))
	   (unless status
	     (error body))
	   (when (hash-table? body)
	     (let-hash body
		       (for (screen .screenboards)
			    (print-screens screen))))))

(def (dashes)
     (let (url (make-dd-url "dash"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (for (dash .dashes)
			      (print-screens dash)))))))

(def (screen-create board_title widgets width height)
     (let ((url (make-dd-url "screen"))
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
       (with ([status body] (rest-call 'post url (default-headers) data))
	     (unless status
               (error body))
	     (present-item body))))

(def (screen-update id width height board_title)
     (let ((url (make-dd-url (format "screen/~a" id)))
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
       (with ([status body] (rest-call 'put url (default-headers) data))
	     (unless status
               (error body))
	     (present-item body))))

(def (delete-tboard dash_id)
     "Delete a timeboard by ID"
     (let ((url (make-dd-url (format "dash/~a" dash_id))))
       (with ([status body] (rest-call 'delete url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (delete-sboard board_id)
     "Delete a screenboard by ID"
     (let ((url (make-dd-url (format "screen/~a" board_id))))
       (with ([status body] (rest-call 'delete url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (share-sboard board_id)
     "Share a screenboard (make it publicly accessible)"
     (let ((url (make-dd-url (format "screen/share/~a" board_id))))
       (with ([status body] (rest-call 'post url (default-headers) "{}"))
             (unless status
               (error body))
             (present-item body))))

(def (revoke-sboard-share board_id)
     "Revoke sharing of a screenboard"
     (let ((url (make-dd-url (format "screen/share/~a" board_id))))
       (with ([status body] (rest-call 'delete url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (search query)
     (let (url (make-dd-url (format "search?q=~a" query)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (let-hash .results
				   (for (m .metrics)
					(displayln "metric: " m))
				   (for (h .hosts)
					(displayln "host: " h))))))))

(def (search-metrics pattern)
     (let ((url (make-dd-url (format "search?q=~a" pattern)))
           (metrics-matched []))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when (hash-table? .?results)
			   ;;(pi .results)
			   (let-hash .results
				     (for (m .metrics)
					  (dp (format "(pregexp-match \"~a\" ~a)" pattern m))
					  (when (pregexp-match pattern m)
					    (set! metrics-matched (cons m metrics-matched))))
				     metrics-matched)))))))

(def (hosts pattern)
     (let ((results (search-hosts pattern)))
       (when (list? results)
	 (for (r results)
              (displayln r)))))

(def (regexp->str regexp)
     (let ((str regexp)
           (regexps [ #\^
                      #\$
                      ]))
       (for (remove regexps)
	    (set! str (string-delete remove str)))
       str))

(def (search-hosts-exact hostname)
     (let ((found #f)
           (url (make-dd-url (format "search?q=~a" hostname))))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when (hash-table? .?results)
			   (let-hash .results
				     (when (and .?hosts
						(list? .hosts))
				       (for (h .hosts)
					    (when (pregexp-match (format "^~a$" hostname) h)
					      (set! found h)))))))))
       found))

(def (search-hosts pattern)
     (let* ((safe-str (regexp->str pattern))
            (url (make-dd-url (format "search?q=~a" safe-str)))
            (hosts-matched []))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (when (hash-table? .?results)
			   (let-hash .results
				     (for (h .hosts)
					  (let ((matches (pregexp-match pattern h)))
					    (when matches
					      (set! hosts-matched (cons h hosts-matched)))))
				     hosts-matched)))))))

(def (clear-tags hostname)
     "Remove a tag from a given hostname"
     (let (host (search-hosts-exact hostname))
       (unless host
	 (error (format "Host not found: ~a" hostname)))
       (let (url (make-dd-url (format "tags/hosts/~a" (web-encode host))))
	 (try
	  (with ([status body] (rest-call 'delete url (default-headers)))
		(unless status
		  (error (format "call failed: ~a" body))))
	  (catch (e)
		 (raise (e)))))))

(def (tag hostname tag)
     "Tag a given hostname with a tag"
     (let ((host (search-hosts-exact hostname))
           (data (json-object->string
		  (hash
                   ("tags" [ tag ])))))
       (let (url (make-dd-url (format "tags/hosts/~a" host)))
	 (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
		 (error body))
               (present-item body)))))

(def (tag-all host-pattern tag)
     "Tag a given hostname with a tag"
     (let ((hosts (search-hosts host-pattern))
           (data (json-object->string (hash ("tags" [ tag ])))))
       (for (h hosts)
	    (let (url (make-dd-url (format "tags/hosts/~a" h)))
              (with ([status body] (rest-call 'post url (default-headers) data))
		    (unless status
		      (error body))
		    (present-item body))))))

(def (tags)
     "Return all tags known to Datadog"
     (let (url (make-dd-url "tags/hosts"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (displayln .?errors)
			 (when (hash-table? .?tags)
			   (for (k (hash-keys .tags))
				(displayln k))))))))

(def (tags-for-metric metric)
     "Return all tags found for a given metric"
     (let (url (make-dd-url "tags/metrics"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (for (k (hash-keys .tags))
			      (displayln k)))))))

(def (tags-for-source source)
     (let (url (make-dd-url (format "tags/hosts/~a" source)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (for (k (hash-keys body))
		    (displayln k))))))

(def (update-host-tags hostname tags)
     "Update (replace) all tags for a host
  hostname: The hostname (required)
  tags: List of tags to set for the host (required)"
     (let* ((url (make-dd-url (format "tags/hosts/~a" hostname)))
            (data (json-object->string (hash ("tags" tags)))))
       (with ([status body] (rest-call 'put url (default-headers) data))
             (unless status
               (error body))
             (present-item body))))

(def (graph query start end)
     (let (url (make-dd-url (format "graph/snapshot?metric_query=~a&start=~a&end=~a" query start end)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (let-hash body
			 (present-item .?snapshot_url))))))

(def (graph-last-secs secs query)
     (let* ((start (float->int (- (time->seconds (current-time)) secs)))
            (end (float->int (time->seconds (current-time)))))
       (graph query start end)))

(def (graph-min query)
     (query-last-secs 60 query))

(def (graph-hour query)
     (query-last-secs 3600 query))

;; *** EMBEDDABLE GRAPHS ***

(def (get-all-embeds)
     "Get a list of all embeddable graphs"
     (let ((url (make-dd-url "graph/embed")))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (create-embed graph_json timeframe size legend title)
     "Create a new embeddable graph
  graph_json: The graph definition in JSON (required)
  timeframe: The timeframe for the graph (optional: 1_hour, 4_hours, 1_day, 2_days, 1_week)
  size: The size of the graph (optional: small, medium, large, xlarge)
  legend: Whether to show legend (optional: yes, no)
  title: The graph title (optional)"
     (let* ((url (make-dd-url "graph/embed"))
            (data-hash (hash ("graph_json" graph_json))))
       (when timeframe
         (hash-put! data-hash "timeframe" timeframe))
       (when size
         (hash-put! data-hash "size" size))
       (when legend
         (hash-put! data-hash "legend" legend))
       (when title
         (hash-put! data-hash "title" title))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (get-embed embed_id size legend template_variables)
     "Get a specific embeddable graph
  embed_id: The embed token (required)
  size: The size of the graph (optional: small, medium, large, xlarge)
  legend: Whether to show legend (optional: yes, no)
  template_variables: Hash of template variables to replace (optional)"
     (let* ((params [])
            (params (if size (cons (format "size=~a" (web-encode size)) params) params))
            (params (if legend (cons (format "legend=~a" (web-encode legend)) params) params))
            (params (if (and template_variables (hash-table? template_variables))
                       (append params
                               (hash-fold
                                (lambda (k v acc)
                                  (cons (format "~a=~a" (web-encode k) (web-encode v)) acc))
                                [] template_variables))
                       params))
            (query-string (if (pair? params)
                            (string-append "?" (string-join params "&"))
                            ""))
            (url (make-dd-url (format "graph/embed/~a~a" embed_id query-string))))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (enable-embed embed_id)
     "Enable a specified embed"
     (let ((url (make-dd-url (format "graph/embed/~a/enable" embed_id))))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (revoke-embed embed_id)
     "Revoke a specified embed"
     (let ((url (make-dd-url (format "graph/embed/~a/revoke" embed_id))))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (edit-monitor id query name message)
     (let ((url (make-dd-url (format "monitor/~a" id)))
           (data (json-object->string
		  (hash
                   ("query" query)
                   ("name" name)
                   ("message" message)))))
       (with ([status body] (rest-call 'put url (default-headers) data))
	     (unless status
               (error body))
	     (when (hash-table? body)
               (present-item body)))))

(def (del-monitor id)
     "Delete an existing Datadog Monitor by id"
     (let (url (make-dd-url (format "monitor/~a" id)))
       (with ([status body] (rest-call 'delete url (default-headers)))
	     (unless status
               (error body))
	     (present-item body))))

(def (new-monitor type query name message tags)
     (let ((url (make-dd-url "monitor"))
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
       (with ([status body] (rest-call 'post url (default-headers) data))
	     (unless status
               (error body))
	     (present-item body))))

(def (monitor id)
     (displayln (format "* Datadog Monitor: ~a" id))
     (let (url (make-dd-url (format "monitor/~a" id)))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (print-monitor-long body))))

(def (print-monitor-long monitor)
     (let-hash monitor
	       (displayln "** Query: " .query)
	       (displayln "*** Id: " .id)
	       (displayln "*** Message:")
	       (displayln .message)
	       (displayln "*** Tags: " .tags)
	       (displayln "*** Creator: " (if (hash-table? .creator) (let-hash .creator .name " " .email " " .handle) "N/A"))
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
			 (when (and .?silenced (hash-table? .silenced))
			   (hash-for-each
			    (lambda (k v)
			      (displayln "	- " k ": " v))
			    .silenced))
			 (when (and .?thresholds (hash-table? .silenced))
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
     (let (url (make-dd-url "monitor"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (displayln "* Datadog Monitors")
	     (for (monitor body)
		  (print-monitor-long monitor)))))

(def (dump-monitors dir)
     (let (url (make-dd-url "monitor"))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (when (list? body)
               (for (monitor body)
		    (let-hash monitor
			      (try
			       (yaml-dump
				(format "~a/~a.yaml" dir .id)
				(format-monitor monitor))
			       (catch (e)
				      (raise e)))))))))

(def (monitors-table)
     (let ((outs [[ "Id" "Name" "Query" "Tags" "Url" ]])
           (url (make-dd-url "monitor")))
       (with ([status body] (rest-call 'get url (default-headers)))
	     (unless status
               (error body))
	     (for (monitor body)
		  (let-hash monitor
			    (set! outs (cons [ .id .name .query (string-join .tags ",") (format "https://~a/monitors/~a" datadog-host .id)] outs))))
	     (style-output outs))))

(def (mute-monitor monitor_id scope end)
     "Mute a monitor
  monitor_id: The monitor ID (required)
  scope: The scope to mute (optional, e.g., 'host:myhost')
  end: POSIX timestamp for when the mute should end (optional)"
     (let* ((url (make-dd-url (format "monitor/~a/mute" monitor_id)))
            (data-hash (hash)))
       (when scope
         (hash-put! data-hash "scope" scope))
       (when end
         (hash-put! data-hash "end" end))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (unmute-monitor monitor_id scope)
     "Unmute a monitor
  monitor_id: The monitor ID (required)
  scope: The scope to unmute (optional)"
     (let* ((url (make-dd-url (format "monitor/~a/unmute" monitor_id)))
            (data-hash (hash)))
       (when scope
         (hash-put! data-hash "scope" scope))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (mute-all-monitors)
     "Mute all monitors"
     (let ((url (make-dd-url "monitor/mute_all")))
       (with ([status body] (rest-call 'post url (default-headers) "{}"))
             (unless status
               (error body))
             (present-item body))))

(def (unmute-all-monitors)
     "Unmute all monitors"
     (let ((url (make-dd-url "monitor/unmute_all")))
       (with ([status body] (rest-call 'post url (default-headers) "{}"))
             (unless status
               (error body))
             (present-item body))))

(def (bulk-resolve-monitors monitor_ids)
     "Resolve multiple monitors at once
  monitor_ids: List of monitor IDs to resolve (required)"
     (let* ((url (make-dd-url "monitor/bulk_resolve"))
            (data (json-object->string (hash ("resolve" monitor_ids)))))
       (with ([status body] (rest-call 'post url (default-headers) data))
             (unless status
               (error body))
             (present-item body))))

(def (format-tboard tboard)
     "Try to order the keys in this hash to consistenly represent them in yaml"
     (let-hash tboard
	       (hash
		(tboard tboard))))

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
		;;     (overall_state .overall_state)
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
		;;     (overall_state_modified .overall_state_modified)
		(tags .tags)
		(message .message))))

(def (data->get url data)
     (if (hash-table? data)
	 (string-append
	  url "?"
	  (string-join
	   (hash-fold
	    (lambda (key val r)
              (cons
               (string-append key "=" val) r))
	    [] data) "&"))
	 (displayln "not a table. got " data)))

(def (config)
  (displayln "Please enter your DataDog API Key:")
  (def api-key (read-password ##console-port))
  (displayln "Please enter your DataDog Application Key:")
  (def app-key (read-password ##console-port))
  ;; SECURITY FIX: Generate single shared key and store it separately
  (let* ((cipher (make-aes-256-ctr-cipher))
         (shared-key (random-bytes (cipher-key-length cipher))))
    ;; Save key to separate secure file
    (save-key-to-file! shared-key)
    ;; Encrypt with shared key - only store IV and ciphertext
    (def secrets-hash (hash
                       ("api-key" (encrypt-string-with-key api-key shared-key))
                       ("app-key" (encrypt-string-with-key app-key shared-key))))
    (def secrets (base64-encode (string->bytes (json-object->string secrets-hash))))
    (displayln "")
    (displayln "Add the following lines to your " config-file)
    (displayln "secrets: " secrets)
    (displayln "")
    (displayln "SECURITY NOTE: Your encryption key is stored separately in " key-file)))

;; SECURITY FIX: Encrypt using provided key (stored separately)
(def (encrypt-string-with-key str key)
  (let* ((cipher (make-aes-256-ctr-cipher))
         (iv (random-bytes (cipher-iv-length cipher)))
         (encrypted (encrypt cipher key iv str))
         (enc-store (u8vector->base64-string encrypted))
         (iv-store (u8vector->base64-string iv)))
    ;; Only store IV and ciphertext, NOT the key
    (hash
     ("iv" iv-store)
     ("password" enc-store))))

;; Legacy: encrypt with random key stored in bundle (insecure, for backward compat)
(def (encrypt-string str)
  (displayln "WARNING: Using legacy encryption with key in config (insecure)")
  (let* ((cipher (make-aes-256-ctr-cipher))
         (iv (random-bytes (cipher-iv-length cipher)))
         (key (random-bytes (cipher-key-length cipher)))
         (encrypted-password (encrypt cipher key iv str))
         (enc-pass-store (u8vector->base64-string encrypted-password))
         (iv-store (u8vector->base64-string iv))
         (key-store (u8vector->base64-string key)))
    (hash
     ("key" key-store)
     ("iv" iv-store)
     ("password" enc-pass-store))))

;; Legacy decrypt with key from bundle
(def (decrypt-password-legacy key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

;; SECURITY FIX: Decrypt using key from separate file
(def (decrypt-password-secure iv password)
  (let ((key (load-key-from-file)))
    (bytes->string
     (decrypt
      (make-aes-256-ctr-cipher)
      key
      (base64-string->u8vector iv)
      (base64-string->u8vector password)))))

(def (decrypt-bundle bundle)
  (let-hash bundle
    (if .?key
      ;; Legacy format: key in bundle (warn user)
      (begin
        (displayln "WARNING: Using legacy config with key in secrets. Run 'config' to upgrade.")
        (decrypt-password-legacy .key .iv .password))
      ;; New format: key in separate file
      (decrypt-password-secure .iv .password))))

(def datadog-auth-url "https://app.datadoghq.com/account/login?redirect=f")

(def (metric-tags metric)
     "Return all tags for a given metric"
     (let (tags (get-metric-tags metric))
       (for (tag tags)
	    (present-item tag))))

(def (metric-tags-web metric)
     "Return all tags for a given metric"
     (let* ((dwl (datadog-web-login))
            (tags (get-metric-tags-web metric dwl)))
       (for (tag tags)
	    (present-item tag))))

(def (get-metric-tags metric)
     "Non-interactive version of metric-tags"
     (let ((tags [])
           (results (query-last-sec 3600 (format "~a{*}by{host}" metric))))
       (when (list? results)
	 (for (item results)
              (when (hash-table? item)
		(let-hash item
			  (when .?tag_set
			    (for (t .tag_set)
				 (unless (member t tags)
				   (set! tags (cons t tags)))))))))
       (sort! tags string<?)))

(def (get-metric-tags-web metric dwl)
     "Non-interactive version of metric-tags"
     (let-hash dwl
	       (let* ((url (format "https://app.datadoghq.com/metric/flat_tags_for_metric?metric=~a&window=86400" metric))
		      (reply (http-get url headers: .headers))
		      (tags (let-hash (from-json (request-text reply)) .tags)))
		 tags)))

(def (tag-in-metric? tag metric)
     "Get a bool for if the given tag submits to the given metric"
     (let (tfm (get-metric-tags metric))
       (if (member tag tfm)
	   metric
	   #f)))

(def (datadog-web-login)
     ;;  (error "Datadog implemented ReCaptcha, so the web only features are disabled"))
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
	       (let* ((url datadog-auth-url)
		      (data (strip-^m (format "username=~a&password=~a" .username .password)))
		      (reply (http-post url
					headers: []
					data: data))
		      (cookies (request-cookies reply))
		      (dogwebu (find-cookie cookies "^dogwebu=")))
		 ;;(dogweb (find-cookie cookies "^dogweb=")))
		 (strip-^m dogwebu))))

(def (find-cookie cookies pattern)
     (let ((cookie-of-interest ""))
       (when (list? cookies)
	 (for (c cookies)
              (when (pregexp-match pattern c)
		(set! cookie-of-interest (car (pregexp-split ";" (cadr (pregexp-split "=" c))))))))
       cookie-of-interest))

(def (datadog-get-dogweb dogwebu)
     (let-hash (load-config)
	       (let* ((url datadog-auth-url)
		      (data (format "username=~a&password=~a&_authentication_token=~a" .username .password dogwebu))
		      (reply (http-post url
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
		      (url (make-dd-url adds)))
		 (with ([status body] (rest-call 'get url (default-headers)))
		       (unless status
			 (error body))
		       (when (hash-table? body)
			 (let-hash body
				   (for (host .host_list)
					(let-hash host
						  (display .name)
						  (let-hash .live_metrics
							    (displayln ": iowait: " .?iowait " load15: " .?load15 " cpu: " .?cpu))))))))))

(def (totals)
     "Fetch Host totals"
     (let-hash (load-config)
	       (let* ((adds "hosts/totals")
		      (url (make-dd-url adds)))
		 (with ([status body] (rest-call 'get url (default-headers)))
		       (unless status
			 (error body))
		       (when (hash-table? body)
			 (let-hash body
				   (displayln "Total Up: " .total_up " Total Active: " .total_active)))))))

(def (stories)
     (let-hash (datadog-web-login)
	       (let (url (format "https://~a/watchdog/stories?page_size=100&stories_api_v2=true" datadog-host))
		 (with ([status body] (rest-call 'get url .headers))
		       (unless status
			 (error body))
		       (present-item body)))))

(def (livetail)
     (let-hash (datadog-web-login)
	       (let (url (format "https://app.datadoghq.com/logs/livetail" datadog-host))
		 (with ([status body] (rest-call 'get url .headers))
		       (unless status
			 (error body))
		       (present-item body)))))

(def (spawn-proc-collectors hosts secs dwl)
     (let ((threads []))
       (for (host hosts)
	    (let (t (spawn (lambda () (get-procs-by-host host secs dwl))))
              (set! threads (cons t threads))))
       threads))

(def (spawn-metric-tags metrics tag secs dwl)
     (let ((threads []))
       (for (metric metrics)
	    (let ((thread
		   (spawn
		    (lambda ()
                      (tag-in-metric? tag metric)))))
              ;;(get-procs-by-host host secs dwl))))
              (set! threads (cons thread threads))))
       threads))

(def (collect-from-pool threads)
     (when (list? threads)
       (let ((data []))
	 (while (> (length threads) 0)
		(let ((running_t 0)
		      (waiting_t 0)
		      (abterminated_t 0)
		      (terminated_t 0))
		  (for (thread threads)
		       (let* ((thread (car threads))
			      (state (thread-state thread)))
			 (cond
			  ((thread-state-running? state) (set! running_t (+ running_t 1)))
			  ((thread-state-waiting? state) (set! waiting_t (+ waiting_t 1)))
			  ((thread-state-abnormally-terminated? state) (set! abterminated_t (+ abterminated_t 1))
			   (let ((state (thread-state-abnormally-terminated-reason state)))
			     (displayln "Error: msg: " (display-exception state))
			     (set! threads (cdr threads))))
			  ((thread-state-normally-terminated? state) (set! terminated_t (+ terminated_t 1))
			   (let* ((results (thread-state-normally-terminated-result state)))
			     (set! data (cons results data))
			     (set! threads (cdr threads))))
			  (else
			   (displayln "unknown state: " (thread-state thread))
			   (set! threads (cdr threads))))))
		  (dp (format "loop: total: ~a running: ~a waiting: ~a terminated: ~a abnormal_terminated: ~a" (length threads) running_t waiting_t terminated_t abterminated_t))
		  (thread-sleep! 1)))
	 data)))

(def (get-procs-by-host host secs dwl)
     (let-hash dwl
	       (let* ((start (float->int (* (- (time->seconds (current-time)) secs) 1000)))
		      (end (float->int (* (time->seconds (current-time)) 1000)))
		      (url (format "https://app.datadoghq.com/proc/query?from=~a&to=~a&size_by=pct_mem&group_by=family&color_by=user&q=processes{host:~a}" start end host))
		      (headers [[ "cookie" :: (format "dogweb=~a; intercom-session=please-add-flat_tags_for_metric-to-your-api-thanks" .dogweb) ]
				[ "authority" :: "app.datadoghq.com" ]])
		      (reply (http-get url headers:  headers))
		      (text (request-text reply))
		      (procs (from-json text)))
		 procs)))

(def (sproc pattern)
     (hosts-proc-search pattern))

(def (hosts-proc-search procpat)
     (let* ((dwl (datadog-web-login))
            (hosts (hosts-with-agent))
            (threads (spawn-proc-collectors hosts 300 dwl))
            (results (collect-from-pool threads)))
       (for (result results)
	    (when (hash-table? result)
              (proc-format result procpat)))))

(def (metrics-by-tag metric-pattern tag)
     (for (metric (sort! (metrics-tag-search metric-pattern tag) string<?))
	  (displayln "< " metric)))

(def (metrics-tag-search metric-pattern tag)
     (let* ((dwl (datadog-web-login))
            (found [])
            (metrics (search-metrics metric-pattern))
            (threads (spawn-metric-tags metrics tag 4 dwl))
            (results (collect-from-pool threads)))
       (for (result results)
	    (when result
              (set! found (flatten (cons result found)))))
       found))

(def (procs host secs)
     "Return all processes for a given host in last n seconds"
     (let ((dwl (datadog-web-login)))
       (let-hash (get-procs-by-host host secs dwl)
		 (for (snapshot .snapshots)
		      (format-snapshot snapshot)))))

(def (proc host pattern dwl)
     "Find any processes who's name matches pattern on the given host and seconds window"
     (let ((procs (get-procs-by-host host 100 dwl)))
       (let-hash procs
		 (let ((results #f)
		       (matches []))
		   (for (snapshot .snapshots)
			(set! matches (cons (match-snapshot snapshot pattern) matches)))
		   matches))))

(def (proc-format procs procpat)
     "Find any processes who's name matches pattern on the given host and seconds window"
     (let-hash procs
	       (let ((results #f)
		     (matches []))
		 (for (snapshot .snapshots)
		      (set! matches (flatten (cons (match-snapshot snapshot procpat) matches))))
		 (when (length>n? (flatten matches) 0)
		   (let ((host (car (pregexp-split "\\}" (cadr (pregexp-split "\\{" .query))))))
		     (displayln host ": " (string-join (delete-duplicates (flatten matches)) ",")))))))

(def (format-snapshot snapshot)
     "Snapshots are lists of pslists."
     ;; [ 1, "rabbitmq", "0.0", "25.84", 14841081856, 8697987072, 0, 0, "beam.smp", 1 ],
     (when (hash-table? snapshot)
       (let-hash snapshot
		 (for (proc .pslist)
		      (with ([
			      ppid
			      user
			      pct1
			      pctmem
			      vsz
			      rss
			      zero1
			      zero2
			      name
			      nprocs
			      ] proc)
			    (displayln (format "ppid?:~a user:~a pct1?:~a pctmem:~a vsz:~a rss:~a zero1?:~a zero2?:~a name:~a nprocs:~a"
					       ppid
					       user
					       pct1
					       pctmem
					       vsz
					       rss
					       zero1
					       zero2
					       name
					       nprocs)))))))

(def (match-snapshot snapshot pattern)
     "Snapshots are lists of pslists."
     ;; [ 1, "rabbitmq", "0.0", "25.84", 14841081856, 8697987072, 0, 0, "beam.smp", 1 ],
     (when (hash-table? snapshot)
       (let ((results []))
	 (let-hash snapshot
		   (for (proc .pslist)
			(with ([ ppid user pct1 pctmem vsz rss zero1 zero2 name nprocs ] proc)
			      (when (pregexp-match pattern name)
				(set! results (cons name results)))))
		   results))))

(def (indexes)
     (let-hash (datadog-web-login)
	       (let* ((url "https://app.datadoghq.com/api/v1/logs/indexes?type=logs")
		      (reply (http-get url headers: .headers))
		      (myjson (from-json (request-text reply))))
		 (let-hash myjson
			   (for (index .indexes)
				(displayln (hash->list index)))))))

(def (status)
     (let* ((url "https://1k6wzpspjf99.statuspage.io/api/v2/status.json")
            (reply (http-get url headers: default-headers))
            (myjson (from-json (request-text reply))))
       (let-hash myjson
		 (displayln (let-hash .page .name " Url: " .url " Updated: " .updated_at))
		 (displayln "Status: " (let-hash .status " Indicator: " .indicator " Description: " .description)))))

(def (default-headers)
     (let-hash (load-config)
	       [["Accept" :: "*/*"]
		["Content-type" :: "application/json"]
		["DD-APPLICATION-KEY" :: .datadog-app-key]
		["DD-API-KEY" :: .datadog-api-key]]))

(def (get-meta-by-host host)
     "Given a pattern, return all the hosts along with meta data about the hosts"
     (let-hash (load-config)
	       (let ((results []))
		 (let lp ((start 0))
		   (let ((url (make-dd-url (format "hosts?filter=~a&group=&start=~a&count=100&discovery=true" host start))))
		     (with ([status body] (rest-call 'get url (default-headers)))
			   (unless status
			     (error body))
			   (when (hash-table? body)
			     (let-hash body
				       (when .?host_list
					 (set! results (flatten (cons .host_list results)))
					 (when (> .total_matching (+ start .total_returned))
					   (lp (+ start .total_returned)))))))))
		 results)))

(def (get-meta-by-host-web host)
     "Given a pattern, return all the hosts along with meta data about the hosts"
     (let ((results [])
           (dwl (datadog-web-login)))
       (displayln (hash->list dwl))
       (let-hash dwl
		 (let lp ((start 0))
		   (let* ((url (format "https://app.datadoghq.com/api/v1/hosts?filter=~a&group=&start=~a&count=100&discovery=true" host start))
			  (reply (http-get url headers: .headers))
			  (myjson (from-json (request-text reply)))
			  (hosts (let-hash myjson .?host_list)))
		     (let-hash myjson
			       (set! results (flatten (cons hosts results)))
			       (when (> (or .?total_matching 0) (+ start (or .?total_returned 0)))
				 (lp (+ start .total_returned)))))))
       results))

(def (get-meta-exact-host-name-match host)
     "Many host are similarly named, we just want the exact match, with anchors"
     (let ((result #f)
           (hosts (get-meta-by-host host)))
       (for (host2 hosts)
	    (let-hash host2
		      (when .?host_name
			(when (pregexp-match (format "^~a$" host) .?host_name)
			  (set! result host2)))))
       result))

(def (agents)
     (let ((hosts (hosts-with-agent)))
       (for (host hosts)
	    (displayln host))))

(def (hosts-with-agent)
     (hosts-with-app "agent"))

(def (find-app app)
     "Return a list of hosts with app listed in it's apps"
     (for (host (sort! (hosts-with-app app) string<?))
	  (displayln host)))

(def (hosts-with-app app)
     "Return a list of hostnames for all servers with agent listed in their apps. Used with proc search to avoid hitting hosts without proc info"
     (let ((secret-agents [])
           (hosts (get-meta-by-host-web "")))
       (for (host hosts)
	    (let-hash host
		      (when (member app .apps)
			(set! secret-agents (flatten (cons .host_name secret-agents))))))
       secret-agents))

(def (host hst)
     "We do not support regexp as hst here as this is used on the api directly to return matching hosts on pattern"
     (let ((results (get-meta-by-host hst))
           (outs [[ "Name" "Hostname" "Id" "Integrations" "Muted?" "Sources" "Tags by Source" "Aliases" "is Up?" "Metrics" ]]))
       (for (host results)
	    (when (hash-table? host)
              (let-hash host
			(set! outs (cons [ .?name
					   .?host_name
					   .?id
					   (jif (sort! .apps string<?) ",")
					   (if .is_muted "True" "False")
					   (jif .sources ",")
					   (hash->str .tags_by_source)
					   (jif .aliases ",")
					   (if .up "True" "False")
					   (hash->str .metrics)
					   ] outs)))))
       (style-output outs)))

(def (mute-host hostname end message override)
     "Mute a host
  hostname: The hostname to mute (required)
  end: POSIX timestamp for when the mute should end (optional)
  message: A message to describe the mute (optional)
  override: Whether to override existing mutes (optional)"
     (let* ((url (make-dd-url (format "host/~a/mute" hostname)))
            (data-hash (hash)))
       (when end
         (hash-put! data-hash "end" end))
       (when message
         (hash-put! data-hash "message" message))
       (when override
         (hash-put! data-hash "override" override))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'post url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))

(def (unmute-host hostname)
     "Unmute a host"
     (let ((url (make-dd-url (format "host/~a/unmute" hostname))))
       (with ([status body] (rest-call 'post url (default-headers) "{}"))
             (unless status
               (error body))
             (present-item body))))

(def (format-no-host host outs)
     (displayln "fnh:" host (length outs))
     (let* ((split (pregexp-split " " host))
            (a (or (car split) "None"))
            (b (if (length>n? split 2) (cadr split) "None"))
            (c (if (length>n? split 3) (cadr split) "None")))
       (cons [ a b c ] outs)))

(def (format-host host outs)
     (when (hash-table? host)
       (let-hash host
		 (cons [
			.?name
			.?host_name
			.?id
			(jif (sort! .apps string<?) ",")
			(if .is_muted "True" "False")
			(jif .sources ",")
			(hash->str .tags_by_source)
			(jif .aliases ",")
			(if .up "True" "False")
			(hash->str .metrics)
			] outs))))

(def (jif lst sep)
     "If we get a list, join it on sep"
     (if (list? lst)
	 (string-join lst sep)
	 lst))

(def (contexts)
     (let-hash (datadog-web-login)
	       (let* ((url "https://app.datadoghq.com/check/contexts?names_only=true")
		      (headers [[ "Cookie" :: (format "dogwebu=~a; dogweb=~a; intercom-session=please-add-flat_tags_for_metric-to-your-api-thanks" .dogwebu .dogweb) ]] )
		      (reply (http-get url headers:  headers))
		      (text (request-text reply))
		      (contexts (from-json text)))
		 (for (context contexts)
		      (display-context context)))))

(def (display-context context)
     (when (hash-table? context)
       (let-hash context
		 (displayln "*** " .name)
		 (displayln "**** Groups")
		 (for (group .groups)
		      (let-hash group
				(displayln "****** " .status " message: " (or .message "None") " " .tags " " .modified))))))

;; ********************* Reporting stuff here. todo move to other project

(def (run-agent-report file)
     "Read in a json inventory and find if they are in Datadog. Identify if they are and spit out the meta info on them"
     (let* ((raw (read-file-string file))
            (inventory (from-json raw))
            (raw2 (get-all-metas))
            (metas (convert-metas-hash-name raw2))
            (alias-hash (convert-metas-hash-aliases raw2))
            (outs [[ "Name" "HostName" "Id" "Applications" "Muted?" "Sources" "Tags" "Aliases" "Up?" "metrics" ]]))
       (for (host inventory)
	    (let-hash host
		      (if .?instance_id ;; aws
			  (let (found (hash-get alias-hash .instance_id))
			    (if found
				(set! outs (cons [
						  .?name
						  .?host_name
						  .?id
						  (jif (sort! .apps string<?) ",")
						  (if .is_muted "True" "False")
						  (jif .sources ",")
						  (hash->str .tags_by_source)
						  (jif .aliases ",")
						  (if .up "True" "False")
						  (hash->str .metrics) ] outs))
				(begin ;; not found
				  (let (alias (hash-get alias-hash .host))
				    (if alias
					(let-hash alias
						  (set! outs (cons [
								    .?name
								    .?host_name
								    .?id
								    (jif (sort! .apps string<?) ",")
								    (if .is_muted "True" "False")
								    (jif .sources ",")
								    (hash->str .tags_by_source)
								    (jif .aliases ",")
								    (if .up "True" "False")
								    (hash->str .metrics) ] outs)))
					(set! outs (cons [ .instance_id .ip .host ] outs)))))))
			  (begin
			    (unless (and .?active
					 (not .active))
			      (let* ((lookup1 (hash-get alias-hash .?host))
				     (lookup2 (hash-key-like alias-hash .?host))
				     (found (or lookup1 lookup2)))
				(if found
				    (let-hash found
					      (set! outs (cons [
								.?name
								.?host_name
								.?id
								(jif (sort! .apps string<?) ",")
								(if .is_muted "True" "False")
								(jif .sources ",")
								(hash->str .tags_by_source)
								(jif .aliases ",")
								(if .up "True" "False")
								(hash->str .metrics) ] outs)))
				    (set! outs (cons [ .?host .?ip ] outs)))))))))
       (style-output outs)))

(def (hash-key-like hsh pat)
     "Search a hash for keys that match a given regexp and return value"
     (present-item pat)
     (when (hash-table? hsh)
       (let ((found #f))
	 (hash-map
	  (lambda (k v)
            (when (and
                   (string? pat)
                   (pregexp-match pat k))
              (set! found v))) hsh)
	 found)))

(def (convert-metas-hash-name metas)
     (let ((meta-hash (hash)))
       (for (meta metas)
	    (let-hash meta
		      (hash-put! meta-hash .name meta)))
       meta-hash))

(def (convert-metas-hash-aliases metas)
     (let ((meta-hash (hash)))
       (for (meta metas)
	    (let-hash meta
		      (when (and (list? .?aliases)
				 (length>n? .aliases 0))
			(for (alias .aliases)
			     (hash-put! meta-hash alias meta)))))
       meta-hash))

(def (get-all-metas)
     "Get the full inventory"
     (get-meta-by-host ""))

(def (billing start end)
     "Get Usage metering from datadog"
     (let-hash (load-config)
	       (let* ((outs [[ "Host Count" "Container Count" "Hour" "Apm Host Count" "Agent Host Count" "Gcp Host Count" "Aws Host Count" ]])
		      (url (make-dd-url (format "usage/hosts?start_hr=~a&end_hr=~a" start end))))
		 (with ([status body] (rest-call 'get url (default-headers)))
		       (unless status
			 (error body))
		       (present-item body)
		       (when (hash-table? body)
			 (let-hash body
				   (for (entry .usage)
					(let-hash entry
						  (set! outs (cons [ .?host_count
								     .?container_count
								     .?hour
								     .?apm_host_count
								     .?agent_host_count
								     .?gcp_host_count
								     .?aws_host_count
								     ] outs)))))
			 (style-output outs))))))

(def (metric-tags-from-file file)
     "Read a list of metrics from file and return all metrics associated with each metric"
     (displayln "| Metric | Tag |")
     (displayln "|-|")
     (let* ((metrics (read-file-lines file))
            (dwl (datadog-web-login)))
       (for (metric metrics)
	    (for (tag (get-metric-tags metric))
		 (displayln "|" metric "|" tag "|")))))

(def (datadog-usage)
     "Get Usage metering from datadog"
     (let-hash (load-config)
	       (displayln "missing")))

(def (get-timeseries-usage start end)
     "Get hourly usage for custom metrics
  start: Start date in YYYY-MM-DD format (required)
  end: End date in YYYY-MM-DD format (required)"
     (let* ((url (make-dd-url (format "usage/timeseries?start_hr=~a&end_hr=~a" start end)))
            (outs [[ "Hour" "Num Custom Timeseries" ]]))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (when (hash-table? body)
               (let-hash body
                         (for (entry .usage)
                              (let-hash entry
                                        (set! outs (cons [ .?hour .?num_custom_timeseries ] outs)))))
               (style-output outs)))))

(def (get-top-avg-metrics month)
     "Get top 500 custom metrics by hourly average
  month: Month in YYYY-MM format (optional)"
     (let* ((url (if month
                    (make-dd-url (format "usage/top_avg_metrics?month=~a" month))
                    (make-dd-url "usage/top_avg_metrics")))
            (outs [[ "Metric Name" "Avg Metric Hour" "Max Metric Hour" ]]))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (when (hash-table? body)
               (let-hash body
                         (when .?metrics
                           (for (entry .metrics)
                                (let-hash entry
                                          (set! outs (cons [ .?metric_name .?avg_metric_hour .?max_metric_hour ] outs)))))
                         (style-output outs))))))

(def (verify-apps)
     "Validate all hosts app list against their apps tag, show those out of sync"
     (let ((hosts (get-all-metas)))
       (for (host hosts)
	    (let-hash host
		      (verify-app-tag host)))))

(def (verify-app-tag host)
     "Verify all Users App tag is consistent with apps."
     (when (hash-table? host)
       (let-hash host
		 (let* ((user-tags (hash-get .tags_by_source 'Users)))
		   (when user-tags
		     (for (tag user-tags)
			  (when (pregexp-match "^apps:" tag)
			    (let* ((want (pregexp-split "_" (cadr (pregexp-split ":" tag))))
				   (missing []))
			      (for (app want)
				   (unless (member app .apps)
				     (set! missing (flatten (cons app missing)))))
			      (when (length>n? missing 0)
				(displayln .?name " missing apps: " (jif (sort! missing string<?) ",")))))))))))

(def (create-user handle name role)
     "Create a Datadog user with:
   Handle must be a valid email address
   name as the user name
   Role as st: standard user, adm: for admin, ro: for readonly"
     (let-hash (load-config)
	       (let ((url (make-dd-url "user"))
		     (data (json-object->string
			    (hash
			     ("handle" handle)
			     ("name" name)
			     ("access_role" role)))))
		 (with ([status body] (rest-call 'post url (default-headers) data))
		       (unless status
			 (error body))
		       (present-item body)))))

(def (del-user handle)
     "Delete a Datadog user with:
   Handle must be a valid email address"
     (let-hash (load-config)
	       (let ((url (make-dd-url (format "user/~a" handle))))
		 (with ([status body] (rest-call 'delete url (default-headers)))
		       (unless status
			 (error body))
		       (present-item body)))))

(def (get-user handle)
     "Get details about a specific user
  handle: The user's handle (email address)"
     (let ((url (make-dd-url (format "user/~a" handle))))
       (with ([status body] (rest-call 'get url (default-headers)))
             (unless status
               (error body))
             (present-item body))))

(def (update-user handle name email disabled access_role)
     "Update a Datadog user
  handle: The user's handle (email address) (required)
  name: The user's name (optional)
  email: The user's new email (optional)
  disabled: Whether the user is disabled (optional)
  access_role: The user's access role (st, adm, ro) (optional)"
     (let* ((url (make-dd-url (format "user/~a" handle)))
            (data-hash (hash)))
       (when name
         (hash-put! data-hash "name" name))
       (when email
         (hash-put! data-hash "email" email))
       (when disabled
         (hash-put! data-hash "disabled" disabled))
       (when access_role
         (hash-put! data-hash "access_role" access_role))
       (let ((data (json-object->string data-hash)))
         (with ([status body] (rest-call 'put url (default-headers) data))
               (unless status
                 (error body))
               (present-item body)))))


(def (check-manifest manifest)
     "Read in a manifest and run all of the checks against the hosts inventory within.
    2. required-tags: (required) verify required-tags exists, and is not empty. Support for both foo:bar, as well as foo: to catch any references to foo:*
      - Tags that are required for each host, and used in monitors, and or set thresholds for monitors
    3. integrations: (required) verify integrations exist and is not empty. For each host ensure they are in the list of find-app integration
      - Given integrations are specific to an application, these would be global to the manifest. e.g. mysql.yaml
    4. metrics: (required) verify metrics list, and not empty. for each host verify they are in metric-tags for the given metric
       - Each host must show up in metric-tags for metric. Tags may have variations on the name. might need new tag to not confuse with other names dd knows
    5. monitors: templates that used the host value, and any specific tags.
      - Setup templates to inherit all tags as variables so they can be used. #{foo:} vs #{foo:bar}"
     (try
      (let* ((manifest (car (yaml-load manifest)))
             (hosts (hash-get manifest "hosts"))
             (required-tags (hash-get manifest "required-tags"))
             (integrations (hash-get manifest "integrations"))
             (metrics (hash-get manifest "metrics"))
             (monitors (hash-get manifest "monitors")))
	(when (list? hosts)
	  (for (host hosts)
               ;; Check Host entries in datadog
               (displayln "<=== " host)
               (let (meta (get-meta-exact-host-name-match host))
		 (if meta
		     (begin
		       (manifest-host-check host meta)
		       (manifest-tag-check required-tags meta)
		       (manifest-integration-check integrations meta)
		       (manifest-metric-check metrics meta)
		       (manifest-monitor-check monitors meta))
		     (displayln (format "Host ~a does not exist in datadog." host)))))))
      (catch (e)
	     (raise e))))

(def (manifest-host-check host meta)
     "Verify hostname matches exactly"
     (let-hash meta
	       (unless .?host_name
		 (displayln "fail: host not found" host))
	       (if (string=? host .?host_name)
		   (displayln "ok: host " host " matches " .?host_name)
		   (displayln "fail: unknown. host: " host " meta: " (present-item meta)))))

(def (manifest-tag-check required-tags meta)
     "Verify that all the tags expected for this host are applied"
     (let-hash meta
	       (when (hash-table? .tags_by_source)
		 (let-hash .tags_by_source
			   (for (tag required-tags)
				(let* ((found #f)
				       (tag (pregexp-replace* "\\|" tag ":"))
				       (pattern (if (pregexp-match "\\w+:$" tag)
						    (format "^~a" tag)
						    (format "^~a$" tag))))
				  (if (and
				       .?Users
				       (list? .Users))
				      (begin
					(for (utag .Users)
					     (when (pregexp-match pattern utag)
					       (displayln (format "ok: required ~a found ~a " tag utag))
					       (set! found #t)))
					(unless found
					  (displayln "fail: missing tag: " tag " found:" .Users)))
				      (displayln "fail: no User tags for host found"))))))))

(def (manifest-integration-check integrations meta)
     "Verify that all the integrations expected for this host are applied"
     (let-hash meta
	       (for (integration integrations)
		    (if (member integration .apps)
			(displayln (format "ok: ~a integration found" integration))
			(displayln (format "fail: ~a integration not found got: ~a" integration .apps))))))

(def (manifest-metric-check metrics meta)
     "Verify that all the hosts are submitting for a given metric"
     (let-hash meta
	       (let (tag (format "host:~a" .host_name))
		 (for (metric metrics)
		      (if (tag-in-metric? tag metric)
			  (displayln "ok: metric " metric " found.")
			  (displayln "fail: metric " metric " not found."))))))

(def (manifest-monitor-check monitors meta)
     "Verify that all the monitors expected for this host are applied"
     #!void)
;;  (display "monitors-check: "))
;;  (present-item monitors))

;; web stuff
(def server-address
     "127.0.0.1:9999")

(def server-url
     (string-append "http://" server-address))

(def (write-simple-handler req res)
     (http-response-write res 200 '(("Content-Type" . "text/plain")) "hello"))

(def (write-chunked-handler req res)
     (http-response-begin res 200 '(("Content-Type" . "text/plain")))
     (http-response-chunk res "hello ")
     (http-response-chunk res "there!")
     (http-response-end res))

(def (start-dd-web)
     (let ((httpd (start-http-server! server-address mux: (make-recursive-http-mux))))
       (http-register-handler httpd "/" root-handler)
       (thread-join! httpd)))

(def (root-handler req res)
     (http-response-write res 200 '(("Content-Type" . "text/plain"))
			  (string-append "hello, " (inet-address->string (http-request-client req)) "\n")))
