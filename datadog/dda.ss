;; -*- Gerbil -*-
;;; © ober 2023
;;; Datadog client binary

(import
  :gerbil/gambit
  :std/crypto/cipher
  :std/db/dbi
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/getopt
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
  :clan/text/yaml
  :std/text/zlib
  :std/markup/sxml
  :ober/oberlib
  "client.ss")

(export main)

(def program-name "datadog")


(def (main . args)
  (def agents
    (command 'agents help: "List all servers with Datadog Agent running."))

  (def billing
    (command 'billing  help: "List Metering/Billing information for timeframe."
	     (argument 'from-date help: "From Date in YYYY-MM-DDTHH. e.g. 2010-02-03T12")
             (argument 'to-date help: "From Date in YYYY-MM-DDTHH. e.g. 2010-02-03T12")))

  (def bulk-resolve-monitors
    (command 'bulk-resolve-monitors help: "Resolve multiple monitors at once"
	     (argument 'monitor-ids help: "Comma-separated list of monitor IDs")))

  (def check-manifest
    (command 'check-manifest help: "Check manifest yaml file"
	     (argument 'manifest help:  "manifest.yaml")))

  (def clear-tags
    (command 'clear-tags help: "Remove all tags from hostname"
	     (argument 'host help: "Hostname to clear the tags on")))

  (def config
    (command 'config help: "Configure credentials for datadog."))

  (def contexts
    (command 'contexts help: "List all contexts"))

  (def create-comment
    (command 'create-comment help: "Create a comment on Datadog"
	     (argument 'message help: "The comment text")
	     (optional-argument 'handle help: "The handle of the user making the comment")
	     (optional-argument 'related-event-id help: "The id of another comment or event to reply to")))

  (def create-embed
    (command 'create-embed help: "Create a new embeddable graph"
	     (argument 'graph-json help: "The graph definition in JSON")
	     (optional-argument 'timeframe help: "The timeframe for the graph (1_hour, 4_hours, 1_day, 2_days, 1_week)")
	     (optional-argument 'size help: "The size of the graph (small, medium, large, xlarge)")
	     (optional-argument 'legend help: "Whether to show legend (yes, no)")
	     (optional-argument 'title help: "The graph title")))

  (def create-user
    (command 'create-user help: "Create a Datadog user. "
	     (argument 'email help: "Email address of user")
	     (argument 'full-user-name help: "Full user name")
	     (argument 'role help: "Standard User: st, Admin: adm, Readonly: ro")))
  (def del-user
    (command 'del-user help: "Disable a Datadog user. "
	     (argument 'email help: "Email of user to remove")))
  (def users
    (command 'users help: "List all Datadog users. "))

  (def delete-comment
    (command 'delete-comment help: "Delete a comment by ID"
	     (argument 'comment-id help: "The ID of the comment to delete")))

  (def delete-event
    (command 'delete-event help: "Delete an event by ID"
	     (argument 'event-id help: "The ID of the event to delete")))

  (def delete-sboard
    (command 'delete-sboard help: "Delete a screenboard by ID"
	     (argument 'board-id help: "The screenboard ID")))

  (def delete-tboard
    (command 'delete-tboard help: "Delete a timeboard by ID"
	     (argument 'dash-id help: "The timeboard ID")))

  (def del-monitor
    (command 'del-monitor help: "Delete monitor."
	     (argument 'id help: "Id of monitor to remove")))

  (def dump-monitors
    (command 'dump-monitors help: "Dump all monitors to yaml definitions in directory passed."
	     (argument 'directory help: "Directory to download all monitors")))

  (def dump-sboards
    (command 'dump-sboards help: "Dump: dump json definitions of all screenboards to <directory>"
	     (argument 'directory help: "Directory to dump sboards to")))

  (def dump-tboards
    (command 'dump-tboards help: "Dump: dump json definitions of all timeboards to <directory>"
	     (argument 'directory help: "Directory to dump tboards to")))

  (def downtimes
    (command 'downtimes help: "List all downtimes"))

  (def edit-comment
    (command 'edit-comment help: "Edit an existing comment"
	     (argument 'comment-id help: "The ID of the comment to edit")
	     (optional-argument 'message help: "The new comment text")
	     (optional-argument 'handle help: "The handle of the user making the edit")))

  (def edit-metric-metadata
    (command 'edit-metric-metadata help: "Edit metric metadata"
	     (argument 'metric help: "The metric name")
	     (argument 'description help: "The metric description")
	     (argument 'short-name help: "The metric short name")))

  (def edit-monitor
    (command 'edit-monitor help: "Update a monitor with new values."
	     (argument 'id)
	     (argument 'new-query)
	     (argument 'new-name)
	     (argument 'new-message)))
  (def enable-embed
    (command 'enable-embed help: "Enable a specified embed"
	     (argument 'embed-id help: "The embed token")))

  ;; (def ems
  ;;   (command 'ems help: "Search event for last minute matching tag."))
  (def events-day
    (command 'events-day help: "List all events for the past day"
	     (argument 'tag-string)))
  (def events-hour
    (command 'events-hour help: "List all events for past hours"
	     (argument 'tag-string)))
  (def events-min
    (command 'events-min help: "List all events for the past minute"
	     (argument 'tag-string)))
  (def events-month
    (command 'events-month help: "List all events for the past month"
	     (argument 'tag-string)))
  (def events-raw
    (command 'events-raw help: "List all events for the past day"
	     (argument 'secs)))
  (def events-week
    (command 'events-week help: "List all events for the past week"
	     (argument 'tag-string)))
  (def find-app
    (command 'find-app help: "List all servers with app"
	     (argument 'application-name)))

  (def get-all-embeds
    (command 'get-all-embeds help: "Get a list of all embeddable graphs"))

  (def get-embed
    (command 'get-embed help: "Get a specific embeddable graph"
	     (argument 'embed-id help: "The embed token")
	     (optional-argument 'size help: "The size of the graph (small, medium, large, xlarge)")
	     (optional-argument 'legend help: "Whether to show legend (yes, no)")
	     (optional-argument 'template-variables help: "Template variables as key=value pairs")))

  (def get-event
    (command 'get-event help: "Get a specific event by ID"
	     (argument 'event-id help: "The event ID")))

  (def get-timeseries-usage
    (command 'get-timeseries-usage help: "Get hourly usage for custom metrics"
	     (argument 'start help: "Start date in YYYY-MM-DD format")
	     (argument 'end help: "End date in YYYY-MM-DD format")))

  (def get-top-avg-metrics
    (command 'get-top-avg-metrics help: "Get top 500 custom metrics by hourly average"
	     (optional-argument 'month help: "Month in YYYY-MM format")))

  (def get-user
    (command 'get-user help: "Get details about a specific user"
	     (argument 'handle help: "The user's handle (email address)")))

  (def graph-min
    (command 'graph-min help: "Create a graph from query."
	     (argument 'query)))
  (def host
    (command 'host help: "host"
	     (argument 'host-pattern)))
  (def hosts
    (command 'hosts help: "List Datadog Hosts that match argument 1."
	     (argument 'host-pattern help: "pattern of host to search for>")))
  (def indexes
    (command 'indexes help: "List Log Indexes."))
  (def live-metrics
    (command 'live-metrics help: "List Datadog live metrics for host."
	     (argument 'hostname)))
  (def livetail
    (command 'livetail help: "List Log Indexes."))
  (def metric-tags
    (command 'metric-tags help: "metric-tags <metric>."
	     (argument 'metric)))
  (def metrics-by-tag
    (command 'metrics-by-tag help: "metrics-by-tag <metric pattern> <tag>"
	     (argument 'metric-pattern)
	     (argument 'tag)))
  (def metric-tags-web
    (command 'metric-tags-web help: "metric-tags <metric>."
	     (argument 'metric)))
  (def metric-tags-from-file
    (command 'metric-tags-from-file help: "metric-tags-from-file <file>."
	     (argument 'file)))
  (def metrics
    (command 'metrics help: "List Datadog Metrics and search on argument 1."
	     (argument 'metric-pattern)))
  (def monitor
    (command 'monitor help: "Describe Monitor."
	     (argument 'id)))
  (def monitors
    (command 'monitors help: "List all monitors."))
  (def monitors-table
    (command 'monitors-table help: "List all monitors - Table format."))

  (def mute-all-monitors
    (command 'mute-all-monitors help: "Mute all monitors"))

  (def mute-host
    (command 'mute-host help: "Mute a host"
	     (argument 'hostname help: "The hostname to mute")
	     (optional-argument 'end help: "POSIX timestamp for when the mute should end")
	     (optional-argument 'message help: "A message to describe the mute")
	     (optional-argument 'override help: "Whether to override existing mutes")))

  (def mute-monitor
    (command 'mute-monitor help: "Mute a monitor"
	     (argument 'monitor-id help: "The monitor ID")
	     (optional-argument 'scope help: "The scope to mute (e.g., 'host:myhost')")
	     (optional-argument 'end help: "POSIX timestamp for when the mute should end")))

  (def new-monitor
    (command 'new-monitor help: "Create new monitor."
	     (argument 'type)
	     (argument 'query)
	     (argument 'name)
	     (argument 'message)
	     (argument 'tags)))

  (def post-check-run
    (command 'post-check-run help: "Post a service check run"
	     (argument 'check help: "The name of the check")
	     (argument 'host-name help: "The name of the host submitting the check")
	     (argument 'status help: "An integer for the status (0=OK, 1=WARNING, 2=CRITICAL, 3=UNKNOWN)")
	     (optional-argument 'timestamp help: "POSIX timestamp (use #f for current time)")
	     (optional-argument 'message help: "Description of why this status occurred")
	     (optional-argument 'tags help: "Comma-separated list of tags for this check")))

  (def query-day
    (command 'query-day help: "<query>: Query metrics for last day."
	     (argument 'query)))
  (def query-hour
    (command 'query-hour help: "<query>: Query metrics for last hour."
	     (argument 'query)))
  (def query-metrics
    (command 'query-metrics help: "<query>: Query metrics for last min."
	     (argument 'begin-epoch)
	     (argument 'end-epoch)
	     (argument 'query)))
  (def query-min
    (command 'query-min help: "<query>: Query metrics for last min."
	     (argument 'query)))

  (def revoke-embed
    (command 'revoke-embed help: "Revoke a specified embed"
	     (argument 'embed-id help: "The embed token")))

  (def revoke-sboard-share
    (command 'revoke-sboard-share help: "Revoke sharing of a screenboard"
	     (argument 'board-id help: "The screenboard ID")))

  (def run-agent-report
    (command 'run-agent-report help: "Hosts Process Search."
	     (argument 'file)))

  (def screen
    (command 'screen help: "Descript existing screen"
	     (argument 'id)))

  (def screen-create
    (command 'screen-create help: "Create a new screenboard"
	     (argument 'board-title help: "The board title")
	     (argument 'widgets help: "Widgets JSON")
	     (argument 'width help: "Width of the board")
	     (argument 'height help: "Height of the board")))

  (def screen-update
    (command 'screen-update help: "Update an existing screenboard"
	     (argument 'id help: "The screenboard ID")
	     (argument 'width help: "Width of the board")
	     (argument 'height help: "Height of the board")
	     (argument 'board-title help: "The board title")))

  (def screens
    (command 'screens help: "List all Screenboards"))

  (def share-sboard
    (command 'share-sboard help: "Share a screenboard (make it publicly accessible)"
	     (argument 'board-id help: "The screenboard ID")))

  (def submit-event
    (command 'submit-event help: "Submit an event to Datadog"
	     (argument 'title help: "Event title")
	     (argument 'text help: "Event text")
	     (argument 'priority help: "Event priority (normal or low)")
	     (argument 'tags help: "Comma-separated list of tags")
	     (argument 'alert-type help: "Alert type (error, warning, info, success)")))

  (def dashes
    (command 'dashes help: "List all TimeBoards"))
  (def search
    (command 'search help: "list all metrics or hosts matching pattern"
	     (argument 'pattern)))
  (def sproc
    (command 'sproc help: "Hosts Process Search."
	     (argument 'pattern)))
  (def start-dd-web
    (command 'start-dd-web help: "Start web server mode"))
  (def status
    (command 'status help: "Get Datadog Status."))
  (def stories
    (command 'stories help: "stories"))
  (def tag
    (command 'tag help: "Add a tag to a hostname"
	     (argument 'host)
	     (argument 'tag help: "name:value")))
  (def tag-all
    (command 'tag-all help: "Add a tag to all hostnames matching pattern"
	     (argument 'hostname-pattern)
	     (argument 'tag help: "name:value")))
  (def tags
    (command 'tags help: "list all tags and hosts"))

  (def tags-for-metric
    (command 'tags-for-metric help: "Return all tags found for a given metric"
	     (argument 'metric help: "The metric name")))

  (def tags-for-source
    (command 'tags-for-source help: "Return all tags for a given source"
	     (argument 'source help: "The source name")))

  (def tboard
    (command 'tboard help: "Get details about a timeboard"
	     (argument 'id help: "The timeboard ID")))

  (def tboard-add-chart
    (command 'tboard-add-chart help: "Add a chart to a timeboard."
	     (argument 'timeboard-id)
	     (argument 'title)
	     (argument 'request)
	     (argument 'viz)))
  (def tboard-create
    (command 'tboard-create help: "Create a new timeboard"
	     (argument 'title)
	     (argument 'description)))
  ;; (def tboard-fancy
  ;;   (command 'tboard-fancy help: "Add charts for metrics which apply to tag provided."
  ;; 	     (argument 'timeboard-id)
  ;; 	     (argument 'metric-pattern)
  ;; 	     (argument 'tag help: "key:value")
  ;; 	     (argument 'replace help: "t or f")))
  (def tboard-mass-add
    (command 'tboard-mass-add help: "Add charts matching regexp metrics to a timeboard."
	     (argument 'timeboard-id)
	     (argument 'pattern)
	     (argument 'host-clause)
	     (argument 'group-by-clause)
	     (argument 'replace help: "t or f")))
  (def tboard-mass-add-many
    (command 'tboard-mass-add-many help: "Add charts matching regexp metrics to a timeboard. Useful for multiple hosts."
	     (argument 'timeboard-id)
	     (argument 'pattern)
	     (argument 'host-clause help: "host1 host2 host3")
	     (argument 'replace help: "t or f")))
  (def totals
    (command 'totals help: "Host Totals."))

  (def unmute-all-monitors
    (command 'unmute-all-monitors help: "Unmute all monitors"))

  (def unmute-host
    (command 'unmute-host help: "Unmute a host"
	     (argument 'hostname help: "The hostname to unmute")))

  (def unmute-monitor
    (command 'unmute-monitor help: "Unmute a monitor"
	     (argument 'monitor-id help: "The monitor ID")
	     (optional-argument 'scope help: "The scope to unmute")))

  (def update-host-tags
    (command 'update-host-tags help: "Update (replace) all tags for a host"
	     (argument 'hostname help: "The hostname")
	     (argument 'tags help: "Comma-separated list of tags to set for the host")))

  (def update-user
    (command 'update-user help: "Update a Datadog user"
	     (argument 'handle help: "The user's handle (email address)")
	     (optional-argument 'name help: "The user's name")
	     (optional-argument 'email help: "The user's new email")
	     (optional-argument 'disabled help: "Whether the user is disabled")
	     (optional-argument 'access-role help: "The user's access role (st, adm, ro)")))

  (def verify-account
    (command 'verify-account help: "Verify account credentials"))
  (def verify-apps
    (command 'verify-apps help: "Verify account credentials"))
  (def view-md
    (command 'view-md help: "Describe metric metadata"
	     (argument 'metric)))

  (call-with-getopt process-args args
		    program: "confluence"
		    help: "Confluence cli in Gerbil"
		    agents
		    billing
		    bulk-resolve-monitors
		    check-manifest
		    clear-tags
		    config
		    contexts
		    create-comment
		    create-embed
		    create-user
		    dashes
		    del-monitor
		    del-user
		    delete-comment
		    delete-event
		    delete-sboard
		    delete-tboard
		    downtimes
		    dump-monitors
		    dump-sboards
		    dump-tboards
		    edit-comment
		    edit-metric-metadata
		    edit-monitor
		    enable-embed
;;		    ems
		    events-day
		    events-hour
		    events-min
		    events-month
		    events-raw
		    events-week
		    find-app
		    get-all-embeds
		    get-embed
		    get-event
		    get-timeseries-usage
		    get-top-avg-metrics
		    get-user
		    graph-min
		    host
		    hosts
		    indexes
		    live-metrics
		    livetail
		    metric-tags
		    metrics-by-tag
		    metric-tags-web
		    metric-tags-from-file
		    metrics
		    monitor
		    monitors
		    monitors-table
		    mute-all-monitors
		    mute-host
		    mute-monitor
		    new-monitor
		    post-check-run
		    query-day
		    query-hour
		    query-metrics
		    query-min
		    revoke-embed
		    revoke-sboard-share
		    run-agent-report
		    screen
		    screen-create
		    screen-update
		    screens
		    search
		    share-sboard
		    sproc
		    start-dd-web
		    status
		    stories
		    submit-event
		    tag
		    tag-all
		    tags
		    tags-for-metric
		    tags-for-source
		    tboard
		    tboard-add-chart
		    tboard-create
;;		    tboard-fancy
		    tboard-mass-add
		    tboard-mass-add-many
		    totals
		    unmute-all-monitors
		    unmute-host
		    unmute-monitor
		    update-host-tags
		    update-user
		    users
		    verify-account
		    verify-apps
		    view-md
		    ))

(def (process-args cmd opt)
  (let-hash opt
    (case cmd
      ((agents)
       (agents))
      ((billing)
       (billing .from-date .to-date))
      ((bulk-resolve-monitors)
       (bulk-resolve-monitors (string-split .monitor-ids #\,)))
      ((check-manifest)
       (check-manifest .manifest))
      ((clear-tags)
       (clear-tags .host))
      ((config)
       (config))
      ((contexts)
       (contexts))
      ((create-comment)
       (create-comment .message .?handle .?related-event-id))
      ((create-embed)
       (create-embed .graph-json .?timeframe .?size .?legend .?title))
      ((create-user)
       (create-user .email .full-user-name .role))
      ((dashes)
       (dashes))
      ((del-monitor)
       (del-monitor .id))
      ((del-user)
       (del-user .email))
      ((delete-comment)
       (delete-comment .comment-id))
      ((delete-event)
       (delete-event .event-id))
      ((delete-sboard)
       (delete-sboard .board-id))
      ((delete-tboard)
       (delete-tboard .dash-id))
      ((downtimes)
       (downtimes))
      ((dump-monitors)
       (dump-monitors .directory))
      ((dump-sboards)
       (dump-sboards .directory))
      ((dump-tboards)
       (dump-tboards .directory))
      ((edit-comment)
       (edit-comment .comment-id .?message .?handle))
      ((edit-metric-metadata)
       (edit-metric-metadata .metric .description .short-name))
      ((edit-monitor)
       (edit-monitor .id .new-query .new-name .new-message))
      ((enable-embed)
       (enable-embed .embed-id))
      ;; ((ems)
      ;;  (ems))
      ((events-day)
       (events-day .tag-string))
      ((events-hour)
       (events-hour .tag-string))
      ((events-min)
       (events-min .tag-string))
      ((events-month)
       (events-month .tag-string))
      ((events-raw)
       (events-raw .secs))
      ((events-week)
       (events-week .tag-string))
      ((find-app)
       (find-app .application-name))
      ((get-all-embeds)
       (get-all-embeds))
      ((get-embed)
       (get-embed .embed-id .?size .?legend .?template-variables))
      ((get-event)
       (get-event .event-id))
      ((get-timeseries-usage)
       (get-timeseries-usage .start .end))
      ((get-top-avg-metrics)
       (get-top-avg-metrics .?month))
      ((get-user)
       (get-user .handle))
      ((graph-min)
       (graph-min .query))
      ((host)
       (host .host-pattern))
      ((hosts)
       (hosts .host-pattern))
      ((indexes)
       (indexes))
      ((live-metrics)
       (live-metrics .hostname))
      ((livetail)
       (livetail))
      ((metric-tags)
       (metric-tags .metric))
      ((metrics-by-tag)
       (metrics-by-tag .metric-pattern .tag))
      ((metric-tags-web)
       (metric-tags-web .metric))
      ((metric-tags-from-file)
       (metric-tags-from-file .file))
      ((metrics)
       (metrics .metric-pattern))
      ((monitor)
       (monitor .id))
      ((monitors)
       (monitors))
      ((monitors-table)
       (monitors-table))
      ((mute-all-monitors)
       (mute-all-monitors))
      ((mute-host)
       (mute-host .hostname .?end .?message .?override))
      ((mute-monitor)
       (mute-monitor .monitor-id .?scope .?end))
      ((new-monitor)
       (new-monitor .type .query .name .message .tags))
      ((post-check-run)
       (post-check-run .check .host-name .status .?timestamp .?message 
                       (if .?tags (string-split .?tags #\,) #f)))
      ((query-day)
       (query-day .query))
      ((query-hour)
       (query-hour .query))
      ((query-metrics)
       (query-metrics .begin-epoch .end-epoch .query))
      ((query-min)
       (query-min .query))
      ((revoke-embed)
       (revoke-embed .embed-id))
      ((revoke-sboard-share)
       (revoke-sboard-share .board-id))
      ((run-agent-report)
       (run-agent-report .file))
      ((screen)
       (screen .id))
      ((screen-create)
       (screen-create .board-title .widgets .width .height))
      ((screen-update)
       (screen-update .id .width .height .board-title))
      ((screens)
       (screens))
      ((search)
       (search .pattern))
      ((share-sboard)
       (share-sboard .board-id))
      ((sproc)
       (sproc .pattern))
      ((start-dd-web)
       (start-dd-web))
      ((status)
       (status))
      ((stories)
       (stories))
      ((submit-event)
       (submit-event .title .text .priority (string-split .tags #\,) .alert-type))
      ((tag)
       (tag .host .tag))
      ((tag-all)
       (tag-all .hostname-pattern .tag))
      ((tags)
       (tags))
      ((tags-for-metric)
       (tags-for-metric .metric))
      ((tags-for-source)
       (tags-for-source .source))
      ((tboard)
       (tboard .id))
      ((tboard-add-chart)
       (tboard-add-chart .timeboard-id .title .request .viz))
      ((tboard-create)
       (tboard-create .title .description))
      ;; ((tboard-fancy)
      ;;  (tboard-fancy .timeboard-id .metric-pattern .tag .replace))
      ((tboard-mass-add)
       (tboard-mass-add .timeboard-id .pattern .host-clause .group-by-clause .replace))
      ((tboard-mass-add-many)
       (tboard-mass-add-many .timeboard-id .pattern .host-clause .replace))
      ((totals)
       (totals))
      ((unmute-all-monitors)
       (unmute-all-monitors))
      ((unmute-host)
       (unmute-host .hostname))
      ((unmute-monitor)
       (unmute-monitor .monitor-id .?scope))
      ((update-host-tags)
       (update-host-tags .hostname (string-split .tags #\,)))
      ((update-user)
       (update-user .handle .?name .?email .?disabled .?access-role))
      ((users)
       (users))
      ((verify-account)
       (verify-account))
      ((verify-apps)
       (verify-apps))
      ((view-md)
       (view-md .metric))
      )))
