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

  (def edit-monitor
    (command 'edit-monitor help: "Update a monitor with new values."
	     (argument 'id)
	     (argument 'new-query)
	     (argument 'new-name)
	     (argument 'new-message)))
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

  (def new-monitor
    (command 'new-monitor help: "Create new monitor."
	     (argument 'type)
	     (argument 'query)
	     (argument 'name)
	     (argument 'message)
	     (argument 'tags)))
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
  (def run-agent-report
    (command 'run-agent-report help: "Hosts Process Search."
	     (argument 'file)))
  (def screen
    (command 'screen help: "Descript existing screen"
	     (argument 'id)))
  (def screens
    (command 'screens help: "List all Screenboards"))
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
		    check-manifest
		    clear-tags
		    config
		    contexts
		    create-user
		    del-user
		    users
		    del-monitor
		    dump-monitors
		    dump-sboards
		    dump-tboards
		    edit-monitor
;;		    ems
		    events-day
		    events-hour
		    events-min
		    events-month
		    events-raw
		    events-week
		    find-app
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
		    new-monitor
		    query-day
		    query-hour
		    query-metrics
		    query-min
		    run-agent-report
		    screen
		    screens
		    dashes
		    search
		    sproc
		    start-dd-web
		    status
		    stories
		    tag
		    tag-all
		    tags
		    tboard-add-chart
		    tboard-create
;;		    tboard-fancy
		    tboard-mass-add
		    tboard-mass-add-many
		    totals
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
      ((check-manifest)
       (check-manifest .manifest))
      ((clear-tags)
       (clear-tags .host))
      ((config)
       (config))
      ((contexts)
       (contexts))
      ((create-user)
       (create-user .email .full-user-name .role))
      ((del-user)
       (del-user .email))
      ((users)
       (users))
      ((del-monitor)
       (del-monitor .id))
      ((dump-monitors)
       (dump-monitors .directory))
      ((dump-sboards)
       (dump-sboards .directory))
      ((dump-tboards)
       (dump-tboards .directory))
      ((edit-monitor)
       (edit-monitor .id .new-query .new-name .new-message))
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
      ((new-monitor)
       (new-monitor .type .query .name .message .tags))
      ((query-day)
       (query-day .query))
      ((query-hour)
       (query-hour .query))
      ((query-metrics)
       (query-metrics .begin-epoch .end-epoch .query))
      ((query-min)
       (query-min .query))
      ((run-agent-report)
       (run-agent-report .file))
      ((screen)
       (screen .id))
      ((screens)
       (screens))
      ((dashes)
       (dashes))
      ((search)
       (search .pattern))
      ((sproc)
       (sproc .pattern))
      ((start-dd-web)
       (start-dd-web))
      ((status)
       (status))
      ((stories)
       (stories))
      ((tag)
       (tag .host .tag))
      ((tag-all)
       (tag-all .hostname-pattern .tag))
      ((tags)
       (tags))
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
      ((verify-account)
       (verify-account))
      ((verify-apps)
       (verify-apps))
      ((view-md)
       (view-md .metric))
      )))
