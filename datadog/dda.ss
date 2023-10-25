;; -*- Gerbil -*-
;;; © ober
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
  :std/xml/ssax
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
             (agument 'to-date help: "From Date in YYYY-MM-DDTHH. e.g. 2010-02-03T12")))

  (def check-manifest
    (command 'check-manifest help: "Check manifest yaml file"
	     (arguments 'manifest help:  "manifest.yaml")))

  (def clear-tags
    (command 'clear-tags help: "Remove all tags from hostname"
	     (argument 'host help: "Hostname to clear the tags on")))

  (def config
    (command 'config help: "Configure credentials for datadog."))

  (def contexts
    (command 'contexts help: "List all contexts"))

  (def create
    (command 'create-user help: "Create a Datadog user. "
	     (argument 'email help: "Email address of user")
	     (argument 'full-user-name help: "Full user name")
	     (argument 'role help: "Standard User: st, Admin: adm, Readonly: ro")))
  (def del
    (command 'del-user help: "Disable a Datadog user. "
	     (argument 'email help: "Email of user to remove")))
  (def users
    (command 'users help "List all Datadog users. "))

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
  (def ems
    (command 'ems help: "Search event for last minute matching tag."))

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
	     (argument ') (usage: "metrics-by-tags <metric pattern> <tag>") (count: 2)))
   ("metric-tags-web" (hash (description: "metric-tags <metric>.") (usage: "metric-tags <metric>") (count: 1)))
   ("metric-tags-from-file" (hash (description: "metric-tags-from-file <file>.") (usage: "metric-tags-from-file <file>") (count: 1)))
   ("metrics" (hash (description: "List Datadog Metrics and search on argument 1.") (usage: "metrics <pattern of metric to search for>") (count: 1)))
   ("monitor" (hash (description: "Describe Monitor.") (usage: "monitor <monitor id>") (count: 1)))
   ("monitors" (hash (description: "List all monitors.") (usage: "monitors") (count: 0)))
   ("monitors-table" (hash (description: "List all monitors - Table format.") (usage: "monitors") (count: 0)))
   ("new-monitor" (hash (description: "Create new monitor.") (usage: "new-monitor <type> <query> <name> <message> <tags>") (count: 5)))
   ("query-day" (hash (description: "<query>: Query metrics for last day.") (usage: "query-day") (count: 1)))
   ("query-hour" (hash (description: "<query>: Query metrics for last hour.") (usage: "query-hour") (count: 1)))
   ("query-metrics" (hash (description: "<query>: Query metrics for last min.") (usage: "query-metrics begin_epoch end_epoch query") (count: 3)))
   ("query-min" (hash (description: "<query>: Query metrics for last min.") (usage: "query-min") (count: 1)))
   ("run-agent-report" (hash (description: "Hosts Process Search.") (usage: "run-agent-report <inventory>") (count: 1)))
   ("screen" (hash (description: "Descript existing screen") (usage: "screen <screen id>") (count: 1)))
   ("screens" (hash (description: "List all Screenboards") (usage: "screens") (count: 0)))
   ("dashes" (hash (description: "List all TimeBoards") (usage: "dashes") (count: 0)))
   ("search" (hash (description: "list all metrics or hosts matching pattern") (usage: "search <pattern>") (count: 1)))
   ("sproc" (hash (description: "Hosts Process Search.") (usage: "sproc <process pattern>") (count: 1)))
   ("start-dd-web" (hash (description: "Start web server mode")(usage: "start-dd-web") (count: 1)))
   ("status" (hash (description: "Get Datadog Status.") (usage: "status") (count: 0)))
   ("stories" (hash (description: "stories") (usage: "stories") (count: 0)))
   ("tag" (hash (description: "Add a tag to a hostname") (usage: "tag <host> <tag name:value>") (count: 2)))
   ("tag-all" (hash (description: "Add a tag to all hostnames matching pattern") (usage: "tag-all <hostname pattern> <tag name:value>") (count: 2)))
   ("tags" (hash (description: "list all tags and hosts") (usage: "tags") (count: 0)))
   ("tboard-add-chart" (hash (description: "Add a chart to a timeboard.") (usage: "tboard-add-chart <timeboard id> <title> <request> <viz>")(count: 4)))
   ("tboard-create" (hash (description: "Create a new timeboard") (usage: "tboard-create <title> <description>") (count: 2)))
   ("tboard-fancy" (hash (description: "Add charts for metrics which apply to tag provided.") (usage: "tboard-fancy <timeboard id> <metric pattern ^$ supported> <tag clause. key:value> <replace all charts? ? t or f>")(count: 4)))
   ("tboard-mass-add" (hash (description: "Add charts matching regexp metrics to a timeboard.") (usage: "tboard-add-chart <timeboard id> <pattern> <host clause> <Group by clause> <replace all charts? ? t or f>")(count: 5)))
   ("tboard-mass-add-many" (hash (description: "Add charts matching regexp metrics to a timeboard. Useful for multiple hosts.") (usage: "tboard-add-chart <timeboard id> <pattern> <host clause 'host1 host2 host3'> <replace all charts? t/f>  ")(count: 4)))
   ("totals" (hash (description: "Host Totals.") (usage: "totals") (count: 0)))
   ("verify-account" (hash (description: "Verify account credentials") (usage: "validate") (count: 0)))
   ("verify-apps" (hash (description: "Verify account credentials") (usage: "verify-apps") (count: 0)))
   ("view-md" (hash (description: "Describe metric metadata") (usage: "view-md") (count: 1)))
   )

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
		    ems
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
		    tboard-fancy
		    tboard-mass-add
		    tboard-mass-add-many
		    totals
		    verify-account
		    verify-apps
		    view-md
   )



(def (process-args cmd opt)
  (let-hash opt
    (case cmd
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
      ems
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
      tboard-fancy
      tboard-mass-add
      tboard-mass-add-many
      totals
      verify-account
      verify-apps
      view-md
