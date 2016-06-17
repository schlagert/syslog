syslog
======

[![Build Status](https://travis-ci.org/schlagert/syslog.png?branch=master)](https://travis-ci.org/schlagert/syslog)

A Syslog based logging framework for Erlang. This project is inspired by the
great work put in the two projects
[sasl_syslog](http://github.com/travelping/sasl_syslog) and
[lager](http://github.com/basho/lager). In fact `syslog` tries to combine both
approaches. In a nutshell `syslog` can be seen as a lightweight version of the
`lager` logging framework supporting only a fully compliant, Erlang-only Syslog
backend allowing remote logging.

The main difference between `sasl_syslog` and `syslog` is that `sasl_syslog`
does only provide logging of `error_logger` reports. However, the `error_logger`
is known for its bad memory consumption behaviour under heavy load (due to its
asynchronous logging mechanism). Additionally, `syslog` provides an optional
RFC 3164 (BSD Syslog) compliant protocol backend which is the only standard
supported by old versions of `syslog-ng` and `rsyslog`.

Compared to `lager`, `syslog` has a very limited set of backends. As its name
implies `syslog` is specialized in delivering its messages using Syslog only,
there is no file or console backend, no custom-written and configurable log
rotation, no line formatting and no tracing support. However, `syslog` does not
rely on port drivers or NIFs to implement the Syslog protocol and it includes
most of the beloved features known from `lager`, for example sync/async
logging and supervised event handler registration.

Features
--------

* Log messages and standard `error_logger` reports according to RFC 3164
  (BSD Syslog) or RFC 5424 (Syslog Protocol) without the need for drivers,
  ports or NIFs.
* System independent logging to local or remote facilities using UDP.
* Robust event handlers - using supervised event handler subscription.
* Optionally independent error messages using a separate facility.
* Get the well-known SASL event format for `supervisor` and `crash` reports.
* Configurable verbosity of SASL printing format (printing depth is also
  configurable).
* Throughput optimization by dynamically switching from synchronous to
  asynchronous mode.

Planned
-------

* Configurable maximum packet size.
* Utilize the RFC 5424 _STRUCTURED-DATA_ field for `info_report`,
  `warning_report` or `error_report` with `proplists`.

Configuration
-------------

The `syslog` application already comes with sensible defaults (except for
the facilities used and the destination host). However, many things can be
customized if desired. For this purpose the following configuration options
are available and can be configured in the application environment:

* `{msg_queue_limit, Limit :: pos_integer() | infinity}`

  Specifies a limit for the number of entries allowed in the `error_logger`
  message queue. If the message queue exceeds this limit `syslog` will
  __drop the events exceeding the limit__. Default is `infinity`.

* `{protocol, rfc3164 | rfc5424}`

  Specifies which protocol standard should be used to format outgoing Syslog
  packets. Default is `rfc3164`.

* `{use_rfc5424_bom, boolean()}`

  Specifies whether the RFC5424 protocol backend should include the UTF-8 BOM
  in the message part of a Syslog packet. Default is `false`.

* `{dest_host, inet:ip_address() | inet:hostname()}`

  Specifies the host to which Syslog packets will be sent. Default is
  `{127, 0, 0, 1}`.

* `{dest_port, inet:port_number()}`

  Specifies the port to which Syslog packets will be sent. Default is `514`.

* `{facility, syslog:facility()}`

  Specifies the facility Syslog packets will be sent with. Default is `daemon`.

* `{crash_facility, syslog:facility()}`

  Specifies the facility Syslog packets with severity `crash`, will be sent
  with. It __replaces__ the previous `error_facility` property. This accompanies
  __a change in behaviour__. Starting with release `2.0.0` error messages will
  also be sent with `facility`. Only crash and supervisor reports will be sent
  to this (maybe) separate facility. If the values of the properties `facility`
  and `crash_facility` differ a short one-line summary will additionally be sent
  to `facility`. Default is `daemon`.

* `{verbose, true | {false, Depth :: pos_integer()}}`

  Configures which pretty printing mode to use when formatting `error_logger`
  reports (that is progress reports, not format messages). If verbose is
  `true` the `~p` format character will be used when formatting terms. This
  will likely result in a lot of multiline strings. If set to `{false, Depth}`
  the `~P` format character is used along with the specified printing depth.
  Default is `true`.

* `{no_progress, boolean()}`

  This flag can be used to completely omit progress reports from the log
  output. So if you you don't care when a new process is started, set this
  flag to `true`. Default is `false`.

* `{async_limit, pos_integer() | undefined}`:

  Specifies the number of entries in the `syslog_logger` message queue to which
  asynchronous logging is allowed. As long as the message queue does not exceed
  this limit every logging statement will by asynchronous. If the message queue
  length exceeds this limit all logging statements will be synchronous, blocking
  the calling process until the logging request was processed. Default is `30`.
  
* `{pool_conf, proplist()}`:

  Specifies the configuration of udp pool senders. `pool_size` - number of workers
  in a pool. `max_overflow` - number of workers to be created when base workers are
  busy. `overflow_ttl` - number of milliseconds to keep created overflow workers,
  `overflow_check_period` - period in milliseconds to check for overflow workers ttl.
  By default pool_conf is [], which means 5 pool_size with 10 overflow workers, which 
  will be created on report and destroyed immediately.
  
* `{no_queue, boolean()}` :

  If no queue is specified, `syslog_logger_h` will drop every report, while pool is busy,
  and no more overflow processes available. Default is `false`. Recommended to use with
  high `max_overflow` value.


If your application really needs fast asynchronous logging and doesn't care
about lost log messages, logging should be done using the `error_logger` API and
the `syslog` application should be configured so that
`async_limit > msg_queue_limit`. This should prevent `syslog` from switching to
synchronous mode as well as limiting the `error_logger` message queue to a
reasonable size.  

Or switching to synchronous loging can be switched off by setting `async_limit` 
as `undefined`.  

The `syslog` application will disable the standard `error_logger` TTY output on
application startup. This has nothing to do with the standard SASL logging. It
only disables non-SASL logging via, for example `error_logger:info_msg/1,2`.
This kind of standard logging can be re-enabled at any time using the following:
```erlang
error_logger:tty(true).
```

The `syslog` application will not touch the standard SASL report handlers
attached to the `error_logger` when SASL starts. However, having SASL progress
reports on TTY can be quite annoying when trying to use the shell. The correct
way to disable this output is to configure the SASL application in the
`sys.config` of a release, for example the following line will instruct SASL
not to attach any TTY handlers to the `error_logger`:
```erlang
{sasl, [{sasl_error_logger, false}]}
```

API
---

The `syslog` application will log everything that is logged using the standard
`error_logger` API. However, __this should not be used for ordinary application
logging__.

The proper way to add logging to your application is to use the API functions
provided by the `syslog` module. These functions are similar to the ones
provided by the `error_logger` module and should feel familiar (see the
`*msg/1,2` functions).

Performance
-----------

Performance profiling has been made with a small script located in the
`benchmark` subdirectory. The figures below show the results of
`benchmark.escript all 100 10000` on an Intel(R) Core(TM)2 Duo CPU running R16B.

The above line starts a benchmark that will spawn 100 processes that each send
log message, using a specific logging framework, in a tight loop for 10000ms.
All log messages will be delivered over UDP (faked remote Syslog) to a socket
opened by the benchmark process. The total duration is the time it took to spawn
the processes, send the messages __and__ the time it took to receive all sent
messages at the socket that the benchmark process listens on.

<img src="https://cloud.githubusercontent.com/assets/404313/12110992/20d2254c-b392-11e5-83dc-64cc59bd7ad6.png" alt="benchmark results" />

As expected `syslog` and `lager` are the top performers. The main reason why
they outperform `log4erl` is the dynamic toggling of synchronous/asynchronous
logging (`log4erl` uses synchronous logging only).

Since `sasl_syslog` uses the asynchronous `error_logger` the number of messages
sent is quite huge. However, it also takes a vast amount of time and memory to
process the long `error_logger` message queue. This is also responsible for the
low number of messages sent per second in total.

A word about the performance of `lager`. Fitting `lager` into the benchmark
was unfortunately a bit tricky since the benchmark needs to know when all
messages were processed. However, `lager_syslog` uses a C port driver calling
`vsyslog` and thus does not support remote syslog. So instead of testing the
`lager_syslog_backend` the benchmark uses the `lager_console_backend`, setting
itself as the receiver for I/O messages and forwards them to the UDP socket
mentioned earlier. This would in fact slow down `lager` a bit, which would
explain the slightly better performance of `syslog`.

History
-------

### Master

* Development, currently no difference to version 2.0.1.

### Version 2.0.1

* Fix event handler supervision. Due to a defective match pattern in
  `syslog_monitor` crashed handlers did not get re-added as expected/supposed.

### Version 2.0.0

* Replace the property `error_facility` with `crash_facility`. Refer to the
  explanation of `crash_facility` above to learn more about this behaviour
  change.
* Performance improvements (e.g. binary as internal message format)

### Version 1.0.0

* Provide discrete API for robust `error_logger` independent logging
* Automatic toggling of sync/async logging for better load protection (default)
* Various performance improvements (e.g. timestamps, process name resolution)
* Configurable verbosity of progress report logging
* Support for release upgrades

### Version 0.0.9

* Supervised `error_logger` integration
* Message queue length based load protection
* RFC 3164 compliant backend (default)
* RFC 5424 compliant backend
* Support for local and remote facilities using UDP
* Separate facility for error messages (default off)
* Standard SASL event format for `supervisor` and `crash` reports

Supervision
-----------

<img src="https://cloud.githubusercontent.com/assets/404313/12110956/c59eec28-b391-11e5-936d-4e236f702ef0.png" alt="syslog supervision" />

For the curious; the above illustration shows the very simple supervision
hierarchy used by the `syslog` application.
