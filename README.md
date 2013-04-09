syslog
======

A Syslog based logger for `error_logger` reports. This project is strongly
inspired by the [sasl_syslog](http://github.com/travelping/sasl_syslog) project
Which in fact delivers quite similar functionality.

The main differences between `sasl_syslog` and `syslog` are the RFC 3164
(BSD Syslog) compliant protocol backend as well as some features that were
borrowed from the [lager](http://github.com/basho/lager) project, e.g. having
the possibility to log into separate files for `info` and `error` or `progress`
reports.

* [Code](http://github.com/schlagert/syslog)
* [EDoc](http://schlagert.github.com/syslog)

Features
--------

* Write standard `error_logger` messages/reports using the Syslog protocol
  without the need of drivers, ports or NIFs.
* Send messages according to RFC 3164 (BSD Syslog) or RFC 5424 (Syslog Protocol).
* Robust event handler by using a supervised event handler subscription.
* Optionally write messages with severity `critical` or `error` into a separate
  facility.
* No load on the `application_controller` ETS table (do not query the
  application environment when constructing a message).
* Get the well-known SASL event format for `supervisor` and `crash` reports.

Planned
-------

* Configurable maximum packet size.
* Configurable short/verbose printing format for progress reports.
* Utilize the RFC 5424 _STRUCTURED-DATA_ field for `info_report`,
  `warning_report` or `error_report` with `proplists`.

Configuration
-------------

The following configuration options are available and can be configured in the
application environment:

* `{enabled, boolean()}`

  Indicating whether Syslog reporting should be started when the `syslog`
  application gets started. Default is `true`.

* `{msg_queue_limit, pos_integer()}`

  Specifies the number of entries in the `error_logger` message queue to which
  `info_msg` and `info_report` events are processed. If the message queue size
  does exceed this limit, informational messages will be _dropped_ by the
  `syslog` application until the message queue recovers. However, error and
  warning messages/reports will always be processed. Default is `10000`.

* `{protocol, rfc3164 | rfc5424}`

  Specifies which protocol standard should be used to format outgoing Syslog
  packets. Default is `rfc3164`.

* `{use_rfc5424_bom, boolean()}`

  Specifies whether the RFC5424 protocol backend should include the UTF-8 BOM
  in the message part of a Syslog packet. Default is `false'.

* `{dest_host, inet:ip_address() | inet:hostname()}`

  Specifies the host to which Syslog packets will be sent. Default is
  `{127, 0, 0, 1}`.

* `{dest_port, inet:port_number()}`

  Specifies the port to which Syslog packets will be sent. Default is `514`.

* `{facility, facility()}`

  Specifies the facility Syslog packets will be sent with. Default is `daemon`.

* `{error_facility, facility()}`

  Specifies the facility Syslog packets with severity `error` or `critical` will
  be sent with. Default is `daemon`.

The `syslog` application will not touch the standard SASL report handlers
attached to the `error_logger` when SASL starts. However, having SASL progress
reports on TTY can be quite annoying when trying to use the shell. The correct
way to disable this output is to configure the SASL application in the
`sys.config` of a release, e.g. the following line will command SASL to not
attach any TTY handlers to the `error_logger`:
```erlang
{sasl, [{sasl_error_logger, false}]}
```

API
---

The `syslog` application will log everything logged with the standard
`error_logger` API. However, for convenience, `syslog` provides the two API
functions `syslog:log/2` and `syslog:log/3` to log messages directly with a
specific severity.

In some cases users may only want to include the application into their release
without enabling it by default. This can be achieved by disabling the
application in the release's `sys.config` file as followed:
```erlang
{syslog, [{enabled, false}]}
```

Syslog logging can then be switched on and off using the `syslog:enable/0`
and `syslog:disable/0` API functions.
