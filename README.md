syslog
======

A Syslog based logging framework for erlang. This project is inspired by the
two great work put in the two projects
[sasl_syslog](http://github.com/travelping/sasl_syslog) and
[lager](http://github.com/basho/lager). In fact `syslog` delivers quite
similar functionality.

The main difference between `sasl_syslog` and `syslog` is that `sasl_syslog`
does only provide logging of `error_logger` reports. However, the `error_logger`
is known for its bad memory consumption behaviour under heavy load (due to its
asynchronous logging mechanism). Additionally, `syslog` provides an optional
RFC 3164 (BSD Syslog) compliant protocol backend which is the only standard
supported by old versions of e.g. `syslog-ng` or `rsyslog`.

On the other side, compared to `lager`, the `syslog` application has a very
limited feature set. As its name infers `syslog` is specialized on delivering
its messages using Syslog only, there are no file or console backends with
custom-written and configurable log rotation or line formatting. However,
`syslog` does not rely on port drivers or NIFs to implement the Syslog protocol
and it includes several robustness features comparable to `lager`.

In a nutshell `syslog` can be seen as a lightweight logging framework using
available Syslog daemons/servers.

* [Code](http://github.com/schlagert/syslog)
* [EDoc](http://schlagert.github.com/syslog)

Features
--------

* Log messages and standard `error_logger` reports according to RFC 3164
  (BSD Syslog) or RFC 5424 (Syslog Protocol) without the need of drivers, ports
  or NIFs.
* Robust event handlers by using a supervised event handler subscription.
* Optionally separate error messages using a separate facility.
* Get the well-known SASL event format for `supervisor` and `crash` reports.
* Configurable verbosity of SASL printing format (printing depth is also
  configurable).

Planned
-------

* Configurable maximum packet size.
* Utilize the RFC 5424 _STRUCTURED-DATA_ field for `info_report`,
  `warning_report` or `error_report` with `proplists`.

Configuration
-------------

The `syslog` application already comes with sensible defaults (except for
example the used facilites and the destination host). However, many things can
be cusomized if desired. For this purpose the following configuration options
are available and can be configured in the application environment:

* `{msg_queue_limit, Limit :: pos_integer() | infinity}`

  Specifies the number of entries in the `error_logger` message queue to which
  events are processed. If the message queue size exceeds this limit messages
  `syslog` will _drop_ the amount of events exceeding the limit. Default is
  `infinity`.

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

* `{facility, syslog:facility()}`

  Specifies the facility Syslog packets will be sent with. Default is `daemon`.

* `{error_facility, syslog:facility()}`

  Specifies the facility Syslog packets with severity `error`, `critical`,
  `alert` or `emergency` will be sent with. Default is `daemon`.

* `{verbose, true | {false, Depth :: pos_integer()}}`

  Configures which pretty printing mode will be used when formatting
  `error_logger` reports (e.g. progress reports, not format messages). If
  verbose is `true` the `~p` format character will be used when formatting
  terms. This will likely result in many multiline strings. If set to
  `{false, Depth}` the `~P` format character will be used along with the
  specified printing depth. Default is `true`.

* `{no_progress, boolean()}`

  This flag can be used to completely omit progress reports from the logging
  output. So if you you don't care when a new process gets started set this to
  `true`. Default is `false`.

The `syslog` application will disable the standard `error_logger` TTY output on
application startup. This has nothing to do with the standard SASL logging. It
only disables non-SASL logging via, e.g. `error_logger:info_msg/1,2`. This
standard logging can be re-enabled at any time using the following:
```erlang
error_logger:tty(true).
```

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
`error_logger` API. However, this should not be used for ordinary logging.
This should be done via the API functions provided by the `syslog` module
which provides functions similar to the ones provided by the `error_logger`
module (see the `*msg/1,2` functions).

Supervision
-----------

<img src="http://schlagert.github.com/syslog/syslog.svg" width="50%" alt="syslog supervision"/>

For curiosity, the above illustration shows the very simple supervision
hierarchy used by the `syslog` application.
