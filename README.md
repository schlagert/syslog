syslog
======

A Syslog based logger for error_logger reports. This project is strongly
inspired by the [sasl_syslog](http://github.com/travelping/sasl_syslog) project
Which in fact delivers quite similar functionality.

The main differences between `sasl_syslog` and `syslog` are the RFC3164
(BSD Syslog) compliant protocol backend as well as some features that were
borrowed from the [lager](http://github.com/basho/lager) project, e.g. having
the possibility to log into separate files for `info` and `error` or `progress`
reports.

Features
--------

* Write standard error_logger messages/reports using the Syslog protocol without
  the need of drivers, ports or NIFs.
* Send messages according to RFC3164 (BSD Syslog) or RFC5424.
* Robust event handler by using a supervised event handler subscription.
* Optionally write messages with severity `critical` or `error` into a separate
  facility.
* Get the well-known event format for `supervisor` and `crash` reports.

Configuration
-------------

The following configuration options are available and can be configured in the
application environment:

* `{enabled, boolean()}`
  Indicating whether Syslog reporting should be started when the `syslog`
  application gets started. Default is `true`.
* `{protocol, rfc3164 | rfc5424}`
  Specifies which protocol standard should be used to format outgoing Syslog
  packets. Default is `rfc3164`.
* `{use_rfc5424_bom, boolean()}`
  Specifies whether the RFC5424 protocol backend should include the UTF-8 BOM in
  the message part of a Syslog packet. Default is `false'.
* `{dest_host, inet:ip_address() | inet:hostname()}`
  Specifies the host to which Syslog packets will be sent. Default is
  `{127, 0, 0, 1}`.
* `{dest_port, auto | inet:port_number()}`
  Specifies the port to which Syslog packets will be sent. Default is `auto`.
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
attach any TTY handlers to `error_logger`:
```erlang
{sasl, [{sasl_error_logger, false}]}
```
