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

TODO
