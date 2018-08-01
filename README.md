syslog
======

[![Build Status](https://travis-ci.org/schlagert/syslog.png?branch=master)](https://travis-ci.org/schlagert/syslog)

A Syslog-based logging framework for Erlang. This project is inspired by the
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
measures to enhance the overall robustness of a node, e.g. load distribution,
back-pressure mechanisms, throughput optimization, etc.

Features
--------

* Log messages and standard `error_logger` reports formatted according to
  RFC 3164 (BSD Syslog) or RFC 5424 (Syslog Protocol) without the need for
  drivers, ports or NIFs.
* Support for sending RFC 5424 _STRUCTURED-DATA_.
* System independent logging to local or remote facilities using one of the
  following transports:
  * UDP (RFC 3164 and RFC 5426)
  * TCP (Octet Counting according to RFC 6587)
  * TCP/TLS (RFC 5425)
* Robust event handlers - using supervised event handler subscription.
* Optionally independent error messages using a separate facility.
* Get the well-known SASL event format for `supervisor` and `crash` reports.
* Configurable verbosity of SASL printing format (printing depth is also
  configurable).
* Load distribution between all concurrently logging processes by moving the
  message formatting into the calling process(es).
* Built-in `lager` backend to bridge between both frameworks.

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

* `{drop_percentage, Percentage :: 1..100}`

  Specifies the number of messages that will be dropped (additionally), if the
  `error_logger` message queue exceeds the configured `msg_queue_limit`. The
  `drop_percentage` is given as percentage (of the `msg_queue_limit`). E.g. if
  `drop_percentage` is `10` (the default), `msg_queue_limit` is `100` and the
  current length of the `error_logger` message queue is 120, then `20 + 10`
  messages will be dropped to give the `syslog_error_h` handler some air to
  catch up.

* `{protocol, rfc3164 | rfc5424 |
              {rfc3164 | rfc5424, tcp | udp} |
              {rfc3164 | rfc5424, udp, [gen_udp:option()]} |
              {rfc3164 | rfc5424, tcp, [gen_tcp:option()]} |
              {rfc5424, tls, [ssl:connect_option()]}}`

  Specifies which protocol/transport standard should be used to format and send
  outgoing Syslog packets. Please note that empty/default TLS/SSL options are
  currently not supported. Default formatting is `rfc3164` and the default
  transport is `udp`.

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

* `{app_name, atom() | string() | binary()}`

  Configured the value reported in the `APP-NAME` field of Syslog messages. If
  not set (the default), the name part of the node name will be used. If the
  node is not alive (not running in distributed mode) the string `beam` will be
  used.

* `{log_level, syslog:severity()}`

  Configures the minimal log level. All messages with a severity value smaller
  then the configured level will be discarded. Default is `debug` (discard
  nothing).

* `{async, boolean()}`

  Specifies whether log message offloading into the `syslog_logger` server
  is done synchronously or asynchronously. It is highly recommended to leave
  this at its default value `false` because asynchronous delivery is really
  dangerous. A simple log burst of a few thousand processes may be enough
  to take your node down (due to out-of-memory).

* `{timeout, pos_integer()}`

  Specifies an upper bound in milliseconds a single process gets blocked in
  a log call (only if `{async, false}`). This is useful if your application
  has performance restrictions (under high load) and you are willing to
  sacrifice safety for this reason. It's still safer to set this to a lower
  value than completely switching to `{async, true}`. Default is `1000`ms.

* `{syslog_error_logger, boolean()}`

  Specifies whether `syslog` will handle messages from the `error_logger`.
  Default is `true`.

* `{multiline_mode, boolean()}`

  Specifies whether `syslog` will send messages potentially containing multiline
  strings. Please be aware, that many Syslog servers will not be able to handle
  multiline messages well, e.g. will insert garbage characters for a newline
  character. Default is `false`.

If your application really needs fast asynchronous logging and you like to live
dangerously, logging can be done either with the `error_logger` or the `syslog`
API and the `syslog` application should be configured with `{async, true}` and
`{msg_queue_limit, infinity}`. This sets `syslog` into asynchronous delivery
mode and all message queues are allowed to grow indefinitely.

The `syslog` application will disable the standard `error_logger` TTY output on
application startup. This has nothing to do with the standard SASL logging. It
only disables non-SASL logging via, for example `error_logger:info_msg/1,2`.
This kind of standard logging can be re-enabled at any time using the following:
```erlang
error_logger:tty(true).
```
This behaviour can now be configured in the application environment. If you
don't want `syslog` to mess with your TTY settings use `{disable_tty, false}`.

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

To facilitate logging of _STRUCTURED-DATA_ the `syslog:msg/5` functions must be
used. This function allows passing a list of structured data elements that will
be formatted and sent to the remote receiver. However, no content checking will
be done. If there's unescaped or unallowed content in the provided structured
data elements invalid Syslog messages will be sent.

The proper way to add logging to your application is to use the API functions
provided by the `syslog` module. These functions are similar to the ones
provided by the `error_logger` module and should feel familiar (see the
`*msg/1,2` functions).

The `syslog` application comes with a built-in, optional backend for `lager`.
This is especially useful if your release has dependencies that require `lager`
although you wish to forward logging using `syslog` only. To forward `lager`
logging into `syslog` you can use something like the following in your
`sys.config`:
```erlang
{lager, [{handlers, [{syslog_lager_backend, []}]}]}
```

You can explicitly specify the logging level
```erlang
{lager, [{handlers, [{syslog_lager_backend, [info]}]}]}
```

It can handle more complex options such as below
```erlang
{lager, [
    {handlers, [
        {syslog_lager_backend, [
          debug,                                 %% Level
          {"sdata_id", [application, pid]},      %% STRUCTURED-DATA
          {lager_default_formatter, [message]}   %% Lager formatting
        ]}
    ]}
]}.
```
It will also handle custom formatters like other existing `lager` backends.

One additional feature this backend provides is the optional ability to store
the metadata from a `lager` message into the _STRUCTURED-DATA_ part of a
RFC 5424 message. It takes a tuple with first element being the
_STRUCTURED-DATA_ id, and the second element being a list of atoms
corresponding to the metadata key whose values you want to pack. If you don't
want to forward any of the metadata or are using RFC 3164 then you can supply
an empty tuple `{}`. Only metadata matching the configured keys will be included
into the _STRUCTURED-DATA_

Performance
-----------

Performance profiling has been made with a the benchmark script located in the
`benchmark` subdirectory. The figures below show the results of best-of-five
runs on an Intel(R) Core(TM) i7-4790 CPU running OTP 17.4.

The benchmark simulates a hard burst by spawning `N` processes that each send
log messages, using a specific logging framework, in a tight loop for _10000ms_.
All log messages will be delivered over UDP (faked remote Syslog) to a socket
opened by the benchmark itself. The total duration is the time it took to spawn
the processes, send the messages __and__ the time it took to receive all sent
messages at the socket that the benchmark process listens on. The benchmark has
two profiles. The `large` profiles uses log messages with a size of ~840bytes
and the `small` profile uses log messages with a size of ~40bytes.

All frameworks are used in their default configuration with their native logging
function (*not* `error_logger`).

To be able to compare `lager` with the other frameworks, the benchmark contains
a very simple backend, that forwards log messages formatted with the
`lager_default_formatter` using `gen_udp`, e.g. like `syslog` does it. The
`sasl_syslog` application is left out of scope, simply because it crashes the
Erlang VM on every run (due to its purely asynchronous logging).

Now let's see the numbers.

### Small Profile

For a relatively small number of senders, all frameworks have a pretty good
throughput and the completion time is almost equal. While memory consumption is
generally low, the `lager` application uses significant more memory. This is due
to the dynamic switching between synchronous and asynchronous message delivery:
When delivery starts in asynchronous mode many log messages pile up in the
internal `gen_event`'s message queue. When the framework finally switches to
synchronous mode the event manager is busy sending out these messages while
senders get blocked.

<img src="https://cloud.githubusercontent.com/assets/404313/16836745/5af8562e-49bf-11e6-8ba0-89a99d1a73f5.png" alt="small benchmark" />

Adding more processes to the party makes the above mentioned effect more
dramatic. While `log4erl` and `syslog` perform quite well by evening the
load on the internal event manager or server using synchronous logging, `lager`
piles up a hugh amount of messages in its internal `gen_event` message queue. It
takes the framework almost two minutes to process these messages.

When looking at the memory usage it can be observed that `lager`'s backend
throttling (which was also used in `syslog` up to this version) is effectively
useless to protect from bursts with small log messages. Switching is simply too
slow and once messages begin to pile up, the node keeps being busy sending these
messages. Additionally, senders may get blocked an indefinite amount of time
(remember that `gen_event:sync_notify/2` uses `infinity` for timeouts). During
the test it could be observed that some senders were blocked over 50 seconds!

### Large Profile

Numbers get interesting when large strings need to be formatted and copied
around. `log4erl` as well as `lager` do the actual message formatting inside
the internal event managers. Additionally, these frameworks pass around the
unprocessed format string and its argument list.

`syslog` on the other hand, is now able to score by offloading the formatting
work into the logging processes and by exchanging binaries with the internal
logging server.

<img src="https://cloud.githubusercontent.com/assets/404313/16836747/5f04b848-49bf-11e6-9c28-603eb66036f4.png" alt="large benchmark" />

Interestingly, the total duration is excellent for all frameworks. Most
probably, copying the large terms makes the logging processes slow enough that
`lager`'s backend throttling can kick in and prevent the internal message queue
from growing too much. However, it could be observed again that some senders
were blocked over 10 seconds.

TODO more details?

History
-------

### Master (3.4.3)

* Add support for process ids formatted as strings in `lager` metadata (thanks
  to @hairyhum)
* Add basic support for OTP 21. This will make `syslog` _work_ with the new
  `logger` API. But be aware that this is only a hack involving the start of the
  legacy `error_logger`. If you want to use this, *do not forget* to set the
  `kernel` environment variable `logger_sasl_compatible` to `true` in your
  release. Unfortunately, the unit tests don't work with OTP 21.

### 3.4.2

* Fix unicode logging (using `~ts` or `~tc`)

### 3.4.1

* Fix compatibility to OTP 20.1

### 3.4.0

* Due to a bug, this release is *not* compatible with OTP 20.1, use release 3.4.1
  instead.
* Allow certain configuration values to be set as string/binary. This especially
  applies to the `dest_host` and `dest_port` options, e.g. it is now possible to
  set addresses like "127.0.0.1" or "::1" as `dest_host`.
  (thanks to @ogolosovskiy)
* Support for optional `multiline` mode, which prevents `syslog` from splitting
  message strings at newline separators (thanks to @ogolosovskiy)

### 3.3.0

* Try to reopen the transport periodically on transport failures/errors (#11).
* Support for configurable conversion of `lager` metadata to _STRUCTURED-DATA_
  (thanks to @walrusVision)
* Support for custom formatters in the `lager` backend (thanks to @walrusVision)

### 3.2.0

* Add support for _STRUCTURED-DATA_ (RFC 5424) using `syslog:msg/5`. This lead
  to __a change of the internal formatter behaviour__. Custom implementations
  of the `syslog_logger` behaviour must be adapted to these changes.
* Error logger robustness has been improved, by giving the `error_logger`
  messages precedence over messages sent via the API.
* The internal `syslog_logger` handles transport errors (gracefully). This way
  crashes are avoided which in turn relieve the `error_logger` when the system
  is under high load already.

### 3.1.0

* Allow configuration of custom protocol backend modules (thanks to @rich)
* Export/move date formatting code into `syslog_lib` (thanks to @rich)
* Make TTY settings configurable in the application environment
* Make `error_logger` integration configurable

### 3.0.0

* `syslog` is now available on [hex.pm](https://hex.pm/packages/syslog_app)
  under the package name `syslog_app` (the name `syslog` is already assigned to
  another project)
* Make project compatible to rebar3
* Remove dynamic switching of log message delivery mode. Make mode explicitly
  configurable with the new `async` directive (which replaces the `async_limit`
  directive) and the `syslog:set_log_mode/1` API.
* Add `app_name` configuration directive to allow configuration of the
  `APP-NAME` field value (thanks to @comtihon).
* Change severity of messages sent by `error_logger:info_[msg|report]/1,2` and
  `syslog:info_msg/1,2` from `notice` to `informational` (thanks to @comtihon).
* Add `log_level` configuration directive. With this configuration it is
  possible to discard messages with undesired severity values (thanks to
  @comtihon).
* Add optional `lager` backend to forward messages from `lager` to `syslog`.
* Further improvement of robustness, especially when many processes try to
  log concurrently by moving the message formatting away from the main event
  manager into the logging processes.

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

<img src="https://cloud.githubusercontent.com/assets/404313/16836729/43c90a66-49bf-11e6-9ec5-d39451c25deb.png" alt="syslog supervision" />

For the curious; the above illustration shows the very simple supervision
hierarchy used by the `syslog` application.
