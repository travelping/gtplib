gtplib
======
[![Build Status][travis badge]][travis]
[![Coverage Status][coveralls badge]][coveralls]
[![Erlang Versions][erlang version badge]][travis]

Erlang library for encoding and decoding GTPv1 and GTPv2 frames.
Erlang netlink wrapper for talking the Linux kernel GTP-U module
(devel version: https://github.com/RoadRunnr/osmo-ggsn,
 upstream:      http://git.osmocom.org/osmo-gtp-kernel/)

BUILDING
--------

Using tetrapak:

    # tetrapak build check

Using rebar:

    # rebar3 compile

<!-- Badges -->
[travis]: https://travis-ci.org/travelping/gtplib
[travis badge]: https://img.shields.io/travis/travelping/gtplib/master.svg?style=flat-square
[coveralls]: https://coveralls.io/github/travelping/gtplib
[coveralls badge]: https://img.shields.io/coveralls/travelping/gtplib/master.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R20.1%20to%21.0-blue.svg?style=flat-square
