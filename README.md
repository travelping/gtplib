gtplib
======
[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Build Status][gh badge]][gh]
[![Coverage Status][coveralls badge]][coveralls]
[![Erlang Versions][erlang version badge]][gh]

Erlang library for encoding and decoding GTPv1, GTPv2 and GTP' frames.

BUILDING
--------

Using rebar:

    # rebar3 compile

# ERLANG Version Support

OTP 24.3 is current the minimum supported version. Older versions might work but are no longer tested.

When in doubt check the `matrix.otpe` setting in [.github/workflows/main.yml](.github/workflows/main.yml) for tested
versions.

<!-- Badges -->
[hexpm]: https://hex.pm/packages/gtplib
[hexpm version]: https://img.shields.io/hexpm/v/gtplib.svg?style=flat-square
[hexpm downloads]: https://img.shields.io/hexpm/dt/gtplib.svg?style=flat-square
[gh]: https://github.com/travelping/gtplib/actions/workflows/main.yml
[gh badge]: https://img.shields.io/github/workflow/status/travelping/gtplib/CI?style=flat-square
[coveralls]: https://coveralls.io/github/travelping/gtplib
[coveralls badge]: https://img.shields.io/coveralls/travelping/gtplib/master.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-24.2%20to%2025.1-blue.svg?style=flat-square
