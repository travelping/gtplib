gtplib
======

Erlang library for encoding and decoding GTPv1, GTPv2 and GTP' frames.

Version 4.0.0 - 01 December 2022
--------------------------------

Major version changed to 4.0 due to incompatible API change in decoding of flags in information elements.

**Features** :rocket:
* [#42](https://github.com/travelping/gtplib/pull/42) add [OpenTelemetry](https://opentelemetry.io/) helpers
  to convert GTP messages into OpenTelemetry attributes.
* [#43](https://github.com/travelping/gtplib/pull/43) convert all GTP flag fields from lists to maps

**Features** :rocket:
* [#39](https://github.com/travelping/gtplib/pull/39) Exporting `ULI`, `RAI` encoding functions and decoding functions

Version 3.2.0 - 22 September 2022
---------------------------

**Features** :rocket:
* [#39](https://github.com/travelping/gtplib/pull/39) Exporting `ULI`, `RAI` encoding functions and decoding functions

Version 3.1.0 - 12 September 2022
---------------------------

**Features** :rocket:
* [#37](https://github.com/travelping/gtplib/pull/37) Exporting `ULI`/`RAI` decoding functions
* [#36](https://github.com/travelping/gtplib/pull/36) Add a plain binary value to the `v1` private extension
* [#34](https://github.com/travelping/gtplib/pull/34) Rework Indication flags
* [#33](https://github.com/travelping/gtplib/pull/33) `APN` Restriction

Version 3.0.0 - 9 July 2021
---------------------------

**Features** :rocket:
* [#31](https://github.com/travelping/gtplib/pull/31) `de/encode` location information into records

Version 2.1.0 - 3 June 2021
---------------------------

**Features** :rocket:
* [#28](https://github.com/travelping/gtplib/pull/28) Normalize `FQDNs` by lowercasing them in all `IEs`

Version 2.0.1 - 25 Mar 2021
---------------------------
**Bugfixes** :bug:
* [#26](https://github.com/travelping/gtplib/pull/26) Add `ppplib` to `gtplib.app.src`

Version 2.0.0 - 8 Mar 2021
---------------------------

**Dependencies** :gear:
* [#20](https://github.com/travelping/gtplib/pull/20) Start use `ppplib`
* [#21](https://github.com/travelping/gtplib/pull/21) Replace `erlando` to `cut` `1.0.3`

**Improvements** :bulb:
* [d78b096](https://github.com/travelping/gtplib/commit/d78b09662f4735c25aa7da1b33d6ba13a52dc06f) remove all reference to `lager`
* [#6](https://github.com/travelping/gtplib/pull/6) improve tests

**Features** :rocket:
* [c40dc18](https://github.com/travelping/gtplib/commit/c40dc183f5c44d72f35e1d5622e33cf3e9342903) implement `GTPv1` Evolved `ARP` and A`PN-AMBR` IEs
* [#9](https://github.com/travelping/gtplib/pull/9) switch `travis-ci` from `.org` to `.com`
* [#10](https://github.com/travelping/gtplib/pull/10) add `R16` IEs
* [#12](https://github.com/travelping/gtplib/pull/12) rename duplicated enum
* [#15](https://github.com/travelping/gtplib/pull/15) Add `.github`
* [#19](https://github.com/travelping/gtplib/pull/19) Add hex to GH action

**Bugfixes** :bug:
* [f036d98](https://github.com/travelping/gtplib/commit/f036d98d2e01a1bde9f471d045cdb024adf80217) fix `de/encode` of Protocol Configuration Options (`PCO`)
* [d990d22](https://github.com/travelping/gtplib/commit/d990d22bbf8f209dfa8fa1f86b45bd18e6ea770b) fix `IMEI(SV)` in `GTPv2` encoding
* [#11](https://github.com/travelping/gtplib/pull/11) fix `GTPv2` flags
* [#13](https://github.com/travelping/gtplib/pull/13) fix wrong `GTPv1` cause codes
* [#14](https://github.com/travelping/gtplib/pull/14) `GTPv2`: fix minimum IE length for all flags IEs

Version 1.3.X -
---------------------------

* rename GTP' enum that duplicate GTPv1 enum

Version 1.3.0 - 01 Aug 2018
---------------------------

* add support for GTP' version 0 to 2
* support Erlang OTP 20.1 through 21.0
* drop support for OTP 19.x and 20.0
* removed tetrapak support

Version 1.2.0 - 21 Jul 2017
---------------------------

* support GTPv2 UE Time Zone message element
* enhanced test suite
* fixed G-PDU decoding
* split message header and message element decoding (API enhancement)
* remove kernel netlink support
* add support for GSM 09.60 version 6.1.0 Release 1997 PCOs

Version 1.1.0 - 21 Oct 2016
---------------------------

* work around for empty MS-ISDN IEs
* discard extra octets in variable length elements
* GTPv2 Non-IP PDP type
* de/encode message elements from/to maps (API change)

Version 1.0.0 - 07 Oct 2016
---------------------------

* Initial release
