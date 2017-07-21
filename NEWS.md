gtplib
======

Erlang library for encoding and decoding GTPv1 and GTPv2 frames.

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
