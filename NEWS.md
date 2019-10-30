gtplib
======

Erlang library for encoding and decoding GTPv1, GTPv2 and GTP' frames.

Version 1.3.1 - 30 Oct 2019
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
