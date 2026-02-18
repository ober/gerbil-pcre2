#!/usr/bin/env gxi
(import :std/build-script
        :std/make)

(defbuild-script
  `((gxc: "pcre2/libpcre2"
          "-cc-options" ,(cppflags "libpcre2-8" "")
          "-ld-options" ,(ldflags "libpcre2-8" "-lpcre2-8"))
    "pcre2/pcre2"))
