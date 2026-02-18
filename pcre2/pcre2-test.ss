;;; pcre2/pcre2-test.ss — Test suite for gerbil-pcre high-level API

(import :std/test
        :gerbil/gambit
        :gerbil-pcre/pcre2/pcre2)

(export pcre2-test)

(def pcre2-test
  (test-suite "gerbil-pcre"

    ;; -----------------------------------------------------------------
    (test-suite "compilation"

      (test-case "pcre2-compile returns pcre-regex"
        (def rx (pcre2-compile "hello"))
        (check (pcre-regex? rx) ? values))

      (test-case "pcre2-regex creates compiled regex with options"
        (def a (pcre2-regex "world"))
        (def b (pcre2-regex "world" caseless: #t))
        (check (pcre-regex? a) ? values)
        (check (pcre-regex? b) ? values))

      (test-case "pcre2-compile raises on bad pattern"
        (check-exception (pcre2-compile "[invalid") PCRE2Error?))

      (test-case "compile with caseless option"
        (def rx (pcre2-regex "HELLO" caseless: #t))
        (check (pcre-regex? rx) ? values)
        (check (pcre2-matches? rx "hello world") ? values))

      (test-case "compile with multiline option"
        (def rx (pcre2-regex "^line" multiline: #t))
        (check (pcre-regex? rx) ? values)))

    ;; -----------------------------------------------------------------
    (test-suite "matching"

      (test-case "pcre2-match returns pcre-match on success"
        (def rx (pcre2-compile "hel+o"))
        (def m  (pcre2-match rx "hello"))
        (check (pcre-match? m) ? values))

      (test-case "pcre2-match returns #f on no match"
        (def rx (pcre2-compile "xyz"))
        (check (pcre2-match rx "hello world") => #f))

      (test-case "pcre2-search finds match with offset"
        (def rx (pcre2-compile "\\d+"))
        (def m  (pcre2-search rx "abc 42 def"))
        (check (pcre-match? m) ? values)
        (check (pcre-match-group m 0) => "42"))

      (test-case "pcre2-matches? returns bool"
        (def rx (pcre2-compile "^\\d+$"))
        (check (pcre2-matches? rx "12345") ? values)
        (check (pcre2-matches? rx "123x5") => #f))

      (test-case "full match group 0 is whole match"
        (def rx (pcre2-compile "f(o+)"))
        (def m  (pcre2-search rx "foobar"))
        (check (pcre-match-group m 0) => "foo")
        (check (pcre-match-group m 1) => "oo"))

      (test-case "unmatched optional group returns #f"
        (def rx (pcre2-compile "(a)?(b)"))
        (def m  (pcre2-match rx "b"))
        (check (pcre-match-group m 1) => #f)
        (check (pcre-match-group m 2) => "b"))

      (test-case "named capture groups"
        (def rx (pcre2-compile "(?P<year>\\d{4})-(?P<month>\\d{2})"))
        (def m  (pcre2-search rx "date: 2024-03"))
        (check (pcre-match-named m "year")  => "2024")
        (check (pcre-match-named m "month") => "03"))

      (test-case "pcre-match->list returns list of strings"
        (def rx (pcre2-compile "(\\w+)\\s+(\\w+)"))
        (def m  (pcre2-match rx "hello world"))
        (def lst (pcre-match->list m))
        (check (car lst)  => "hello world")
        (check (cadr lst) => "hello")
        (check (caddr lst) => "world"))

      (test-case "pcre-match->alist uses named groups"
        (def rx (pcre2-compile "(?P<a>\\w+) (?P<b>\\w+)"))
        (def m  (pcre2-match rx "foo bar"))
        (def al (pcre-match->alist m))
        (check (assoc "a" al) => '("a" . "foo"))
        (check (assoc "b" al) => '("b" . "bar")))

      (test-case "pcre-match-positions returns start/end pair"
        (def rx (pcre2-compile "(\\d+)"))
        (def m  (pcre2-search rx "abc 99 def"))
        (def pos (pcre-match-positions m 0))
        (check (pair? pos) ? values)
        (check (car pos) => 4)
        (check (cdr pos) => 6)))

    ;; -----------------------------------------------------------------
    (test-suite "substitution"

      (test-case "pcre2-replace replaces first match"
        (def rx  (pcre2-compile "o+"))
        (check (pcre2-replace rx "foobar" "0") => "f0bar"))

      (test-case "pcre2-replace-all replaces all matches"
        (def rx  (pcre2-compile "o"))
        (check (pcre2-replace-all rx "foobar" "0") => "f00bar"))

      (test-case "pcre2-replace with backreference"
        (def rx  (pcre2-compile "(\\w+)"))
        (check (pcre2-replace rx "hello world" "[$1]" extended: #t) => "[hello] world"))

      (test-case "pcre2-replace no match returns original"
        (def rx (pcre2-compile "xyz"))
        (check (pcre2-replace rx "hello" "Y") => "hello")))

    ;; -----------------------------------------------------------------
    (test-suite "iteration"

      (test-case "pcre2-find-all returns list of pcre-match objects"
        (def rx (pcre2-compile "\\d+"))
        (def all (pcre2-find-all rx "1 two 3 four 5"))
        (check (length all) => 3)
        (check (pcre-match? (car all)) ? values)
        (check (pcre-match-group (car all) 0) => "1")
        (check (pcre-match-group (cadr all) 0) => "3")
        (check (pcre-match-group (caddr all) 0) => "5"))

      (test-case "pcre2-find-all with no matches returns empty"
        (def rx (pcre2-compile "\\d+"))
        (check (pcre2-find-all rx "no digits here") => '()))

      (test-case "pcre2-extract returns list of matching strings"
        (def rx (pcre2-compile "\\d+"))
        (def extracted (pcre2-extract rx "a1 b22 c333"))
        (check extracted => '("1" "22" "333")))

      (test-case "pcre2-fold accumulates over matches"
        (def rx (pcre2-compile "\\d+"))
        (def sum (pcre2-fold rx
                             (lambda (m acc)
                               (+ acc (string->number (pcre-match-group m 0))))
                             0
                             "1 plus 2 plus 3"))
        (check sum => 6))

      (test-case "pcre2-split splits on delimiter"
        (def rx (pcre2-compile ",\\s*"))
        (check (pcre2-split rx "a, b, c") => '("a" "b" "c")))

      (test-case "pcre2-split with no match returns singleton"
        (def rx (pcre2-compile "X"))
        (check (pcre2-split rx "hello") => '("hello")))

      (test-case "pcre2-split handles zero-length matches"
        (check (pcre2-split "(?=\\d)" "a1b2c3") => '("a" "1b" "2c" "3")))

      (test-case "pcre2-partition returns alternating non-match/match list"
        (def rx (pcre2-compile "\\d+"))
        ;; Returns: (pre1 match1 pre2 match2 ... tail)
        (check (pcre2-partition rx "a1b2c3") => '("a" "1" "b" "2" "c" "3" ""))))

    ;; -----------------------------------------------------------------
    (test-suite "utilities"

      (test-case "pcre2-quote escapes metacharacters"
        (def q (pcre2-quote "foo.bar(baz)?"))
        (def rx (pcre2-compile q))
        (check (pcre2-matches? rx "foo.bar(baz)?") ? values)
        ;; Should not match unescaped version
        (check (pcre2-matches? rx "fooXbarYbazZ") => #f)))

    ;; -----------------------------------------------------------------
    (test-suite "pregexp-compatible API"

      (test-case "pcre2-pregexp-match like pregexp-match"
        (def m (pcre2-pregexp-match "(\\w+)" "hello world"))
        (check (list? m) ? values)
        (check (car m) => "hello"))

      (test-case "pcre2-pregexp-match returns #f on no match"
        (check (pcre2-pregexp-match "\\d+" "no numbers") => #f))

      (test-case "pcre2-pregexp-match with start/end bounds"
        (def m (pcre2-pregexp-match "\\w+" "abc def" 4))
        (check (car m) => "def"))

      (test-case "pcre2-pregexp-match-positions returns spans"
        (def pos (pcre2-pregexp-match-positions "(\\d+)" "abc 42 def"))
        (check (list? pos) ? values)
        (check (car pos) => '(4 . 6))
        (check (cadr pos) => '(4 . 6)))

      (test-case "pcre2-pregexp-replace replaces first"
        (check (pcre2-pregexp-replace "o+" "foobar" "0") => "f0bar"))

      (test-case "pcre2-pregexp-replace* replaces all"
        (check (pcre2-pregexp-replace* "o" "foobar" "0") => "f00bar"))

      (test-case "pcre2-pregexp-quote escapes metacharacters"
        (check (string? (pcre2-pregexp-quote "a.b[c]")) ? values)))))
