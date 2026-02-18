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
        (check (string? (pcre2-pregexp-quote "a.b[c]")) ? values)))

    ;; -----------------------------------------------------------------
    (test-suite "UTF-8 support"

      (test-case "compile pattern with non-ASCII"
        (def rx (pcre2-compile "(?=é)"))
        (check (pcre-regex? rx) ? values))

      (test-case "search in UTF-8 subject"
        (def rx (pcre2-compile "caf."))
        (def m  (pcre2-search rx "café"))
        (check (pcre-match? m) ? values)
        (check (pcre-match-group m 0) => "café"))

      (test-case "match with multi-byte chars returns correct positions"
        (def rx (pcre2-compile "(é)"))
        (def m  (pcre2-search rx "café"))
        (def pos (pcre-match-positions m 0))
        (check (car pos) => 3)   ; char index, not byte index
        (check (cdr pos) => 4))

      (test-case "named groups with UTF-8"
        (def rx (pcre2-compile "(?P<city>\\w+)\\s+(?P<country>\\w+)"))
        (def m  (pcre2-search rx "Zürich Schweiz"))
        (check (pcre-match-named m "city")    => "Zürich")
        (check (pcre-match-named m "country") => "Schweiz"))

      (test-case "find-all with multi-byte subject"
        (def rx (pcre2-compile "\\w+"))
        (def all (pcre2-extract rx "café résumé naïve"))
        (check all => '("café" "résumé" "naïve")))

      (test-case "replace in UTF-8 text"
        (check (pcre2-replace "é" "café" "ee") => "cafee"))

      (test-case "replace-all in UTF-8 text"
        (check (pcre2-replace-all "é" "résumé" "ee") => "reesumee"))

      (test-case "split on UTF-8 delimiter"
        (check (pcre2-split "·" "a·b·c") => '("a" "b" "c")))

      (test-case "partition with UTF-8"
        (def result (pcre2-partition "\\d+" "café1résumé2fin"))
        (check result => '("café" "1" "résumé" "2" "fin")))

      (test-case "pcre2-matches? with UTF-8"
        (check (pcre2-matches? "^café$" "café") ? values)
        (check (pcre2-matches? "^café$" "cafe") => #f))

      (test-case "zero-length match in UTF-8 subject"
        (def rx (pcre2-compile "(?=é)"))
        (def all (pcre2-find-all rx "aéb"))
        (check (length all) => 1)
        (check (pcre-match-positions (car all) 0) => '(1 . 1))))

    ;; -----------------------------------------------------------------
    (test-suite "error handling"

      (test-case "pcre2-error? recognizes PCRE2 errors"
        (check-exception (pcre2-compile "[bad") pcre2-error?))

      (test-case "pcre2-replace raises on bad extended replacement"
        (check-exception
          (pcre2-replace "(a)" "aaa" "${bad" extended: #t)
          PCRE2Error?))

      (test-case "pcre2-replace no-match returns original"
        (check (pcre2-replace "xyz" "hello" "Y") => "hello"))

      (test-case "pcre2-replace-all no-match returns original"
        (check (pcre2-replace-all "xyz" "hello" "Y") => "hello")))

    ;; -----------------------------------------------------------------
    (test-suite "resource management"

      (test-case "pcre2-release! releases without error"
        (def rx (pcre2-compile "test"))
        (pcre2-release! rx)
        ;; After release, the struct still exists but resources are freed
        (check (pcre-regex? rx) ? values)))

    ;; -----------------------------------------------------------------
    (test-suite "edge cases"

      (test-case "pcre2-split with limit"
        (check (pcre2-split "," "a,b,c,d" 2) => '("a" "b,c,d"))
        (check (pcre2-split "," "a,b,c,d" 3) => '("a" "b" "c,d")))

      (test-case "pcre2-search with start offset"
        (def rx (pcre2-compile "\\d+"))
        (def m  (pcre2-search rx "12 34 56" 3))
        (check (pcre-match-group m 0) => "34"))

      (test-case "string pattern auto-compilation via cache"
        ;; String patterns should be auto-compiled via ensure-regex
        (def m (pcre2-search "\\d+" "abc 42 def"))
        (check (pcre-match? m) ? values)
        (check (pcre-match-group m 0) => "42")
        ;; Same pattern re-used from cache
        (def m2 (pcre2-search "\\d+" "xyz 99"))
        (check (pcre-match-group m2 0) => "99"))

      (test-case "zero-length matches in fold"
        (def result (pcre2-extract "" "ab"))
        ;; Empty pattern matches at every position
        (check (length result) => 3)
        (check (car result) => ""))

      (test-case "pcre2-quote with UTF-8 metacharacters"
        ;; Test that non-ASCII chars pass through quote unchanged
        (def q (pcre2-quote "café[1]"))
        (check (pcre2-matches? q "café[1]") ? values)))))
