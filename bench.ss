;;; bench.ss — Comprehensive regex benchmark: gerbil-pcre vs pregexp vs SRFI-115
;;;
;;; Build:  gerbil build
;;; Run:    .gerbil/bin/bench

(import :gerbil/gambit
        :std/format
        :std/srfi/1
        :std/srfi/115
        :std/pregexp
        :gerbil-pcre/pcre2/pcre2)

(export main)

;;; ---------------------------------------------------------------------------
;;; Timing infrastructure
;;; ---------------------------------------------------------------------------

;; Returns (wall-secs cpu-secs gc-secs gc-count) for running thunk N times.
(def (bench-run thunk iterations)
  (##gc)  ;; clean GC state
  (let ((s (##process-statistics)))
    (let loop ((i 0))
      (when (< i iterations)
        (thunk)
        (loop (+ i 1))))
    (let ((e (##process-statistics)))
      (values (- (f64vector-ref e 2) (f64vector-ref s 2))  ; wall
              (+ (- (f64vector-ref e 0) (f64vector-ref s 0))
                 (- (f64vector-ref e 1) (f64vector-ref s 1))) ; cpu (user+sys)
              (- (f64vector-ref e 5) (f64vector-ref s 5))  ; gc real
              (inexact->exact (- (f64vector-ref e 6) (f64vector-ref s 6)))))))  ; gc count

;; Auto-calibrate: find iteration count that makes wall time >= target-secs.
(def (calibrate thunk (target-secs 0.5))
  (let loop ((n 100))
    (let-values (((wall _cpu _gc _gcs) (bench-run thunk n)))
      (if (>= wall target-secs)
        n
        (loop (inexact->exact (ceiling (* n (/ target-secs (max wall 0.001))))))))))

;; Run a single benchmark: auto-calibrate, then time.
;; Returns (iterations wall cpu gc-time gc-count).
(def (bench name thunk)
  (let ((n (calibrate thunk)))
    (let-values (((wall cpu gc gcs) (bench-run thunk n)))
      (values n wall cpu gc gcs))))

;;; ---------------------------------------------------------------------------
;;; Output helpers
;;; ---------------------------------------------------------------------------

;; Fixed-point number formatter: (fmt-fixed 3.14159 1) => "3.1"
(def (fmt-fixed n decimals)
  (let* ((factor (expt 10 decimals))
         (rounded (inexact->exact (round (* (exact->inexact n) factor))))
         (neg? (< rounded 0))
         (abs-r (abs rounded))
         (int-part (quotient abs-r factor))
         (frac-part (remainder abs-r factor))
         (frac-str (number->string frac-part))
         (frac-padded (string-append
                       (make-string (max 0 (- decimals (string-length frac-str))) #\0)
                       frac-str)))
    (string-append (if neg? "-" "")
                   (number->string int-part) "." frac-padded)))

;; Pad string to width with trailing spaces
(def (pad-right s width)
  (let ((s (if (string? s) s (format "~a" s))))
    (if (>= (string-length s) width) s
      (string-append s (make-string (- width (string-length s)) #\space)))))

(def (header . cols)
  (displayln (make-string 78 #\-))
  (for-each (lambda (c) (display (pad-right c 14))) cols)
  (newline)
  (displayln (make-string 78 #\-)))

(def (row lib n wall cpu gc gcs)
  (let ((us/op (* (/ wall n) 1e6)))
    (display (pad-right lib 14))
    (display (pad-right (fmt-fixed us/op 1) 14))
    (display (pad-right (number->string n) 14))
    (display (pad-right (fmt-fixed wall 3) 14))
    (display (pad-right (fmt-fixed gc 3) 14))
    (display (pad-right (number->string gcs) 14))
    (newline)))

(def (section title)
  (newline)
  (displayln "=== " title " ===")
  (header "Library" "us/op" "Iters" "Wall(s)" "GC(s)" "GCs"))

(def (run-bench name thunk)
  (let-values (((n wall cpu gc gcs) (bench name thunk)))
    (row name n wall cpu gc gcs)))

;;; ---------------------------------------------------------------------------
;;; Test data
;;; ---------------------------------------------------------------------------

;; Short string
(def short-str "The quick brown fox jumps over 42 lazy dogs.")

;; Medium string — ~1KB of realistic log-like text
(def medium-str
  (let ((line "2024-03-15T10:23:45.678Z INFO  [worker-7] request_id=abc123 method=GET path=/api/v2/users status=200 duration=12ms bytes=4521\n"))
    (let loop ((i 0) (acc '()))
      (if (= i 10)
        (apply string-append (reverse acc))
        (loop (+ i 1) (cons line acc))))))

;; Long string — ~100KB
(def long-str
  (let loop ((i 0) (acc '()))
    (if (= i 100)
      (apply string-append (reverse acc))
      (loop (+ i 1) (cons medium-str acc)))))

;; UTF-8 string (Latin-1 safe — pcre2 char-string FFI can't handle >U+00FF)
(def utf8-str "Ünïcödé café résumé naïve Zürich Ça fait données françaises äöüß ")
(def utf8-long
  (let loop ((i 0) (acc '()))
    (if (= i 500)
      (apply string-append (reverse acc))
      (loop (+ i 1) (cons utf8-str acc)))))

;; String with many matches (for extract/find-all benchmarks)
(def many-numbers "x1 x22 x333 x4444 x55555 x666666 x7777777 x88888888 x999999999 ")
(def many-numbers-long
  (let loop ((i 0) (acc '()))
    (if (= i 200)
      (apply string-append (reverse acc))
      (loop (+ i 1) (cons many-numbers acc)))))

;; Email-like pattern test string
(def email-str "Contact alice@example.com or bob.smith@foo.co.uk for info. CC: charlie+tag@test.org")

;; Backtracking-heavy string
(def backtrack-str (string-append (make-string 30 #\a) "!"))

;;; ---------------------------------------------------------------------------
;;; Pre-compiled patterns
;;; ---------------------------------------------------------------------------

;; -- pcre2 --
(def pcre-digit+     (pcre2-compile "\\d+"))
(def pcre-word+      (pcre2-compile "\\w+"))
(def pcre-literal    (pcre2-compile "lazy"))
(def pcre-date       (pcre2-compile "(\\d{4})-(\\d{2})-(\\d{2})"))
(def pcre-email      (pcre2-compile "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"))
(def pcre-log-field  (pcre2-compile "(\\w+)=(\\S+)"))
(def pcre-comma      (pcre2-compile ",\\s*"))
(def pcre-backtrack  (pcre2-compile "a*a*a*a*a*b"))  ;; classic catastrophic backtracking pattern
(def pcre-dotstar    (pcre2-compile ".*foo"))
(def pcre-alt3       (pcre2-compile "GET|POST|DELETE"))
(def pcre-uri        (pcre2-compile "/api/v\\d+/\\w+"))

;; -- srfi-115 --
(def sre-digit+     (regexp '(+ digit)))
(def sre-word+      (regexp '(+ (or alphanumeric #\_))))
(def sre-literal    (regexp "lazy"))
(def sre-date       (regexp '(: (submatch (= 4 digit)) #\- (submatch (= 2 digit)) #\- (submatch (= 2 digit)))))
(def sre-email      (regexp '(: (+ (or alphanumeric #\. #\_ #\% #\+ #\-))
                                #\@
                                (+ (or alphanumeric #\. #\-))
                                #\.
                                (>= 2 alpha))))
(def sre-log-field  (regexp '(: (submatch (+ (or alphanumeric #\_)))
                                #\=
                                (submatch (+ (~ whitespace))))))
(def sre-comma      (regexp '(: #\, (* space))))
(def sre-backtrack  (regexp '(: (* #\a) (* #\a) (* #\a) (* #\a) (* #\a) #\b)))
(def sre-dotstar    (regexp '(: (* any) "foo")))
(def sre-alt3       (regexp '(or "GET" "POST" "DELETE")))
(def sre-uri        (regexp '(: "/api/v" (+ digit) "/" (+ (or alphanumeric #\_)))))

;; -- pregexp (uses string patterns; pregexp compiles lazily) --
(def pre-digit+     (pregexp "\\d+"))
(def pre-word+      (pregexp "\\w+"))
(def pre-literal    (pregexp "lazy"))
(def pre-date       (pregexp "(\\d{4})-(\\d{2})-(\\d{2})"))
(def pre-email      (pregexp "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"))
(def pre-log-field  (pregexp "(\\w+)=(\\S+)"))
(def pre-comma      (pregexp ",\\s*"))
(def pre-backtrack  (pregexp "a*a*a*a*a*b"))
(def pre-dotstar    (pregexp ".*foo"))
(def pre-alt3       (pregexp "GET|POST|DELETE"))
(def pre-uri        (pregexp "/api/v\\d+/\\w+"))

;;; ---------------------------------------------------------------------------
;;; Benchmarks
;;; ---------------------------------------------------------------------------

(def (main . args)

(displayln "")
(displayln "╔══════════════════════════════════════════════════════════════════════════════╗")
(displayln "║       Regex Benchmark: gerbil-pcre (PCRE2+JIT) vs pregexp vs SRFI-115      ║")
(displayln "╚══════════════════════════════════════════════════════════════════════════════╝")
(displayln "")
(displayln "  pcre2  = gerbil-pcre with PCRE2 + JIT")
(displayln "  pre    = :std/pregexp (pure Scheme)")
(displayln "  sre    = :std/srfi/115 (pure Scheme, SRE syntax)")
(displayln "")
(displayln "  us/op  = microseconds per operation (lower is better)")
(displayln "")
(displayln "String sizes:")
(displayln "  short  = " (string-length short-str) " chars")
(displayln "  medium = " (string-length medium-str) " chars")
(displayln "  long   = " (string-length long-str) " chars")
(displayln "  utf8   = " (string-length utf8-long) " chars")

;; --- 1. Pattern compilation ---
(section "1. Pattern Compilation")
(run-bench "pcre2"   (lambda () (pcre2-compile "\\d+")))
(run-bench "pregexp" (lambda () (pregexp "\\d+")))
(run-bench "srfi115" (lambda () (regexp '(+ digit))))

;; --- 2. Simple literal search (short string) ---
(section "2. Literal Search (short string)")
(run-bench "pcre2"   (lambda () (pcre2-search pcre-literal short-str)))
(run-bench "pregexp" (lambda () (pregexp-match pre-literal short-str)))
(run-bench "srfi115" (lambda () (regexp-search sre-literal short-str)))

;; --- 3. Boolean match test ---
(section "3. Boolean Match \\d+ (short string)")
(run-bench "pcre2"   (lambda () (pcre2-matches? pcre-digit+ "12345")))
(run-bench "pregexp" (lambda () (pregexp-match pre-digit+ "12345")))
(run-bench "srfi115" (lambda () (regexp-matches? sre-digit+ "12345")))

;; --- 4. Search \\d+ in medium string ---
(section "4. Search \\d+ (medium string, ~1KB)")
(run-bench "pcre2"   (lambda () (pcre2-search pcre-digit+ medium-str)))
(run-bench "pregexp" (lambda () (pregexp-match pre-digit+ medium-str)))
(run-bench "srfi115" (lambda () (regexp-search sre-digit+ medium-str)))

;; --- 5. Search \\d+ in long string ---
(section "5. Search \\d+ (long string, ~100KB)")
(run-bench "pcre2"   (lambda () (pcre2-search pcre-digit+ long-str)))
(run-bench "pregexp" (lambda () (pregexp-match pre-digit+ long-str)))
(run-bench "srfi115" (lambda () (regexp-search sre-digit+ long-str)))

;; --- 6. Capture groups — date pattern ---
(section "6. Capture Groups (date YYYY-MM-DD)")
(let ((date-str "Event on 2024-03-15 at noon"))
  (run-bench "pcre2"   (lambda () (pcre2-search pcre-date date-str)))
  (run-bench "pregexp" (lambda () (pregexp-match pre-date date-str)))
  (run-bench "srfi115" (lambda () (regexp-search sre-date date-str))))

;; --- 7. Extract all matches (\\d+) from string with many numbers ---
(section "7. Extract All \\d+ (~2000 matches)")
(run-bench "pcre2"   (lambda () (pcre2-extract pcre-digit+ many-numbers-long)))
(run-bench "pregexp" ;; pregexp has no extract-all, build from match-positions
  (lambda ()
    (let loop ((s many-numbers-long) (acc '()))
      (let ((m (pregexp-match-positions pre-digit+ s)))
        (if m
          (let ((end (cdar m)))
            (loop (substring s end (string-length s))
                  (cons (substring s (caar m) end) acc)))
          (reverse acc))))))
(run-bench "srfi115" (lambda () (regexp-extract sre-digit+ many-numbers-long)))

;; --- 8. Replace first ---
(section "8. Replace First (\\d+ → \"NUM\")")
(run-bench "pcre2"   (lambda () (pcre2-replace pcre-digit+ short-str "NUM")))
(run-bench "pregexp" (lambda () (pregexp-replace pre-digit+ short-str "NUM")))
(run-bench "srfi115" (lambda () (regexp-replace sre-digit+ short-str "NUM")))

;; --- 9. Replace all ---
(section "9. Replace All \\w+ → \"X\" (medium string)")
(run-bench "pcre2"   (lambda () (pcre2-replace-all pcre-word+ medium-str "X")))
(run-bench "pregexp" (lambda () (pregexp-replace* pre-word+ medium-str "X")))
(run-bench "srfi115" (lambda () (regexp-replace-all sre-word+ medium-str "X")))

;; --- 10. Split ---
(section "10. Split on comma")
(let ((csv "alpha, beta, gamma, delta, epsilon, zeta, eta, theta, iota, kappa"))
  (run-bench "pcre2"   (lambda () (pcre2-split pcre-comma csv)))
  (run-bench "pregexp" (lambda () (pregexp-split pre-comma csv)))
  (run-bench "srfi115" (lambda () (regexp-split sre-comma csv))))

;; --- 11. Email extraction ---
(section "11. Email Pattern Search")
(run-bench "pcre2"   (lambda () (pcre2-search pcre-email email-str)))
(run-bench "pregexp" (lambda () (pregexp-match pre-email email-str)))
(run-bench "srfi115" (lambda () (regexp-search sre-email email-str)))

;; --- 12. Alternation (GET|POST|DELETE in log line) ---
(section "12. Alternation (GET|POST|DELETE)")
(let ((log-line "2024-03-15T10:23:45Z DELETE /api/v2/users/123 HTTP/1.1"))
  (run-bench "pcre2"   (lambda () (pcre2-search pcre-alt3 log-line)))
  (run-bench "pregexp" (lambda () (pregexp-match pre-alt3 log-line)))
  (run-bench "srfi115" (lambda () (regexp-search sre-alt3 log-line))))

;; --- 13. URI pattern in long log ---
(section "13. URI Pattern /api/v\\d+/\\w+ (long string)")
(run-bench "pcre2"   (lambda () (pcre2-extract pcre-uri long-str)))
(run-bench "pregexp"
  (lambda ()
    (let loop ((s long-str) (acc '()))
      (let ((m (pregexp-match-positions pre-uri s)))
        (if m
          (let ((end (cdar m)))
            (loop (substring s end (string-length s))
                  (cons (substring s (caar m) end) acc)))
          (reverse acc))))))
(run-bench "srfi115" (lambda () (regexp-extract sre-uri long-str)))

;; --- 14. Catastrophic backtracking pattern ---
(section "14. Backtracking (a*a*a*a*a*b on \"aaa...a!\")")
(run-bench "pcre2"   (lambda () (pcre2-matches? pcre-backtrack backtrack-str)))
(run-bench "pregexp" (lambda () (pregexp-match pre-backtrack backtrack-str)))
(run-bench "srfi115" (lambda () (regexp-matches? sre-backtrack backtrack-str)))

;; --- 15. Greedy .* scan ---
(section "15. Greedy .* Scan (\".*foo\" on long string)")
(let ((str (string-append (make-string 10000 #\x) "foo")))
  (run-bench "pcre2"   (lambda () (pcre2-search pcre-dotstar str)))
  (run-bench "pregexp" (lambda () (pregexp-match pre-dotstar str)))
  (run-bench "srfi115" (lambda () (regexp-search sre-dotstar str))))

;; --- 16. UTF-8 search ---
(section "16. UTF-8 Search (\\w+ in ~36KB UTF-8 string)")
(run-bench "pcre2"   (lambda () (pcre2-search pcre-word+ utf8-long)))
(run-bench "pregexp" (lambda () (pregexp-match pre-word+ utf8-long)))
(run-bench "srfi115" (lambda () (regexp-search sre-word+ utf8-long)))

;; --- 17. UTF-8 extract all ---
(section "17. UTF-8 Extract All \\w+ (~36KB UTF-8 string)")
(run-bench "pcre2"   (lambda () (pcre2-extract pcre-word+ utf8-long)))
(run-bench "pregexp"
  (lambda ()
    (let loop ((s utf8-long) (acc '()))
      (let ((m (pregexp-match-positions pre-word+ s)))
        (if m
          (let ((end (cdar m)))
            (loop (substring s end (string-length s))
                  (cons (substring s (caar m) end) acc)))
          (reverse acc))))))
(run-bench "srfi115" (lambda () (regexp-extract sre-word+ utf8-long)))

;; --- 18. Key=Value extraction from log (capture groups, many matches) ---
(section "18. Key=Value Extraction (capture groups, medium string)")
(run-bench "pcre2"   (lambda () (pcre2-find-all pcre-log-field medium-str)))
(run-bench "pregexp"
  (lambda ()
    (let loop ((s medium-str) (acc '()))
      (let ((m (pregexp-match-positions pre-log-field s)))
        (if m
          (let ((end (cdar m)))
            (loop (substring s end (string-length s))
                  (cons m acc)))
          (reverse acc))))))
(run-bench "srfi115"
  (lambda ()
    (regexp-fold sre-log-field
                 (lambda (i m str acc) (cons m acc))
                 '() medium-str
                 (lambda (i m str acc) (reverse acc)))))

;; --- 19. No-match search in long string ---
(section "19. No-Match Search (pattern absent, long string)")
(let ((pcre-none (pcre2-compile "ZZZNOTFOUND999"))
      (pre-none  (pregexp "ZZZNOTFOUND999"))
      (sre-none  (regexp "ZZZNOTFOUND999")))
  (run-bench "pcre2"   (lambda () (pcre2-search pcre-none long-str)))
  (run-bench "pregexp" (lambda () (pregexp-match pre-none long-str)))
  (run-bench "srfi115" (lambda () (regexp-search sre-none long-str))))

;; --- 20. Fold/accumulate over all matches ---
(section "20. Fold — Sum All Numbers in Long String")
(run-bench "pcre2"
  (lambda ()
    (pcre2-fold pcre-digit+
                (lambda (m acc) (+ acc (string->number (pcre-match-group m 0))))
                0 long-str)))
(run-bench "pregexp"
  (lambda ()
    (let loop ((s long-str) (acc 0))
      (let ((m (pregexp-match-positions pre-digit+ s)))
        (if m
          (let ((start (caar m)) (end (cdar m)))
            (loop (substring s end (string-length s))
                  (+ acc (string->number (substring s start end)))))
          acc)))))
(run-bench "srfi115"
  (lambda ()
    (regexp-fold sre-digit+
                 (lambda (i m str acc)
                   (+ acc (string->number (regexp-match-submatch m 0))))
                 0 long-str
                 (lambda (i m str acc) acc))))

;; --- Summary ---
(newline)
(displayln (make-string 78 #\=))
(displayln "Benchmark complete.")
(newline)

) ;; end main
