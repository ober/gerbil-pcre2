;;; pcre2/pcre2.ss — Layer 2: High-level idiomatic Gerbil API for PCRE2
;;;
;;; Import this module for all regex needs. The low-level FFI layer
;;; (pcre2/libpcre2.ss) is an implementation detail.

(import :gerbil/gambit
        :std/error
        :std/sugar
        :std/srfi/1
        ./libpcre2)

(export
  ;; Types
  pcre-regex? pcre-match?

  ;; Compilation
  pcre2-compile pcre2-regex

  ;; Matching
  pcre2-match pcre2-search pcre2-matches?

  ;; Match result access
  pcre-match-group pcre-match-named
  pcre-match-positions
  pcre-match->list pcre-match->alist

  ;; Substitution
  pcre2-replace pcre2-replace-all

  ;; Iteration
  pcre2-find-all pcre2-extract pcre2-fold
  pcre2-split pcre2-partition

  ;; Utilities
  pcre2-quote pcre2-release!

  ;; Error type
  pcre2-error? PCRE2Error?

  ;; Pregexp-compatible API
  pcre2-pregexp-match pcre2-pregexp-match-positions
  pcre2-pregexp-replace pcre2-pregexp-replace*
  pcre2-pregexp-quote)

;;;
;;; Error handling
;;;

(deferror-class (PCRE2Error Error) () pcre2-error?)

(def (raise-pcre2-error where errorcode)
  (raise (make-PCRE2Error
          (ffi-pcre2-get-error-message errorcode)
          where: where
          irritants: [errorcode])))

(def (raise-pcre2-compile-error where pattern errorcode offset)
  (raise (make-PCRE2Error
          (string-append (ffi-pcre2-get-error-message errorcode)
                         " at offset " (number->string offset)
                         " in pattern: " pattern)
          where: where
          irritants: [errorcode offset pattern])))

;;;
;;; Core struct types
;;;

;; Compiled regex — holds FFI pointer, match data, and metadata.
;; The span-vec field is internal; use pcre-match-group/positions accessors.
(defstruct pcre-regex
  (code          ; pcre2-code* (FFI pointer, GC-managed)
   match-data    ; pcre2-match-data* (FFI pointer, GC-managed)
   pattern       ; original pattern string (for debugging/display)
   capture-count ; uint32: number of capturing groups
   jit?          ; boolean: JIT compilation succeeded
   name-table)   ; alist ((name . group-number) ...) or '()
  final: #t)

;; Match result — immutable snapshot of one successful match.
;; span-vec: vector of (start . end) pairs, or #f for unset groups.
(defstruct pcre-match
  (span-vec    ; internal: vector of (start . end) or #f
   subject     ; subject string (for substring extraction)
   name-table) ; alist ((name . group-number) ...) for named access
  final: #t)

;; Mutex protecting the static substitute buffer in libpcre2
(def _substitute-mutex (make-mutex 'pcre2-substitute))

;; Mutex protecting compile-time C statics (_ffi_errorcode, _ffi_erroroffset, _ffi_errbuf)
(def _compile-mutex (make-mutex 'pcre2-compile))

;; Mask options to unsigned 32-bit for FFI (define-const loads PCRE2_ANCHORED
;; as signed -2147483648 which Gambit rejects for unsigned-int32 parameters)
(def (u32 n) (bitwise-and n #xFFFFFFFF))

;;;
;;; UTF-8 byte/char offset conversion
;;;
;;; PCRE2 works in byte offsets; Scheme works in character indices.
;;; These helpers convert between the two using the UTF-8 byte vector.
;;;

(def (utf8-byte-length b)
  ;; Number of bytes in the UTF-8 character starting with byte b.
  (cond ((< b #x80) 1) ((< b #xE0) 2) ((< b #xF0) 3) (else 4)))

(def (byte-offset->char-index bv byte-off)
  ;; Convert a UTF-8 byte offset to a Scheme character index.
  (let loop ((bi 0) (ci 0))
    (if (>= bi byte-off) ci
      (loop (+ bi (utf8-byte-length (u8vector-ref bv bi)))
            (+ ci 1)))))

(def (char-index->byte-offset bv char-idx)
  ;; Convert a Scheme character index to a UTF-8 byte offset.
  (let loop ((bi 0) (ci 0))
    (if (>= ci char-idx) bi
      (loop (+ bi (utf8-byte-length (u8vector-ref bv bi)))
            (+ ci 1)))))

;;;
;;; Name table parsing
;;;

(def (pcre2-build-name-table code)
  (let ((ncount (ffi-pcre2-name-count code)))
    (if (= ncount 0)
      '()
      (let loop ((i 0) (acc '()))
        (if (= i ncount)
          (reverse acc)
          (let ((name  (ffi-pcre2-name-entry-name code i))
                (group (ffi-pcre2-name-entry-group code i)))
            (loop (+ i 1) (cons (cons name group) acc))))))))

;;;
;;; Compilation
;;;

(def (pcre2-compile pattern
                    (options (bitwise-ior PCRE2_UTF PCRE2_UCP))
                    jit: (jit? #t))
  ;; Compile a PCRE2 pattern. Returns a pcre-regex struct.
  ;; Raises PCRE2Error on invalid pattern.
  ;; JIT is enabled by default — improves match speed significantly.
  ;; Mutex protects C statics used during compile.
  (mutex-lock! _compile-mutex)
  (unwind-protect
    (let* ((code (ffi-pcre2-compile pattern (u32 options))))
      (when (not code)
        (raise-pcre2-compile-error
         'pcre2-compile pattern
         (ffi-pcre2-compile-errorcode)
         (ffi-pcre2-compile-erroroffset)))
      ;; Attempt JIT (gracefully skip if platform doesn't support it)
      (def jit-ok? #f)
      (when jit?
        (let ((jrc (ffi-pcre2-jit-compile code PCRE2_JIT_COMPLETE)))
          (set! jit-ok? (= jrc 0))))
      (let* ((md    (ffi-pcre2-match-data-create-from-pattern code))
             (cnt   (ffi-pcre2-capture-count code))
             (names (pcre2-build-name-table code)))
        (make-pcre-regex code md pattern cnt jit-ok? names)))
    (mutex-unlock! _compile-mutex)))

(def (pcre2-regex pattern
                  caseless: (caseless #f)
                  multiline: (multiline #f)
                  dotall: (dotall #f)
                  extended: (extended #f)
                  ungreedy: (ungreedy #f)
                  utf: (utf #t)
                  ucp: (ucp #t)
                  literal: (literal #f)
                  jit: (jit #t))
  ;; Compile with named flag options.
  (let ((opts (bitwise-ior
               (if utf       PCRE2_UTF       0)
               (if ucp       PCRE2_UCP       0)
               (if caseless  PCRE2_CASELESS  0)
               (if multiline PCRE2_MULTILINE 0)
               (if dotall    PCRE2_DOTALL    0)
               (if extended  PCRE2_EXTENDED  0)
               (if ungreedy  PCRE2_UNGREEDY  0)
               (if literal   PCRE2_LITERAL   0))))
    (pcre2-compile pattern opts jit: jit)))

;;;
;;; String pattern cache (LRU with mutex, alist adequate for max=64)
;;;

(def _cache-mutex (make-mutex 'pcre2-cache))
(def _cache-max   64)
(def _cache       '())  ; alist: (pattern . pcre-regex)

(def (pcre2-compile/cached pattern)
  (mutex-lock! _cache-mutex)
  (unwind-protect
    (let ((entry (assoc pattern _cache)))
      (if entry
        (begin
          ;; Move to front for LRU behavior
          (set! _cache (cons entry (filter (lambda (e) (not (eq? e entry))) _cache)))
          (cdr entry))
        (let ((rx (pcre2-compile pattern)))
          (set! _cache (cons (cons pattern rx) _cache))
          (when (> (length _cache) _cache-max)
            (set! _cache (take _cache _cache-max)))
          rx)))
    (mutex-unlock! _cache-mutex)))

(def (ensure-regex pattern-or-regex)
  (cond
    ((pcre-regex? pattern-or-regex) pattern-or-regex)
    ((string? pattern-or-regex)     (pcre2-compile/cached pattern-or-regex))
    (else (error "Expected string or pcre-regex" pattern-or-regex))))

;;;
;;; Internal matching helper
;;;

(def (pcre2-do-match rx subject start options (md #f) (subject-bytes #f))
  ;; Run match and return pcre-match or #f.
  ;; Allocates per-call match-data for thread safety unless md is provided.
  ;; subject-bytes: optional pre-computed (string->bytes subject) for iteration.
  ;; Converts PCRE2 byte offsets to Scheme character indices.
  (let* ((code (pcre-regex-code rx))
         (md   (or md (ffi-pcre2-match-data-create-from-pattern code)))
         (bv   (or subject-bytes (string->bytes subject)))
         (byte-start (if (zero? start) 0 (char-index->byte-offset bv start)))
         (opts (u32 options))
         (rc   (if (pcre-regex-jit? rx)
                 (ffi-pcre2-jit-match code subject byte-start opts md)
                 (ffi-pcre2-match     code subject byte-start opts md))))
    (if (< rc 0)
      #f
      (let* ((ncap (+ (pcre-regex-capture-count rx) 1))
             (sv   (make-vector ncap #f)))
        (let loop ((i 0))
          (when (< i ncap)
            (unless (ffi-pcre2-ovector-is-unset? md i)
              (let ((bstart (ffi-pcre2-ovector-start md i))
                    (bend   (ffi-pcre2-ovector-end   md i)))
                (vector-set! sv i
                  (cons (byte-offset->char-index bv bstart)
                        (byte-offset->char-index bv bend)))))
            (loop (+ i 1))))
        (make-pcre-match sv subject (pcre-regex-name-table rx))))))

;;;
;;; Matching API
;;;

(def (pcre2-match rx/str subject (start 0))
  ;; Full match: pattern must match the entire subject (anchored both ends).
  (let ((rx (ensure-regex rx/str)))
    (pcre2-do-match rx subject start
                    (bitwise-ior PCRE2_ANCHORED PCRE2_ENDANCHORED))))

(def (pcre2-search rx/str subject (start 0))
  ;; Search: find first occurrence anywhere in subject from start.
  (let ((rx (ensure-regex rx/str)))
    (pcre2-do-match rx subject start 0)))

(def (pcre2-matches? rx/str subject (start 0))
  ;; Test-only: returns boolean. No capture overhead.
  ;; Uses minimal match-data (1 slot) since we only need yes/no.
  (let* ((rx   (ensure-regex rx/str))
         (code (pcre-regex-code rx))
         (md   (ffi-pcre2-match-data-create 1))
         (byte-start (if (zero? start) 0
                       (char-index->byte-offset (string->bytes subject) start)))
         (rc   (if (pcre-regex-jit? rx)
                 (ffi-pcre2-jit-match code subject byte-start 0 md)
                 (ffi-pcre2-match     code subject byte-start 0 md))))
    (>= rc 0)))

;;;
;;; Match result access
;;;

(def (pcre-match-group m (n 0))
  ;; Return group n as a string, or #f if unset.
  (let ((pair (vector-ref (pcre-match-span-vec m) n)))
    (and pair (substring (pcre-match-subject m)
                         (car pair) (cdr pair)))))

(def (pcre-match-named m name)
  ;; Return named capture group as a string, or #f if not found/unset.
  (let ((entry (assoc name (pcre-match-name-table m))))
    (and entry (pcre-match-group m (cdr entry)))))

(def (pcre-match-positions m (n 0))
  ;; Return (start . end) byte positions for group n, or #f if unset.
  (vector-ref (pcre-match-span-vec m) n))

(def (pcre-match->list m)
  ;; Return all groups as a list of strings (with #f for unset groups).
  (let* ((sv (pcre-match-span-vec m))
         (n  (vector-length sv)))
    (let loop ((i 0) (acc '()))
      (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons (pcre-match-group m i) acc))))))

(def (pcre-match->alist m)
  ;; Return named captures as an alist ((name . string-or-#f) ...)
  (map (lambda (entry)
         (cons (car entry) (pcre-match-named m (car entry))))
       (pcre-match-name-table m)))

;;;
;;; Substitution
;;;

(def (pcre2-replace rx/str subject replacement
                    (start 0)
                    extended: (extended #f))
  ;; Replace the first occurrence of rx with replacement.
  ;; Use $1, ${name} etc. in replacement with extended: #t.
  ;; Raises PCRE2Error on substitution errors (e.g. bad backreference).
  (let* ((rx   (ensure-regex rx/str))
         (opts (u32 (if extended PCRE2_SUBSTITUTE_EXTENDED 0)))
         (byte-start (if (zero? start) 0
                       (char-index->byte-offset (string->bytes subject) start))))
    (mutex-lock! _substitute-mutex)
    (unwind-protect
      (let ((rc (ffi-pcre2-do-substitute
                 (pcre-regex-code rx)
                 subject byte-start opts
                 (pcre-regex-match-data rx)
                 replacement)))
        (cond
          ((>= rc 0)
           (begin0 (ffi-pcre2-substitute-result)
                   (ffi-pcre2-substitute-free)))
          ((= rc PCRE2_ERROR_NOMATCH) subject)
          (else (raise-pcre2-error 'pcre2-replace rc))))
      (mutex-unlock! _substitute-mutex))))

(def (pcre2-replace-all rx/str subject replacement
                         (start 0)
                         extended: (extended #f))
  ;; Replace all non-overlapping occurrences of rx with replacement.
  ;; Raises PCRE2Error on substitution errors (e.g. bad backreference).
  (let* ((rx   (ensure-regex rx/str))
         (opts (u32 (bitwise-ior PCRE2_SUBSTITUTE_GLOBAL
                                 (if extended PCRE2_SUBSTITUTE_EXTENDED 0))))
         (byte-start (if (zero? start) 0
                       (char-index->byte-offset (string->bytes subject) start))))
    (mutex-lock! _substitute-mutex)
    (unwind-protect
      (let ((rc (ffi-pcre2-do-substitute
                 (pcre-regex-code rx)
                 subject byte-start opts
                 (pcre-regex-match-data rx)
                 replacement)))
        (cond
          ((>= rc 0)
           (begin0 (ffi-pcre2-substitute-result)
                   (ffi-pcre2-substitute-free)))
          ((= rc PCRE2_ERROR_NOMATCH) subject)
          (else (raise-pcre2-error 'pcre2-replace-all rc))))
      (mutex-unlock! _substitute-mutex))))

;;;
;;; Iteration / fold
;;;

(def (pcre2-fold rx/str kons knil subject (start 0))
  ;; Fold kons over all non-overlapping matches in subject.
  ;; kons :: (pcre-match accumulator -> accumulator)
  (let* ((rx   (ensure-regex rx/str))
         (md   (ffi-pcre2-match-data-create-from-pattern (pcre-regex-code rx)))
         (slen (string-length subject))
         (bv   (string->bytes subject)))
    (let loop ((pos start) (acc knil))
      (let ((m (pcre2-do-match rx subject pos 0 md bv)))
        (if (not m)
          acc
          (let* ((span (vector-ref (pcre-match-span-vec m) 0))
                 (end  (cdr span))
                 ;; Advance past zero-length matches by 1 character
                 (next (if (= (car span) end) (+ end 1) end)))
            (if (> next slen)
              (kons m acc)
              (loop next (kons m acc)))))))))

(def (pcre2-find-all rx/str subject (start 0))
  ;; Return a list of all non-overlapping pcre-match objects.
  (reverse (pcre2-fold rx/str cons '() subject start)))

(def (pcre2-extract rx/str subject (start 0))
  ;; Return a list of all matching substrings (group 0 from each match).
  (reverse
   (pcre2-fold rx/str
               (lambda (m acc) (cons (pcre-match-group m 0) acc))
               '() subject start)))

(def (pcre2-split rx/str subject (limit #f))
  ;; Split subject by regex delimiter. Returns list of substrings.
  ;; limit: max number of result parts (not splits).
  ;; Uses seg-start to track segment boundaries separately from search position,
  ;; so zero-length matches work correctly as split points.
  (let* ((rx   (ensure-regex rx/str))
         (md   (ffi-pcre2-match-data-create-from-pattern (pcre-regex-code rx)))
         (slen (string-length subject))
         (bv   (string->bytes subject)))
    (let loop ((pos 0) (seg-start 0) (acc '()) (count 1))
      (if (and limit (>= count limit))
        (reverse (cons (substring subject seg-start slen) acc))
        (let ((m (pcre2-do-match rx subject pos 0 md bv)))
          (if (not m)
            (reverse (cons (substring subject seg-start slen) acc))
            (let* ((span   (vector-ref (pcre-match-span-vec m) 0))
                   (mstart (car span))
                   (mend   (cdr span)))
              (if (= mstart mend)
                ;; Zero-length match
                (if (= mstart pos)
                  ;; At current pos — advance one character without splitting
                  (if (>= (+ pos 1) slen)
                    (reverse (cons (substring subject seg-start slen) acc))
                    (loop (+ pos 1) seg-start acc count))
                  ;; Ahead of current pos — valid split point
                  (loop mend mend
                        (cons (substring subject seg-start mstart) acc)
                        (+ count 1)))
                ;; Normal match
                (loop mend mend
                      (cons (substring subject seg-start mstart) acc)
                      (+ count 1))))))))))

(def (pcre2-partition rx/str subject)
  ;; Partition subject into alternating non-match/match substrings.
  ;; Returns (pre-match1 match1 pre-match2 match2 ... tail)
  (let* ((rx   (ensure-regex rx/str))
         (md   (ffi-pcre2-match-data-create-from-pattern (pcre-regex-code rx)))
         (slen (string-length subject))
         (bv   (string->bytes subject)))
    (let loop ((pos 0) (acc '()))
      (let ((m (pcre2-do-match rx subject pos 0 md bv)))
        (if (not m)
          (reverse (cons (substring subject pos slen) acc))
          (let* ((span   (vector-ref (pcre-match-span-vec m) 0))
                 (mstart (car span))
                 (mend   (cdr span))
                 (pre    (substring subject pos mstart))
                 (hit    (pcre-match-group m 0))
                 (next   (if (= mstart mend) (+ mend 1) mend)))
            (if (> next slen)
              (reverse (cons hit (cons pre acc)))
              (loop next (cons hit (cons pre acc))))))))))

;;;
;;; Pattern quoting
;;;

(def _meta-chars-list (string->list "\\^$.|?*+()[]{}#"))

(def (pcre2-quote str)
  ;; Escape all PCRE2 metacharacters in str.
  (let ((p (open-output-string)))
    (string-for-each
     (lambda (c)
       (when (memv c _meta-chars-list)
         (write-char #\\ p))
       (write-char c p))
     str)
    (get-output-string p)))

;;;
;;; Resource management
;;;

(def (pcre2-release! regex)
  ;; Explicitly release FFI resources (GC handles automatically).
  (when (pcre-regex? regex)
    (let ((md   (pcre-regex-match-data regex))
          (code (pcre-regex-code regex)))
      (when md   (foreign-release! md))
      (when code (foreign-release! code)))))

;;;
;;; Pregexp-compatible API
;;;

(def (pcre2-pregexp-match pattern subject (start 0) (end #f))
  ;; Like pregexp-match: returns list of strings or #f.
  (let* ((subj (if end (substring subject start end) (substring subject start (string-length subject))))
         (m    (pcre2-search pattern subj 0)))
    (and m (pcre-match->list m))))

(def (pcre2-pregexp-match-positions pattern subject (start 0) (end #f))
  ;; Like pregexp-match-positions: returns list of (start . end) or #f.
  (let* ((subj (if end (substring subject start end) (substring subject start (string-length subject))))
         (m    (pcre2-search pattern subj 0)))
    (and m
         (let* ((sv (pcre-match-span-vec m))
                (n  (vector-length sv)))
           (let loop ((i 0) (acc '()))
             (if (= i n)
               (reverse acc)
               (let ((pair (vector-ref sv i)))
                 (loop (+ i 1)
                       (cons (if pair
                               (cons (+ start (car pair))
                                     (+ start (cdr pair)))
                               #f)
                             acc)))))))))

(def (pcre2-pregexp-replace pattern subject replacement)
  ;; Like pregexp-replace: replace first occurrence.
  (pcre2-replace pattern subject replacement))

(def (pcre2-pregexp-replace* pattern subject replacement)
  ;; Like pregexp-replace*: replace all occurrences.
  (pcre2-replace-all pattern subject replacement))

(def (pcre2-pregexp-quote str)
  ;; Like pregexp-quote.
  (pcre2-quote str))
