# gerbil-pcre Code Review Findings

**Date**: 2026-02-18
**Build**: passing (all tests OK)
**Test coverage**: 100% of exported symbols tested
**Overall**: Solid, well-structured FFI wrapper. Several issues found ranging from a real bug to style improvements.

---

## 1. BUG: Misleading docstring in `pcre-match-positions`

**File**: `pcre2/pcre2.ss:296`
**Severity**: Low (documentation bug)

The comment says "Return (start . end) **byte** positions" but the function actually returns **character** indices, because `pcre2-do-match` already converts byte offsets to character indices at lines 247-248.

```scheme
;; Current (wrong):
;; Return (start . end) byte positions for group n, or #f if unset.

;; Should be:
;; Return (start . end) character positions for group n, or #f if unset.
```

---

## 2. BUG: `pcre2-release!` can corrupt the pattern cache

**File**: `pcre2/pcre2.ss:478-484` and `pcre2/pcre2.ss:198-212`
**Severity**: Medium

`pcre2-compile/cached` stores compiled regexes in a global LRU cache. When a user calls any API with a string pattern (e.g., `pcre2-search "\\d+" subject`), the regex is cached via `ensure-regex` → `pcre2-compile/cached`. If a user stores a reference to a regex obtained this way and then calls `pcre2-release!` on it, the cache entry is corrupted:

```scheme
(def rx (pcre2-compile/cached "\\d+"))
(pcre2-release! rx)  ;; corrupts the cache entry
(pcre2-search "\\d+" "42")  ;; finds the released regex in cache → use-after-free
```

While `pcre2-compile/cached` is internal, `ensure-regex` is called from all public APIs when given a string pattern. A user calling `pcre2-release!` on a regex that was originally obtained via a string-pattern API would corrupt the cache.

**Suggested fix**: Either (a) remove released entries from the cache in `pcre2-release!`, or (b) document that `pcre2-release!` must only be used with regexes from `pcre2-compile`/`pcre2-regex`, not string patterns.

---

## 3. THREAD SAFETY: Compile statics inconsistency

**File**: `pcre2/libpcre2.ss:112-113` vs `pcre2/libpcre2.ss:129`
**Severity**: Low (protected by Scheme mutex, but inconsistent)

`_ffi_errbuf` is correctly declared `__thread`, but `_ffi_errorcode` and `_ffi_erroroffset` are plain `static`. The Scheme-level `_compile-mutex` prevents practical races, but for defense-in-depth, these should also be `__thread`:

```c
// Current:
static int    _ffi_errorcode   = 0;
static size_t _ffi_erroroffset = 0;

// Better:
static __thread int    _ffi_errorcode   = 0;
static __thread size_t _ffi_erroroffset = 0;
```

---

## 4. C COMPILER WARNINGS: `const` qualifier discarding

**File**: `pcre2/libpcre2.ss` (C preamble)
**Severity**: Low (3 compiler warnings)

Three C functions return `const char*` but Gambit's `char-string` type expects `char*`, producing `-Wdiscarded-qualifiers` warnings during build. Fix by casting the return values:

- **Line 130** (`ffi_pcre2_get_error_message`): Change `return _ffi_errbuf;` to `return (char*)_ffi_errbuf;` (or remove `const` from the function signature since `_ffi_errbuf` is already a mutable `__thread char[]`)
- **Line 213** (`ffi_pcre2_substitute_result`): Change to `return _ffi_subst_buf ? (char*)_ffi_subst_buf : (char*)"";`
- **Line 290** (`ffi_pcre2_name_entry_name`): Cast to `return (char*)(table + index * entry_size + 2);`

---

## 5. SECURITY SCANNER: False positives (not real issues)

**File**: `pcre2/libpcre2.ss:302`, `pcre2/pcre2.ss:465`

- **CRITICAL on line 302**: False positive. The `return 0` is a defensive bounds check (`if (index >= name_count) return 0`), not an `#ifdef` stub pattern.
- **MEDIUM on line 465**: Known false positive. `open-output-string` is an in-memory port with no OS resources to leak. No `unwind-protect` needed.

---

## 6. STYLE: Missing `transparent: #t` on structs

**File**: `pcre2/pcre2.ss:71, 82`
**Severity**: Informational

Both `pcre-regex` and `pcre-match` use `final: #t` but not `transparent: #t`. Adding it improves debugging/printing — values display their fields in the REPL instead of opaque `#<pcre-regex>`.

---

## 7. PERFORMANCE: `pcre2-matches?` allocates match-data per call

**File**: `pcre2/pcre2.ss:272`
**Severity**: Low (GC pressure in hot paths)

Each call to `pcre2-matches?` creates a fresh match-data object via `ffi-pcre2-match-data-create 1`. For hot-path boolean tests this adds GC pressure. Consider using the regex's stored match-data (with appropriate locking) or a thread-local reusable match-data for the `matches?` fast path.

---

## 8. PERFORMANCE: O(n) UTF-8 offset conversion per group

**File**: `pcre2/pcre2.ss:109-121`
**Severity**: Low (affects long strings with many capture groups)

`byte-offset->char-index` walks the byte vector linearly for each captured group. For patterns with many groups on long strings, total cost is O(n × groups). A single-pass conversion that builds all char indices at once would be more efficient.

---

## 9. PERFORMANCE: Cache mutex held during compilation

**File**: `pcre2/pcre2.ss:207`
**Severity**: Low (reduces concurrency)

`pcre2-compile/cached` holds `_cache-mutex` while calling `pcre2-compile`, which may be slow (especially with JIT). This blocks all cache lookups during compilation. Consider a double-checked locking pattern: release the cache mutex, compile, reacquire, and check if another thread already inserted.

---

## 10. LINT: Arity checker false positives

**Severity**: Informational (no action needed)

The 5 arity warnings from static analysis are all false positives caused by keyword arguments (`jit:`, `extended:`) not being recognized by the checker. Functions `pcre2-compile`, `pcre2-regex`, `pcre2-replace`, and `pcre2-replace-all` all use keyword args correctly.

---

## 11. LINT: Test file duplicate-definition warnings

**Severity**: Informational (no action needed)

The 52 duplicate-definition warnings in `pcre2-test.ss` are all false positives — each `test-case` creates its own scope, so redefining `rx`, `m`, etc. across test cases is standard practice.

---

## 12. MINOR: `pcre2-quote` doesn't escape `-`

**File**: `pcre2/pcre2.ss:461`
**Severity**: Informational

The dash `-` is a metacharacter inside character classes (`[a-z]`). Since `pcre2-quote` escapes `[` and `]`, quoted strings can't end up inside a character class, so this is technically safe. But other regex libraries (Python's `re.escape`, etc.) do escape it preemptively. Worth considering for maximum safety.

---

## 13. BUG: `char-string` FFI type crashes on non-Latin-1 characters

**File**: `pcre2/libpcre2.ss` (all `char-string` parameters), `pcre2/pcre2.ss` (all matching/replace functions)
**Severity**: High

All FFI functions that pass strings to PCRE2 use Gambit's `char-string` type, which can only represent characters in the ISO-8859-1 / Latin-1 range (U+0000–U+00FF). Strings containing characters above U+00FF — such as `€` (U+20AC), `—` (U+2014), CJK characters, or emoji — cause a runtime crash:

```
*** ERROR IN pcre2-do-match -- (Argument 2) Can't convert to C char-string
```

This is despite the library compiling patterns with `PCRE2_UTF | PCRE2_UCP` by default and advertising UTF-8 support. The existing UTF-8 tests all pass because they only use Latin-1-range characters (`é`, `ü`, `ö`, `ç`, etc.).

**Reproducer**:
```scheme
(pcre2-search "." "hello€world")   ;; CRASH — € is U+20AC
(pcre2-search "." "em—dash")       ;; CRASH — — is U+2014
(pcre2-search "." "日本語")         ;; CRASH — CJK characters
```

**Suggested fix**: Replace `char-string` with a `scheme-object` + manual UTF-8 encoding approach in the C shims. Convert Scheme strings to UTF-8 byte vectors (`string->bytes`) on the Scheme side, pass them as `scheme-object` or `nonnull-unsigned-int8-vector`, and use the byte data pointer in C. This is the standard Gambit pattern for full Unicode FFI strings.

---

## Summary by Priority

| #  | Severity | Category       | Issue                                                                 |
|----|----------|----------------|-----------------------------------------------------------------------|
| 13 | **High** | Bug            | `char-string` FFI crashes on non-Latin-1 Unicode (€, CJK, emoji)      |
| 2  | Medium   | Bug            | `pcre2-release!` can corrupt pattern cache                            |
| 1  | Low      | Bug            | `pcre-match-positions` docstring says "byte" but returns char indices |
| 3  | Low      | Thread safety  | Compile statics should be `__thread`                                  |
| 4  | Low      | Build          | 3 C compiler warnings (`const` qualifier)                             |
| 7  | Low      | Performance    | `pcre2-matches?` allocates per call                                   |
| 8  | Low      | Performance    | O(n) UTF-8 conversion per group                                       |
| 9  | Low      | Performance    | Cache mutex held during compilation                                   |
| 6  | Info     | Style          | Missing `transparent: #t` on structs                                  |
| 12 | Info     | Robustness     | `pcre2-quote` doesn't escape `-`                                      |
| 5  | Info     | False positive | Security scanner flags are not real issues                            |
| 10 | Info     | False positive | Arity checker false positives (keyword args)                          |
| 11 | Info     | False positive | Test lint false positives (scoped redefinitions)                      |
