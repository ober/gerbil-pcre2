;;; pcre2/libpcre2.ss — Layer 1: Raw FFI bindings to libpcre2-8
;;;
;;; Uses :std/foreign begin-ffi. Links against -lpcre2-8 via pkg-config.
;;; The high-level API lives in pcre2/pcre2.ss.

(import :std/foreign)
(export #t)

(begin-ffi
  ;; Exported symbols
  (pcre2-code?
   pcre2-match-data?

   ;; Compile options
   PCRE2_CASELESS PCRE2_MULTILINE PCRE2_DOTALL PCRE2_EXTENDED
   PCRE2_UTF PCRE2_UCP PCRE2_ANCHORED PCRE2_ENDANCHORED
   PCRE2_UNGREEDY PCRE2_NO_AUTO_CAPTURE PCRE2_DUPNAMES
   PCRE2_LITERAL PCRE2_EXTENDED_MORE PCRE2_MATCH_INVALID_UTF
   PCRE2_FIRSTLINE PCRE2_DOLLAR_ENDONLY PCRE2_MATCH_UNSET_BACKREF
   PCRE2_NEVER_UCP PCRE2_NEVER_UTF PCRE2_NEVER_BACKSLASH_C
   PCRE2_NO_AUTO_POSSESS PCRE2_NO_DOTSTAR_ANCHOR PCRE2_NO_START_OPTIMIZE
   PCRE2_NO_UTF_CHECK PCRE2_ALT_BSUX PCRE2_ALT_CIRCUMFLEX PCRE2_ALT_VERBNAMES
   PCRE2_ALLOW_EMPTY_CLASS PCRE2_AUTO_CALLOUT PCRE2_USE_OFFSET_LIMIT

   ;; Match options
   PCRE2_NOTBOL PCRE2_NOTEOL PCRE2_NOTEMPTY PCRE2_NOTEMPTY_ATSTART
   PCRE2_PARTIAL_SOFT PCRE2_PARTIAL_HARD
   PCRE2_NO_JIT PCRE2_COPY_MATCHED_SUBJECT

   ;; Substitute options
   PCRE2_SUBSTITUTE_GLOBAL PCRE2_SUBSTITUTE_EXTENDED
   PCRE2_SUBSTITUTE_UNSET_EMPTY PCRE2_SUBSTITUTE_UNKNOWN_UNSET
   PCRE2_SUBSTITUTE_OVERFLOW_LENGTH PCRE2_SUBSTITUTE_LITERAL
   PCRE2_SUBSTITUTE_MATCHED PCRE2_SUBSTITUTE_REPLACEMENT_ONLY

   ;; JIT options
   PCRE2_JIT_COMPLETE PCRE2_JIT_PARTIAL_SOFT PCRE2_JIT_PARTIAL_HARD

   ;; Error codes
   PCRE2_ERROR_NOMATCH PCRE2_ERROR_PARTIAL
   PCRE2_ERROR_BADOFFSET PCRE2_ERROR_BADOPTION
   PCRE2_ERROR_MATCHLIMIT PCRE2_ERROR_DEPTHLIMIT
   PCRE2_ERROR_NOMEMORY PCRE2_ERROR_JIT_STACKLIMIT
   PCRE2_ERROR_NOSUBSTRING

   ;; Info constants
   PCRE2_INFO_CAPTURECOUNT PCRE2_INFO_NAMECOUNT
   PCRE2_INFO_NAMEENTRYSIZE PCRE2_INFO_NAMETABLE
   PCRE2_INFO_JITSIZE PCRE2_INFO_SIZE

   ;; Config constants
   PCRE2_CONFIG_JIT PCRE2_CONFIG_UNICODE

   ;; Core functions
   ffi-pcre2-compile
   ffi-pcre2-compile-errorcode
   ffi-pcre2-compile-erroroffset
   ffi-pcre2-match
   ffi-pcre2-match-data-create-from-pattern
   ffi-pcre2-match-data-create
   ffi-pcre2-get-ovector-count
   ffi-pcre2-ovector-start
   ffi-pcre2-ovector-end
   ffi-pcre2-ovector-is-unset?
   ffi-pcre2-get-error-message
   ffi-pcre2-get-startchar

   ;; Substitute (via static buffer — not thread-safe; use mutex in caller)
   ffi-pcre2-do-substitute
   ffi-pcre2-substitute-result
   ffi-pcre2-substitute-free

   ;; Named groups & pattern info
   ffi-pcre2-substring-number-from-name
   ffi-pcre2-capture-count
   ffi-pcre2-name-count
   ffi-pcre2-name-entry-size
   ffi-pcre2-name-entry-name
   ffi-pcre2-name-entry-group

   ;; JIT
   ffi-pcre2-jit-compile
   ffi-pcre2-jit-match)

  ;; -----------------------------------------------------------------------
  ;; C preamble
  ;; -----------------------------------------------------------------------
  (c-declare #<<C-END
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* -------------------------------------------------------------------
 * GC finalizers — must return ___FIX(___NO_ERR)
 * ------------------------------------------------------------------- */
___SCMOBJ ffi_pcre2_code_free(void *ptr) {
    pcre2_code_free_8((pcre2_code_8*)ptr);
    return ___FIX(___NO_ERR);
}

___SCMOBJ ffi_pcre2_match_data_free(void *ptr) {
    pcre2_match_data_free_8((pcre2_match_data_8*)ptr);
    return ___FIX(___NO_ERR);
}

/* -------------------------------------------------------------------
 * Compile wrapper — stores error info in statics (not thread-safe
 * for concurrent compiles, but compile is normally done once).
 * ------------------------------------------------------------------- */
static int    _ffi_errorcode   = 0;
static size_t _ffi_erroroffset = 0;

static pcre2_code_8* ffi_pcre2_compile(
    const char* pattern, uint32_t options)
{
    return pcre2_compile_8(
        (PCRE2_SPTR8)pattern, PCRE2_ZERO_TERMINATED, options,
        &_ffi_errorcode, &_ffi_erroroffset, NULL);
}

static int    ffi_pcre2_compile_errorcode(void)   { return _ffi_errorcode; }
static size_t ffi_pcre2_compile_erroroffset(void) { return _ffi_erroroffset; }

/* -------------------------------------------------------------------
 * Error message
 * ------------------------------------------------------------------- */
static __thread char _ffi_errbuf[512];
static const char* ffi_pcre2_get_error_message(int errorcode) {
    int rc = pcre2_get_error_message_8(
        errorcode, (PCRE2_UCHAR8*)_ffi_errbuf, sizeof(_ffi_errbuf));
    if (rc < 0) return "Unknown PCRE2 error";
    return _ffi_errbuf;
}

/* -------------------------------------------------------------------
 * Ovector access
 * ------------------------------------------------------------------- */
static size_t ffi_pcre2_ovector_start(pcre2_match_data_8* md, uint32_t idx) {
    uint32_t count = pcre2_get_ovector_count_8(md);
    if (idx >= count) return PCRE2_UNSET;
    PCRE2_SIZE* ov = pcre2_get_ovector_pointer_8(md);
    return ov[2*idx];
}

static size_t ffi_pcre2_ovector_end(pcre2_match_data_8* md, uint32_t idx) {
    uint32_t count = pcre2_get_ovector_count_8(md);
    if (idx >= count) return PCRE2_UNSET;
    PCRE2_SIZE* ov = pcre2_get_ovector_pointer_8(md);
    return ov[2*idx + 1];
}

/* Check if ovector entry is PCRE2_UNSET (SIZE_MAX) — avoids bignum in Scheme */
static int ffi_pcre2_ovector_is_unset(pcre2_match_data_8* md, uint32_t idx) {
    uint32_t count = pcre2_get_ovector_count_8(md);
    if (idx >= count) return 1;
    PCRE2_SIZE* ov = pcre2_get_ovector_pointer_8(md);
    return ov[2*idx] == PCRE2_UNSET;
}

/* -------------------------------------------------------------------
 * Substitute — uses static buffer, returns count or error.
 * NOT thread-safe — the high-level layer must hold a mutex.
 * ------------------------------------------------------------------- */
static char* _ffi_subst_buf = NULL;

static int ffi_pcre2_do_substitute(
    const pcre2_code_8* code,
    const char* subject,
    size_t startoffset,      uint32_t options,
    pcre2_match_data_8* match_data,
    const char* replacement)
{
    if (_ffi_subst_buf) { free(_ffi_subst_buf); _ffi_subst_buf = NULL; }

    /* First pass: get required output size */
    size_t subject_length = strlen(subject);
    size_t replacement_length = strlen(replacement);
    size_t outlen = subject_length + replacement_length + 256;
    _ffi_subst_buf = (char*)malloc(outlen);
    if (!_ffi_subst_buf) return PCRE2_ERROR_NOMEMORY;

    uint32_t opts = options | PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
    int rc = pcre2_substitute_8(
        code,
        (PCRE2_SPTR8)subject, PCRE2_ZERO_TERMINATED,
        startoffset, opts,
        match_data, NULL,
        (PCRE2_SPTR8)replacement, PCRE2_ZERO_TERMINATED,
        (PCRE2_UCHAR8*)_ffi_subst_buf, &outlen);

    if (rc == PCRE2_ERROR_NOMEMORY) {
        /* outlen holds the required size — retry */
        free(_ffi_subst_buf);
        _ffi_subst_buf = (char*)malloc(outlen + 1);
        if (!_ffi_subst_buf) return PCRE2_ERROR_NOMEMORY;
        size_t outlen2 = outlen + 1;
        rc = pcre2_substitute_8(
            code,
            (PCRE2_SPTR8)subject, PCRE2_ZERO_TERMINATED,
            startoffset, opts,
            match_data, NULL,
            (PCRE2_SPTR8)replacement, PCRE2_ZERO_TERMINATED,
            (PCRE2_UCHAR8*)_ffi_subst_buf, &outlen2);
    }

    if (rc < 0) { free(_ffi_subst_buf); _ffi_subst_buf = NULL; }
    return rc;
}

static const char* ffi_pcre2_substitute_result(void) {
    return _ffi_subst_buf ? _ffi_subst_buf : "";
}

static void ffi_pcre2_substitute_free(void) {
    if (_ffi_subst_buf) { free(_ffi_subst_buf); _ffi_subst_buf = NULL; }
}

/* -------------------------------------------------------------------
 * Wrappers that pass NULL for context/stack parameters
 * ------------------------------------------------------------------- */
static int ffi_pcre2_match(
    const pcre2_code_8* code,
    const char* subject,
    size_t startoffset, uint32_t options,
    pcre2_match_data_8* match_data)
{
    return pcre2_match_8(code, (PCRE2_SPTR8)subject, PCRE2_ZERO_TERMINATED,
                         startoffset, options, match_data, NULL);
}

static pcre2_match_data_8* ffi_pcre2_match_data_create_from_pattern(
    const pcre2_code_8* code)
{
    return pcre2_match_data_create_from_pattern_8(code, NULL);
}

static pcre2_match_data_8* ffi_pcre2_match_data_create(uint32_t ovecsize) {
    return pcre2_match_data_create_8(ovecsize, NULL);
}

static int ffi_pcre2_jit_match(
    const pcre2_code_8* code,
    const char* subject,
    size_t startoffset, uint32_t options,
    pcre2_match_data_8* match_data)
{
    return pcre2_jit_match_8(code, (PCRE2_SPTR8)subject, PCRE2_ZERO_TERMINATED,
                              startoffset, options, match_data, NULL);
}

/* -------------------------------------------------------------------
 * Named groups & pattern info
 * ------------------------------------------------------------------- */
static int ffi_pcre2_substring_number_from_name(
    const pcre2_code_8* code, const char* name)
{
    return pcre2_substring_number_from_name_8(code, (PCRE2_SPTR8)name);
}

static uint32_t ffi_pcre2_capture_count(const pcre2_code_8* code) {
    uint32_t count = 0;
    pcre2_pattern_info_8(code, PCRE2_INFO_CAPTURECOUNT, &count);
    return count;
}

static uint32_t ffi_pcre2_name_count(const pcre2_code_8* code) {
    uint32_t count = 0;
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMECOUNT, &count);
    return count;
}

static uint32_t ffi_pcre2_name_entry_size(const pcre2_code_8* code) {
    uint32_t size = 0;
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMEENTRYSIZE, &size);
    return size;
}

static const char* ffi_pcre2_name_entry_name(
    const pcre2_code_8* code, uint32_t index)
{
    PCRE2_SPTR8 table = NULL;
    uint32_t entry_size = 0;
    uint32_t name_count = 0;
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMETABLE, &table);
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMEENTRYSIZE, &entry_size);
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMECOUNT, &name_count);
    if (!table || entry_size == 0 || index >= name_count) return "";
    return (const char*)(table + index * entry_size + 2);
}

static uint32_t ffi_pcre2_name_entry_group(
    const pcre2_code_8* code, uint32_t index)
{
    PCRE2_SPTR8 table = NULL;
    uint32_t entry_size = 0;
    uint32_t name_count = 0;
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMETABLE, &table);
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMEENTRYSIZE, &entry_size);
    pcre2_pattern_info_8(code, PCRE2_INFO_NAMECOUNT, &name_count);
    if (!table || entry_size == 0 || index >= name_count) return 0;
    const uint8_t* entry = table + index * entry_size;
    return ((uint32_t)entry[0] << 8) | (uint32_t)entry[1];
}

C-END
  )

  ;; -----------------------------------------------------------------------
  ;; Pointer types with GC finalizers
  ;; -----------------------------------------------------------------------
  (c-define-type pcre2_code_t "pcre2_code_8")
  (c-define-type pcre2-code*
    (pointer pcre2_code_t (pcre2-code*) "ffi_pcre2_code_free"))

  (c-define-type pcre2_match_data_t "pcre2_match_data_8")
  (c-define-type pcre2-match-data*
    (pointer pcre2_match_data_t (pcre2-match-data*) "ffi_pcre2_match_data_free"))

  ;; -----------------------------------------------------------------------
  ;; Constants — compile options
  ;; -----------------------------------------------------------------------
  (define-const PCRE2_CASELESS)
  (define-const PCRE2_MULTILINE)
  (define-const PCRE2_DOTALL)
  (define-const PCRE2_EXTENDED)
  (define-const PCRE2_UTF)
  (define-const PCRE2_UCP)
  (define-const PCRE2_ANCHORED)
  (define-const PCRE2_ENDANCHORED)
  (define-const PCRE2_UNGREEDY)
  (define-const PCRE2_NO_AUTO_CAPTURE)
  (define-const PCRE2_DUPNAMES)
  (define-const PCRE2_LITERAL)
  (define-const PCRE2_EXTENDED_MORE)
  (define-const PCRE2_MATCH_INVALID_UTF)
  (define-const PCRE2_FIRSTLINE)
  (define-const PCRE2_DOLLAR_ENDONLY)
  (define-const PCRE2_MATCH_UNSET_BACKREF)
  (define-const PCRE2_NEVER_UCP)
  (define-const PCRE2_NEVER_UTF)
  (define-const PCRE2_NEVER_BACKSLASH_C)
  (define-const PCRE2_NO_AUTO_POSSESS)
  (define-const PCRE2_NO_DOTSTAR_ANCHOR)
  (define-const PCRE2_NO_START_OPTIMIZE)
  (define-const PCRE2_NO_UTF_CHECK)
  (define-const PCRE2_ALT_BSUX)
  (define-const PCRE2_ALT_CIRCUMFLEX)
  (define-const PCRE2_ALT_VERBNAMES)
  (define-const PCRE2_ALLOW_EMPTY_CLASS)
  (define-const PCRE2_AUTO_CALLOUT)
  (define-const PCRE2_USE_OFFSET_LIMIT)

  ;; Match options
  (define-const PCRE2_NOTBOL)
  (define-const PCRE2_NOTEOL)
  (define-const PCRE2_NOTEMPTY)
  (define-const PCRE2_NOTEMPTY_ATSTART)
  (define-const PCRE2_PARTIAL_SOFT)
  (define-const PCRE2_PARTIAL_HARD)
  (define-const PCRE2_NO_JIT)
  (define-const PCRE2_COPY_MATCHED_SUBJECT)

  ;; Substitute options
  (define-const PCRE2_SUBSTITUTE_GLOBAL)
  (define-const PCRE2_SUBSTITUTE_EXTENDED)
  (define-const PCRE2_SUBSTITUTE_UNSET_EMPTY)
  (define-const PCRE2_SUBSTITUTE_UNKNOWN_UNSET)
  (define-const PCRE2_SUBSTITUTE_OVERFLOW_LENGTH)
  (define-const PCRE2_SUBSTITUTE_LITERAL)
  (define-const PCRE2_SUBSTITUTE_MATCHED)
  (define-const PCRE2_SUBSTITUTE_REPLACEMENT_ONLY)

  ;; JIT options
  (define-const PCRE2_JIT_COMPLETE)
  (define-const PCRE2_JIT_PARTIAL_SOFT)
  (define-const PCRE2_JIT_PARTIAL_HARD)

  ;; Error codes
  (define-const PCRE2_ERROR_NOMATCH)
  (define-const PCRE2_ERROR_PARTIAL)
  (define-const PCRE2_ERROR_BADOFFSET)
  (define-const PCRE2_ERROR_BADOPTION)
  (define-const PCRE2_ERROR_MATCHLIMIT)
  (define-const PCRE2_ERROR_DEPTHLIMIT)
  (define-const PCRE2_ERROR_NOMEMORY)
  (define-const PCRE2_ERROR_JIT_STACKLIMIT)
  (define-const PCRE2_ERROR_NOSUBSTRING)

  ;; Info constants
  (define-const PCRE2_INFO_CAPTURECOUNT)
  (define-const PCRE2_INFO_NAMECOUNT)
  (define-const PCRE2_INFO_NAMEENTRYSIZE)
  (define-const PCRE2_INFO_NAMETABLE)
  (define-const PCRE2_INFO_JITSIZE)
  (define-const PCRE2_INFO_SIZE)

  ;; Config constants
  (define-const PCRE2_CONFIG_JIT)
  (define-const PCRE2_CONFIG_UNICODE)

  ;; -----------------------------------------------------------------------
  ;; Tier 1 — Core FFI functions
  ;; -----------------------------------------------------------------------

  ;; Compile: returns pcre2-code* or #f on error (check errorcode/erroroffset)
  (define-c-lambda ffi-pcre2-compile
    (char-string unsigned-int32)
    pcre2-code*
    "ffi_pcre2_compile")

  (define-c-lambda ffi-pcre2-compile-errorcode () int
    "ffi_pcre2_compile_errorcode")

  (define-c-lambda ffi-pcre2-compile-erroroffset () size_t
    "ffi_pcre2_compile_erroroffset")

  ;; Match: returns count (>0) on success, negative error code on failure
  (define-c-lambda ffi-pcre2-match
    (pcre2-code* char-string size_t unsigned-int32 pcre2-match-data*)
    int
    "ffi_pcre2_match")

  (define-c-lambda ffi-pcre2-match-data-create-from-pattern
    (pcre2-code*)
    pcre2-match-data*
    "ffi_pcre2_match_data_create_from_pattern")

  (define-c-lambda ffi-pcre2-match-data-create
    (unsigned-int32)
    pcre2-match-data*
    "ffi_pcre2_match_data_create")

  (define-c-lambda ffi-pcre2-get-ovector-count
    (pcre2-match-data*)
    unsigned-int32
    "pcre2_get_ovector_count_8")

  (define-c-lambda ffi-pcre2-ovector-start
    (pcre2-match-data* unsigned-int32)
    size_t
    "ffi_pcre2_ovector_start")

  (define-c-lambda ffi-pcre2-ovector-end
    (pcre2-match-data* unsigned-int32)
    size_t
    "ffi_pcre2_ovector_end")

  ;; Returns #t if ovector group idx is unset (avoids SIZE_MAX bignum)
  (define-c-lambda ffi-pcre2-ovector-is-unset?
    (pcre2-match-data* unsigned-int32)
    bool
    "ffi_pcre2_ovector_is_unset")

  (define-c-lambda ffi-pcre2-get-error-message
    (int)
    char-string
    "ffi_pcre2_get_error_message")

  (define-c-lambda ffi-pcre2-get-startchar
    (pcre2-match-data*)
    size_t
    "pcre2_get_startchar_8")

  ;; Substitute via static buffer (protect with mutex in caller).
  ;; String lengths computed via PCRE2_ZERO_TERMINATED in C.
  (define-c-lambda ffi-pcre2-do-substitute
    (pcre2-code*        ; code
     char-string        ; subject
     size_t             ; startoffset (byte offset)
     unsigned-int32     ; options
     pcre2-match-data*  ; match_data (may be #f)
     char-string)       ; replacement
    int
    "ffi_pcre2_do_substitute")

  ;; Retrieve the result string (copies to Scheme string)
  (define-c-lambda ffi-pcre2-substitute-result () char-string
    "ffi_pcre2_substitute_result")

  ;; Free the static result buffer
  (define-c-lambda ffi-pcre2-substitute-free () void
    "ffi_pcre2_substitute_free")

  ;; -----------------------------------------------------------------------
  ;; Tier 2 — Named groups & pattern info
  ;; -----------------------------------------------------------------------

  (define-c-lambda ffi-pcre2-substring-number-from-name
    (pcre2-code* char-string)
    int
    "ffi_pcre2_substring_number_from_name")

  (define-c-lambda ffi-pcre2-capture-count
    (pcre2-code*)
    unsigned-int32
    "ffi_pcre2_capture_count")

  (define-c-lambda ffi-pcre2-name-count
    (pcre2-code*)
    unsigned-int32
    "ffi_pcre2_name_count")

  (define-c-lambda ffi-pcre2-name-entry-size
    (pcre2-code*)
    unsigned-int32
    "ffi_pcre2_name_entry_size")

  (define-c-lambda ffi-pcre2-name-entry-name
    (pcre2-code* unsigned-int32)
    char-string
    "ffi_pcre2_name_entry_name")

  (define-c-lambda ffi-pcre2-name-entry-group
    (pcre2-code* unsigned-int32)
    unsigned-int32
    "ffi_pcre2_name_entry_group")

  ;; -----------------------------------------------------------------------
  ;; Tier 2 — JIT
  ;; -----------------------------------------------------------------------

  (define-c-lambda ffi-pcre2-jit-compile
    (pcre2-code* unsigned-int32)
    int
    "pcre2_jit_compile_8")

  (define-c-lambda ffi-pcre2-jit-match
    (pcre2-code* char-string size_t unsigned-int32 pcre2-match-data*)
    int
    "ffi_pcre2_jit_match")

) ;; end begin-ffi
