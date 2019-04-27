(pre-include
  "libguile.h"
  "sph-sp.h" "./foreign/sph/helper.c" "./foreign/sph/guile.c" "./config.c" "./foreign/sph/float.c")

(pre-define
  status-group-sp-guile "sp-guile"
  (scm-from-sp-file pointer) (scm-make-foreign-object-1 scm-type-file pointer)
  (scm-from-sp-convolution-filter-state pointer)
  (scm-make-foreign-object-1 scm-type-convolution-filter-state pointer) (scm-from-sp-path pointer)
  (scm-make-foreign-object-1 scm-type-sp-path pointer) (scm->sp-file a)
  (convert-type (scm-foreign-object-ref a 0) sp-file-t*) (scm->sp-convolution-filter-state a)
  (convert-type (scm-foreign-object-ref a 0) sp-convolution-filter-state-t*) (scm->sp-path a)
  (pointer-get (convert-type (scm-foreign-object-ref a 0) sp-path-t*)) (scm->sp-samples a)
  (begin
    "gives a pointer to the memory region"
    (convert-type (SCM-BYTEVECTOR-CONTENTS a) sp-sample-t*))
  (scm->sp-samples-length a) (sp-octets->samples (SCM-BYTEVECTOR-LENGTH a))
  (scm->sp-sample-counts a) (convert-type (SCM-BYTEVECTOR-CONTENTS a) sp-sample-count-t*)
  scm-samples? scm-is-bytevector
  (define-sp-sine! scm-id f)
  (begin
    "defines scm-sp-sine!, scm-sp-sine-lq!"
    (define (scm-id scm-data scm-len scm-sample-duration scm-freq scm-phase scm-amp)
      (SCM SCM SCM SCM SCM SCM SCM)
      (f
        (scm->sp-sample-count scm-len)
        (scm->sp-float scm-sample-duration)
        (scm->sp-float scm-freq)
        (scm->sp-float scm-phase) (scm->sp-float scm-amp) (scm->sp-samples scm-data))
      (return SCM-UNSPECIFIED)))
  ; error handling
  (scm-from-status-error a)
  (scm-c-error a.group (sp-guile-status-name a) (sp-guile-status-description a))
  (scm-c-error group name description)
  (scm-call-1
    scm-rnrs-raise
    (scm-list-4
      (scm-from-latin1-symbol group)
      (scm-from-latin1-symbol name)
      (scm-cons (scm-from-latin1-symbol "description") (scm-from-utf8-string description))
      (scm-cons (scm-from-latin1-symbol "c-routine") (scm-from-latin1-symbol __FUNCTION__))))
  (scm-from-status-return result)
  (return
    (if* status-is-success result
      (scm-from-status-error status)))
  (scm-from-status-dynwind-end-return result)
  (if status-is-success
    (begin
      (scm-dynwind-end)
      (return result))
    (return (scm-from-status-error status))))

(enum (sp-status-id-missing-argument sp-status-id-argument-size-insufficient))

(declare
  scm-type-file SCM
  scm-type-convolution-filter-state SCM
  scm-symbol-line SCM
  scm-symbol-bezier SCM
  scm-symbol-move SCM
  scm-symbol-constant SCM
  scm-symbol-path SCM
  scm-type-sp-path SCM
  scm-rnrs-raise SCM)

(define (sp-guile-status-description a) (uint8-t* status-t)
  "get the description if available for a status"
  (declare b char*)
  (cond
    ( (not (strcmp status-group-sp-guile a.group))
      (case = a.id
        (sp-status-id-missing-argument (set b "missing argument"))
        (sp-status-id-argument-size-insufficient (set b "argument size insufficient"))
        (else (set b ""))))
    (else (set b (sp-status-description a))))
  (return (convert-type b uint8-t*)))

(define (sp-guile-status-name a) (uint8-t* status-t)
  "get the name if available for a status"
  (declare b char*)
  (cond
    ( (not (strcmp status-group-sp-guile a.group))
      (case = a.id
        (sp-status-id-missing-argument (set b "missing-argument"))
        (sp-status-id-argument-size-insufficient (set b "argument-size-insufficient"))
        (else (set b "unknown"))))
    (else (set b (sp-status-name a))))
  (return (convert-type b uint8-t*)))

(define (scm->channel-data a result-channel-count result-channel-data)
  (status-t SCM sp-channel-count-t* sp-sample-t***)
  "(samples ...):channels ...:block integer output -> status-t
  result is set to null if channel-data is empty"
  status-declare
  (declare
    channel-data sp-sample-t**
    channel-count sp-channel-count-t
    i sp-channel-count-t)
  (set channel-count (scm->size-t (scm-length a)))
  (if (not channel-count)
    (begin
      (set
        *result-channel-data 0
        *result-channel-count 0)
      (goto exit)))
  (status-require (sph-helper-calloc (* channel-count (sizeof sp-sample-t*)) &channel-data))
  (for ((set i 0) (< i channel-count) (set i (+ 1 i)))
    (set
      (array-get channel-data i) (scm->sp-samples (scm-first a))
      a (scm-tail a)))
  (set
    *result-channel-data channel-data
    *result-channel-count channel-count)
  (label exit
    (return status)))

(define (scm-c-take-channel-data a channel-count sample-count)
  (SCM sp-sample-t** sp-channel-count-t sp-sample-count-t)
  "get a guile scheme object for channel data sample arrays. returns a list of sample-vectors.
  eventually frees given channel data.
  all sample vectors must have equal length"
  (declare scm-result SCM)
  (set scm-result SCM-EOL)
  (sc-comment "sample vectors prepended in reverse order")
  (while channel-count
    (set
      channel-count (- channel-count 1)
      scm-result (scm-cons (scm-c-take-samples (array-get a channel-count) sample-count) scm-result)))
  (free a)
  (return scm-result))

(define (scm-sp-file-channel-count scm-a) (SCM SCM)
  (return (scm-from-sp-channel-count (: (scm->sp-file scm-a) channel-count))))

(define (scm-sp-file-sample-rate scm-a) (SCM SCM)
  (return (scm-from-sp-sample-rate (: (scm->sp-file scm-a) sample-rate))))

(define (scm-sp-file-position? scm-a) (SCM SCM)
  (return (scm-from-bool (bit-and sp-file-bit-position (: (scm->sp-file scm-a) flags)))))

(define (scm-sp-file-input? scm-a) (SCM SCM)
  (return (scm-from-bool (bit-and sp-file-bit-input (: (scm->sp-file scm-a) flags)))))

(define (scm-sp-file-position scm-a) (SCM SCM)
  "returns the current file position offset in number of samples"
  (declare position sp-sample-count-t)
  (sp-file-position (scm->sp-file scm-a) &position)
  (return (scm-from-sp-sample-count position)))

(define (scm-sp-file-close a) (SCM SCM)
  status-declare
  (set status (sp-file-close (scm->sp-file a)))
  (scm-from-status-return SCM-UNSPECIFIED))

(define (scm-sp-file-position-set scm-file scm-sample-offset) (SCM SCM SCM)
  status-declare
  (set status (sp-file-position-set (scm->sp-file scm-file) (scm->size-t scm-sample-offset)))
  (scm-from-status-return SCM-UNSPECIFIED))

(define (scm-sp-convolve out a b carryover carryover-len) (SCM SCM SCM SCM SCM SCM)
  (declare
    a-len sp-sample-count-t
    b-len sp-sample-count-t
    c-len sp-sample-count-t)
  (set
    a-len (scm->sp-samples-length a)
    b-len (scm->sp-samples-length b)
    c-len
    (if* (scm-is-integer carryover-len) (scm->sp-sample-count carryover-len)
      (scm->sp-samples-length carryover)))
  (if (< c-len (- b-len 1))
    (scm-c-error
      status-group-sp-guile
      "invalid-argument-size" "carryover argument bytevector must be at least (- (length b) 1)"))
  (sp-convolve
    (scm->sp-samples a)
    a-len (scm->sp-samples b) b-len c-len (scm->sp-samples carryover) (scm->sp-samples out))
  (return SCM-UNSPECIFIED))

(define
  (scm-sp-windowed-sinc-lp-hp scm-out scm-in scm-cutoff scm-transition scm-is-high-pass scm-state)
  (SCM SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    state sp-convolution-filter-state-t*
    is-high-pass boolean)
  (set
    is-high-pass (scm-is-true scm-is-high-pass)
    state
    (if* (scm-is-true scm-state) (scm->sp-convolution-filter-state scm-state)
      0))
  (status-require
    (sp-windowed-sinc-lp-hp
      (scm->sp-samples scm-in)
      (scm->sp-samples-length scm-in)
      (scm->sp-float scm-cutoff)
      (scm->sp-float scm-transition) is-high-pass &state (scm->sp-samples scm-out)))
  (if (not (scm-is-true scm-state)) (set scm-state (scm-from-sp-convolution-filter-state state)))
  (label exit
    (scm-from-status-return scm-state)))

(define
  (scm-sp-windowed-sinc-bp-br
    scm-out
    scm-in
    scm-cutoff-l
    scm-cutoff-h scm-transition-l scm-transition-h scm-is-reject scm-state scm-in-start scm-rest)
  (SCM SCM SCM SCM SCM SCM SCM SCM SCM SCM SCM)
  "uses a rest argument because c functions for guile are limited to 10 arguments"
  status-declare
  (declare
    state sp-convolution-filter-state-t*
    is-reject boolean
    in-start sp-sample-count-t
    in-count sp-sample-count-t
    out-start sp-sample-count-t)
  (set
    is-reject (scm-is-true scm-is-reject)
    state
    (if* (scm-is-true scm-state) (scm->sp-convolution-filter-state scm-state)
      0)
    in-start
    (if* (scm-is-undefined scm-in-start) 0
      (scm->sp-sample-count scm-in-start))
    in-count
    (if* (scm-is-null scm-rest) (- (scm->sp-samples-length scm-in) in-start)
      (scm->sp-sample-count (scm-first scm-rest)))
    out-start
    (if* (or (scm-is-null scm-rest) (scm-is-null (scm-tail scm-rest))) 0
      (scm->sp-sample-count (scm-first (scm-tail scm-rest)))))
  (status-require
    (sp-windowed-sinc-bp-br
      (+ in-start (scm->sp-samples scm-in))
      in-count
      (scm->sp-float scm-cutoff-l)
      (scm->sp-float scm-cutoff-h)
      (scm->sp-float scm-transition-l)
      (scm->sp-float scm-transition-h) is-reject &state (+ out-start (scm->sp-samples scm-out))))
  (if (not (scm-is-true scm-state)) (set scm-state (scm-from-sp-convolution-filter-state state)))
  (label exit
    (scm-from-status-return scm-state)))

(define (scm-sp-windowed-sinc-lp-hp-ir scm-cutoff scm-transition scm-is-high-pass)
  (SCM SCM SCM SCM)
  (declare
    ir sp-sample-t*
    ir-len sp-sample-count-t)
  status-declare
  (status-require
    (sp-windowed-sinc-lp-hp-ir
      (scm->sp-float scm-cutoff)
      (scm->sp-float scm-transition) (scm-is-true scm-is-high-pass) &ir &ir-len))
  (label exit
    (scm-from-status-return (scm-c-take-samples ir ir-len))))

(define
  (scm-sp-windowed-sinc-bp-br-ir
    scm-cutoff-l scm-cutoff-h scm-transition-l scm-transition-h scm-is-reject)
  (SCM SCM SCM SCM SCM SCM)
  (declare
    ir sp-sample-t*
    ir-len sp-sample-count-t)
  status-declare
  (status-require
    (sp-windowed-sinc-bp-br-ir
      (scm->sp-float scm-cutoff-l)
      (scm->sp-float scm-cutoff-h)
      (scm->sp-float scm-transition-l)
      (scm->sp-float scm-transition-h) (scm-is-true scm-is-reject) &ir &ir-len))
  (label exit
    (scm-from-status-return (scm-c-take-samples ir ir-len))))

(define (scm-c-sp-ir-f arguments out-ir out-len) (status-t void* sp-sample-t** sp-sample-count-t*)
  "an sp ir-f that takes a scm procedure and arguments it is to be called with as a list.
  the procedure should return a sample vector as a single argument"
  status-declare
  (declare
    scm-f SCM
    scm-arguments SCM
    scm-ir SCM
    ir-len sp-sample-count-t
    ir sp-sample-t*)
  (set
    scm-f (pointer-get (convert-type arguments SCM*))
    scm-arguments (pointer-get (+ 1 (convert-type arguments SCM*)))
    scm-ir (scm-apply-0 scm-f scm-arguments))
  (if (not (scm-samples? scm-ir))
    (status-set-both-goto status-group-sp-guile sp-status-id-undefined))
  (sc-comment "copy data as it will be owned by convolution-filter state")
  (set ir-len (scm->sp-samples-length scm-ir))
  (status-require (sph-helper-malloc (* ir-len (sizeof sp-sample-t)) &ir))
  (memcpy ir (scm->sp-samples scm-ir) (* ir-len (sizeof sp-sample-t)))
  (set
    *out-ir ir
    *out-len ir-len)
  (label exit
    (return status)))

(define (scm-sp-convolution-filter scm-out scm-in scm-ir-f scm-ir-f-arguments scm-state)
  (SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    state sp-convolution-filter-state-t*
    ir-f-arguments (array SCM ((* 2 (sizeof SCM)))))
  (set
    state
    (if* (scm-is-true scm-state) (scm->sp-convolution-filter-state scm-state)
      0)
    (array-get ir-f-arguments 0) scm-ir-f
    (array-get ir-f-arguments 1) scm-ir-f-arguments)
  (status-require
    (sp-convolution-filter
      (scm->sp-samples scm-in)
      (scm->sp-samples-length scm-in) scm-c-sp-ir-f ir-f-arguments 2 &state (scm->sp-samples scm-out)))
  (if (not (scm-is-true scm-state))
    (begin
      (set scm-state (scm-from-sp-convolution-filter-state state))))
  (label exit
    (scm-from-status-return scm-state)))

(define (debug-display-sample-array a len) (void sp-sample-t* sp-sample-count-t)
  "display a sample array in one line"
  (declare i sp-sample-count-t)
  (printf "%.17g" (array-get a 0))
  (for ((set i 1) (< i len) (set i (+ 1 i)))
    (printf " %.17g" (array-get a i)))
  (printf "\n"))

(define (scm-sp-fft scm-input) (SCM SCM)
  status-declare
  (declare
    i sp-sample-count-t
    input-len sp-sample-count-t
    input/output-real double*
    input/output-imag double*
    scm-output SCM)
  (scm-dynwind-begin 0)
  (set input-len (scm-c-vector-length scm-input))
  (status-require (sph-helper-malloc (* input-len (sizeof sp-sample-t)) &input/output-real))
  (status-require (sph-helper-malloc (* input-len (sizeof sp-sample-t)) &input/output-imag))
  (scm-dynwind-free input/output-real)
  (scm-dynwind-free input/output-imag)
  (for ((set i 0) (< i input-len) (set i (+ 1 i)))
    (set
      (array-get input/output-real i) (scm->sp-sample (scm-real-part (scm-c-vector-ref scm-input i)))
      (array-get input/output-imag i) (scm->sp-sample (scm-imag-part (scm-c-vector-ref scm-input i)))))
  (status-require (sp-fft input-len input/output-real input/output-imag))
  (set scm-output (scm-c-make-vector input-len SCM-BOOL-F))
  (for ((set i 0) (< i input-len) (set i (+ 1 i)))
    (scm-c-vector-set!
      scm-output
      i (scm-c-make-rectangular (array-get input/output-real i) (array-get input/output-imag i))))
  (label exit
    (scm-from-status-dynwind-end-return scm-output)))

(define (scm-sp-ffti scm-input) (SCM SCM)
  status-declare
  (declare
    i sp-sample-count-t
    input-len sp-sample-count-t
    input/output-real double*
    input/output-imag double*
    scm-output SCM)
  (scm-dynwind-begin 0)
  (set input-len (scm-c-vector-length scm-input))
  (status-require (sph-helper-malloc (* input-len (sizeof sp-sample-t)) &input/output-real))
  (status-require (sph-helper-malloc (* input-len (sizeof sp-sample-t)) &input/output-imag))
  (scm-dynwind-free input/output-real)
  (scm-dynwind-free input/output-imag)
  (for ((set i 0) (< i input-len) (set i (+ 1 i)))
    (set
      (array-get input/output-real i) (scm->sp-sample (scm-real-part (scm-c-vector-ref scm-input i)))
      (array-get input/output-imag i) (scm->sp-sample (scm-imag-part (scm-c-vector-ref scm-input i)))))
  (status-require (sp-ffti input-len input/output-real input/output-imag))
  (set scm-output (scm-c-make-vector input-len SCM-BOOL-F))
  (for ((set i 0) (< i input-len) (set i (+ 1 i)))
    (scm-c-vector-set!
      scm-output
      i (scm-c-make-rectangular (array-get input/output-real i) (array-get input/output-imag i))))
  (label exit
    (scm-from-status-dynwind-end-return scm-output)))

(define (scm-sp-file-open scm-path mode scm-channel-count scm-sample-rate) (SCM SCM SCM SCM SCM)
  status-declare
  (declare
    path uint8-t*
    scm-result SCM
    file sp-file-t*)
  (scm-dynwind-begin 0)
  (set path (scm->locale-string scm-path))
  (scm-dynwind-free path)
  (set file (scm-gc-malloc-pointerless (sizeof sp-file-t) "sp-file"))
  (if (not file) (status-set-both-goto sp-status-group-sp sp-status-id-memory))
  (status-require
    (sp-file-open
      path
      (scm->uint8 mode)
      (if* (scm-is-undefined scm-channel-count) 0
        (scm->sp-channel-count scm-channel-count))
      (if* (scm-is-undefined scm-sample-rate) 0
        (scm->sp-sample-rate scm-sample-rate))
      file))
  (set scm-result (scm-from-sp-file file))
  (label exit
    (scm-from-status-dynwind-end-return scm-result)))

(define (scm-f64vector-sum a start end) (SCM SCM SCM SCM)
  (return
    (scm-from-double
      (f64-sum
        (+
          (if* (scm-is-undefined start) 0
            (scm->size-t start))
          (convert-type (SCM-BYTEVECTOR-CONTENTS a) double*))
        (*
          (if* (scm-is-undefined end) (/ (SCM-BYTEVECTOR-LENGTH a) (sizeof double))
            (- end (+ 1 start)))
          (sizeof double))))))

(define (scm-f32vector-sum a start end) (SCM SCM SCM SCM)
  (return
    (scm-from-double
      (f32-sum
        (+
          (if* (scm-is-undefined start) 0
            (scm->size-t start))
          (convert-type (SCM-BYTEVECTOR-CONTENTS a) float*))
        (*
          (if* (scm-is-undefined end) (/ (SCM-BYTEVECTOR-LENGTH a) (sizeof float))
            (- end (+ 1 start)))
          (sizeof float))))))

(define (scm-f64-nearly-equal? a b margin) (SCM SCM SCM SCM)
  (return (scm-from-bool (f64-nearly-equal (scm->double a) (scm->double b) (scm->double margin)))))

(define (scm-sp-file-read scm-file scm-sample-count) (SCM SCM SCM)
  status-declare
  (declare
    channel-count sp-channel-count-t
    channel-data sp-sample-t**
    file sp-file-t*
    result-sample-count sp-sample-count-t
    sample-count sp-sample-count-t
    scm-result SCM)
  (set
    channel-data 0
    file (scm->sp-file scm-file)
    sample-count (scm->sp-sample-count scm-sample-count)
    channel-count file:channel-count)
  (status-require (sp-block-alloc channel-count sample-count &channel-data))
  (status-require (sp-file-read file sample-count channel-data &result-sample-count))
  (set scm-result (scm-c-take-channel-data channel-data channel-count sample-count))
  (label exit
    (if status-is-failure
      (begin
        (if channel-data (sp-block-free channel-data channel-count))
        (if (= sp-status-id-eof status.id)
          (set
            status.id status-id-success
            scm-result SCM-EOF-VAL))))
    (scm-from-status-return scm-result)))

(define (scm-sp-file-write scm-file scm-channel-data scm-sample-count) (SCM SCM SCM SCM)
  status-declare
  (declare
    channel-data sp-sample-t**
    channel-count sp-channel-count-t
    result-sample-count sp-sample-count-t
    sample-count sp-sample-count-t
    scm-result SCM)
  (set
    channel-data 0
    sample-count (scm->sp-sample-count scm-sample-count))
  (status-require (scm->channel-data scm-channel-data &channel-count &channel-data))
  (status-require
    (sp-file-write (scm->sp-file scm-file) channel-data sample-count &result-sample-count))
  (set scm-result (scm-from-sp-sample-count result-sample-count))
  (scm-remember-upto-here-1 scm-channel-data)
  (label exit
    (if channel-data (free channel-data))
    (scm-from-status-return scm-result)))

(define (scm-sp-window-blackman a width) (SCM SCM SCM)
  (scm-from-sp-float (sp-window-blackman (scm->sp-float a) (scm->sp-sample-count width))))

(define (scm-sp-convolution-filter-state-finalize a) (void SCM)
  (sp-convolution-filter-state-free (scm->sp-convolution-filter-state a)))

(pre-define
  (scm->sp-path-segment-count a) (scm->uint16 a)
  (scm->sp-path-value a) (scm->double a))

; unfinished binding to sp-path

#;(define (scm-sp-path-new scm-segments) (SCM SCM)
  status-declare
  (declare
    scm-path SCM
    scm-s SCM
    i sp-path-segment-count-t
    s scm-segment-t
    p sp-path-point-t
    scm-interpolator SCM
    scm-points SCM
    scm-point SCM
    scm-options SCM
    sp-path-count-t points-len
    path sp-path-t*
    segment-len sp-path-segment-count-t
    segments sp-path-segment-t*
    out sp-path-value-t*)
  (set
    segments-len (scm->sp-path-segment-count (scm-length scm-segments))
    path (scm-gc-malloc-pointerless (sizeof sp-path-t) "sp-path-t"))
  (if (not path) (status-set-both-goto sp-status-group-sp sp-status-id-memory))
  (set segments (scm-gc-malloc-pointerless (* segments-len (sizeof sp-segment-t)) "sp-segments-t"))
  (if (not segments) (status-set-both-goto sp-status-group-sp sp-status-id-memory))
  (for
    ( (set i 0)
      (< i segments-len)
      (set
        i (+ 1 i)
        scm-segments (scm-tail scm-segments)))
    (set
      scm-s (scm-first scm-segments)
      scm-interpolator (scm-c-vector-ref scm-s 0)
      scm-points (scm-c-vector-ref scm-s 1)
      scm-options (scm-c-vector-ref scm-s 2))
    (case scm-is-eq? scm-interpolator
      (scm-symbol-line (set s.interpolator sp-path-i-line))
      (scm-symbol-move (set s.interpolator sp-path-i-move))
      (scm-symbol-constant (set s.interpolator sp-path-i-constant))
      (scm-symbol-bezier (set s.interpolator sp-path-i-bezier))
      (scm-symbol-path
        (set
          s.interpolator sp-path-i-path
          s.options (scm->sp-path (scm-assq-ref scm-options (scm-from-latin1-symbol "path"))))))
    (set points-len (sp-path-interpolator-points-len s.interpolator))
    (for ((set i 0) (< i points-len) (set i (+ 1 i)))
      (set
        scm-point (scm-c-vector-ref scm-points i)
        p.x (scm->sp-sample-count (scm-c-vector-ref scm-point 0))
        p.y (scm->sp-path-value (scm-c-vector-ref scm-point 1))
        (array-get s.points i) p))
    (set (array-get segments i) s))
  ;(status-require (sp-path-new segments-len segments path))
  ;(set scm-path (scm-from-sp-path path))
  (set scm-path #t)
  (label exit
    (scm-from-status-return scm-path)))

(define (scm-sp-path-finalize a) (void SCM) (sp-path-free (scm->sp-path a)))

(define
  (scm-sp-moving-average
    scm-out scm-in scm-prev scm-next scm-radius scm-in-start scm-in-count scm-out-start)
  (SCM SCM SCM SCM SCM SCM SCM SCM SCM)
  "start/end are indexes counted from 0"
  status-declare
  (declare
    in sp-sample-t*
    in-end sp-sample-t*
    prev sp-sample-t*
    prev-end sp-sample-t*
    next sp-sample-t*
    next-end sp-sample-t*
    in-start sp-sample-count-t
    in-count sp-sample-count-t
    out-start sp-sample-count-t)
  (set
    in (scm->sp-samples scm-in)
    in-end (+ (scm->sp-samples-length scm-in) in)
    in-start
    (if* (scm-is-undefined scm-in-start) 0
      (scm->sp-sample-count scm-in-start))
    in-count
    (if* (scm-is-undefined scm-in-count) (- in-end in in-start)
      (scm->sp-sample-count scm-in-count))
    out-start
    (if* (scm-is-undefined scm-out-start) 0
      (scm->sp-sample-count scm-out-start)))
  (if (scm-is-true scm-prev)
    (set
      prev (scm->sp-samples scm-prev)
      prev-end (+ (scm->sp-samples-length scm-prev) prev))
    (set
      prev 0
      prev-end 0))
  (if (scm-is-true scm-next)
    (set
      next (scm->sp-samples scm-next)
      next-end (+ (scm->sp-samples-length scm-next) next))
    (set
      next 0
      next-end 0))
  (status-require
    (sp-moving-average
      in
      in-end
      (+ in-start in)
      (+ (+ in-start in-count) in)
      prev
      prev-end
      next next-end (scm->sp-sample-count scm-radius) (+ out-start (scm->sp-samples scm-out))))
  (label exit
    (scm-from-status-return SCM-UNSPECIFIED)))

(define (scm->sp-fm-synth-config scm-config channel-count config-len config)
  (status-t SCM sp-channel-count-t sp-fm-synth-count-t* sp-fm-synth-operator-t**)
  "memory will be managed by the guile garbage collector"
  status-declare
  (declare
    channel-i sp-channel-count-t
    c-len sp-sample-count-t
    c sp-fm-synth-operator-t*
    op sp-fm-synth-operator-t*
    i sp-fm-synth-count-t
    scm-op SCM)
  (set
    c-len (scm->sp-fm-synth-count (scm-length scm-config))
    c (scm-gc-malloc-pointerless (* c-len (sizeof sp-fm-synth-operator-t)) "sp-fm-synth-config"))
  (if (not c) (status-set-both-goto sp-status-group-sp sp-status-id-memory))
  (for
    ( (set i 0) (< i c-len)
      (set
        i (+ 1 i)
        scm-config (scm-tail scm-config)))
    (set
      scm-op (scm-first scm-config)
      op (+ c i)
      op:modifies (scm->sp-fm-synth-count (scm-c-vector-ref scm-op 0)))
    (for ((set channel-i 0) (< channel-i channel-count) (set channel-i (+ 1 channel-i)))
      (set
        (array-get op:amplitude channel-i)
        (scm->sp-samples (scm-c-vector-ref (scm-c-vector-ref scm-op 1) channel-i))
        (array-get op:wavelength channel-i)
        (scm->sp-sample-counts (scm-c-vector-ref (scm-c-vector-ref scm-op 2) channel-i))
        (array-get op:phase-offset channel-i)
        (scm->sp-sample-count (scm-c-vector-ref (scm-c-vector-ref scm-op 3) channel-i)))))
  (set
    *config-len c-len
    *config c)
  (label exit
    (return status)))

(define
  (scm-sp-fm-synth
    scm-out scm-out-start scm-channel-count scm-start scm-duration scm-config scm-state)
  (SCM SCM SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    channel-count sp-channel-count-t
    config-len sp-fm-synth-count-t
    config sp-fm-synth-operator-t*
    duration sp-sample-count-t
    i sp-channel-count-t
    out (array sp-sample-t* sp-fm-synth-channel-limit)
    out-start sp-sample-count-t
    state sp-sample-count-t*)
  (set
    channel-count (scm->sp-channel-count scm-channel-count)
    duration (scm->sp-sample-count scm-duration)
    out-start (scm->sp-sample-count scm-out-start)
    state
    (if* (scm-is-true scm-state) (scm->sp-sample-counts scm-state)
      0))
  (for
    ( (set i 0) (< i channel-count)
      (set
        i (+ 1 i)
        scm-out (scm-tail scm-out)))
    (set (array-get out i) (+ out-start (scm->sp-samples (scm-first scm-out)))))
  (status-require (scm->sp-fm-synth-config scm-config channel-count &config-len &config))
  (status-require
    (sp-fm-synth
      out channel-count (scm->sp-sample-count scm-start) duration config-len config &state))
  (if (not (scm-is-true scm-state))
    (set scm-state (scm-c-take-sample-counts state (* config-len channel-count))))
  (label exit
    (scm-from-status-return scm-state)))

(define (scm->sp-asynth-config scm-config channel-count config-len config)
  (status-t SCM sp-channel-count-t sp-asynth-count-t* sp-asynth-partial-t**)
  "memory will be managed by the guile garbage collector"
  status-declare
  (declare
    channel-i sp-channel-count-t
    c-len sp-sample-count-t
    c sp-asynth-partial-t*
    prt sp-asynth-partial-t*
    i sp-asynth-count-t
    scm-prt SCM)
  (set
    c-len (scm->sp-asynth-count (scm-length scm-config))
    c (scm-gc-malloc-pointerless (* c-len (sizeof sp-asynth-partial-t)) "sp-asynth-config"))
  (if (not c) (status-set-both-goto sp-status-group-sp sp-status-id-memory))
  (for
    ( (set i 0) (< i c-len)
      (set
        i (+ 1 i)
        scm-config (scm-tail scm-config)))
    (set
      scm-prt (scm-first scm-config)
      prt (+ c i)
      prt:start (scm->sp-sample-count (scm-c-vector-ref scm-prt 0))
      prt:end (scm->sp-sample-count (scm-c-vector-ref scm-prt 1)))
    (for ((set channel-i 0) (< channel-i channel-count) (set channel-i (+ 1 channel-i)))
      (set
        (array-get prt:amplitude channel-i)
        (scm->sp-samples (scm-c-vector-ref (scm-c-vector-ref scm-prt 2) channel-i))
        (array-get prt:wavelength channel-i)
        (scm->sp-sample-counts (scm-c-vector-ref (scm-c-vector-ref scm-prt 3) channel-i))
        (array-get prt:phase-offset channel-i)
        (scm->sp-sample-count (scm-c-vector-ref (scm-c-vector-ref scm-prt 4) channel-i)))))
  (set
    *config-len c-len
    *config c)
  (label exit
    (return status)))

(define
  (scm-sp-asynth
    scm-out scm-out-start scm-channel-count scm-start scm-duration scm-config scm-state)
  (SCM SCM SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    channel-count sp-channel-count-t
    config-len sp-asynth-count-t
    config sp-asynth-partial-t*
    duration sp-sample-count-t
    i sp-channel-count-t
    out (array sp-sample-t* sp-asynth-channel-limit)
    out-start sp-sample-count-t
    state sp-sample-count-t*)
  (set
    channel-count (scm->sp-channel-count scm-channel-count)
    duration (scm->sp-sample-count scm-duration)
    out-start (scm->sp-sample-count scm-out-start)
    state
    (if* (scm-is-true scm-state) (scm->sp-sample-counts scm-state)
      0))
  (for
    ( (set i 0) (< i channel-count)
      (set
        i (+ 1 i)
        scm-out (scm-tail scm-out)))
    (set (array-get out i) (+ out-start (scm->sp-samples (scm-first scm-out)))))
  (status-require (scm->sp-asynth-config scm-config channel-count &config-len &config))
  (status-require
    (sp-asynth out channel-count (scm->sp-sample-count scm-start) duration config-len config &state))
  (if (not (scm-is-true scm-state))
    (set scm-state (scm-c-take-sample-counts state (* config-len channel-count))))
  (label exit
    (scm-from-status-return scm-state)))

(pre-define (define-scm-sp-state-variable-filter suffix)
  (define
    ( (pre-concat scm-sp-state-variable-filter_ suffix)
      scm-out scm-out-start scm-in scm-in-start scm-in-count scm-cutoff scm-q-factor scm-state)
    (SCM SCM SCM SCM SCM SCM SCM SCM SCM)
    ( (pre-concat sp-state-variable-filter_ suffix)
      (+ (scm->sp-sample-count scm-out-start) (scm->sp-samples scm-out))
      (+ (scm->sp-sample-count scm-in-start) (scm->sp-samples scm-in))
      (scm->sp-sample-count scm-in-count)
      (scm->sp-float scm-cutoff) (scm->sp-float scm-q-factor) (scm->sp-samples scm-state))
    (return scm-state)))

(define-scm-sp-state-variable-filter lp)
(define-scm-sp-state-variable-filter hp)
(define-scm-sp-state-variable-filter bp)
(define-scm-sp-state-variable-filter br)
(define-scm-sp-state-variable-filter peak)
(define-scm-sp-state-variable-filter all)

(define (sp-guile-init) void
  (declare
    type-slots SCM
    scm-symbol-data SCM
    m SCM)
  (sp-initialise)
  (set
    m (scm-c-resolve-module "sph sp")
    scm-rnrs-raise (scm-c-public-ref "rnrs exceptions" "raise")
    scm-symbol-data (scm-from-latin1-symbol "data")
    scm-symbol-line (scm-from-latin1-symbol "line")
    scm-symbol-bezier (scm-from-latin1-symbol "bezier")
    scm-symbol-constant (scm-from-latin1-symbol "constant")
    scm-symbol-move (scm-from-latin1-symbol "move")
    scm-symbol-path (scm-from-latin1-symbol "path")
    type-slots (scm-list-1 scm-symbol-data)
    scm-type-file (scm-make-foreign-object-type (scm-from-latin1-symbol "sp-file") type-slots 0)
    scm-type-convolution-filter-state
    (scm-make-foreign-object-type
      (scm-from-latin1-symbol "sp-convolution-filter-state")
      type-slots scm-sp-convolution-filter-state-finalize)
    scm-type-sp-path
    (scm-make-foreign-object-type
      (scm-from-latin1-symbol "sp-path") type-slots scm-sp-path-finalize))
  (scm-c-module-define m "sp-file-mode-read" (scm-from-uint8 sp-file-mode-read))
  (scm-c-module-define m "sp-file-mode-write" (scm-from-uint8 sp-file-mode-write))
  (scm-c-module-define m "sp-file-mode-read-write" (scm-from-uint8 sp-file-mode-read-write))
  scm-c-define-procedure-c-init
  (scm-c-define-procedure-c
    "sp-state-variable-filter-lp"
    8
    0
    0
    scm-sp-state-variable-filter-lp
    "out out-start in in-start in-count cutoff q-factor state -> unspecified")
  (scm-c-define-procedure-c
    "sp-state-variable-filter-hp"
    8
    0
    0
    scm-sp-state-variable-filter-hp
    "out out-start in in-start in-count cutoff q-factor state -> unspecified")
  (scm-c-define-procedure-c
    "sp-state-variable-filter-bp"
    8
    0
    0
    scm-sp-state-variable-filter-bp
    "out out-start in in-start in-count cutoff q-factor state -> unspecified")
  (scm-c-define-procedure-c
    "sp-state-variable-filter-br"
    8
    0
    0
    scm-sp-state-variable-filter-br
    "out out-start in in-start in-count cutoff q-factor state -> unspecified")
  (scm-c-define-procedure-c
    "sp-state-variable-filter-peak"
    8
    0
    0
    scm-sp-state-variable-filter-peak
    "out out-start in in-start in-count cutoff q-factor state -> unspecified")
  (scm-c-define-procedure-c
    "sp-state-variable-filter-all"
    8
    0
    0
    scm-sp-state-variable-filter-all
    "out out-start in in-start in-count cutoff q-factor state -> unspecified")
  (scm-c-define-procedure-c
    "sp-fm-synth"
    7 0 0 scm-sp-fm-synth "out out-start channel-count start duration config state -> state")
  (scm-c-define-procedure-c
    "sp-asynth"
    7 0 0 scm-sp-asynth "out out-start channel-count start duration config state -> state")
  (scm-c-define-procedure-c
    "sp-convolve" 4 1 0 scm-sp-convolve "out a b carryover [carryover-len] -> unspecified")
  (scm-c-define-procedure-c "sp-window-blackman" 2 0 0 scm-sp-window-blackman "real width -> real")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-lp-hp"
    6
    0
    0
    scm-sp-windowed-sinc-lp-hp
    "out in cutoff transition is-high-pass state -> state
    samples samples real:0..0.5 real:0..0.5 boolean convolution-filter-state -> unspecified
    apply a windowed-sinc low-pass or high-pass filter to \"in\", write to \"out\" and return
    an updated state object.
    if state object is false, create a new state.
    cutoff and transition are as a fraction of the sampling-rate")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-bp-br"
    8
    1
    1
    scm-sp-windowed-sinc-bp-br
    "out in cutoff-l cutoff-h transition-l transition-h is-reject state in-start in-end out-start -> state
    samples samples real:0..0.5 real real:0..0.5 real boolean convolution-filter-state -> unspecified
    like sp-windowed-sinc-lp-hp but as a band-pass or band-reject filter.
    if state is false then a new state object will be returned.
    optimised to become a low-pass or high-pass at the ends")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-lp-hp-ir"
    3
    0
    0
    scm-sp-windowed-sinc-lp-hp-ir
    "real real boolean -> samples
    cutoff transition is-high-pass -> ir
    get an impulse response kernel for a low-pass or high-pass filter")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-bp-br-ir"
    5
    0
    0
    scm-sp-windowed-sinc-bp-br-ir
    "real real real real boolean -> samples
    cutoff-l cutoff-h transition-l transition-h is-reject -> ir
    get an impulse response kernel for a band-pass or band-reject filter")
  (scm-c-define-procedure-c
    "sp-convolution-filter"
    5
    0
    0
    scm-sp-convolution-filter
    "out in ir-f ir-f-arguments state -> state
     samples samples procedure list false/sp-convolution-filter-state -> sp-convolution-filter-state
     if state is false then a new state object will be returned")
  (scm-c-define-procedure-c
    "sp-moving-average"
    5
    3
    0
    scm-sp-moving-average
    "out in previous next radius [in-start in-count out-start] -> unspecified
     samples samples samples samples integer [integer integer integer] -> unspecified")
  (scm-c-define-procedure-c "sp-fft" 1 0 0 scm-sp-fft "#(complex ...) -> #(complex ...)")
  (scm-c-define-procedure-c
    "sp-ffti"
    1 0 0 scm-sp-ffti
    "#(complex ...) -> #(complex ...)
    inverse discrete fourier transform")
  (scm-c-define-procedure-c
    "sp-file-open" 2 2 0 scm-sp-file-open "path mode [channel-count sample-rate] -> sp-file")
  (scm-c-define-procedure-c "sp-file-close" 1 0 0 scm-sp-file-close "sp-file -> boolean")
  (scm-c-define-procedure-c "sp-file-input?" 1 0 0 scm-sp-file-input? "sp-file -> boolean")
  (scm-c-define-procedure-c "sp-file-position?" 1 0 0 scm-sp-file-position? "sp-file -> boolean")
  (scm-c-define-procedure-c "sp-file-position" 1 0 0 scm-sp-file-position "sp-file -> integer")
  (scm-c-define-procedure-c
    "sp-file-channel-count" 1 0 0 scm-sp-file-channel-count "sp-file -> integer")
  (scm-c-define-procedure-c
    "sp-file-sample-rate" 1 0 0 scm-sp-file-sample-rate "sp-file -> integer")
  (scm-c-define-procedure-c
    "f32vector-sum" 1 2 0 scm-f32vector-sum "f32vector [start end] -> number")
  (scm-c-define-procedure-c
    "f64vector-sum" 1 2 0 scm-f64vector-sum "f64vector [start end] -> number")
  (scm-c-define-procedure-c
    "f64-nearly-equal?"
    3 0 0 scm-f64-nearly-equal?
    "a b margin -> boolean
    number number number -> boolean")
  (scm-c-define-procedure-c
    "sp-file-read"
    2 0 0 scm-sp-file-read "sp-file integer:sample-count -> (sample-vector ...):channel-data")
  (scm-c-define-procedure-c
    "sp-file-write"
    3
    0
    0
    scm-sp-file-write
    "sp-file (sample-vector ...):channel-data [integer:sample-count] -> unspecified
  write sample data to the channels of file")
  (scm-c-define-procedure-c
    "sp-file-position-set"
    2
    0
    0
    scm-sp-file-position-set
    "sp-file integer:sample-offset -> boolean
    sample-offset can be negative, in which case it is from the end of the file"))