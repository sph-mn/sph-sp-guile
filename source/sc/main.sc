(pre-include "./helper.c" "./foreign/sph/float.c")
(define-sp-sine! scm-sp-sine! sp-sine)
(define-sp-sine! scm-sp-sine-lq! sp-sine)

(define (scm-sp-port-channel-count scm-a) (SCM SCM)
  (return (scm-from-sp-channel-count (: (scm->sp-port scm-a) channel-count))))

(define (scm-sp-port-sample-rate scm-a) (SCM SCM)
  (return (scm-from-sp-sample-rate (: (scm->sp-port scm-a) sample-rate))))

(define (scm-sp-port-position? scm-a) (SCM SCM)
  (return (scm-from-bool (bit-and sp-port-bit-position (: (scm->sp-port scm-a) flags)))))

(define (scm-sp-port-input? scm-a) (SCM SCM)
  (return (scm-from-bool (bit-and sp-port-bit-input (: (scm->sp-port scm-a) flags)))))

(define (scm-sp-port-position scm-a) (SCM SCM)
  "returns the current port position offset in number of samples"
  (declare position sp-sample-count-t)
  (sp-port-position (scm->sp-port scm-a) &position)
  (return (scm-from-sp-sample-count position)))

(define (scm-sp-port-close a) (SCM SCM)
  status-declare
  (set status (sp-port-close (scm->sp-port a)))
  (scm-from-status-return SCM-UNSPECIFIED))

(define (scm-sp-port-position-set scm-port scm-sample-offset) (SCM SCM SCM)
  status-declare
  (set status (sp-port-position-set (scm->sp-port scm-port) (scm->size-t scm-sample-offset)))
  (scm-from-status-return SCM-UNSPECIFIED))

(define (scm-sp-convolve! result a b carryover) (SCM SCM SCM SCM SCM)
  (declare
    a-len sp-sample-count-t
    b-len sp-sample-count-t
    c-len sp-sample-count-t)
  (set
    a-len (scm->sp-samples-length a)
    b-len (scm->sp-samples-length b)
    c-len (scm->sp-samples-length b))
  (if (< c-len b-len)
    (scm-c-error
      status-group-sp-guile
      "invalid-argument-size"
      "carryover argument bytevector must be at least as large as the second argument bytevector"))
  (sp-convolve
    (scm->sp-samples a)
    a-len (scm->sp-samples b) b-len c-len (scm->sp-samples carryover) (scm->sp-samples result))
  (return SCM-UNSPECIFIED))

(define (scm-sp-windowed-sinc-state-create scm-sample-rate scm-freq scm-transition scm-state)
  (SCM SCM SCM SCM SCM)
  status-declare
  (declare
    state-null (struct-variable sp-windowed-sinc-state-t 0)
    state sp-windowed-sinc-state-t*)
  (if (and (not SCM-UNDEFINED) (scm-is-true scm-state))
    (set state (scm->sp-windowed-sinc scm-state))
    (set
      state (scm-gc-malloc-pointerless (sizeof sp-windowed-sinc-state-t) "windowed-sinc-state")
      *state state-null))
  (status-require
    (sp-windowed-sinc-state-create
      (scm->sp-sample-rate scm-sample-rate)
      (scm->sp-float scm-freq) (scm->sp-float scm-transition) &state))
  (set scm-state (scm-from-sp-windowed-sinc state))
  (label exit
    (scm-from-status-return scm-state)))

(define
  (scm-sp-windowed-sinc! scm-result scm-source scm-sample-rate scm-freq scm-transition scm-state)
  (SCM SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare state sp-windowed-sinc-state-t*)
  (if (scm-is-undefined scm-state)
    (set scm-state
      (scm-sp-windowed-sinc-state-create scm-sample-rate scm-freq scm-transition scm-state)))
  (set
    state (scm->sp-windowed-sinc scm-state)
    status
    (sp-windowed-sinc
      (scm->sp-samples scm-source)
      (scm->sp-samples-length scm-source)
      (scm->sp-sample-rate scm-sample-rate)
      (scm->sp-float scm-freq) (scm->sp-float scm-transition) &state (scm->sp-samples scm-result)))
  (scm-from-status-return scm-state))

(define
  (scm-sp-moving-average! scm-result scm-source scm-prev scm-next scm-radius scm-start scm-end)
  (SCM SCM SCM SCM SCM SCM SCM SCM)
  "start/end are indexes counted from 0"
  status-declare
  (declare
    source-len sp-sample-count-t
    prev-len sp-sample-count-t
    next-len sp-sample-count-t
    prev sp-sample-t*
    next sp-sample-t*
    start sp-sample-count-t
    end sp-sample-count-t)
  (set
    source-len (scm->sp-samples-length scm-source)
    start
    (if* (and (not (scm-is-undefined scm-start)) (scm-is-true scm-start))
      (scm->sp-sample-count scm-start)
      0)
    end
    (if* (and (not (scm-is-undefined scm-end)) (scm-is-true scm-end))
      (scm->sp-sample-count scm-end)
      (- source-len 1)))
  (if (scm-is-true scm-prev)
    (set
      prev (scm->sp-samples scm-prev)
      prev-len (scm->sp-samples-length scm-prev))
    (set
      prev 0
      prev-len 0))
  (if (scm-is-true scm-next)
    (set
      next (scm->sp-samples scm-next)
      next-len (scm->sp-samples-length scm-next))
    (set
      next 0
      next-len 0))
  (status-require
    (sp-moving-average
      (scm->sp-samples scm-source)
      source-len
      prev
      prev-len next next-len (scm->sp-sample-count scm-radius) start end (scm->sp-samples scm-result)))
  (label exit
    (scm-from-status-return SCM-UNSPECIFIED)))

(define (scm-sp-fftr scm-source) (SCM SCM)
  status-declare
  (declare
    result-len sp-sample-count-t
    scm-result SCM)
  (set
    result-len (/ (* 3 (scm->sp-samples-length scm-source)) 2)
    scm-result (scm-c-make-sp-samples result-len))
  (status-require
    (sp-fftr
      result-len
      (scm->sp-samples scm-source) (scm->sp-samples-length scm-source) (scm->sp-samples scm-result)))
  (label exit
    (scm-from-status-return scm-result)))

(define (scm-sp-fftri scm-source) (SCM SCM)
  status-declare
  (declare
    result-len sp-sample-count-t
    scm-result SCM)
  (set
    result-len (* (- (scm->sp-samples-length scm-source) 1) 2)
    scm-result (scm-c-make-sp-samples result-len))
  (status-require
    (sp-fftri
      result-len
      (scm->sp-samples scm-source) (scm->sp-samples-length scm-source) (scm->sp-samples scm-result)))
  (label exit
    (scm-from-status-return scm-result)))

(define (scm-sp-alsa-open scm-device-name scm-mode scm-channel-count scm-sample-rate scm-latency)
  (SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    device-name uint8-t*
    scm-result SCM
    port sp-port-t*)
  (scm-dynwind-begin 0)
  (set device-name (scm->locale-string scm-device-name))
  (scm-dynwind-free device-name)
  (set port (scm-gc-malloc-pointerless (sizeof sp-port-t) "sp-port"))
  (status-require
    (sp-alsa-open
      device-name
      (scm->uint8 scm-mode)
      (scm->sp-channel-count scm-channel-count)
      (scm->sp-sample-rate scm-sample-rate) (scm->sp-sample-count scm-latency) port))
  (set scm-result (scm-from-sp-port port))
  (label exit
    (scm-from-status-dynwind-end-return scm-result)))

(define (scm-sp-file-open scm-path mode scm-channel-count scm-sample-rate) (SCM SCM SCM SCM SCM)
  status-declare
  (declare
    path uint8-t*
    scm-result SCM
    port sp-port-t*)
  (scm-dynwind-begin 0)
  (set path (scm->locale-string scm-path))
  (scm-dynwind-free path)
  (set port (scm-gc-malloc-pointerless (sizeof sp-port-t) "sp-port"))
  (status-require
    (sp-file-open
      path
      (scm->uint8 mode)
      (if* (scm-is-undefined scm-channel-count) 0
        (scm->sp-channel-count scm-channel-count))
      (if* (scm-is-undefined scm-sample-rate) 0
        (scm->sp-sample-rate scm-sample-rate))
      port))
  (set scm-result (scm-from-sp-port port))
  (label exit
    (scm-from-status-dynwind-end-return scm-result)))

(define (scm-f64vector-sum a start end) (SCM SCM SCM SCM)
  (return
    (scm-from-double
      (f64-sum
        (+
          (if* (scm-is-undefined start) 0
            (scm->size-t start))
          (convert-type (SCM-BYTEVECTOR-CONTENTS a) f64*))
        (*
          (if* (scm-is-undefined end) (/ (SCM-BYTEVECTOR-LENGTH a) (sizeof f64))
            (- end (+ 1 start)))
          (sizeof f64))))))

(define (scm-f32vector-sum a start end) (SCM SCM SCM SCM)
  (return
    (scm-from-double
      (f32-sum
        (+
          (if* (scm-is-undefined start) 0
            (scm->size-t start))
          (convert-type (SCM-BYTEVECTOR-CONTENTS a) f32*))
        (*
          (if* (scm-is-undefined end) (/ (SCM-BYTEVECTOR-LENGTH a) (sizeof f32))
            (- end (+ 1 start)))
          (sizeof f32))))))

(define (scm-f64-nearly-equal? a b margin) (SCM SCM SCM SCM)
  (return (scm-from-bool (f64-nearly-equal (scm->double a) (scm->double b) (scm->double margin)))))

(define (scm-sp-port-read scm-port scm-sample-count) (SCM SCM SCM)
  status-declare
  (declare
    channel-count sp-channel-count-t
    channel-data sp-sample-t**
    port sp-port-t*
    result-sample-count sp-sample-count-t
    sample-count sp-sample-count-t
    scm-result SCM)
  (set
    channel-data 0
    port (scm->sp-port scm-port)
    sample-count (scm->sp-sample-count scm-sample-count)
    channel-count port:channel-count)
  (status-require (sp-alloc-channel-array channel-count sample-count &channel-data))
  (status-require (sp-port-read port sample-count channel-data &result-sample-count))
  (set scm-result (scm-c-take-channel-data channel-data channel-count sample-count))
  (label exit
    (if status-is-failure
      (begin
        (if channel-data (sp-channel-data-free channel-data channel-count))
        (if (= sp-status-id-eof status.id)
          (set
            status.id status-id-success
            scm-result SCM-EOF-VAL))))
    (scm-from-status-return scm-result)))

(define (scm-sp-port-write scm-port scm-channel-data scm-sample-count) (SCM SCM SCM SCM)
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
    (sp-port-write (scm->sp-port scm-port) channel-data sample-count &result-sample-count))
  (set scm-result (scm-from-sp-sample-count result-sample-count))
  (scm-remember-upto-here-1 scm-channel-data)
  (label exit
    (if channel-data (free channel-data))
    (scm-from-status-return scm-result)))

(define (scm-sp-sample-format) SCM
  (case = sp-sample-format
    (sp-sample-format-f64 (return (scm-from-latin1-symbol "f64")))
    (sp-sample-format-f32 (return (scm-from-latin1-symbol "f32")))
    (sp-sample-format-int32 (return (scm-from-latin1-symbol "int32")))
    (sp-sample-format-int16 (return (scm-from-latin1-symbol "int16")))
    (sp-sample-format-int8 (return (scm-from-latin1-symbol "int8")))))

(define (sp-guile-init) void
  (declare
    type-slots SCM
    scm-symbol-data SCM
    m SCM)
  (set
    m (scm-c-resolve-module "sph sp")
    scm-rnrs-raise (scm-c-public-ref "rnrs exceptions" "raise")
    scm-symbol-data (scm-from-latin1-symbol "data")
    type-slots (scm-list-1 scm-symbol-data)
    scm-type-port (scm-make-foreign-object-type (scm-from-latin1-symbol "sp-port") type-slots 0)
    scm-type-windowed-sinc
    (scm-make-foreign-object-type (scm-from-latin1-symbol "sp-windowed-sinc") type-slots 0))
  (scm-c-module-define m "sp-sample-format" (scm-sp-sample-format sp-sample-format))
  (scm-c-module-define m "sp-port-mode-read" (scm-from-uint8 sp-port-mode-read))
  (scm-c-module-define m "sp-port-mode-write" (scm-from-uint8 sp-port-mode-write))
  (scm-c-module-define m "sp-port-mode-read-write" (scm-from-uint8 sp-port-mode-read-write))
  scm-c-define-procedure-c-init
  (scm-c-define-procedure-c
    "sp-sine!"
    6
    0
    0
    scm-sp-sine!
    "data len sample-duration freq phase amp -> unspecified
    sample-vector integer integer rational rational rational rational")
  (scm-c-define-procedure-c
    "sp-sine-lq!"
    6
    0
    0
    scm-sp-sine-lq!
    "data len sample-duration freq phase amp  -> unspecified
    sample-vector integer integer rational rational rational rational
    faster, lower precision version of sp-sine!.
    currently faster by a factor of about 2.6")
  (scm-c-define-procedure-c
    "sp-convolve!" 4 0 0 scm-sp-convolve! "result a b carryover -> unspecified")
  (scm-c-define-procedure-c
    "sp-windowed-sinc!"
    5
    1
    0
    scm-sp-windowed-sinc!
    "result source sample-rate freq transition state -> state
    sample-vector sample-vector integer number number windowed-sinc-state -> unspecified
    apply a windowed-sinc low-pass filter to source and write to result and return
    an updated state object.
    if no state object has been given, create a new state")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-state"
    3
    1
    0
    scm-sp-windowed-sinc-state-create
    "sample-rate radian-frequency transition [state] -> state
    rational rational rational [sp-windowed-sinc] -> sp-windowed-sinc")
  (scm-c-define-procedure-c
    "sp-moving-average!"
    5
    2
    0
    scm-sp-moving-average!
    "result source previous next radius [start end] -> unspecified
    sample-vector sample-vector sample-vector sample-vector integer integer integer [integer]")
  (scm-c-define-procedure-c
    "sp-fftr"
    1
    0
    0
    scm-sp-fftr
    "sample-vector:values-at-times -> sample-vector:frequencies
    discrete fourier transform on the input data. only the real part")
  (scm-c-define-procedure-c
    "sp-fftri"
    1
    0
    0
    scm-sp-fftri
    "sample-vector:frequencies -> sample-vector:values-at-times
    inverse discrete fourier transform on the input data. only the real part")
  (scm-c-define-procedure-c
    "sp-alsa-open"
    5 0 0 scm-sp-alsa-open "device-name mode channel-count sample-rate latency -> sp-port")
  (scm-c-define-procedure-c
    "sp-file-open" 2 2 0 scm-sp-file-open "path mode [channel-count sample-rate] -> sp-port")
  (scm-c-define-procedure-c "sp-port-close" 1 0 0 scm-sp-port-close "sp-port -> boolean")
  (scm-c-define-procedure-c "sp-port-input?" 1 0 0 scm-sp-port-input? "sp-port -> boolean")
  (scm-c-define-procedure-c "sp-port-position?" 1 0 0 scm-sp-port-position? "sp-port -> boolean")
  (scm-c-define-procedure-c "sp-port-position" 1 0 0 scm-sp-port-position "sp-port -> integer")
  (scm-c-define-procedure-c
    "sp-port-channel-count" 1 0 0 scm-sp-port-channel-count "sp-port -> integer")
  (scm-c-define-procedure-c
    "sp-port-sample-rate" 1 0 0 scm-sp-port-sample-rate "sp-port -> integer")
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
    "sp-port-read"
    2 0 0 scm-sp-port-read "sp-port integer:sample-count -> (sample-vector ...):channel-data")
  (scm-c-define-procedure-c
    "sp-port-write"
    3
    0
    0
    scm-sp-port-write
    "sp-port (sample-vector ...):channel-data [integer:sample-count] -> unspecified
  write sample data to the channels of port")
  (scm-c-define-procedure-c
    "sp-port-position-set"
    2
    0
    0
    scm-sp-port-position-set
    "sp-port integer:sample-offset -> boolean
    sample-offset can be negative, in which case it is from the end of the port"))