(pre-include "./helper.c" "./foreign/sph/float.c")

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

(define (scm-sp-convolve! out a b carryover carryover-len) (SCM SCM SCM SCM SCM SCM)
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
  (scm-sp-windowed-sinc-lp-hp! scm-out scm-in scm-cutoff scm-transition scm-is-high-pass scm-state)
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
  (scm-sp-windowed-sinc-bp-br!
    scm-out
    scm-in scm-cutoff-l scm-cutoff-h scm-transition-l scm-transition-h scm-is-reject scm-state)
  (SCM SCM SCM SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    state sp-convolution-filter-state-t*
    is-reject boolean)
  (set
    is-reject (scm-is-true scm-is-reject)
    state
    (if* (scm-is-true scm-state) (scm->sp-convolution-filter-state scm-state)
      0))
  (status-require
    (sp-windowed-sinc-bp-br
      (scm->sp-samples scm-in)
      (scm->sp-samples-length scm-in)
      (scm->sp-float scm-cutoff-l)
      (scm->sp-float scm-cutoff-h)
      (scm->sp-float scm-transition-l)
      (scm->sp-float scm-transition-h) is-reject &state (scm->sp-samples scm-out)))
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

(define (scm-sp-convolution-filter! scm-out scm-in scm-ir-f scm-ir-f-arguments scm-state)
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
  (if (not (scm-is-true scm-state)) (set scm-state (scm-from-sp-convolution-filter-state state)))
  (label exit
    (scm-from-status-return scm-state)))

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

(define (scm-sp-fftr scm-input) (SCM SCM)
  status-declare
  (declare
    i sp-sample-count-t
    input-len sp-sample-count-t
    output-len sp-sample-count-t
    output sp-sample-t*
    scm-c-output-len sp-sample-count-t
    scm-output SCM)
  (scm-dynwind-begin 0)
  (set
    output 0
    input-len (scm->sp-samples-length scm-input)
    output-len (sp-fftr-output-len input-len)
    scm-c-output-len (/ output-len 2)
    scm-output (scm-c-make-vector scm-c-output-len SCM-BOOL-F))
  (status-require (sph-helper-malloc (* output-len (sizeof sp-sample-t)) &output))
  (scm-dynwind-free output)
  (status-require (sp-fftr (scm->sp-samples scm-input) input-len output))
  (sc-comment "convert to scheme complex numbers")
  (for ((set i 0) (< i scm-c-output-len) (set i (+ 1 i)))
    (scm-c-vector-set!
      scm-output
      i (scm-c-make-rectangular (array-get output (* 2 i)) (array-get output (+ 1 (* 2 i))))))
  (label exit
    (scm-from-status-dynwind-end-return scm-output)))

(define (scm-sp-fftri scm-input) (SCM SCM)
  "scm-sp-fftri takes scheme complex numbers, sp-fftri takes complex numbers as alternated real/imaginary values in an array"
  status-declare
  (declare
    output-len sp-sample-count-t
    scm-output SCM
    scm-c-input-len sp-sample-count-t
    i sp-sample-count-t
    input sp-sample-t*
    input-len sp-sample-count-t)
  (scm-dynwind-begin 0)
  (set
    scm-c-input-len (scm-c-vector-length scm-input)
    input-len (* 2 scm-c-input-len)
    output-len (sp-fftri-output-len input-len)
    scm-output (scm-c-make-sp-samples output-len))
  (sc-comment "convert from scheme complex numbers to alternated array")
  (status-require (sph-helper-malloc (* input-len (sizeof sp-sample-t)) &input))
  (scm-dynwind-free input)
  (for ((set i 0) (< i scm-c-input-len) (set i (+ 1 i)))
    (set
      (array-get input (* 2 i)) (scm->sp-sample (scm-real-part (scm-c-vector-ref scm-input i)))
      (array-get input (+ 1 (* 2 i))) (scm->sp-sample (scm-imag-part (scm-c-vector-ref scm-input i)))))
  (status-require (sp-fftri input input-len (scm->sp-samples scm-output)))
  (label exit
    (scm-from-status-dynwind-end-return scm-output)))

(define (scm-sp-alsa-open scm-device-name scm-mode scm-channel-count scm-sample-rate scm-latency)
  (SCM SCM SCM SCM SCM SCM)
  status-declare
  (declare
    device-name uint8-t*
    latency int32-t
    scm-result SCM
    port sp-port-t*)
  (scm-dynwind-begin 0)
  (set
    device-name (scm->locale-string scm-device-name)
    latency
    (if* (scm-is-undefined scm-latency) -1
      (scm->sp-sample-count scm-latency)))
  (scm-dynwind-free device-name)
  (set port (scm-gc-malloc-pointerless (sizeof sp-port-t) "sp-port"))
  (status-require
    (sp-alsa-open
      device-name
      (scm->uint8 scm-mode)
      (scm->sp-channel-count scm-channel-count) (scm->sp-sample-rate scm-sample-rate) latency port))
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

(define (scm-sp-window-blackman a width) (SCM SCM SCM)
  (scm-from-sp-float (sp-window-blackman (scm->sp-float a) (scm->sp-sample-count width))))

(define (scm-sp-sample-format) SCM
  (case = sp-sample-format
    (sp-sample-format-f64 (return (scm-from-latin1-symbol "f64")))
    (sp-sample-format-f32 (return (scm-from-latin1-symbol "f32")))
    (sp-sample-format-int32 (return (scm-from-latin1-symbol "int32")))
    (sp-sample-format-int16 (return (scm-from-latin1-symbol "int16")))
    (sp-sample-format-int8 (return (scm-from-latin1-symbol "int8")))))

(define (scm-sp-convolution-filter-state-finalize a) (void SCM)
  (sp-convolution-filter-state-free (scm->sp-convolution-filter-state a)))

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
    scm-type-convolution-filter-state
    (scm-make-foreign-object-type
      (scm-from-latin1-symbol "sp-convolution-filter-state")
      type-slots scm-sp-convolution-filter-state-finalize))
  (scm-c-module-define m "sp-sample-format" (scm-sp-sample-format sp-sample-format))
  (scm-c-module-define m "sp-port-mode-read" (scm-from-uint8 sp-port-mode-read))
  (scm-c-module-define m "sp-port-mode-write" (scm-from-uint8 sp-port-mode-write))
  (scm-c-module-define m "sp-port-mode-read-write" (scm-from-uint8 sp-port-mode-read-write))
  scm-c-define-procedure-c-init
  (scm-c-define-procedure-c
    "sp-convolve!" 4 1 0 scm-sp-convolve! "out a b carryover [carryover-len] -> unspecified")
  (scm-c-define-procedure-c "sp-window-blackman" 2 0 0 scm-sp-window-blackman "real width -> real")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-lp-hp!"
    6
    0
    0
    scm-sp-windowed-sinc-lp-hp!
    "out in cutoff transition is-high-pass state -> state
    samples samples real:0..0.5 real:0..0.5 boolean convolution-filter-state -> unspecified
    apply a windowed-sinc low-pass or high-pass filter to \"in\", write to \"out\" and return
    an updated state object.
    if state object is false, create a new state.
    cutoff and transition are as a fraction of the sampling-rate")
  (scm-c-define-procedure-c
    "sp-windowed-sinc-bp-br!"
    8
    0
    0
    scm-sp-windowed-sinc-bp-br!
    "out in  cutoff-l cutoff-h transition-l transition-h is-reject state -> state
    samples samples real:0..0.5 real real:0..0.5 real boolean convolution-filter-state -> unspecified
  like sp-windowed-sinc-lp-hp! but as a band-pass or band-reject filter")
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
    "sp-convolution-filter!"
    5
    0
    0
    scm-sp-convolution-filter!
    "out in ir-f ir-f-arguments state -> state
     samples samples procedure list sp-convolution-filter-state -> sp-convolution-filter-state")
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
    "sample-vector:values-at-times -> #(complex ...):frequencies
    discrete fourier transform on the input data. only the real part")
  (scm-c-define-procedure-c
    "sp-fftri"
    1
    0
    0
    scm-sp-fftri
    "#(complex ...):frequencies -> sample-vector:values-at-times
    inverse discrete fourier transform on the input data. only the real part")
  (scm-c-define-procedure-c
    "sp-alsa-open"
    4 1 0 scm-sp-alsa-open "device-name mode channel-count sample-rate [latency] -> sp-port")
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