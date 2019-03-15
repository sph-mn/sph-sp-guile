(pre-include "./helper.c" "./foreign/sph/float.c")

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
  (if (not (scm-is-true scm-state))
    (begin
      (set scm-state (scm-from-sp-convolution-filter-state state))))
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
    input/output-real sp-sample-t*
    input/output-imag sp-sample-t*
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
    input/output-real sp-sample-t*
    input/output-imag sp-sample-t*
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
  (status-require (sp-alloc-channel-array channel-count sample-count &channel-data))
  (status-require (sp-file-read file sample-count channel-data &result-sample-count))
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
    scm-type-file (scm-make-foreign-object-type (scm-from-latin1-symbol "sp-file") type-slots 0)
    scm-type-convolution-filter-state
    (scm-make-foreign-object-type
      (scm-from-latin1-symbol "sp-convolution-filter-state")
      type-slots scm-sp-convolution-filter-state-finalize))
  (scm-c-module-define m "sp-file-mode-read" (scm-from-uint8 sp-file-mode-read))
  (scm-c-module-define m "sp-file-mode-write" (scm-from-uint8 sp-file-mode-write))
  (scm-c-module-define m "sp-file-mode-read-write" (scm-from-uint8 sp-file-mode-read-write))
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