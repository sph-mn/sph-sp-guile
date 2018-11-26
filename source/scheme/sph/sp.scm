(library (sph sp)
  (export
    f32vector-sum
    f64-nearly-equal?
    f64vector-sum
    sp-alsa-open
    sp-convolve
    sp-convolve!
    sp-duration->sample-count
    sp-fftr
    sp-fftri
    sp-file-open
    sp-moving-average
    sp-moving-average!
    sp-pi
    sp-plot-render
    sp-port-channel-count
    sp-port-close
    sp-port-input?
    sp-port-mode-read
    sp-port-mode-read-write
    sp-port-mode-write
    sp-port-position
    sp-port-position-set
    sp-port-position?
    sp-port-read
    sp-port-sample-rate
    sp-port-write
    sp-port?
    sp-sample-count->duration
    sp-sample-format
    sp-samples->list
    sp-samples-copy
    sp-samples-copy-zero
    sp-samples-copy-zero*
    sp-samples-from-list
    sp-samples-length
    sp-samples-map
    sp-samples-map!
    sp-samples-new
    sp-samples-set!
    sp-samples?
    sp-segments->alsa
    sp-segments->file
    sp-segments->plot
    sp-segments->plot-render
    sp-sinc
    sp-sine!
    sp-sine-lq!
    sp-spectral-inversion
    sp-spectral-reversal
    sp-window-blackman
    sp-windowed-sinc
    sp-windowed-sinc!
    sp-windowed-sinc-state-update)
  (import
    (guile)
    (rnrs bytevectors)
    (sph)
    (sph process)
    (sph string)
    (sph uniform-vector)
    (sph vector))

  (load-extension "libguile-sph-sp" "sp_guile_init")
  (define sp-pi (* 4 (atan 1)))

  (define (sp-sinc a) "the normalised sinc function"
    ; re-implemented in scheme
    (if (= 0 a) 1 (/ (sin (* sp-pi a)) (* sp-pi a))))

  (define (sp-spectral-inversion a) "sample-vector -> sample-vector"
    "modify an impulse response kernel for spectral inversion.
    length must be odd and \"a\" must have left-right symmetry for the result to be valid.
    flips the frequency response top to bottom"
    (let ((b (sp-samples-map (l (a) (* -1 a)) a)) (center-index (/ (- (sp-samples-length a) 1) 2)))
      (sp-samples-set! b center-index (+ 1 (sp-samples-ref b center-index))) b))

  (define (sp-spectral-reversal a)
    "inverts the sign for samples at odd indexes for an impulse response kernel.
     a-len must be odd and \"a\" must have left-right symmetry for the result to be valid.
     flips the frequency response left to right"
    (let (b (sp-samples-copy a))
      (let loop ((index (- (sp-samples-length a) 2)))
        (if (> index 1)
          (begin (sp-samples-set! b index (* -1 (sp-samples-ref b index))) (loop (- index 2))) b))))

  (define-syntax-rule (sp-samples-new-f uv-create uv-make)
    ; a procedure similar to vector-make except that the fill value can be a procedure used to set the elements
    (l (length value) (if (procedure? value) (uv-create length value) (uv-make length value))))

  (define sp-samples-map
    (case sp-sample-format
      ((f64) f64vector-map)
      ((f32) f32vector-map)))

  (define sp-samples-map!
    (case sp-sample-format
      ((f64) f64vector-map!)
      ((f32) f32vector-map!)))

  (define sp-samples-new
    (case sp-sample-format
      ((f64) (sp-samples-new-f f64vector-create make-f64vector))
      ((f32) (sp-samples-new-f f32vector-create make-f32vector))))

  (define sp-samples-set!
    (case sp-sample-format
      ((f64) f64vector-set!)
      ((f32) f32vector-set!)))

  (define sp-samples-ref
    (case sp-sample-format
      ((f64) f64vector-ref)
      ((f32) f32vector-ref)))

  (define sp-samples?
    (case sp-sample-format
      ((f64) f64vector?)
      ((f32) f32vector?)))

  (define sp-samples->list
    (case sp-sample-format
      ((f64) f64vector->list)
      ((f32) f32vector->list)))

  (define sp-samples-from-list
    (case sp-sample-format
      ((f64) list->f64vector)
      ((f32) list->f32vector)))

  (define sp-samples-copy-zero
    (case sp-sample-format
      ((f64) f64vector-copy-zero)
      ((f32) f32vector-copy-zero)))

  (define sp-samples-copy-zero*
    (case sp-sample-format
      ((f64) f64vector-copy-zero*)
      ((f32) f32vector-copy-zero*)))

  (define sp-samples-length
    (case sp-sample-format
      ((f64) f64vector-length)
      ((f32) f32vector-length)))

  (define sp-samples-copy
    (case sp-sample-format
      ((f64) f64vector-copy)
      ((f32) f32vector-copy)))

  (define (sp-duration->sample-count seconds sample-rate) (* seconds sample-rate))
  (define (sp-sample-count->duration sample-count sample-rate) (/ sample-count sample-rate))

  (define (sp-segments->file a path sample-rate)
    "(#(#(sample ...):channel ...):segment ...) string -> unspecified
     write chunks of audio data to file. first argument is a list of lists of sample vectors per channel"
    (if (not (null? a))
      (let (out (sp-file-open path sp-port-mode-write (vector-length (first a)) sample-rate))
        (each (l (segment) (sp-port-write out segment (sp-samples-length (vector-first segment))))
          a)
        (sp-port-close out))))

  (define* (sp-segments->alsa a sample-rate #:optional (device "default") (latency 4096))
    "(#(vector:channel ...) ...) -> unspecified
     write chunks of audio data to an alsa device"
    (if (not (null? a))
      (let
        (out (sp-alsa-open device sp-port-mode-write (vector-length (first a)) sample-rate latency))
        (each (l (segment) (sp-port-write out segment (sp-samples-length (vector-first segment))))
          a)
        (sp-port-close out))))

  (define (sp-segments->plot a path channel)
    "(#(vector:channel ...) ...) string ->
     write gnuplot compatible sample data to file at path"
    (call-with-output-file path
      (l (file)
        (each
          (l (segment)
            (each (l (sample) (display sample file) (newline file))
              (sp-samples->list (vector-ref segment channel))))
          a)
        (newline file))))

  (define (sp-plot-render file-path)
    (process-replace-p "gnuplot" "--persist"
      "-e" (string-append "set yrange [-1:1]; plot " (string-quote file-path) " with lines")))

  (define (sp-segments->plot-render a path channel) (sp-segments->plot a path channel)
    (sp-plot-render path))

  (define* (sp-moving-average source prev next distance #:optional start end)
    "sample-vector false/sample-vector false/sample-vector integer [integer/false integer/false] -> sample-vector"
    (sp-samples-copy-zero* source
      (l (a) (sp-moving-average! a source prev next distance start end))))

  (define* (sp-convolve a b #:optional carryover)
    (let
      ( (result (sp-samples-copy-zero a))
        (carryover
          (if carryover (sp-samples-copy carryover) (sp-samples-new (- (sp-samples-length b) 1) 0))))
      (sp-convolve! result a b carryover) (pair result carryover)))

  (define* (sp-windowed-sinc source sample-rate freq transition #:optional is-high-pass state)
    "sample-vector integer number number false/windowed-sinc-state -> sample-vector
     state is still eventually going to be modified"
    (let (result (sp-samples-copy-zero source))
      (pair result (sp-windowed-sinc! result source sample-rate freq transition is-high-pass state)))))
