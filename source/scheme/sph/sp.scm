(library (sph sp)
  (export
    f32vector-sum
    f64-nearly-equal?
    f64vector-sum
    sp-alsa-open
    sp-clip
    sp-convolve
    sp-convolve!
    sp-duration->sample-count
    sp-fftr
    sp-fftr->plot-file
    sp-fftr-plot-display
    sp-fftr-plot-file-display
    sp-fftri
    sp-file-open
    sp-fold-integers
    sp-generate
    sp-moving-average
    sp-moving-average!
    sp-noise-exponential
    sp-noise-normal
    sp-noise-uniform
    sp-path
    sp-path-new
    sp-path-new-p
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
    sp-samples->plot-file
    sp-samples-copy
    sp-samples-copy-zero
    sp-samples-copy-zero*
    sp-samples-from-list
    sp-samples-length
    sp-samples-map
    sp-samples-map!
    sp-samples-new
    sp-samples-plot-display
    sp-samples-plot-file-display
    sp-samples-set!
    sp-samples?
    sp-segment
    sp-segments->alsa
    sp-segments->file
    sp-segments->plot
    sp-segments->plot-render
    sp-sinc
    sp-sine
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
    (sph list)
    (sph math)
    (sph process)
    (sph string)
    (sph uniform-vector)
    (sph vector))

  (load-extension "libguile-sph-sp" "sp_guile_init")
  (define sp-pi (* 4 (atan 1)))

  (define-syntax-rule (sp-samples-new-f uv-create uv-make)
    ; a procedure similar to vector-make except that the fill value can be a procedure used to set the elements
    (l (length value) (if (procedure? value) (uv-create length value) (uv-make length value))))

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

  (define* (sp-windowed-sinc source sample-rate cutoff transition #:optional is-high-pass state)
    "sample-vector integer number number false/windowed-sinc-state -> sample-vector
     cutoff and transition are radian frequencies.
     state is still eventually going to be modified"
    (let (result (sp-samples-copy-zero source))
      (pair result
        (sp-windowed-sinc! result source sample-rate cutoff transition is-high-pass state))))

  (define (sp-samples-plot-file-display file-path)
    (execute "gnuplot" "--persist"
      "-e"
      (string-append "set key off; unset xtics; set tics scale 0; plot " (string-quote file-path)
        " with lines lc rgb \"blue\"")))

  (define (sp-samples->plot-file a path)
    (call-with-output-file path
      (l (port) (each (l (a) (display-line a port)) (sp-samples->list a)))))

  (define (sp-samples-plot-display a)
    (let (path (tmpnam)) (sp-samples->plot-file a path) (sp-samples-plot-file-display path)))

  (define sp-fftr-plot-file-display sp-samples-plot-file-display)

  (define (sp-fftr->plot-file a path)
    (call-with-output-file path
      (l (port)
        (each-with-index (l (index a) (and (< 0 index) (even? index) (display-line a port)))
          (sp-samples->list a)))))

  (define (sp-fftr-plot-display a)
    (let (path (tmpnam)) (sp-fftr->plot-file a path) (sp-fftr-plot-file-display path)))

  (define (sp-sine time freq)
    "returns a sample value for a sine value after \"time\" seconds.
     freq: radians per second
     time: seconds since start with phase offset 0
     phase: repeats each 2pi"
    ; wavelength = pi radians
    (sin (* time freq)))

  (define* (sp-noise-uniform #:optional (state *random-state*)) (random:uniform state))
  (define* (sp-noise-exponential #:optional (state *random-state*)) (random:exp state))
  (define* (sp-noise-normal #:optional (state *random-state*)) (random:normal state))

  (define (sp-fold-integers start end f . states)
    (let loop ((index start) (states states))
      (if (< index end) (loop (+ 1 index) (apply f index states)) states)))

  (define (sp-segment size channel-count f . states)
    "integer integer false/procedure:{index states ... -> number/vector} -> (#(vector:channel ...) . states)
     create a new segment with values set by calling f for each sample.
     if the argument for f is false, return empty sample vectors"
    (let (result (list->vector (map-integers channel-count (l a (sp-samples-new size 0)))))
      (if f
        (let loop ((index 0) (states states))
          (if (< index size)
            (let* ((data-and-states (apply f index states)) (data (first data-and-states)))
              (cond
                ((number? data) (sp-samples-set! (vector-first result) index data))
                ( (vector? data)
                  (vector-each-with-index
                    (l (channel data) (sp-samples-set! (vector-ref result channel) index data)) data))
                (else (raise (q sp-invalid-sample-f-result))))
              (loop (+ 1 index) (tail data-and-states)))
            (pair result states)))
        (pair result states))))

  (define (sp-clip a) "eventually adjust value to not exceed -1 or 1"
    (if (< 0 a) (min 1.0 a) (max -1.0 a)))

  (define (sp-generate sample-rate channel-count start duration segment-f sample-f . states)
    "integer integer integer procedure false/procedure any ... -> (any ...):states
     calls segment-f for the samples of each second in duration. calls sample-f for each sample.
     if sample-f is false then segment-f is called with new segment data set to zero.
     segment-f :: env time segment custom ... -> states
     sample-f :: env time custom ... -> sample-value"
    (let*
      ((sample-duration (/ 1 sample-rate)) (env (vector sample-rate sample-duration channel-count)))
      (apply sp-fold-integers start
        (+ start duration)
        (l (time . states)
          (apply segment-f env
            time
            (apply sp-segment sample-rate
              channel-count
              (and sample-f
                (l (index . states) (apply sample-f env (+ time (* index sample-duration)) states)))
              states)))
        states)))

  (define sp-path-new-p
    (let
      ( (line-new
          (l (result start sample-duration a)
            "add line interpolating procedures to result with params \"a\""
            (let*
              ( (points (pair start (map-slice 2 vector a)))
                (segments
                  (map-segments 2
                    (l (p1 p2)
                      (let*
                        ((start (vector-first p1)) (end (vector-first p2)) (duration (- end start)))
                        (vector start end
                          (l (time)
                            (vector-second (linear-interpolation (/ (- time start) duration) p1 p2))))))
                    points)))
              (list (append result segments) (last points)))))
        (bezier-new
          (l (result start sample-duration a)
            (let*
              ( (points (pair start (map-slice 2 vector a)))
                (segment
                  (let*
                    ( (p1 (first points)) (p2 (last points)) (start (vector-first p1))
                      (end (vector-first p2)) (duration (- end start)))
                    (vector start end
                      (l (time)
                        (vector-second (apply bezier-curve (/ (- time start) duration) points)))))))
              (list (append result (list segment)) (last points)))))
        (arc-new
          (l (result start sample-duration a)
            "the arc ends at point (x, y)
            the ellipse has the two radii (rx, ry)
            the x-axis of the ellipse is rotated by x-axis-rotation"
            (apply
              (l* (x y radius-x #:optional (radius-y radius-x) (rotation 0) large-arc sweep)
                (let*
                  ( (p1 start) (p2 (vector x y)) (start (vector-first start))
                    (end x) (duration (- end start)))
                  (list
                    (append result
                      (list
                        (vector start end
                          (l (time)
                            (vector-second
                              (first
                                (elliptical-arc (/ (- time start) duration) p1
                                  p2 radius-x radius-y rotation large-arc sweep)))))))
                    (vector x y))))
              a))))
      (l (sample-rate segments)
        "number ((symbol:type any:parameter ...) ...) -> path-state
        draw a path of multiple segments between points interpolated by functions selectable for each segment.
        returns a state object that is to be passed to sp-path to get a point on the path.
        there are no required segment types but at least one must be given.
        if no start is given then the path starts at 0. start can also be used as a path segment to create gaps.
        this implementation is similar to the path element of svg vector graphics.
        for \"arc\" see how arcs are created with a path with svg graphics
        segment types:
          (start time value)
          (bezier time value time/value ...)
          (line time value time/value ...)
          (arc x y radius-x [radius-y rotation large-arc sweep])
        usage
          (sp-path-new 16000 (start 0.2 0) (line 0.5 0.25) (line 0.8 0.4) (line 1.2 0.01))"
        (let*
          ( (sample-duration (/ 1 sample-rate))
            (segments
              (first
                (fold-multiple
                  (l (a result start)
                    (case (first a)
                      ((line) (line-new result start sample-duration (tail a)))
                      ((bezier) (bezier-new result start sample-duration (tail a)))
                      ((arc) (arc-new result start sample-duration (tail a)))
                      ((start) (list result (apply vector (tail a))))))
                  segments null #(0 0)))))
          (let ((next (first segments)) (index-i 0) (index (apply vector segments)))
            (list next index-i index))))))

  (define-syntax-rule (sp-path-new sample-rate (symbol param ...) ...)
    (sp-path-new-p sample-rate (list (list (quote symbol) param ...) ...)))

  (define (sp-path-advance path-state)
    "path-state -> path-state
     stop interpolating the current segment"
    (apply
      (l (next index-i index)
        (let (index-i (+ 1 index-i))
          (if (< index-i (vector-length index)) (list (vector-ref index index-i) index-i index)
            (list #f #f index))))
      path-state))

  (define sp-path-segment-start (vector-accessor 0))
  (define sp-path-segment-end (vector-accessor 1))
  (define sp-path-segment-f (vector-accessor 2))

  (define* (sp-path time path #:optional (c pair))
    "number path-state [procedure -> result] -> (data . path-state)/any
     get value at time for a path created by sp-path-new.
     returns zero for gaps or after the end of the path"
    (let (a (first path))
      (if (and a (>= time (sp-path-segment-start a)))
        (if (< time (sp-path-segment-end a)) (pair ((sp-path-segment-f a) time) path)
          (sp-path time (sp-path-advance path) c))
        (pair 0 path)))))
