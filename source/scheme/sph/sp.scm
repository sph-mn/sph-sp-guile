(library (sph sp)
  (export
    differences
    f32vector-sum
    f64-nearly-equal?
    f64vector-sum
    sp-alsa-open
    sp-asymmetric-moving
    sp-asymmetric-moving-average
    sp-asymmetric-moving-median
    sp-asymmetric-moving-out
    sp-change-limiter
    sp-clip~
    sp-convolution-filter!
    sp-convolve
    sp-convolve!
    sp-duration->sample-count
    sp-factor->rads
    sp-fft-resynth
    sp-fftr
    sp-fftri
    sp-file-open
    sp-filter-bank
    sp-float-sum
    sp-fold-file
    sp-fold-file-overlap
    sp-fold-frames
    sp-fold-integers
    sp-generate
    sp-grain-map
    sp-hz->rads
    sp-moving-average
    sp-moving-average!
    sp-noise-band
    sp-noise-exponential~
    sp-noise-normal~
    sp-noise-uniform~
    sp-overlap
    sp-path
    sp-path-new
    sp-path-new-p
    sp-phase
    sp-phase-cycle
    sp-phase-sine-width
    sp-pi
    sp-plot-fft
    sp-plot-fft->file
    sp-plot-fft-display-file
    sp-plot-samples
    sp-plot-samples->file
    sp-plot-samples-display-file
    sp-plot-segments
    sp-plot-segments->file
    sp-plot-spectrum
    sp-plot-spectrum->file
    sp-plot-spectrum-display-file
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
    sp-rads->factor
    sp-rads->hz
    sp-rectangle~
    sp-rectangular
    sp-sample-align
    sp-sample-align-list
    sp-sample-count->duration
    sp-sample-format
    sp-sample-sum
    sp-samples->list
    sp-samples-apply-blackman-window
    sp-samples-apply-hann-window
    sp-samples-copy
    sp-samples-copy-zero
    sp-samples-copy-zero*
    sp-samples-divide
    sp-samples-extract
    sp-samples-extract-padded
    sp-samples-from-list
    sp-samples-length
    sp-samples-list-add-offsets
    sp-samples-map
    sp-samples-map!
    sp-samples-map-with
    sp-samples-map-with-index
    sp-samples-multiply
    sp-samples-new
    sp-samples-ref
    sp-samples-set!
    sp-samples-split
    sp-samples-threshold
    sp-samples?
    sp-scheduler
    sp-segment
    sp-segments->alsa
    sp-segments->file
    sp-sinc
    sp-sine!
    sp-sine-lq!
    sp-sine-of-width
    sp-sines~
    sp-sine~
    sp-spectral-inversion
    sp-spectral-reversal
    sp-spectrum
    sp-triangle
    sp-triangle~
    sp-window-blackman
    sp-window-hann
    sp-windowed-sinc-bp-br
    sp-windowed-sinc-bp-br!
    sp-windowed-sinc-bp-br-ir
    sp-windowed-sinc-lp-hp
    sp-windowed-sinc-lp-hp!
    sp-windowed-sinc-lp-hp-ir)
  (import
    (guile)
    (rnrs bytevectors)
    (rnrs sorting)
    (sph)
    (sph list)
    (sph math)
    (sph number)
    (sph process)
    (sph string)
    (sph uniform-vector)
    (sph vector)
    (only (sph other) each-integer)
    (only (srfi srfi-1) drop-right))

  (load-extension "libguile-sph-sp" "sp_guile_init")
  (define sp-pi (* 4 (atan 1)))

  (define-syntax-rule (sp-samples-new-f uv-create uv-make)
    ; a procedure similar to vector-make except that the fill value can be a procedure {index -> value} used to set the elements
    (l* (length #:optional (value 0))
      (if (procedure? value) (uv-create length value) (uv-make length value))))

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

  (define sp-samples-map-with
    (case sp-sample-format
      ((f64) f64vector-map-with)
      ((f32) f32vector-map-with)))

  (define sp-samples-map-with-index
    (case sp-sample-format
      ((f64) f64vector-map-with-index)
      ((f32) f32vector-map-with-index)))

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

  (define* (sp-moving-average source prev next distance #:optional start end)
    "sample-vector false/sample-vector false/sample-vector integer [integer/false integer/false] -> sample-vector"
    (sp-samples-copy-zero* source
      (l (a) (sp-moving-average! a source prev next distance start end))))

  (define* (sp-convolve a b #:optional carryover carryover-len)
    (let
      ( (result (sp-samples-copy-zero a))
        (carryover
          (if carryover (sp-samples-copy carryover) (sp-samples-new (- (sp-samples-length b) 1) 0))))
      (sp-convolve! result a b carryover carryover-len) (pair result carryover)))

  (define* (sp-windowed-sinc-lp-hp in cutoff transition is-high-pass state)
    "samples real real boolean false/convolution-filter-state -> samples
     state is still eventually going to be modified"
    (let (out (sp-samples-copy-zero in))
      (pair out (sp-windowed-sinc-lp-hp! out in cutoff transition is-high-pass state))))

  (define* (sp-windowed-sinc-bp-br in cutoff-l cutoff-h transition-l transition-h is-reject state)
    "samples real real real boolean false/convolution-filter-state -> samples
     state is still eventually going to be modified"
    (let (out (sp-samples-copy-zero in))
      (pair out
        (sp-windowed-sinc-bp-br! out in cutoff-l cutoff-h transition-l transition-h is-reject state))))

  (define* (sp-plot-samples-display-file file-path #:key (type (q lines)) (color "blue"))
    "string #:type symbol:lines/points #:color string -> unspecified
     type and color correspond to gnuplot options"
    (execute "gnuplot" "--persist"
      "-e"
      (string-append "set key off; set size ratio 0.5; plot " (string-quote file-path)
        " with "
        (if (string? type) type
          (case type
            ((points) "points pointtype 5 ps 0.3")
            (else "lines")))
        " lc rgb \"" color "\"")))

  (define (sp-plot-samples->file a path)
    (call-with-output-file path
      (l (port) (each (l (a) (display-line a port)) (sp-samples->list a)))))

  (define (sp-plot-samples a . display-args)
    "samples [#:type #:color] -> unspecified
     for display-args see sp-plot-samples-display-file"
    (let (path (tmpnam)) (sp-plot-samples->file a path)
      (apply sp-plot-samples-display-file path display-args)))

  (define (sp-plot-segments->file a path channel)
    "(#(vector:channel ...) ...) string ->
     write gnuplot compatible sample data for one channel to file at path.
     the file can be rendered with sp-samples-plot-file-display"
    (call-with-output-file path
      (l (file)
        (each
          (l (segment)
            (each (l (sample) (display-line sample file))
              (sp-samples->list (vector-ref segment channel))))
          a)
        (newline file))))

  (define (sp-plot-segments a path channel) "(vector ...) string integer:0..n"
    (let (path (tmpnam)) (sp-plot-segments->file a path channel)
      (sp-plot-samples-display-file path)))

  (define (sp-window-hann offset size) (* 0.5 (- 1 (cos (/ (* 2 sp-pi offset) (- size 1))))))

  (define (sp-samples-apply-hann-window a)
    (let (a-length (sp-samples-length a))
      (sp-samples-map-with-index (l (index a) (* a (sp-window-hann index a-length))) a)))

  (define (sp-samples-apply-blackman-window a)
    (let (a-length (sp-samples-length a))
      (sp-samples-map-with-index (l (index a) (* a (sp-window-blackman index a-length))) a)))

  (define (sp-plot-spectrum-display-file path) (sp-plot-samples-display-file path #:type "histeps"))

  (define (sp-plot-spectrum->file a path)
    "apply sp-spectrum on \"a\" and write the result to file at path"
    (call-with-output-file path
      (l (port) (vector-each (l (a) (display-line a port)) (sp-spectrum a)))))

  (define (sp-plot-spectrum a)
    (let (path (tmpnam)) (sp-plot-spectrum->file a path) (sp-plot-fft-display-file path)))

  (define (sp-spectrum a) "samples -> #(real ...)"
    (vector-map (l (b) (* 2 (/ (magnitude b) (sp-samples-length a)))) (sp-fftr a)))

  (define sp-plot-fft-display-file sp-plot-spectrum-display-file)

  (define (sp-plot-fft->file a path) "write the magnitudes of the fft result to file at path"
    (call-with-output-file path
      (l (port) (vector-each (l (a) (display-line (magnitude a) port)) a))))

  (define (sp-plot-fft a)
    (let (path (tmpnam)) (sp-plot-fft->file a path) (sp-plot-fft-display-file path)))

  (define (sp-sine~ offset freq)
    "real:radians:phase-offset real:radians-per-s -> real:sample
     result phase repeats each 2pi"
    (sin (* freq offset)))

  (define (sp-sines~ offset . freq)
    "number:radians number:radians-per-s ... -> real:0..1:sample
     get a value for a sum of sines of all specified frequencies with
     decreasing amplitude per added sine"
    (apply + (map-with-index (l (index a) (/ (sp-sine~ offset a) (+ 1 index))) freq)))

  (define* (sp-noise-uniform~ #:optional (state *random-state*)) (- (* 2 (random:uniform state)) 1))
  (define* (sp-noise-exponential~ #:optional (state *random-state*)) (- (* 2 (random:exp state)) 1))
  (define* (sp-noise-normal~ #:optional (state *random-state*)) (- (* 2 (random:normal state)) 1))

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

  (define (sp-clip~ a) "eventually adjust value to not exceed -1 or 1"
    (if (< 0 a) (min 1.0 a) (max -1.0 a)))

  (define (sp-generate sample-rate channel-count duration segment-f sample-f . states)
    "integer integer procedure false/procedure any ... -> (any ...):states
     helper to create sample vectors.
     calls segment-f for the samples for each second in duration. calls sample-f for each sample.
     if sample-f is false then segment-f is called with new segment data set to zero.
     segment-f :: env offset:seconds segment custom ... -> (any ...):state
     sample-f :: env offset:sample-count custom ... -> (sample-value any:state-value ...)"
    (let*
      ((sample-duration (/ 1 sample-rate)) (env (vector sample-rate sample-duration channel-count)))
      (apply sp-fold-integers duration
        (l (offset . states)
          (apply segment-f env
            offset
            (apply sp-segment sample-rate
              channel-count
              (and sample-f
                (l (index . states) (apply sample-f env (+ index (* offset sample-rate)) states)))
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
        (pair 0 path))))

  (define (sp-rectangular x a b c d)
    "integer:sample-count ... -> real:sample
     alternate between c and d for durations of a and b"
    (let (remainder (modulo x (+ a b))) (if (< remainder a) c d)))

  (define (sp-triangle x a b height)
    (let (remainder (modulo x (+ a b)))
      (if (< remainder a) (* remainder (/ height a)) (* (- b (- remainder a)) (/ height b)))))

  (define (sp-rectangle~ x a b)
    "integer:sample-count ... -> real:sample
     get a value for a rectangular wave with given lengths of sides a and b"
    (sp-rectangular x a b -1 1))

  (define (sp-triangle~ x a b)
    "integer:sample-count ... -> real:sample
     return a sample for a sine wave with side length a and b in number of samples.
     creates saw waves if either a or b is 0"
    (- (sp-triangle x a b 2) 1))

  (define (sp-phase-sine-width offset width)
    "integer:sample-count integer:sample-count -> real
     return the phase offset in radians for a sine that completes a full cycle
     in width number of samples"
    (* (modulo offset width) (/ (* 2 sp-pi) width)))

  (define (sp-phase-cycle width height state)
    "integer integer false/previous-result -> (result _ ...):state
     a linear phase generator that uses the given width and height only if it doesnt interrupt an active cycle.
     this keeps phases continuous and cycles phase aligned but doesnt create higher resolution transitions between cycles"
    (apply
      (l (x active-width active-height)
        (if (= x active-width) (list 0 0 width height)
          (list (* (+ 1 x) (/ active-height active-width)) (+ 1 x) active-width active-height)))
      (or (and state (tail state)) (list 0 width height))))

  (define (sp-phase y change phase-size)
    "number number number -> number
     phase generator that allows for high resolution modulation and non-linear transitions.
     * y: previous result or another starting value to continue from
     * change: how fast the phase should progress
     * phase-size: value at which the cycle should repeat
     example: (sp-phase 0.0 (/ (* 2 sp-pi) 200) (* 2 sp-pi))"
    (let (y (float-sum change y)) (if (< phase-size y) (float-sum y (- phase-size)) y)))

  (define sp-float-sum float-sum)
  (define sp-sample-sum float-sum)
  (define (sp-rads->hz radians-per-second) "real -> real:hertz" (/ radians-per-second (* 2 sp-pi)))
  (define (sp-hz->rads hertz) "real -> real:radians-per-second" (* hertz 2 sp-pi))

  (define (sp-rads->factor radians-per-second sample-rate)
    "real integer -> real
     convert a radian frequency value to a fraction of the sample rate.
     0.5 is the maximum possible representable frequency, which in hertz is sample-rate divided by 2"
    (/ (/ radians-per-second (* 2 sp-pi)) sample-rate))

  (define (sp-factor->rads a sample-rate)
    "convert a fraction of the sample-rate to radian frequency" (* 2 sp-pi a sample-rate))

  (define (sp-filter-bank in points state)
    "samples ((cutoff-l cutoff-h transition-l transition-h) ...) list -> ((samples ...) . state)
     apply a series of band-pass filters and return the filtered bands as separate sample arrays.
     splits a signal into frequency bands.
     transition and cutoff are as fractions of the sample-rate 0..0.5.
     state is reset when length of points changes"
    (fold-right
      (l (a state result)
        (apply
          (l (cutoff-l cutoff-h transition-l transition-h)
            (let (out (sp-samples-copy-zero in))
              (sp-windowed-sinc-bp-br! out in cutoff-l cutoff-h transition-l transition-h #f state)
              (pair (pair out (first result)) (pair state (tail result)))))
          a))
      (pair null null) points
      (if (and state (= (length points) (length state))) state (make-list (length points) #f))))

  (define (differences a)
    "return a list of differences between each two subsequent values in a given list.
     result length is (length a) minus one.
     example: (differences (list 1 3 7 8 6)) -> (2 4 1 -2)"
    (pair-fold-right
      (l (a result) (if (null? (tail a)) result (pair (- (first (tail a)) (first a)) result))) null a))

  (define (sp-asymmetric-moving f current-value width state)
    "procedure real integer list -> (any:result-value . state)
     f :: current-value (previous-value ...) -> any
     apply f with the current value and previous input values and return
     a pair with the result of calling f and the state for the next call.
     width must be greater than zero.
     state can be the empty list for the first call"
    (pair (f current-value state)
      (pair current-value (if (< (length state) width) state (drop-right state 1)))))

  (define (sp-asymmetric-moving-out f current-value width state)
    "procedure real integer (real:previous-value ...) -> (any:result-value previous-value ...):state
     like sp-asymmetric-moving but f is called with previous output values"
    (pair (f current-value state) (if (< (length state) width) state (drop-right state 1))))

  (define (sp-asymmetric-moving-average current-value width state)
    "real integer list -> (result-value . state)
     a moving average filter that only uses the current and past values.
     the longer the width, the more calls with a higher value it takes to reach the higher value"
    (sp-asymmetric-moving
      (l (current previous)
        (/ (apply sp-float-sum (pair current previous)) (+ 1 (length previous))))
      current-value width state))

  (define (sp-asymmetric-moving-median current-value width state)
    "real integer list -> (result-value . state)
     a moving average filter that only uses the current and past values.
     the longer the width, the more calls with a higher value it takes to reach the higher value"
    (sp-asymmetric-moving
      (l (current previous)
        (let* ((sorted (list-sort < (pair current previous))) (size (length sorted)))
          (if (odd? size) (list-ref sorted (/ (- size 1) 2))
            (let ((index-a (- (/ size 2) 1)) (index-b (/ size 2)))
              (/ (+ (list-ref sorted index-a) (list-ref sorted index-b)) 2)))))
      current-value width state))

  (define (sp-change-limiter current-value width max-factor state)
    "real integer real list -> (real:result-value real ...):state
     prevents change from being greater than +- max-factor times the average of previous differences between result values.
     state can be the empty list for the first call"
    (sp-asymmetric-moving-out
      (l (current previous)
        (if (< (length previous) 2) current
          (let*
            ( (average-change (/ (abs (apply + (differences previous))) (- (length previous) 1)))
              (max-change (* max-factor average-change)))
            (max (min (+ max-change (first previous)) current) (- (first previous) max-change)))))
      current-value width state))

  (define (sp-fold-integers count f . init)
    "integer procedure any ... -> (any ...)
     f :: integer any:custom ... -> (any:custom ...)
     fold over integers from 0 to count minus 1 with zero or more separate state variables"
    (let loop ((a 0) (b init)) (if (< a count) (loop (+ 1 a) (apply f a b)) b)))

  (define (sp-sample-align f update-f x width . custom)
    "procedure procedure integer integer any ... -> (result-value . (x width custom ...))
     f :: x width custom ... -> (result custom ...)
     update-f :: width custom ... -> (width custom ...)
     f is called with the current x value that increases by one with each call, width and custom values.
     update-f is called after width number of calls to update or reset the width and custom values"
    (apply
      (l (x width . custom)
        (apply (l (result . custom) (pairs result x width custom)) (apply f x width custom)))
      (if (< x width) (apply list (+ 1 x) width custom)
        (apply list 1 (apply update-f width custom)))))

  (define (sp-sample-align-list size f update-f . custom)
    "calls sp-sample-align size times and returns the results in a list.
     example:
     (sp-sample-align-list sample-rate
       (l (x w h . a) (pairs h h a))
       (l (w h . a) (pairs (random-between 1 8) (random-between 0 200) a)) 0.0)"
    (reverse
      (first
        (sp-fold-integers size
          (l (x samples state)
            (apply (l (result . state) (list (pair result samples) state))
              (apply sp-sample-align f update-f state)))
          null (pair 0 (apply update-f 0 custom))))))

  (define* (sp-overlap a b #:optional (overlap-factor 0.5))
    "false/samples false/samples [real] -> false/samples
     return an overlapping portion between a and b if neither is false and both are large enough.
     otherwise return false"
    (and a b
      (let*
        ( (a-length (sp-samples-length a)) (b-length (sp-samples-length b))
          (overlap-length (inexact->exact (round (* a-length overlap-factor))))
          (ab-length (* 2 overlap-length)))
        (and (<= overlap-length b-length)
          (sp-samples-new ab-length
            (l (index)
              (let (a-index (+ (- a-length overlap-length) index))
                (if (< a-index a-length) (sp-samples-ref a a-index)
                  (sp-samples-ref b (- index overlap-length))))))))))

  (define*
    (sp-noise-band size center width state #:key (transition 0.08) (noise-f sp-noise-uniform~))
    "get a sample vector with noise in a specific frequency band.
     center, width and transition are as a fraction of the sample rate from 0 to 0.5"
    ; todo: change to start/end frequency and check if initial filter delay occurs
    (sp-windowed-sinc-bp-br (sp-samples-new size (l (index) (noise-f))) (- center (/ width 2))
      (+ center (/ width 2)) transition transition #f state))

  (define (sp-sine-of-width x width)
    "return a value for a repeating sine with given sample width at sample offset x"
    (sin (* x (/ (* 2 sp-pi) width))))

  (define (sp-samples-divide a divisor) (sp-samples-map (l (b) (if (zero? b) b (/ b divisor))) a))
  (define (sp-samples-multiply a factor) (sp-samples-map (l (b) (* b factor)) a))

  (define (sp-fft-resynth f a)
    "map the frequency domain of time domain samples.
     call f with the result of a fft on samples and pass the result to fftri
     and scale output to match input.
     example
     (define samples (sp-fft-resynth (lambda (a) (vector-set! a 300 (make-rectangular 500 0)) a) sine))"
    (sp-samples-divide (sp-fftri (f (sp-fftr a))) (sp-samples-length a)))

  (define (sp-fold-file f segment-size path . custom)
    "procedure integer string any ... -> any
     f :: #(samples ...):channels custom ... -> custom
     fold over sample vectors read from file"
    (let*
      ( (input (sp-file-open path sp-port-mode-read))
        (result
          (let loop ((a (sp-port-read input segment-size)))
            (if (vector? a)
              (apply f a
                (if (= segment-size (sp-samples-length (vector-first a)))
                  (loop (sp-port-read input segment-size)) custom))
              custom))))
      (sp-port-close input) result))

  (define (sp-fold-file-overlap f segment-size overlap-factor path . custom)
    "procedure integer real string any ... -> any
     f :: #(samples:channel ...) any ... -> (any ...)
     like sp-fold-file but additionally maps sample arrays build
     from overlap-factor number of samples from the end of each previous
     and beginning of each current sample array"
    (tail
      (apply sp-fold-file
        (l (a previous . custom)
          (let
            (overlap
              (and (not (null? previous))
                (vector-map (l (a b) (sp-overlap a b overlap-factor)) (first previous) a)))
            (pair (pair a previous)
              (if overlap (apply f a (apply f overlap custom)) (apply f a custom)))))
        segment-size path null custom)))

  (define* (sp-samples-extract input start count)
    "integer integer samples -> samples
     extract from input beginning from index start as many samples as available
     up to a maximum of count.
     start must be in bounds"
    (sp-samples-new (min count (- (sp-samples-length input) start))
      (l (index) (sp-samples-ref input (+ start index)))))

  (define* (sp-samples-extract-padded input start count)
    "samples integer integer -> samples
     treat input as a sample vector surrounded by an infinite number of zero samples
     where input starts at index zero.
     extract count number of samples beginning from index start.
     start can be negative and start plus count can go over the length of input and the result will
     be zero padded accordingly"
    (let*
      ( (input-size (sp-samples-length input)) (output (sp-samples-new count 0))
        (output-start (if (< start 0) (* -1 start) 0)) (input-start (if (< start 0) 0 start))
        (input-end (min input-size (+ input-start (max 0 (- count output-start)))))
        (copy-count (- input-end input-start)))
      (let loop ((i 0))
        (if (< i copy-count)
          (begin
            (sp-samples-set! output (+ output-start i) (sp-samples-ref input (+ input-start i)))
            (loop (+ 1 i)))))
      output))

  (define (sp-samples-split b count)
    "samples integer -> (samples ...)
     splits a sample vector into count multiple sample vectors.
     if (input-size / count) is not an integer then it is rounded to the next higher integer
     and the part sample vector is zero padded to be of equal size as all other parts"
    (let* ((input-size (sp-samples-length b)) (part-size (ceiling (/ input-size count))))
      (let loop ((index 0))
        (if (< index input-size)
          (pair (sp-samples-extract-padded b index part-size) (loop (+ part-size index))) null))))

  (define* (sp-samples-list-add-offsets b #:optional (start 0))
    "(samples ...) [integer] -> ((sample-offset samples) ...)
     map each samples vector in input to a pair with the cumulative sample
     offset of the length of sample vectors starting from start.
     for example a list with sample vector sizes 8 2 3 would create
     a list ((0 samples) (8 samples) (10 samples))"
    (let loop ((b b) (offset start))
      (if (null? b) b
        (pair (pair offset (first b)) (loop (tail b) (+ (sp-samples-length (first b)) offset))))))

  (define (sp-scheduler additions output-size state)
    "additions integer state/false -> (output:samples/false state)
     additions: ((integer:sample-offset . samples) ...)/false
     add sample vectors to be included in output sample vectors after specified sample offsets.
     * length of output is always output-size per call
     * length of total output can be longer than total combined input. output is buffered and zero padded as needed
     * addition offsets are always relative to the current call
     * if additions is null then scheduled output is returned until none is left in which case output will be false
     * state: (integer:sample-counter . list:sorted-scheduled-segments)
     example use cases: delay lines, grain duplication/reduction or basic sequencing
     example
       (let*
         ( (state1 (sp-scheduler (list (pair 8 samples)) 10 #f))
           (state2 (sp-scheduler null 10 (tail state1))))
         (list (first state1) (first state2)))"
    (let*
      ( (state (or state (list 0 null))) (offset (first state)) (scheduled (second state))
        (next-offset (+ offset output-size)) (output (sp-samples-new output-size 0))
        (additions (map (l (b) (pair (max offset (+ offset (first b))) (tail b))) additions))
        (scheduled
          (fold
            (l (b result)
              (let ((samples-offset (first b)) (samples (tail b)))
                (if (< samples-offset next-offset)
                  (let*
                    ( (samples-len (sp-samples-length samples))
                      (count-that-fits (min samples-len (- next-offset samples-offset)))
                      (count-that-doesnt-fit (- samples-len count-that-fits)))
                    (each-integer count-that-fits
                      (l (i)
                        (let (output-i (+ (- samples-offset offset) i))
                          (sp-samples-set! output output-i
                            (+ (sp-samples-ref output output-i) (sp-samples-ref samples i))))))
                    (if (zero? count-that-doesnt-fit) result
                      (pair
                        (pair next-offset
                          (sp-samples-extract samples count-that-fits count-that-doesnt-fit))
                        result)))
                  (pair b result))))
            null (append additions scheduled))))
      (list output next-offset scheduled)))

  (define (sp-grain-map input count f state)
    "samples/integer integer procedure false/state -> (false/samples:output . state)
     f :: ((offset . samples:grain) ...) -> ((offset . samples) ...)
     if input is an integer, it returns unemitted output of that integer length.
     if there is no more output then output is false"
    (if (integer? input) (sp-scheduler null input state)
      (sp-scheduler (f (sp-samples-list-add-offsets (sp-samples-split input count) 0))
        (sp-samples-length input) state)))

  (define (sp-fold-frames f input frame-size overlap-factor . custom)
    "procedure samples integer real:0..1 any ... -> (any ...):custom
     f :: samples any:custom ... -> (custom ...)
     call f with each overlapping frame of input samples and other custom values.
     # example
     frame-size: 100, overlap-factor: 0.5, frames: -50..50 0..100 50..150"
    (let*
      ( (hop-size (inexact->exact (round (* overlap-factor frame-size))))
        (input-size (sp-samples-length input)))
      (let loop ((i 0) (custom custom))
        (if (<= (+ frame-size i) input-size)
          (loop (+ hop-size i) (apply f (sp-samples-extract-padded input i frame-size) custom))
          custom))))

  (define (sp-samples-threshold a limit)
    "set to zero every sample with absolute value below threshold value"
    (sp-samples-map (l (a) (absolute-threshold a limit)) a)))
