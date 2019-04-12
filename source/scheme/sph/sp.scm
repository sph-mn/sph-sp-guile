(library (sph sp)
  (export
    f32vector-sum
    f64-nearly-equal?
    f64vector-sum
    sp-block-new
    sp-block-overlap
    sp-call-with-output-file
    sp-change-limiter
    sp-convolution-filter!
    sp-convolve
    sp-convolve!
    sp-fft
    sp-fft-resynth
    sp-ffti
    sp-fftr
    sp-fftri
    sp-file-channel-count
    sp-file-close
    sp-file-fold
    sp-file-fold-overlap
    sp-file-input?
    sp-file-mode-read
    sp-file-mode-read-write
    sp-file-mode-write
    sp-file-open
    sp-file-position
    sp-file-position-set
    sp-file-position?
    sp-file-read
    sp-file-sample-rate
    sp-file-write
    sp-file?
    sp-filter-bank
    sp-float-sum
    sp-fold-frames
    sp-fold-integers
    sp-grain-map
    sp-map-fold-integers
    sp-moving-average!
    sp-pi
    sp-plot-samples
    sp-plot-samples->file
    sp-plot-samples-display-file
    sp-plot-spectrum
    sp-plot-spectrum->file
    sp-plot-spectrum-display-file
    sp-sample-sum
    sp-samples->list
    sp-samples-absolute-max
    sp-samples-apply-blackman-window
    sp-samples-apply-hann-window
    sp-samples-copy
    sp-samples-copy-zero
    sp-samples-copy-zero*
    sp-samples-divide
    sp-samples-each-index
    sp-samples-extract
    sp-samples-extract-padded
    sp-samples-from-list
    sp-samples-length
    sp-samples-list-add-offsets
    sp-samples-map
    sp-samples-map!
    sp-samples-map-with
    sp-samples-map-with!
    sp-samples-map-with-index
    sp-samples-multiply
    sp-samples-new
    sp-samples-new-f
    sp-samples-passthrough
    sp-samples-ref
    sp-samples-set!
    sp-samples-split
    sp-samples-threshold
    sp-samples-zero!
    sp-samples?
    sp-scheduler
    sp-set-unity-gain
    sp-sinc
    sp-spectrum
    sp-window-hann
    sp-windowed-sinc-bp-br!
    sp-windowed-sinc-bp-br-ir
    sp-windowed-sinc-lp-hp!
    sp-windowed-sinc-lp-hp-ir
    sph-sp-description)
  (import
    (sph)
    (sph uniform-vector)
    (only (guile)
      call-with-output-file
      load-extension
      inexact->exact
      make-list
      f64vector-length
      f64vector-set!
      f64vector-ref
      list->f64vector
      f64vector->list
      f64vector?
      make-f64vector
      system*
      tmpnam)
    (only (rnrs sorting) list-sort)
    (only (sph list) fold-integers map-integers)
    (only (sph math) absolute-threshold differences)
    (only (sph number) float-sum)
    (only (sph other) begin-first each-integer)
    (only (sph vector) vector-each vector-first)
    (only (srfi srfi-1) drop-right))

  (load-extension "libguile-sph-sp" "sp_guile_init")

  (define* (sp-samples-new length #:optional (value 0))
    (if (procedure? value) (f64vector-create length value) (make-f64vector length value)))

  (define sph-sp-description
    "sph-sp bindings and additional utilities.
     block: (samples:channel ...)")

  (define sp-pi (* 4 (atan 1)))
  (define sp-float-sum float-sum)
  (define sp-sample-sum float-sum)
  (define sp-samples-map f64vector-map)
  (define sp-samples-map-with f64vector-map-with)
  (define sp-samples-map-with! f64vector-map-with!)
  (define sp-samples-map-with-index f64vector-map-with-index)
  (define sp-samples-map! f64vector-map!)
  (define sp-samples-each-index f64vector-each-index)
  (define sp-samples-set! f64vector-set!)
  (define sp-samples-ref f64vector-ref)
  (define sp-samples? f64vector?)
  (define sp-samples->list f64vector->list)
  (define sp-samples-from-list list->f64vector)
  (define sp-samples-copy-zero f64vector-copy-zero)
  (define sp-samples-copy-zero* f64vector-copy-zero*)
  (define sp-samples-length f64vector-length)
  (define sp-samples-copy f64vector-copy)
  (define sp-samples-range-map! f64vector-range-map!)
  (define (sp-samples-divide a divisor) (sp-samples-map (l (b) (if (zero? b) b (/ b divisor))) a))
  (define (sp-samples-multiply a factor) (sp-samples-map (l (b) (* b factor)) a))
  (define (sp-block-new channels size) (map-integers channels (l (n) (sp-samples-new size))))

  (define (sp-fftr a) "samples -> #(complex ...)"
    (let*
      ( (b (sp-fft (list->vector (map (l (a) (make-rectangular a 0)) (sp-samples->list a)))))
        (c-len (+ 1 (/ (vector-length b) 2))) (c (make-vector c-len 0)))
      (let loop ((i 0)) (if (< i c-len) (begin (vector-set! c i (vector-ref b i)) (loop (+ 1 i))))) c))

  (define (sp-fftri a) "#(complex ...) -> samples"
    (sp-samples-from-list (map real-part (vector->list (sp-ffti a)))))

  (define (sp-spectrum a) "samples -> #(real ...)"
    (vector-map (l (b) (* 2 (/ (magnitude b) (sp-samples-length a)))) (sp-fftr a)))

  (define* (sp-plot-samples-display-file file-path #:key (type (q lines)) (color "blue"))
    "string #:type symbol:lines/points #:color string -> unspecified
     type and color correspond to gnuplot options"
    (system* "gnuplot" "--persist"
      "-e"
      (string-append "set key off; set size ratio 0.5; plot \"" file-path
        "\"" " with "
        (if (string? type) type
          (case type
            ((points) "points pointtype 5 ps 0.3")
            (else "lines")))
        " lc rgb \"" color "\"")))

  (define (sp-plot-samples->file a path)
    (call-with-output-file path
      (l (file) (each (l (a) (display-line a file)) (sp-samples->list a)))))

  (define (sp-plot-samples a . display-args)
    "samples [#:type #:color] -> unspecified
     for display-args see sp-plot-samples-display-file"
    (let (path (tmpnam)) (sp-plot-samples->file a path)
      (apply sp-plot-samples-display-file path display-args)))

  (define (sp-plot-spectrum-display-file path) (sp-plot-samples-display-file path #:type "histeps"))

  (define (sp-plot-spectrum->file a path)
    "apply sp-spectrum on \"a\" and write the result to file at path"
    (call-with-output-file path
      (l (file) (vector-each (l (a) (display-line a file)) (sp-spectrum a)))))

  (define (sp-plot-spectrum a)
    (let (path (tmpnam)) (sp-plot-spectrum->file a path) (sp-plot-spectrum-display-file path)))

  (define (sp-samples-threshold a limit)
    "set to zero every sample with absolute value below threshold value"
    (sp-samples-map (l (a) (absolute-threshold a limit)) a))

  (define (sp-map-fold-integers count f . custom)
    "f :: integer custom ... -> (map-result custom ...)
     map the first result element and fold the rest"
    (let loop ((i 0) (map-result null) (custom custom))
      (if (< i count)
        (apply (l (a . custom) (loop (+ 1 i) (pair a map-result) custom)) (apply f i custom))
        (pair (reverse map-result) custom))))

  (define (sp-fold-integers count f . init)
    "integer procedure any ... -> (any ...)
     f :: integer any:custom ... -> (any:custom ...)
     fold over integers from 0 to count minus 1 with zero or more custom state variables"
    (let loop ((a 0) (b init)) (if (< a count) (loop (+ 1 a) (apply f a b)) b)))

  (define* (sp-convolve a b #:optional carryover carryover-len)
    (let
      ( (result (sp-samples-copy-zero a))
        (carryover
          (if carryover (sp-samples-copy carryover) (sp-samples-new (- (sp-samples-length b) 1) 0))))
      (sp-convolve! result a b carryover carryover-len) (pair result carryover)))

  (define (sp-window-hann offset size) (* 0.5 (- 1 (cos (/ (* 2 sp-pi offset) (- size 1))))))

  (define (sp-sinc a) "the normalised sinc function"
    ; re-implemented in scheme
    (if (= 0 a) 1 (/ (sin (* sp-pi a)) (* sp-pi a))))

  (define (sp-call-with-output-file path channels sample-rate f)
    (let* ((file (sp-file-open path sp-file-mode-write channels sample-rate)) (result (f file)))
      (sp-file-close file) result))

  (define (sp-file-fold f segment-size path . custom)
    "procedure integer string any ... -> any
     f :: #(samples ...):channels custom ... -> custom
     fold over sample vectors read from file"
    (let*
      ( (input (sp-file-open path sp-file-mode-read))
        (result
          (let loop ((a (sp-file-read input segment-size)))
            (if (vector? a)
              (apply f a
                (if (= segment-size (sp-samples-length (vector-first a)))
                  (loop (sp-file-read input segment-size)) custom))
              custom))))
      (sp-file-close input) result))

  (define (sp-file-fold-overlap f segment-size overlap-factor path . custom)
    "procedure integer real string any ... -> any
     f :: #(samples:channel ...) any ... -> (any ...)
     like sp-file-fold but additionally maps sample arrays build
     from overlap-factor number of samples from the end of each previous
     and beginning of each current sample array"
    (tail
      (apply sp-file-fold
        (l (a previous . custom)
          (let
            (overlap
              (and (not (null? previous))
                (vector-map (l (a b) (sp-block-overlap a b overlap-factor)) (first previous) a)))
            (pair (pair a previous)
              (if overlap (apply f a (apply f overlap custom)) (apply f a custom)))))
        segment-size path null custom)))

  (define (sp-fold-frames f input frame-size overlap-factor . custom)
    "procedure samples integer real:0..1 any ... -> (any ...):custom
     f :: samples any:custom ... -> (custom ...)
     call f with each overlapping frame from input samples and other custom values.
     # example
     frame-size: 100, overlap-factor: 0.5, frames: -50..50 0..100 50..150"
    (let*
      ( (hop-size (inexact->exact (round (* overlap-factor frame-size))))
        (input-size (sp-samples-length input)))
      (let loop ((i 0) (custom custom))
        (if (<= (+ frame-size i) input-size)
          (loop (+ hop-size i) (apply f (sp-samples-extract-padded input i frame-size) custom))
          custom))))

  (define* (sp-block-overlap a b #:optional (overlap-factor 0.5))
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

  (define (sp-grain-map input count f state)
    "samples/integer integer procedure false/state -> (false/samples:output . state)
     f :: ((offset . samples:grain) ...) -> ((offset . samples) ...)
     if input is an integer, it returns unemitted output of that integer length.
     if there is no more output then output is false"
    (if (integer? input) (sp-scheduler null input state)
      (sp-scheduler (f (sp-samples-list-add-offsets (sp-samples-split input count) 0))
        (sp-samples-length input) state)))

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

  (define (sp-fft-resynth f a)
    "samples procedure:{#(complex ...) -> #(complex ...)} -> samples
     map the frequency domain of time domain samples.
     call f with the result of a fft on samples and pass the result to fftri
     and scale output to match input.
     example
     (define samples (sp-fft-resynth (lambda (a) (vector-set! a 300 (make-rectangular 500 0)) a) sine))"
    (sp-samples-divide (sp-fftri (f (sp-fftr a))) (sp-samples-length a)))

  (define*
    (sp-samples-zero! a #:optional (out-start 0) (count (- (sp-samples-length a) out-start)))
    "samples -> samples
     set all values in sample array to zero and return it"
    (each-integer count (l (index) (sp-samples-set! a index 0)) out-start) a)

  (define* (sp-samples-absolute-max in #:optional (in-start 0) (in-count (sp-samples-length in)))
    "samples [integer integer] -> sample
     get the maximum value in samples array, disregarding sign"
    (fold-integers in-count 0
      (l (index result)
        (let (a (abs (sp-samples-ref in (+ in-start index)))) (if (> a result) a result)))))

  (define (sp-set-unity-gain out in in-start in-count out-start)
    "adjust amplitude of out to match the one of in"
    (let*
      ( (difference
          (/ (sp-samples-absolute-max out out-start in-count)
            (sp-samples-absolute-max in in-start in-count)))
        (correction (float-sum 1 (/ (- 1 difference) difference))))
      (if (not (zero? difference))
        (each-integer in-count
          (l (index)
            (sp-samples-set! out (+ out-start index)
              (* correction (sp-samples-ref out (+ out-start index)))))))))

  (define*
    (sp-samples-passthrough out in #:optional (in-start 0)
      (in-count (- (sp-samples-length in) in-start))
      (out-start 0))
    (each-integer in-count
      (l (index) (sp-samples-set! out (+ out-start index) (sp-samples-ref in (+ in-start index)))))))
