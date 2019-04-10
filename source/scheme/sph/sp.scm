(library (sph sp)
  (export
    f32vector-sum
    f64-nearly-equal?
    f64vector-sum
    sp-asymmetric-moving
    sp-asymmetric-moving-average
    sp-asymmetric-moving-median
    sp-asymmetric-moving-out
    sp-block-new
    sp-block-overlap
    sp-call-with-output-file
    sp-change-limiter
    sp-cheap-filter!
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
    sp-moving-average
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
    sp-windowed-sinc-bp-br
    sp-windowed-sinc-bp-br!
    sp-windowed-sinc-bp-br-ir
    sp-windowed-sinc-lp-hp
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

  (define* (sp-moving-average in prev next radius #:optional in-start in-count out-start)
    "samples false/samples false/samples integer [integer/false integer/false] -> samples"
    (sp-samples-copy-zero* in
      (l (out)
        (sp-moving-average! out in
          prev next radius (or in-start 0) (or in-count (sp-samples-length in)) (or out-start 0)))))

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
      (list out (sp-windowed-sinc-lp-hp! out in cutoff transition is-high-pass state))))

  (define* (sp-windowed-sinc-bp-br in cutoff-l cutoff-h transition-l transition-h is-reject state)
    "samples real real real boolean false/convolution-filter-state -> samples
     like sp-windowed-sinc-bp-br! but creates a new output samples vector as long as the input.
     state is still eventually going to be modified"
    (let (out (sp-samples-copy-zero in))
      (list out
        (sp-windowed-sinc-bp-br! out in cutoff-l cutoff-h transition-l transition-h is-reject state))))

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
    (sp-multipass-transfer! transfer-f out in passes state #:optional in-start in-count out-start)
    "procedure samples samples integer pair [integer integer integer] -> state
     transfers in to out via one or multiple passes through one-pass-f possibly using temporary buffers.
     the last samples from result out and in for each pass are kept in state to be passed to the next call to sp-multipass-transfer
     in case it is needed in the next call to keep processing seamless.
     transfer-f :: sample:in sample:prev-in sample:prev-out -> sample"
    ; keeps seamlessness between calls and between pass count changes.
    ; the last input/output samples per pass are kept in state.
    ; for multiple passes, uses two temporary buffers and writes to output on the final pass.
    (define (transfer transfer-f out in in-start in-count out-start prev-in prev-out)
      "call transfer-f for current in values including the previous value to be able to make it seamless if
       the function depends on the previous values"
      (sp-samples-set! out out-start (transfer-f (sp-samples-ref in in-start) prev-in prev-out))
      (each-integer (- in-count 1)
        (l (index)
          (let ((out-index (+ out-start index)) (in-index (+ in-start index)))
            (sp-samples-set! out out-index
              (transfer-f (sp-samples-ref in in-index) (sp-samples-ref in (- in-index 1))
                (sp-samples-ref out (- out-index 1))))))
        1))
    (define (get-last samples offset length) (sp-samples-ref samples (+ offset (- length 1))))
    (define (prev-next a in out in-start out-start)
      "if there is no previous data for the current pass then take from current data"
      (let (rest (tail a))
        (if (null? rest)
          (pair (pair (sp-samples-ref in in-start) (sp-samples-ref out out-start)) rest) rest)))
    (let (prev (or state (list (pair (sp-samples-ref in in-start) 0))))
      (if (= 1 passes)
        (let (prev (first prev))
          (transfer transfer-f out in in-start in-count out-start (first prev) (tail prev))
          (list (pair (get-last in in-start in-count) (get-last out out-start in-count))))
        (let*
          ( (in-temp
              (let (a (sp-samples-new in-count))
                ; first pass
                (transfer transfer-f a
                  in in-start in-count 0 (first (first prev)) (tail (first prev)))
                a))
            (prev-rest (prev-next prev in in-temp in-start 0))
            (result-prev
              (list (pair (get-last in in-start in-count) (get-last in-temp 0 in-count)))))
          (let loop
            ( (passes (- passes 1)) (in-temp in-temp) (out-temp (sp-samples-new in-count))
              (prev-rest prev-rest) (prev (first prev-rest)) (result-prev result-prev))
            (if (< 1 passes)
              (begin
                ; more than one pass left
                (transfer transfer-f out-temp in-temp 0 in-count 0 (first prev) (tail prev))
                (let
                  ( (prev-rest (prev-next prev-rest in-temp out-temp 0 0))
                    (result-prev
                      (pair (pair (get-last in-temp 0 in-count) (get-last out-temp 0 in-count))
                        result-prev)))
                  (loop (- passes 1) out-temp
                    (sp-samples-zero! in-temp) prev-rest (first prev-rest) result-prev)))
              (begin
                ; last pass
                (transfer transfer-f out in-temp 0 in-count out-start (first prev) (tail prev))
                (reverse
                  (pair (pair (get-last in-temp 0 in-count) (get-last out out-start in-count))
                    result-prev)))))))))

  (define*
    (sp-state-variable-filter type out in cutoff resonance state #:optional (in-start 0)
      (out-start 0)
      (in-count (- (sp-samples-length in) in-start)))
    "symbol:low/high/band/notch/peak/all samples samples real real pair [integer integer integer] -> state
     a fast filter that supports multiple filter types in one.
     cutoff is as a fraction of the sample rate between 0 and 0.5.
     uses the state-variable filter described here:
     * http://www.cytomic.com/technical-papers
     * http://www.earlevel.com/main/2016/02/21/filters-for-synths-starting-out/"
    (define (transfer-f g a1 a2 f)
      (l (index state)
        "integer pair -> pair
      calculate shared base values, set output at index to the result of calling f, then return the new state"
        (let*
          ( (ic1eq (first state)) (ic2eq (tail state)) (v0 (sp-samples-ref in index))
            (v1 (+ (* a1 ic1eq) (* a2 (- v0 ic2eq)))) (v2 (+ ic2eq (* g v1))))
          (sp-samples-set! out index (f v0 v1 v2)) (pair (- (* 2 v1) ic1eq) (- (* 2 v2) ic2eq)))))
    (let*
      ( (g (tan (* sp-pi cutoff))) (k (- 2 (* 2 resonance))) (a1 (/ 1 (+ 1 (* g (+ g k)))))
        (a2 (* g a1)))
      (fold-integers in-count (or state (pair 0 0))
        (transfer-f g a1
          a2
          (case type
            ((low) (l (v0 v1 v2) v2))
            ((high) (l (v0 v1 v2) (- v0 (* k v1) v2)))
            ((band) (l (v0 v1 v2) v1))
            ((notch) (l (v0 v1 v2) (- v0 (* k v1))))
            ((peak) (l (v0 v1 v2) (+ (- (* 2 v2) v0) (* k v1))))
            ((all) (l (v0 v1 v2) (- v0 (* 2 k v1)))))))))

  (define*
    (sp-samples-passthrough out in #:optional (in-start 0)
      (in-count (- (sp-samples-length in) in-start))
      (out-start 0))
    (each-integer in-count
      (l (index) (sp-samples-set! out (+ out-start index) (sp-samples-ref in (+ in-start index))))))

  (define (sp-one-pole-lp out in cutoff passes state in-start in-count out-start)
    "the higher the cutoff, the longer the transition band of the one-pole. that is why it isnt suited for band-pass"
    (sp-multipass-transfer!
      (l (in prev-in prev-out) (float-sum prev-out (* (* cutoff 2) (float-sum in (- prev-out))))) out
      in passes state in-start in-count out-start))

  (define (sp-one-pole-hp out in cutoff passes state in-start in-count out-start)
    (sp-multipass-transfer!
      (l (in prev-in prev-out) (* (* cutoff 2) (float-sum prev-out in (- prev-in)))) out
      in passes state in-start in-count out-start))

  (define*
    (sp-cheap-filter! out in cut-l cut-h passes state #:key (in-start 0)
      (in-count (sp-samples-length in))
      (out-start 0)
      (unity-gain #t)
      one-pole)
    "samples samples real pair [integer integer integer integer] -> samples
     a fast high/low/band-pass filter with a long transition band, defined by
       y[i] := (1 - cutoff) * (y[i-1] + x[i] - x[i-1])
     cutoff is between 0 and 1 inclusively. cutoff values of 0 and 1 will lead to passthrough or zero output respectively"
    (let (state (or state (pair #f #f)))
      (cond
        ( (or (= 0.5 cut-l) (= 0 cut-h)) (sp-samples-zero! out out-start)
          (pair (list (pair 0 0)) (pair 0 0)))
        ( (and (= 0.5 cut-h) (= 0 cut-l))
          (sp-samples-passthrough out in in-start in-count out-start)
          (pair
            (list
              (pair (sp-samples-ref out (+ out-start (- in-count 1)))
                (sp-samples-ref in (+ in-start (- in-count 1)))))
            (pair 0 0)))
        ( (= 0.5 cut-h)
          (begin-first
            (if one-pole
              (pair (sp-one-pole-hp out in cut-l passes (first state) in-start in-count out-start)
                #f)
              (pair #f (sp-state-variable-filter (q high) out in cut-l 0 (tail state))))
            (if unity-gain (sp-set-unity-gain out in in-start in-count out-start))))
        ( (= 0 cut-l)
          (begin-first
            (if one-pole
              (pair (sp-one-pole-lp out in cut-h passes (first state) in-start in-count out-start)
                #f)
              (pair #f (sp-state-variable-filter (q low) out in cut-h 0 (tail state))))
            (if unity-gain (sp-set-unity-gain out in in-start in-count out-start))))
        (else
          (begin-first
            (if one-pole
              (pair
                (let (temp (sp-samples-new in-count))
                  (sp-one-pole-lp temp in cut-h passes (first state) in-start in-count out-start)
                  (sp-one-pole-hp out temp cut-l passes (first state) in-start in-count out-start))
                #f)
              (pair #f
                (sp-state-variable-filter (q band) out
                  in (+ cut-l (/ (- cut-h cut-l) 2)) 0 (tail state))))
            (if unity-gain (sp-set-unity-gain out in in-start in-count out-start))))))))
