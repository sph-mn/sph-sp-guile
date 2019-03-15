(library (sph sp vectorise)
  (export
    sp-vectorise)
  (import
    (rnrs sorting)
    (sph)
    (sph list)
    (sph math)
    (sph sp)
    (sph vector)
    (only (guile)
      floor
      inexact->exact
      display)
    (only (srfi srfi-1) drop split-at))

  (define sph-sp-vectorise-description
    "experimental. convert a time/magnitude samples array to sines and noise parameters that can be used to approximately recreate the sound.
     uses a multiresolution fast fourier transform based analysis that balances time and frequency resolution based on amount of change
     and other weights")

  (define (sp-fft-bins-magnitudes a) (vector-map (l (b) (/ (magnitude b) (vector-length a))) a))
  (define (sp-fft-drop-dc a) (list->vector (drop (vector->list a) 1)))

  (define* (sp-fft-threshold a #:optional (max-factor 1.0e-4))
    "#(complex ...) real -> #(complex ...)
     remove all values with magnitudes smaller than (max / max-divisor).
     this is mainly to remove non-useful rounding error phase values that would lead to large angles"
    (let*
      ( (a-list (vector->list a)) (magnitudes (map magnitude a-list))
        (threshold (* (apply max magnitudes) max-factor)))
      (apply vector (map (l (a b) (if (> threshold b) 0 a)) a-list magnitudes))))

  (define (sp-frames-count data-size duration overlap-factor)
    "calculate how many frames with overlap can be taken from data-size"
    (- (/ data-size (* duration overlap-factor)) (- (/ 1 overlap-factor) 1)))

  (define (sp-vectorise-fft-series f input frame-size padded-frame-size . custom)
    "return the result for ffts on all frames of frame-size that fit into input with an overlap.
     fft input is zero padded so that results have the same size for different effective input
     durations"
    (let
      ( (overlap-factor 0.5) (input-threshold 0.001) (bin-magnitude-threshold 0.01)
        (window-f sp-samples-apply-hann-window))
      (apply sp-fold-frames
        (l (input . custom)
          (apply f
            (sp-fft-threshold
              (sp-fft-drop-dc
                (sp-fftr
                  (sp-samples-threshold
                    (sp-samples-extract-padded (window-f input)
                      (round (/ (- frame-size padded-frame-size) 2)) padded-frame-size)
                    input-threshold)))
              bin-magnitude-threshold)
            custom))
        input frame-size overlap-factor custom)))

  (define (debug-log-line a . b) (display-line (pair a b)) (display-line "") a)

  (define (sp-vectorise-series input duration scaled-duration)
    "samples integer integer -> ((vector:bins number:change integer:duration):series-element ...):series"
    (reverse
      (first
        (sp-vectorise-fft-series
          (l (bins result previous-bin-magnitudes . custom)
            (let*
              ( (bin-magnitudes (sp-fft-bins-magnitudes bins))
                (change
                  (if previous-bin-magnitudes
                    (absolute-threshold
                      (vector-relative-change-index/value previous-bin-magnitudes bin-magnitudes)
                      0.001)
                    0)))
              (pairs (pair (list bins change duration) result) bin-magnitudes custom)))
          input duration scaled-duration null #f))))

  (define (combine-one a b a-change b-change duration-offset count-ratio)
    "receives the same number of bins for the same frequency ranges and combines them.
     this is the function that adds information from higher resolution fft results to lower resolution results"
    ; the weighting could be improved. the most useful feature is perhaps that it is or can be like an duration-adaptive fft
    (let*
      ( (a-magnitudes (map magnitude a)) (a-imaginary (map imag-part a))
        (a-mean (arithmetic-mean a-magnitudes)) (a-imaginary-mean (arithmetic-mean a-imaginary))
        (b-magnitudes (map magnitude b)) (b-imaginary (map imag-part b)))
      (map complex-from-magnitude-and-imaginary
        (linearly-interpolate (/ 1 (+ 1 duration-offset)) a-magnitudes b-magnitudes)
        (linearly-interpolate (/ 1 (+ 1 duration-offset)) a-magnitudes b-magnitudes))))

  (define (combine-series-one a b duration-offset count-ratio)
    "(series-element ...) (series-element) integer -> (series-element ...)
     adds information from series b to a.
     each element has the same bin count but effective input durations are different between series
     and are scaled to match in size"
    (map
      (l (a)
        (apply
          (l (a-bins a-change a-duration b-bins b-change b-duration)
            (let loop ((index count-ratio))
              (if (< index (vector-length a-bins))
                (let
                  (values
                    (map
                      (l (bins) (map-integers count-ratio (l (c) (vector-ref bins (- index c)))))
                      (list a-bins b-bins)))
                  (each-with-index
                    (l (c-index c-value) (vector-set! a-bins (- index c-index) c-value))
                    (combine-one (first values) (second values)
                      a-change b-change duration-offset count-ratio))
                  (loop (+ count-ratio index)))
                (list a-bins b-change a-duration))))
          (append a b)))
      a))

  (define (combine-series a b duration-offset)
    "(series-element ...) (series-element ...) integer -> (series-element ...)"
    (let (count-ratio (inexact->exact (floor (/ (length a) (length b)))))
      (let loop ((a a) (b b) (result null))
        (if (null? b) result
          (let (a (apply-values pair (split-at a count-ratio)))
            (loop (tail a) (tail b)
              (append result (combine-series-one (first a) (first b) duration-offset count-ratio))))))))

  (define* (sp-vectorise input #:key duration)
    "samples [integer] -> (series-element ...)
     duration is an exponent for (2 ** exponent)
     result length is the length of the first series.
     result elements are (2**duration * overlap-factor) samples apart.
     analyses only parts of input that powers of two length frames fit into"
    (let*
      ( (max-exponent (inexact->exact (floor (log2 (sp-samples-length input)))))
        (max-duration (expt 2 max-exponent)) (min-exponent (or duration (round (/ max-exponent 2)))))
      (let loop ((exponent min-exponent) (result null))
        (if (<= exponent max-exponent)
          (let (series (sp-vectorise-series input (expt 2 exponent) max-duration))
            (loop (+ 1 exponent)
              (if (null? result) series (combine-series result series (- exponent min-exponent)))))
          result)))))
