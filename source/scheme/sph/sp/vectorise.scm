(library (sph sp vectorise)
  (export
    sp-vectorise)
  (import
    (rnrs sorting)
    (sph)
    (sph list)
    (sph math)
    (sph other)
    (sph sp)
    (sph sp experimental)
    (sph vector)
    (only (guile) floor inexact->exact)
    (only (srfi srfi-1) drop split-at))

  (define sph-sp-vectorise-description
    "experimental. convert a time/magnitude samples array to sines and noise parameters that can be used to approximately recreate the sound.
     uses a multiresolution fast fourier transform based analysis that balances time and frequency resolution based on amount of change
     and other weights")

  (define (sp-fft-bins-magnitudes a) (vector-map (l (b) (/ (magnitude b) (vector-length a))) a))
  (define (sp-fft-drop-dc a) (list->vector (drop (vector->list a) 1)))

  (define (sp-frames-count data-size duration overlap-factor)
    "calculate how many frames with overlap can be taken from data-size"
    (- (/ data-size (* duration overlap-factor)) (- (/ 1 overlap-factor) 1)))

  (define (sp-vectorise-fft-series f input frame-size padded-frame-size . custom)
    "return the result for ffts on all frames of frame-size that fit into input with an overlap.
     fft input is zero padded so that results have the same size for different effective input
     durations"
    (let
      ( (overlap-factor 0.5) (input-threshold 0.001) (bin-magnitude-threshold 100)
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
                      1.0e-4)
                    0)))
              (pairs (pair (list bins change duration) result) bin-magnitudes custom)))
          input duration scaled-duration null #f))))

  (define (complex-from-magnitude-and-imaginary m i)
    "create a complex number from a magnitude and the imaginary part of the number"
    ; sqrt gives a complex number if the input value is negative
    (let* ((a (- (* m m) (* i i))) (b (sqrt (abs a))) (c (if (< a 0) (- b) b)))
      (make-rectangular c i)))

  (define (combine-one a b)
    "receives the same number of bins for the same frequency ranges and combines them.
     this is the function that adds information from higher resolution fft results to lower resolution results"
    (let*
      ( (a-magnitudes (map magnitude a)) (a-imaginary (map imag-part a))
        (a-mean (arithmetic-mean a-magnitudes)) (b-magnitudes (map magnitude b))
        (b-imaginary (map imag-part b)))
      (map complex-from-magnitude-and-imaginary (scale-to-mean a-mean b-magnitudes) b-imaginary)))

  (define (combine-series-one a b duration-offset count-ratio)
    "(series-element ...) (series-element) integer -> (series-element ...)
     adds information from series b to a.
     each element has the same bin count but effective input durations are different between series
     and are scaled to match in size"
    (map
      (l (a)
        (apply
          (l (a-bins a-change a-duration b-bins b-change b-duration) (display-line a-bins)
            (let loop ((index count-ratio))
              (if (< index (vector-length a-bins))
                (begin
                  (each-with-index
                    (l (c-index c-value) (vector-set! a-bins (- index c-index) c-value))
                    (apply combine-one
                      (map
                        (l (bins) (map-integers count-ratio (l (c) (vector-ref bins (- index c)))))
                        (list a-bins b-bins))))
                  (loop (+ 1 index)))
                (list a-bins a-change a-duration))))
          (append a b)))
      a))

  (define (combine-series a b duration-offset)
    "(series-element ...) (series-element ...) integer -> (series-element ...)"
    (let (count-ratio (inexact->exact (round (/ (length a) (length b)))))
      (let loop ((a a) (b b) (result null))
        (if (null? b) result
          (let (a (apply-values pair (split-at a count-ratio)))
            (loop (tail a) (tail b)
              (append result (combine-series-one (first a) (first b) duration-offset count-ratio))))))))

  (define* (sp-vectorise input #:key duration)
    "samples [integer] -> (series-element ...)
     duration is an exponent for (2 ** exponent)
     result length is the length of the first series.
     result elements are (start-duration * overlap-factor) samples apart.
     analyses only parts of input that powers of two length frames fit into"
    (let*
      ( (end-exponent (inexact->exact (floor (log2 (sp-samples-length input)))))
        (end-duration (expt 2 end-exponent))
        (start-exponent (or duration (round (/ end-exponent 2)))))
      (let loop ((exponent start-exponent) (result null))
        (if (<= exponent end-exponent)
          (let (series (sp-vectorise-series input (expt 2 exponent) end-duration))
            (loop (+ 1 exponent)
              (if (null? result) series (combine-series result series (- exponent start-exponent)))))
          result)))))
