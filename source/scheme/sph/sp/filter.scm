(library (sph sp filter)
  (export
    sp-asymmetric-moving
    sp-asymmetric-moving-average
    sp-asymmetric-moving-median
    sp-asymmetric-moving-out
    sp-cheap-filter!
    sp-filter!
    sp-moving-average
    sp-multipass!
    sp-multipass-fir!
    sp-one-pole-hp
    sp-one-pole-lp
    sp-state-variable-filter
    sp-windowed-sinc-bp-br
    sp-windowed-sinc-lp-hp
    sph-sp-filter-description)
  (import
    (rnrs sorting)
    (sph)
    (sph list)
    (sph math)
    (sph number)
    (sph other)
    (sph sp)
    (only (guile) make-list)
    (only (srfi srfi-1) drop-right))

  (define sph-sp-filter-description
    "attenuating frequencies.
     the main filter procedures are sp-filter! and sp-cheap-filter!.
     both have parameters to control the portion of the input/output sample array that is being used
     to be able to process with parameters updated with certain sample resolution independent of sample array size")

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

  (define sp-filter! sp-windowed-sinc-bp-br!)

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

  (define (sp-state-variable-filter type out in cutoff q-factor state in-start in-count out-start)
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
      ( (g (tan (* sp-pi cutoff))) (k (- 2 (* 2 q-factor))) (a1 (/ 1 (+ 1 (* g (+ g k)))))
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

  (define (sp-multipass! f out in passes state in-start in-count out-start)
    "procedure samples samples integer (any ...)
     f :: out in in-start in-count out-start state -> state
     call f possibly multiple times with prepared output buffers to process input/output in series.
     internally uses one custom state value per pass so they are matched for seamlessness.
     state can be the empty list if no state values are available"
    (define (first-or-false a) (if (null? a) #f (first a)))
    (define (tail-or-null a) (if (null? a) a (tail a)))
    (if (= 1 passes) (list (f out in in-start in-count out-start (first-or-false state)))
      (let*
        ( (out-temp (sp-samples-new in-count))
          (result-state (list (f out-temp in in-start in-count 0 (first-or-false state))))
          (state (tail-or-null state)))
        (let loop
          ( (passes (- passes 1)) (in-temp out-temp) (out-temp (sp-samples-new in-count))
            (state state) (result-state result-state))
          (if (< 1 passes)
            (let
              (result-state
                (pair (f out-temp in-temp 0 in-count 0 (first-or-false state)) result-state))
              (loop (- passes 1) out-temp
                (sp-samples-zero! in-temp) (tail-or-null state) result-state))
            (reverse
              (pair (f out in-temp 0 in-count out-start (first-or-false state)) result-state)))))))

  (define*
    (sp-cheap-filter! type out in cutoff passes state #:key (q-factor 0) (in-start 0)
      (in-count (- (sp-samples-length in) in-start))
      (out-start 0)
      (unity-gain #t))
    "symbol samples samples real:0..0.5 real:0..1 integer [integer integer integer] -> unspecified
     a less processing intensive filter based on sp-state-variable-filter.
     type can be low, high, band, notch, peak or all"
    (sp-multipass!
      (l (out in in-start in-count out-start state)
        (sp-state-variable-filter type out in cutoff q-factor state in-start in-count out-start))
      out in passes state in-start in-count out-start)
    (if unity-gain (sp-set-unity-gain out in in-start in-count out-start)))

  (define (sp-multipass-fir! transfer-f out in passes state in-start in-count out-start)
    "procedure samples samples integer list [integer integer integer] -> unspecified
     transfer-f :: sample:in sample:prev-in sample:prev-out -> sample
     call transfer-f for each sample of in with preceeding input/output values.
     preceeding values are taken from the last call if available in state and separate values are used for each state.
     the state object passed to transfer-f can be false if no preceeding values are available.
     use case: fir filters"
    (sp-multipass!
      (l (out in in-start in-count out-start state)
        (let (prev (or state (list (sp-samples-ref in in-start) (sp-samples-ref out out-start))))
          (sp-samples-set! out out-start (apply transfer-f (sp-samples-ref in in-start) prev))
          (each-integer (- in-count 1)
            (l (index)
              (let ((out-index (+ out-start index)) (in-index (+ in-start index)))
                (sp-samples-set! out out-index
                  (transfer-f (sp-samples-ref in in-index) (sp-samples-ref in (- in-index 1))
                    (sp-samples-ref out (- out-index 1))))))
            1)
          (list (sp-samples-ref in (+ in-start (- in-count 1)))
            (sp-samples-ref out (+ out-start (- in-count 1))))))
      out in passes state in-start in-count out-start))

  (define (sp-one-pole-lp out in cutoff passes state in-start in-count out-start)
    "a one-pole filter with the transfer function y(n) = ((2 * cutoff) * (x(n) - y(n - 1))).
     the higher the cutoff, the less attenuated the stop-band"
    (sp-multipass-fir!
      (l (in prev-in prev-out) (float-sum prev-out (* (* cutoff 2) (float-sum in (- prev-out))))) out
      in passes state in-start in-count out-start))

  (define (sp-one-pole-hp out in cutoff passes state in-start in-count out-start)
    (sp-multipass-fir!
      (l (in prev-in prev-out) (* (* cutoff 2) (float-sum prev-out in (- prev-in)))) out
      in passes state in-start in-count out-start))

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

  (define* (sp-moving-average in prev next radius #:optional in-start in-count out-start)
    "samples false/samples false/samples integer [integer/false integer/false] -> samples"
    (sp-samples-copy-zero* in
      (l (out)
        (sp-moving-average! out in
          prev next radius (or in-start 0) (or in-count (sp-samples-length in)) (or out-start 0))))))
