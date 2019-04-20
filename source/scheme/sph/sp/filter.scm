(library (sph sp filter)
  (export
    sp-asymmetric-moving
    sp-asymmetric-moving-average
    sp-asymmetric-moving-median
    sp-asymmetric-moving-out
    sp-cheap-filter
    sp-filter
    sp-multipass
    sp-multipass-fir
    sp-one-pole-hp
    sp-one-pole-lp
    sp-state-variable-filter
    sph-sp-filter-description)
  (import
    (rnrs sorting)
    (sph)
    (sph list)
    (sph math)
    (sph number)
    (sph other)
    (sph sp)
    (only (guile) make-list simple-format)
    (only (srfi srfi-1) drop-right))

  (define sph-sp-filter-description
    "attenuating frequencies.
     the main filter procedures are sp-filter and sp-cheap-filter.
     both have parameters to control the portion of the input/output sample array that is being used
     to be able to process with parameters updated with certain sample resolution independent of sample array size")

  (define (sp-filter out in cutoff-l cutoff-h transition-l transition-h is-reject state)
    (sp-windowed-sinc-bp-br out in cutoff-l cutoff-h transition-l transition-h is-reject state))

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
              (sp-windowed-sinc-bp-br out in cutoff-l cutoff-h transition-l transition-h #f state)
              (pair (pair out (first result)) (pair state (tail result)))))
          a))
      (pair null null) points
      (if (and state (= (length points) (length state))) state (make-list (length points) #f))))

  (define (sp-multipass f out in passes state in-start in-count out-start)
    "procedure samples samples integer (any ...)
     f :: out in in-start in-count out-start state -> state
     call f possibly multiple times with prepared output buffers to process input/output in series.
     internally uses one custom state value per pass so they are matched for seamlessness.
     state contains custom values updated with f and can be an empty list if no state values are available"
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
    (sp-cheap-filter type out in cutoff passes state #:key (q-factor 0) (in-start 0)
      (in-count (- (sp-samples-length in) in-start))
      (out-start 0)
      (unity-gain #t))
    "symbol samples samples real:0..0.5 integer samples [keys ...] -> unspecified
     a less processing intensive filter based on sp-state-variable-filter.
     type can be low, high, band, notch, peak or all.
     the basic filter algorithm is cheaper than the sp-filter windowed sinc, especially with frequently changing parameter values,
     but multiple passes and the processing for unity-gain relativise that quite a bit"
    (let*
      ( (f
          (case type
            ((low) sp-state-variable-filter-lp)
            ((high) sp-state-variable-filter-hp)
            ((band) sp-state-variable-filter-bp)
            ((reject) sp-state-variable-filter-br)
            ((peak) sp-state-variable-filter-peak)
            ((all) sp-state-variable-filter-all)))
        (state
          (sp-multipass
            (l (out in in-start in-count out-start state)
              (f out out-start in in-start in-count cutoff q-factor (or state (sp-samples-new 2))))
            out in passes state in-start in-count out-start)))
      (if unity-gain (sp-set-unity-gain out in in-start in-count out-start)) state))

  "out out-start in in-start in-count cutoff q-factor state -> unspecified"

  (define (sp-multipass-fir transfer-f out in passes state in-start in-count out-start)
    "procedure samples samples integer list [integer integer integer] -> unspecified
     transfer-f :: sample:in sample:prev-in sample:prev-out -> sample
     call transfer-f for each sample of in with preceeding input/output values.
     preceeding values are taken from the last call if available in state and separate values are used for each state.
     the state object passed to transfer-f can be false if no preceeding values are available.
     use case: fir filters"
    (sp-multipass
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
    (sp-multipass-fir
      (l (in prev-in prev-out) (float-sum prev-out (* (* cutoff 2) (float-sum in (- prev-out))))) out
      in passes state in-start in-count out-start))

  (define (sp-one-pole-hp out in cutoff passes state in-start in-count out-start)
    (sp-multipass-fir
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
      current-value width state)))
