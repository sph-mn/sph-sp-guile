(library (sph sp synthesise)
  (export
    seq
    seq-event-end
    seq-event-end-set!
    seq-event-f
    seq-event-f-set!
    seq-event-new
    seq-event-start
    seq-event-start-set!
    seq-event-state
    seq-event-state-set!
    sp-band-event
    sp-band-partials
    sp-block
    sp-block-series
    sp-blocks->file
    sp-call-with-output-file
    sp-clip~
    sp-noise-exponential~
    sp-noise-normal~
    sp-noise-uniform~
    sp-path
    sp-phase
    sp-samples-list-add-offsets
    sp-sine~
    sp-square~
    sp-wave-event
    sph-sp-synthesis-description)
  (import
    (rnrs exceptions)
    (sph)
    (sph list)
    (sph sp)
    (sph spline-path)
    (sph vector)
    (only (guile)
      const
      make-list
      random:exp
      random:uniform
      random:normal
      *random-state*)
    (only (sph number) float-sum))

  (define sph-sp-synthesis-description
    "wave and noise generators, sequencing, block generation.
     time is in number of samples")

  (define* (sp-noise-uniform~ #:optional (state *random-state*)) (- (* 2 (random:uniform state)) 1))
  (define* (sp-noise-exponential~ #:optional (state *random-state*)) (- (* 2 (random:exp state)) 1))
  (define* (sp-noise-normal~ #:optional (state *random-state*)) (- (* 2 (random:normal state)) 1))

  (define (sp-square~ t width) "integer integer -> sample"
    (if (< (round t) (ceiling (/ width 2))) -1 1))

  (define (sp-sine~ t width)
    "return a value for a repeating sine with given sample width at sample offset t"
    (sin (* t (/ (* 2 sp-pi) width))))

  (define (sp-clip~ a) "eventually adjust value to not exceed -1 or 1"
    (if (< 0 a) (min 1.0 a) (max -1.0 a)))

  (define (sp-phase y change phase-size)
    "number number number -> number
     phase generator that allows for high resolution modulation and non-linear transitions.
     * y: previous result or another starting value to continue from
     * change: how fast the phase should progress. frequency
     * phase-size: value at which the cycle should repeat
     example: (sp-phase 0.0 (/ (* 2 sp-pi) 200) (* 2 sp-pi))"
    (let (y (float-sum change y)) (if (< phase-size y) (float-sum y (- phase-size)) y)))

  (define*
    (sp-wave-event start end amplitudes wavelength #:key (phase 0) (generator sin)
      (phase-length (* 2 sp-pi)))
    "integer integer (partial-config ...) -> seq-event
     partial-config: ((amplitude ...) wavelength phase-offset)
     phase-offset: number
     amplitude, wavelength: sp-path"
    (seq-event-new start end
      (let
        ( (amplitudes (map sp-path amplitudes)) (wavelength (sp-path wavelength))
          (null-point (make-list (length amplitudes) 0)))
        (l (t size output event)
          (seq-event-state-set! event
            (sp-block t size
              output
              (l (t phase)
                (let (wavelength (wavelength t))
                  (if (zero? wavelength) (list null-point phase)
                    (let*
                      ( (phase (sp-phase phase (/ phase-length wavelength) phase-length))
                        (sample (generator phase)))
                      (list (map (l (a) (* (a t) sample)) amplitudes) phase)))))
              (seq-event-state event)))))
      phase))

  (define* (sp-path a #:optional (dimension 1))
    "-> procedure:{t -> number/(number ...)}
     return a procedure that gives point values for time offset values.
     dimension selects a number from result points.
     dimension is ignored for procedures and numbers.
     input can be
     * procedure: will be used as is
     * number/(number): will be returned for any point on path
     * (list ...): a (sph spline-path) configuration"
    (cond
      ((procedure? a) a)
      ( (spline-path? a)
        (spline-path->procedure
          (if dimension
            (let (a (spline-path-copy a))
              (spline-path-output-mapper-set! a (l (a t) (list-ref a dimension))) a)
            a)))
      ((number? a) (const a))
      ( (list? a)
        (if (list? (first a))
          (spline-path->procedure
            (spline-path-new a #f (and dimension (l (a t) (list-ref a dimension)))))
          (const (if dimension (list-ref a dimension) a))))
      (else (raise (q invalid-sp-path)))))

  (define (sp-block t size output f . state)
    "integer integer (samples:channel ...) f any ... -> output
     add to output size number of samples generated by f.
     samples are summed with existing samples"
    (first
      (apply sp-fold-integers size
        (l (sample-index . state)
          (apply
            (l (samples . state)
              (for-each
                (l (a b)
                  (sp-samples-set! a sample-index (float-sum (sp-samples-ref a sample-index) b)))
                output samples)
              state)
            (apply f (+ t sample-index) state)))
        state)))

  (define (sp-block-series channels count size f . state)
    "integer integer integer procedure:{integer integer block any ... -> } any ... -> (samples ...)
     call seq repeatedly to create a list of blocks of specified count and size"
    (apply sp-map-fold-integers count
      (l (t . state)
        (let (output (map-integers channels (l (a) (sp-samples-new size))))
          (list output (apply f (* t size) size output state))))
      state))

  (define (sp-band-partials t partials filter-states)
    "-> (((sample:channel ...) . filter-state) ...)"
    (map
      (l (partial state) "-> ((sample ...) state)"
        (apply
          (l (amplitudes cutoff-l cutoff-h noise transition-l transition-h)
            (apply
              (l (sample state)
                (let (sample (sp-samples-ref sample 0))
                  (pair (map (l (a) (* (a t) sample)) amplitudes) state)))
              (sp-windowed-sinc-bp-br (sp-samples-new 1 (noise)) (cutoff-l t)
                (cutoff-h t) (transition-l t) (transition-h t) #f state)))
          partial))
      partials filter-states))

  (define*
    (sp-band-event start end partials #:optional (default-noise sp-noise-uniform~)
      (default-transition (const 0.01)))
    "integer integer (partial-config ...) -> seq-event
     partial-config: ((amplitude ...) cutoff-l cutoff-h [noise transition-l transition-h])
     noise: procedure:{-> sample}
     all other arguments: sp-path-argument
     normalises partial configs and returns an event that creates blocks of partials"
    (seq-event-new start end
      (let*
        ( (partials
            (map-apply
              (l* (#:key amp cut-l cut-h noise trn-l trn-h)
                (list (map sp-path amp) (sp-path cut-l)
                  (sp-path cut-h) (or noise default-noise)
                  (or (and trn-l (sp-path trn-l)) default-transition)
                  (or (and trn-h (sp-path trn-h)) default-transition)))
              partials)))
        (l (t size output event)
          (seq-event-state-set! event
            (sp-block t size
              output
              (l (t state)
                (let (samples-and-state (sp-band-partials t partials state))
                  (list (apply map float-sum (map first samples-and-state))
                    (map tail samples-and-state))))
              (seq-event-state event)))))
      (make-list (length partials) #f)))

  (define* (seq-event-new start end f #:optional event-state)
    "procedure integer [integer any] -> vector" (vector start end f event-state))

  (define seq-event-start (vector-accessor 0))
  (define seq-event-end (vector-accessor 1))
  (define seq-event-f (vector-accessor 2))
  (define seq-event-state (vector-accessor 3))
  (define seq-event-start-set! (vector-setter 0))
  (define seq-event-end-set! (vector-setter 1))
  (define seq-event-f-set! (vector-setter 2))
  (define seq-event-state-set! (vector-setter 3))

  (define (seq time size output events)
    "integer integer (samples:channel ...) (event ...) any -> events
     repeatedly calls functions starting from specified times and returns sample arrays of requested size.
     calls one or multiple functions at predefined times.
     the functions can write to the output sample array.
     size: the number of samples that should be written to output"
    (filter
      (l (a)
        (let ((start (seq-event-start a)) (end (seq-event-end a)))
          (if (< time start) #t
            (if (> time end) #f (begin ((seq-event-f a) (- time start) size output a) #t)))))
      events))

  (define (sp-call-with-output-file path channels sample-rate f)
    (let* ((file (sp-file-open path sp-file-mode-write channels sample-rate)) (result (f file)))
      (sp-file-close file) result))

  (define (sp-blocks->file a path channels sample-rate size)
    "((samples:channel ...):block ...) string integer integer integer -> unspecified"
    (if (not (null? a))
      (sp-call-with-output-file path channels
        sample-rate (l (file) (each (l (a) (sp-file-write file a size)) a)))))

  (define* (sp-samples-list-add-offsets b #:optional (start 0))
    "(samples ...) [integer] -> ((sample-offset samples) ...)
     map each samples vector in input to a pair with the cumulative sample
     offset of the length of sample vectors starting from start.
     for example a list with sample vector sizes 8 2 3 would create
     a list ((0 samples) (8 samples) (10 samples))"
    (let loop ((b b) (offset start))
      (if (null? b) b
        (pair (pair offset (first b)) (loop (tail b) (+ (sp-samples-length (first b)) offset)))))))