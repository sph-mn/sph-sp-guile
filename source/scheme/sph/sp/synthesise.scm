(library (sph sp synthesise)
  (export
    seq
    seq-block-series
    seq-block-series->file
    seq-event-data
    seq-event-data-end
    seq-event-data-f
    seq-event-data-start
    seq-event-group
    seq-event-list
    seq-event-new
    seq-event-state
    seq-event-state-update
    sp-band-event
    sp-band-partials
    sp-blocks->file
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
    (ice-9 futures)
    (rnrs exceptions)
    (sph)
    (sph list)
    (sph sp)
    (sph spline-path)
    (sph vector)
    (only (guile)
      compose
      const
      make-list
      modulo
      random:exp
      random:uniform
      random:normal
      *random-state*)
    (only (sph number) float-sum)
    (only (sph other) each-integer)
    (only (srfi srfi-1) zip))

  (define sph-sp-synthesis-description
    "wave and noise generators, sequencing, block generation.
     time is in number of samples.
     an sp-path is an argument that sp-path->t-function accepts
                      ")

  (define* (sp-noise-uniform~ #:optional (state *random-state*)) (- (* 2 (random:uniform state)) 1))
  (define* (sp-noise-exponential~ #:optional (state *random-state*)) (- (* 2 (random:exp state)) 1))
  (define* (sp-noise-normal~ #:optional (state *random-state*)) (- (* 2 (random:normal state)) 1))

  (define* (sp-square~ t #:optional (wavelength 96000))
    "integer integer -> sample
     center falls between two samples with even wavelengths"
    (if (< (modulo (* 2 t) (* 2 wavelength)) wavelength) -1 1))

  (define* (sp-sine~ t #:optional (wavelength 96000))
    "integer integer -> sample
     return a value for a repeating sine with a wavelength of width.
     if wavelength is divisible by four then maxima are sample aligned"
    (sin (* t (/ (* 2 sp-pi) wavelength))))

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

  (define* (sp-path->t-function a #:optional (dimension 1))
    "sp-path -> procedure:{t -> number/(number ...)}
     return a procedure that gives point values for time offset values.
     dimension selects a number from result points.
     dimension is ignored for procedures and numbers.
     # input
     * procedure: will be used as is
     * number/(number): will be returned for any point on path
     * (list ...): a (sph spline-path) configuration"
    (cond
      ((procedure? a) a)
      ( (spline-path? a)
        (let (b (spline-path->procedure a)) (if dimension (l (t) (list-ref (b t) dimension)) b)))
      ((number? a) (const a))
      ( (list? a)
        (if (list? (first a)) (sp-path->t-function (spline-path-new a) dimension)
          (const (if dimension (list-ref a dimension) a))))
      (else (raise (q invalid-sp-path)))))

  (define*
    (sp-wave-event start end amplitudes wavelength #:key (phase 0) (generator sp-sine~)
      (phase-length 96000))
    "integer integer (partial-config ...) -> seq-event
     partial-config: ((amplitude ...) wavelength phase-offset)
     phase-offset: number
     amplitude, wavelength: sp-path"
    ; advances phase only in sample steps
    (seq-event-new start end
      (let
        ( (amplitudes (map sp-path->t-function amplitudes))
          (wavelength (sp-path->t-function wavelength))
          (null-samples (make-list (length amplitudes) 0)))
        (l (t size output event)
          (seq-event-state-update event
            (fold-integers size (seq-event-state event)
              (l (sample-index phase)
                (let (wavelength (wavelength t))
                  (if (zero? wavelength) phase
                    (let*
                      ( (phase (sp-phase phase (round (/ phase-length wavelength)) phase-length))
                        (sample (generator phase)))
                      (each
                        (l (output a)
                          (sp-samples-set! output sample-index (* (a (+ t sample-index)) sample)))
                        output amplitudes)
                      phase))))))))
      phase))

  (define*
    (sp-band-event start end amplitudes cut-l cut-h #:key (noise sp-noise-uniform~) (trn-l 0.01)
      (trn-h 0.01)
      (reject #f))
    "integer integer (sp-path ...) sp-path sp-path #:noise procedure #:trn-l sp-path #:trn-h sp-path #:reject boolean -> event
     create a band of noise"
    (let (amplitudes (map sp-path->t-function amplitudes))
      (seq-event-new start end
        (if (and (number? cut-l) (number? cut-h) (number? trn-l) (number? trn-h))
          (l (t size output event)
            (apply
              (l (samples state)
                (each
                  (l (output a)
                    (sp-samples-each-index
                      (l (sample-index)
                        (sp-samples-set! output sample-index
                          (* (a (+ t sample-index)) (sp-samples-ref samples sample-index))))
                      output))
                  output amplitudes)
                (seq-event-state-update event state))
              (sp-windowed-sinc-bp-br (sp-samples-new size (l (a) (noise))) cut-l
                cut-h trn-l trn-h reject (seq-event-state event))))
          (let
            ( (cut-l (sp-path->t-function cut-l)) (cut-h (sp-path->t-function cut-h))
              (trn-l (sp-path->t-function trn-l)) (trn-h (sp-path->t-function trn-h)))
            (l (t size output event)
              (seq-event-state-update event
                (fold-integers size (seq-event-state event)
                  (l (sample-index state)
                    (apply
                      (l (samples state)
                        (let (sample (sp-samples-ref samples 0))
                          (each
                            (l (output a)
                              (sp-samples-set! output sample-index
                                (* (a (+ t sample-index)) sample)))
                            output amplitudes)
                          state))
                      (sp-windowed-sinc-bp-br (sp-samples-new 1 (l (a) (noise))) (cut-l t)
                        (cut-h t) (trn-l t) (trn-h t) reject state))))))))
        #f)))

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
        (pair (pair offset (first b)) (loop (tail b) (+ (sp-samples-length (first b)) offset))))))

  (define* (seq-event-new start end f #:optional state)
    "procedure integer [integer any] -> seq-event" (pair state (vector start end f)))

  (define (seq-event-list . a)
    "seq-event ... -> seq-events
     create a events list sorted for seq"
    (list-sort-with-accessor < (compose seq-event-data-start seq-event-data) a))

  (define (seq-event-group start end events) "integer integer seq-events -> seq-event"
    (seq-event-new start end
      (l (t size output event) (seq t size output (seq-event-state event))) events))

  (define (seq-event-state-update a state) (pair state (tail a)))
  (define seq-event-data-start (vector-accessor 0))
  (define seq-event-data-end (vector-accessor 1))
  (define seq-event-data-f (vector-accessor 2))
  (define seq-event-data tail)
  (define seq-event-state first)

  (define (seq time size output events)
    "integer integer (samples:channel ...) seq-events -> seq-events
     calls one or multiple functions in parallel at predefined times and sums result samples.
     the returned objects contains the updated events list and can be passed to the next call to seq"
    ; take sorted event objects that have a start/end time and a function that is called with a time offset and an output array.
    ; each block, filter events that write into the block, allocate arrays for the portions they write, call the events in parallel,
    ; merge the event blocks into the final output block. return a list of events without finished events.
    (define (finish results rest-events)
      (let loop ((results results) (events null))
        (if (null? results) (append events rest-events)
          (apply
            (l (offset event-output-size event-output event)
              (each
                (l (output event-output)
                  (each-integer event-output-size
                    (l (sample-index)
                      (sp-samples-set! output (+ offset sample-index)
                        (float-sum (sp-samples-ref output (+ offset sample-index))
                          (sp-samples-ref event-output sample-index))))))
                output event-output)
              (loop (tail results) (pair event events)))
            (touch (first results))))))
    (let ((block-end (+ time size)) (channels (length output)))
      (let loop ((results null) (rest events))
        (if (null? rest) (finish results rest)
          (let*
            ( (event (first rest)) (data (seq-event-data event)) (start (seq-event-data-start data))
              (end (seq-event-data-end data)))
            (if (< block-end start) (finish results rest)
              (if (> time end) (loop results (tail rest))
                (loop
                  (pair
                    (future
                      (let*
                        ( (block-offset (if (> start time) (- start time) 0))
                          (block-offset-right (if (< end block-end) (- block-end end) 0))
                          (size (- size block-offset block-offset-right))
                          (output (map-integers channels (l (n) (sp-samples-new size)))))
                        (list block-offset size
                          output ((seq-event-data-f data) (- time start) size output event))))
                    results)
                  (tail rest)))))))))

  (define* (seq-block-series time channels count events f #:key (block-size 96000) (progress #f))
    "integer integer integer seq-events procedure:{(samples:channel ...) seq-events -> seq-events} -> seq-events"
    (fold-integers count events
      (l (block-index events)
        (if progress
          (begin
            (display-line (string-append "processing block " (number->string block-index) "..."))
            (if (= count (+ 1 block-index)) (display-line "processing finished"))))
        (let (output (sp-block-new channels block-size))
          (f output (seq time block-size output events))))))

  (define*
    (seq-block-series->file path time channels count events #:key (block-size 96000)
      (sample-rate 96000)
      (progress #f))
    "string integer integer seq-events [#:block-size integer #:sample-rate integer] -> seq-events"
    (sp-call-with-output-file path channels
      sample-rate
      (l (file)
        (seq-block-series time channels
          count events
          (l (output events) (sp-file-write file output block-size) events) #:block-size
          block-size #:progress progress)))))
