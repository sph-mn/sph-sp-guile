(library (sph sp synthesise)
  (export
    seq
    seq-block-series
    seq-block-series->file
    seq-block-series->list
    seq-event-data
    seq-event-data-end
    seq-event-data-f
    seq-event-data-start
    seq-event-group
    seq-event-group-map
    seq-event-new
    seq-event-state
    seq-event-state-update
    seq-events-new
    seq-parallel
    sp-band-event
    sp-band-partials
    sp-block->file
    sp-blocks->file
    sp-clip~
    sp-noise-exponential~
    sp-noise-normal~
    sp-noise-uniform~
    sp-path
    sp-path*
    sp-path-procedure
    sp-phase
    sp-rectangle
    sp-rectangle~
    sp-samples-list-add-offsets
    sp-sawtooth~
    sp-sine~
    sp-square~
    sp-triangle
    sp-triangle~
    sp-wave-event
    sph-sp-synthesis-description)
  (import
    (rnrs exceptions)
    (sph)
    (sph futures)
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
    (only (rnrs base) set!)
    (only (sph number) float-sum)
    (only (sph other) each-integer procedure->cached-procedure))

  (define sph-sp-synthesis-description
    "sequencing and sound synthesis with composable sequencer objects.
     time is in number of samples")

  (define (sp-rectangle t width-a width-b min-value max-value)
    "integer integer number number -> number
     alternate between min and max value for durations of width-a and width-b"
    (let (remainder (modulo t (+ width-a width-b))) (if (< remainder width-a) min-value max-value)))

  (define (sp-triangle t a b height)
    "integer integer number -> number
     return a value for a triangle wave with center offset a and b left and right respectively"
    (let (remainder (modulo t (+ a b)))
      (if (< remainder a) (* remainder (/ height a)) (* (- b (- remainder a)) (/ height b)))))

  (define* (sp-noise-uniform~ #:optional (state *random-state*)) (- (* 2 (random:uniform state)) 1))
  (define* (sp-noise-exponential~ #:optional (state *random-state*)) (- (* 2 (random:exp state)) 1))
  (define* (sp-noise-normal~ #:optional (state *random-state*)) (- (* 2 (random:normal state)) 1))

  (define* (sp-square~ t #:optional (wavelength 96000))
    "integer integer -> sample
     center will fall nicely between two samples if the wavelength is even"
    (if (< (modulo (* 2 t) (* 2 wavelength)) wavelength) -1 1))

  (define* (sp-sine~ t #:optional (wavelength 96000))
    "integer integer -> sample
     return a value for a repeating sine with a wavelength of width.
     if wavelength is divisible by four then maxima are sample aligned"
    (sin (* t (/ (* 2 sp-pi) wavelength))))

  (define (sp-rectangle~ t a b)
    "integer sample sample -> sample
     get a value for a repeating rectangular wave at offset t with given side durations a and b"
    (sp-rectangle t a b -1 1))

  (define* (sp-triangle~ t #:optional (a 48000) (b 48000))
    "integer:sample-count ... -> real:sample
     return a sample for a triangular wave with center offsets a left and b right.
     creates saw waves if either a or b is 0"
    (- (sp-triangle t a b 2) 1))

  (define* (sp-sawtooth~ t #:optional (wavelength 96000)) (sp-triangle~ t wavelength 0))
  (define (sp-clip~ a) "limit value to not exceed -1 or 1" (if (< 0 a) (min 1.0 a) (max -1.0 a)))

  (define (sp-phase y change phase-size)
    "number number number -> number
     phase generator that allows for high resolution modulation and non-linear transitions.
     * y: previous result or another starting value to continue from
     * change: how fast the phase should progress. frequency
     * phase-size: value at which the cycle should repeat
     example: (sp-phase 0.0 (/ (* 2 sp-pi) 200) (* 2 sp-pi))"
    (let (y (float-sum change y)) (if (< phase-size y) (float-sum y (- phase-size)) y)))

  (define* (sp-path a #:key (dimension 1) deep mapper randomise repeat reverse scale shift stretch)
    "spline-path/spline-path-config/number/point [keys ...] -> spline-path
     create a new path or modify an existing one.
     combines spline-path-new, spline-path-modify and spline-path-constant.
     if #:dimension is a number then only the point value of that dimension is returned as a single number. the default is one"
    (let
      (path
        (cond
          ((spline-path? a) a)
          ((number? a) (spline-path-constant a))
          ((list? a) (if (every number? a) (apply spline-path-constant a) (spline-path-new a)))))
      (spline-path-modify path #:deep
        deep #:randomise
        randomise #:repeat
        repeat #:reverse
        reverse #:scale
        scale #:shift
        shift #:stretch
        stretch #:mapper-add
        (append (or mapper null)
          (list (list (q dimension) #f (l (point) (list-ref point dimension))))))))

  (define-syntax-rule (sp-path-new* (options ...) segment ...)
    (sp-path-new (list (quasiquote segment) ...) options ...))

  (define (sp-path-procedure a) (if (procedure? a) a (spline-path->procedure (sp-path a))))

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
        ( (amplitudes (map sp-path-procedure amplitudes))
          (wavelength (sp-path-procedure wavelength))
          (null-samples (make-list (length amplitudes) 0)))
        (l (time offset size output event)
          (seq-event-state-update event
            (fold-integers size (seq-event-state event)
              (l (sample-index phase)
                (let (wavelength (wavelength time))
                  (if (zero? wavelength) phase
                    (let*
                      ( (phase (sp-phase phase (round (/ phase-length wavelength)) phase-length))
                        (sample (generator phase)))
                      (each
                        (l (output a)
                          (sp-samples-set! output (+ offset sample-index)
                            (float-sum (sp-samples-ref output (+ offset sample-index))
                              (* (a (+ time sample-index)) sample))))
                        output amplitudes)
                      phase))))))))
      phase))

  (define (limit-t a step-size) (let (diff (modulo a step-size)) (if (zero? diff) a (- a diff))))

  (define (get-noise-f repeat-noise noise start end)
    (if repeat-noise
      (let (samples (sp-samples-new (min 96000 (- end start)) (l a (noise))))
        (l (size) "optimisation where the same noise samples keep being used as filter input"
          (if (> size (sp-samples-length samples))
            (set! samples (sp-samples-new size (l a (noise)))))
          samples))
      (l (size) (sp-samples-new size (l a (noise))))))

  (define*
    (sp-band-event start end amplitudes cut-l cut-h #:key (noise sp-noise-uniform~) (trn-l 0.01)
      (trn-h 0.01)
      reject
      (resolution 96)
      repeat-noise)
    "integer integer (sp-path ...) sp-path sp-path [keys ...] -> seq-event
     create a band of noise filtered by a windowed sinc filter.
     if repeat-noise is true, source noise samples are reused and not regenerated.
     # keys
     noise: procedure:{-> sample}
     trn-l: sp-path
     trn-h: sp-path
     reject: boolean
     repeat-noise: boolean
     resolution: integer"
    (let
      ( (amplitudes (map sp-path-procedure amplitudes)) (cut-l (sp-path-procedure cut-l))
        (cut-h (sp-path-procedure cut-h)) (trn-l (sp-path-procedure trn-l))
        (trn-h (sp-path-procedure trn-h)) (get-noise (get-noise-f repeat-noise noise start end)))
      (seq-event-new start end
        (l (t offset size output event)
          (let*
            ( (count (ceiling (/ size resolution))) (samples (sp-samples-new (* resolution count)))
              (noise-samples (get-noise (* resolution count)))
              (filter-state
                (fold-integers count (seq-event-state event)
                  (l (block-index filter-state)
                    (let* ((block-offset (* resolution block-index)) (t (+ t block-offset)))
                      (sp-windowed-sinc-bp-br! samples noise-samples
                        (cut-l t) (cut-h t)
                        (trn-l t) (trn-h t) reject filter-state block-offset resolution block-offset))))))
            (each
              (l (output a) "apply amplitudes and sum into output"
                (each-integer size
                  (l (index)
                    (sp-samples-set! output (+ offset index)
                      (float-sum (sp-samples-ref output (+ offset index))
                        (* (a (+ t index)) (sp-samples-ref samples index)))))))
              output amplitudes)
            (seq-event-state-update event filter-state)))
        #f)))

  #;(define*
    (sp-low-pass-event start end amplitudes radius #:key (noise sp-noise-uniform~) (resolution 96)
      repeat-noise)
    "integer integer (sp-path ...) sp-path [keys ...] -> seq-event
     create a band of noise filtered by a centered moving average filter.
     amount gives the radius of the moving average.
     if repeat-noise is true, source noise samples are reused and not regenerated.
     # keys
     noise: procedure:{-> sample}
     repeat-noise: boolean
     resolution: integer"
    (let
      ( (amplitudes (map sp-path-procedure amplitudes)) (radius (sp-path-procedure radius))
        (get-noise (get-noise-f repeat-noise noise start end)))
      (seq-event-new start end
        (l (t offset size output event)
          (let*
            ( (count (ceiling (/ size resolution))) (samples (sp-samples-new (* resolution count)))
              (noise-samples (get-noise (* resolution count)))
              (filter-state
                (fold-integers count (seq-event-state event)
                  (l (block-index filter-state)
                    (let* ((block-offset (* resolution block-index)) (t (+ t block-offset)))
                      (sp-moving-average! samples noise-samples
                        noise-samples noise-samples
                        (round (radius t)) block-offset resolution block-offset)
                      #f)))))
            (each
              (l (output a) "apply amplitudes and sum into output"
                (each-integer size
                  (l (index)
                    (sp-samples-set! output (+ offset index)
                      (float-sum (sp-samples-ref output (+ offset index))
                        (* (a (+ t index)) (sp-samples-ref samples index)))))))
              output amplitudes)
            (seq-event-state-update event filter-state)))
        #f)))

  (define* (sp-block->file a path sample-rate #:optional channels)
    "(samples:channel ...):block string integer [integer] -> unspecified"
    (sp-blocks->file (list a) path sample-rate channels))

  (define* (sp-blocks->file a path sample-rate #:optional channels)
    "((samples:channel ...):block ...) string integer [integer] -> unspecified"
    (if (not (null? a))
      (sp-call-with-output-file path (or channels (length (first a)))
        sample-rate
        (l (file) (each (l (block) (sp-file-write file block (sp-samples-length (first block)))) a)))))

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

  (define (seq-events-new . a)
    "seq-event ... -> seq-events
     create a events list sorted for seq"
    (list-sort-with-accessor < (compose seq-event-data-start seq-event-data) a))

  (define (seq-event-group start end events) "integer integer seq-events -> seq-event"
    (seq-event-new start end
      (l (t offset size output event)
        (seq-event-state-update event (seq t offset size output (seq-event-state event))))
      events))

  (define (seq-event-group-map start end f events . custom)
    "integer integer procedure:{(samples ...) -> unspecified} seq-events -> seq-event
     evaluate events with seq and write the results into a temporary block which is passed to f
     for further processing"
    (let*
      ( (eval-events
          (procedure->cached-procedure
            (l (t size channels events custom c)
              (let*
                ( (event-output (sp-block-new channels size))
                  (events (seq t 0 size event-output events)))
                (c event-output events (apply f size event-output custom))))
            (l (t size . a) (pair t size)))))
      (seq-event-new start end
        (l (t offset size output event)
          (let (state (seq-event-state event))
            (eval-events t size
              (length output) (first state)
              (tail state)
              (l (event-output events custom)
                (each
                  (l (a b)
                    (each-integer size
                      (l (index)
                        (sp-samples-set! a (+ offset index)
                          (float-sum (sp-samples-ref a (+ offset index)) (sp-samples-ref b index))))))
                  output event-output)
                (seq-event-state-update event (pair events custom))))))
        (pair events custom))))

  (define (seq-event-state-update a state) "return a new event with the state updated"
    (pair state (tail a)))

  (define seq-event-data-start (vector-accessor 0))
  (define seq-event-data-end (vector-accessor 1))
  (define seq-event-data-f (vector-accessor 2))
  (define seq-event-data tail)
  (define seq-event-state first)

  (define* (seq-parallel time offset size output events)
    "integer integer integer (samples:channel ...) seq-events -> seq-events
     calls one or multiple functions that add to the given output block in parallel at predefined times and sums result samples.
     write to output after given offset. output samples length must be equal or greater than offset + size.
     the returned object is are the events with finished events removed and can be passed to the next call to seq-parallel or seq.
     seq-parallel can be nested, but unless more cpu cores are available then using seq will be more efficient.
     events are created with seq-events-new and seq-event-new.
     # example
     (let
       ( (result (sp-block-new 1 96000))
         (events
           (seq-events-new
             (seq-event-new 10000 48000
               (lambda (time offset size output event)
                 (for-each
                   (lambda (output)
                     (each-integer size
                       (l (index)
                         (sp-samples-set! output (+ offset index)
                           (sp-sample-sum (sp-samples-ref output (+ offset index))
                             (sp-sine~ (+ index time) 5000))))))
                   output)
                 event)))))
       (seq-parallel 0 0 96000 result events)
       (sp-plot-samples (first result)))"
    ; take sorted event objects that have a start/end time and a function that is called with a time offset and an output array.
    ; each block, filter events that write into the block, allocate arrays for the portions they write to, call the events in parallel,
    ; merge the event blocks into the given output block. return a list of events with finished events removed.
    (define (merge results rest-events)
      (let loop ((results results) (events null))
        (if (null? results) (append (reverse events) rest-events)
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
    (let ((time-end (+ time size)) (channels (length output)))
      (let loop ((results null) (rest events))
        (if (null? rest) (merge results rest)
          (let*
            ( (event (first rest)) (data (seq-event-data event)) (start (seq-event-data-start data))
              (end (seq-event-data-end data)))
            (if (< time-end start) (merge results rest)
              (if (> time end) (loop results (tail rest))
                (loop
                  (pair
                    (future
                      (let*
                        ( (time-offset (if (> start time) (- start time) 0))
                          (time-offset-right (if (< end time-end) (- time-end end) 0))
                          (size (- size time-offset time-offset-right))
                          (output (sp-block-new channels size)))
                        (list (+ offset time-offset) size
                          output
                          ( (seq-event-data-f data) (- (+ time time-offset) start) 0
                            size output event))))
                    results)
                  (tail rest)))))))))

  (define* (seq time offset size output events)
    "integer integer integer (samples:channel ...) seq-events -> seq-events
     like seq-parallel but each event is evaluated one after another and writes directly into output"
    (let ((time-end (+ time size)) (channels (length output)))
      (let loop ((results null) (rest events))
        (if (null? rest) (append (reverse results) rest)
          (let*
            ( (event (first rest)) (data (seq-event-data event)) (start (seq-event-data-start data))
              (end (seq-event-data-end data)))
            (if (< time-end start) (append (reverse results) rest)
              (if (> time end) (loop results (tail rest))
                (loop
                  (pair
                    (let*
                      ( (time-offset (if (> start time) (- start time) 0))
                        (time-offset-right (if (< end time-end) (- time-end end) 0))
                        (size (- size time-offset time-offset-right)))
                      ( (seq-event-data-f data) (- (+ time time-offset) start)
                        (+ offset time-offset) size output event))
                    results)
                  (tail rest)))))))))

  (define*
    (seq-block-series time channels count events f custom #:key (block-size 96000) (progress #f)
      (parallel #t))
    "integer integer integer seq-events procedure:{(samples:channel ...) seq-events custom ... -> (seq-events custom ...)} -> (seq-events custom ...)"
    (let (seq (if parallel seq-parallel seq))
      (apply sp-fold-integers count
        (l (block-index events . custom)
          (if progress
            (display-line
              (string-append "processing block " (number->string (+ 1 block-index)) "...")))
          (let*
            ( (output (sp-block-new channels block-size))
              (result
                (apply f output (seq (* block-index block-size) 0 block-size output events) custom)))
            (if progress (if (= count (+ 1 block-index)) (display-line "processing finished")))
            result))
        events custom)))

  (define*
    (seq-block-series->list time channels count events #:key (block-size 96000) (progress #f)
      (parallel #t))
    "-> (events block ...)"
    (apply (l (events . blocks) (pair events (reverse blocks)))
      (seq-block-series time channels
        count events
        (l (output events . result) (pair events (pair output result))) null
        #:block-size block-size #:progress progress #:parallel parallel)))

  (define*
    (seq-block-series->file path time channels count events #:key (block-size 96000)
      (sample-rate 96000)
      (progress #f)
      (parallel #t))
    "string integer integer seq-events [#:block-size integer #:sample-rate integer] -> seq-events"
    (sp-call-with-output-file path channels
      sample-rate
      (l (file)
        (seq-block-series time channels
          count events
          (l (output events) (sp-file-write file output block-size) (list events)) null
          #:block-size block-size #:progress progress #:parallel parallel)))))
