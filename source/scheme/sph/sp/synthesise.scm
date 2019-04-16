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
    seq-event-end
    seq-event-group
    seq-event-group-map
    seq-event-new
    seq-event-start
    seq-event-state
    seq-event-state-update
    seq-events-end
    seq-events-from-list
    seq-events-new
    seq-events-start
    seq-parallel
    sp-block->file
    sp-blocks->file
    sp-cheap-noise-event
    sp-clip~
    sp-events->block
    sp-fm-synth-event
    sp-fm-synth-event*
    sp-noise-event
    sp-noise-uniform~
    sp-path
    sp-path*
    sp-path-procedure
    sp-path-procedure-fast
    sp-phase
    sp-phase-float
    sp-precompiled-event
    sp-rectangle
    sp-rectangle~
    sp-samples-list-add-offsets
    sp-sawtooth~
    sp-sine-2~
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
    (sph sp filter)
    (sph spline-path)
    (sph vector)
    (only (guile)
      compose
      const
      floor
      inexact->exact
      make-list
      modulo
      random:exp
      random:uniform
      random:normal
      *random-state*)
    (only (rnrs base) set!)
    (only (sph number) float-sum)
    (only (sph other) each-integer procedure->cached-procedure)
    (only (srfi srfi-1) partition))

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

  (define* (sp-square~ t #:optional (wavelength 96000))
    "integer integer -> sample
     center will fall nicely between two samples if the wavelength is even"
    (if (< (modulo (* 2 t) (* 2 wavelength)) wavelength) -1 1))

  (define sine-96000
    (let ((step (/ (* 2 sp-pi) 96000)) (samples (sp-samples-new 96000)))
      (each-integer 96000 (l (t) (sp-samples-set! samples t (sin (* t step))))) samples))

  (define (sp-sine~ t) (sp-samples-ref sine-96000 t))

  (define* (sp-sine-2~ t #:optional (wavelength 96000))
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
     phase generator that allows for high resolution modulation.
     * y: previous result or another starting value to continue from
     * change: how fast the phase should progress. frequency
     * phase-size: value at which the cycle should repeat
     example: (sp-phase 0.0 100 96000)"
    (let (y (+ change y)) (if (<= phase-size y) (modulo y phase-size) y)))

  (define (sp-phase-float y change phase-size)
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

  (define* (sp-path-procedure-fast a #:key (dimension 1))
    "spline-path/spline-path-config/number/point [keys ...] -> procedure"
    (let*
      ( (path
          (cond
            ((spline-path? a) a)
            ((number? a) (spline-path-constant a))
            ((list? a) (if (every number? a) (apply spline-path-constant a) (spline-path-new a)))
            (else a)))
        (path-procedure (if (procedure? path) path (spline-path->procedure-fast path))))
      (if dimension (compose (l (point) (list-ref point dimension)) path-procedure) path-procedure)))

  (define (sp-event-f-with-resolution resolution amplitudes input-f block-f output-map-f)
    "helper that calls f with a block-count and a buffer where it can write to.
     f also receives and updates the event state.
     use case is to process in custom sub-block lengths.
     input-f :: size -> any
     block-f :: time offset size samples:output any:input any:event-state -> event-state
     output-map-f :: sample amplitude time -> sample"
    (l (t offset size output event)
      (let*
        ( (resolution (min size resolution))
          (block-count (if (= size resolution) 1 (floor (/ size resolution))))
          (block-rest (modulo size resolution)) (out (sp-samples-new size))
          (in (input-f size))
          (state
            (fold-integers block-count (seq-event-state event)
              (l (block-index state)
                (let* ((block-offset (* resolution block-index)) (t (+ t block-offset)))
                  (block-f t block-offset resolution out in state)))))
          (state
            (if (zero? block-rest) state
              (let (block-offset (* resolution block-count))
                (block-f (+ t block-offset) block-offset block-rest out in state)))))
        (each
          (l (output a) "apply amplitudes and sum into output"
            (each-integer size
              (l (index)
                (sp-samples-set! output (+ offset index)
                  (float-sum (sp-samples-ref output (+ offset index))
                    (output-map-f (sp-samples-ref out index) a (+ t index)))))))
          output amplitudes)
        (seq-event-state-update event state))))

  (define*
    (sp-wave-event start end amplitudes wavelength #:key (phase 0)
      (generator (l (a b) (sp-sine~ a)))
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
              (l (index phase)
                (let (wavelength (wavelength (+ time index)))
                  (if (zero? wavelength) phase
                    (let*
                      ( (phase (sp-phase phase (round (/ phase-length wavelength)) phase-length))
                        (sample (generator phase)))
                      (each
                        (l (output a)
                          (sp-samples-set! output (+ offset index)
                            (float-sum (sp-samples-ref output (+ offset index))
                              (* (a (+ time index)) sample))))
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
    (sp-noise-event start end amplitudes cut-l cut-h #:key (noise sp-noise-uniform~) (trn-l 0.01)
      (trn-h 0.01)
      reject
      (resolution 96)
      repeat-noise)
    "integer integer (sp-path ...) sp-path sp-path [keys ...] -> seq-event
     create noise filtered by sp-filter!.
     # keys
     noise: procedure:{-> sample}   (gives noise samples)
     trn-l: sp-path   (transition bandwidth)
     trn-h: sp-path   (transition bandwidth)
     reject: boolean   (band reject / notch filter if true)
     repeat-noise: boolean   (if true then source noise samples are reused and not regenerated)
     resolution: integer   (number of samples after which parameters are updated)"
    (let
      ( (amplitudes (map sp-path-procedure amplitudes)) (cut-l (sp-path-procedure cut-l))
        (cut-h (sp-path-procedure cut-h)) (trn-l (sp-path-procedure trn-l))
        (trn-h (sp-path-procedure trn-h)) (get-noise (get-noise-f repeat-noise noise start end)))
      (seq-event-new start end
        (sp-event-f-with-resolution resolution amplitudes
          get-noise
          (l (t start size output input state)
            (sp-windowed-sinc-bp-br! output input
              (cut-l t) (cut-h t) (trn-l t) (trn-h t) reject state start size start))
          (l (output-sample amp t) (* (amp t) output-sample)))
        #f)))

  (define*
    (sp-cheap-noise-event start end amplitudes cutoff passes type #:key (q-factor 0)
      (noise sp-noise-uniform~)
      (resolution 96)
      repeat-noise)
    "integer integer (sp-path ...) sp-path integer symbol [keys ...] -> seq-event
     like sp-noise-event but using sp-cheap-filter!.
     see sp-cheap-filter! for more details.
     type: symbol:low/high/band/peak/notch/all
     # keys
     noise: procedure:{-> sample}
     q-factor: real:0..1
     repeat-noise: boolean
     resolution: integer"
    (let
      ( (amplitudes (map sp-path-procedure amplitudes)) (cutoff (sp-path-procedure cutoff))
        (get-noise (get-noise-f repeat-noise noise start end)))
      (seq-event-new start end
        (sp-event-f-with-resolution resolution amplitudes
          get-noise
          (l (t start size output input state)
            (sp-cheap-filter! type output
              input (cutoff t)
              passes state
              #:q-factor q-factor #:in-start start #:in-count size #:out-start start #:unity-gain #t))
          (l (output-sample amp t) (* (amp t) output-sample)))
        null)))

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

  (define (seq-events-from-list a)
    "(seq-event ...) -> seq-events
     same as seq-events-from-list except that events are passed as a list instead of multiple arguments"
    (list-sort-with-accessor < (compose seq-event-data-start seq-event-data) a))

  (define (seq-events-new . a)
    "seq-event ... -> seq-events
     create a events-list sorted for seq"
    (seq-events-from-list a))

  (define (seq-event-group start end events) "integer integer seq-events -> seq-event"
    (seq-event-new start end
      (l (t offset size output event)
        (seq-event-state-update event (seq t offset size output (seq-event-state event))))
      events))

  (define (seq-event-group-map start end f events . custom)
    "integer integer procedure:{size (samples ...) custom ... -> ((samples ...) custom ...)} seq-events -> seq-event
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
  (define seq-event-start (compose seq-event-data-start seq-event-data))
  (define seq-event-end (compose seq-event-data-end seq-event-data))
  (define seq-events-start (compose seq-event-start first))
  (define seq-events-end (l (a) (apply max (map seq-event-end a))))

  (define* (seq-parallel time offset size output events)
    "integer integer integer (samples:channel ...) seq-events -> seq-events
     calls one or multiple functions that add to the given output block in parallel at event-defined times and sums result samples.
     write to output after given offset. output samples length must be equal or greater than offset + size.
     the output block to fill will be of specified size.
     the returned object is are the events with finished events removed and can be passed to the next call to seq-parallel or seq.
     seq-parallel can be nested, but unless more cpu cores are available then using seq will be more efficient.
     events are created with seq-events-new and seq-event-new.
     # example
     (let
       ( (result (sp-block-new 1 96000))
         (events
           (seq-events-new*
             (seq-event-new 10000 48000
               (lambda (time offset size output event)
                 (for-each
                   (lambda (output)
                     (each-integer size
                       (l (index)
                         (sp-samples-set! output (+ offset index)
                           (sp-sample-sum (sp-samples-ref output (+ offset index))
                             (sp-sine~ (+ index time)))))))
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
        (l (block-index events . custom) "-> events custom ..."
          (if progress
            (display-line
              (string-append "processing block " (number->string (+ 1 block-index)) "...")))
          (let*
            ( (output (sp-block-new channels block-size))
              (result
                (apply f (seq (* block-index block-size) 0 block-size output events) output custom)))
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
        (l (events output . result) (pair events (pair output result))) null
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
          (l (events output) (sp-file-write file output block-size) (list events)) null
          #:block-size block-size #:progress progress #:parallel parallel))))

  (define* (sp-events->block channels events #:optional (start-offset 0))
    "seq-events -> (samples:channel ...):block
     evaluate events and return their output in a new block"
    (let*
      ( (start (seq-events-start events)) (end (seq-events-end events)) (size (- end start))
        (block (sp-block-new channels (- end start))))
      (seq start-offset 0 size block events) block))

  (define (sp-precompiled-event channels events)
    "integer seq-events -> procedure:{start end -> seq-event}
     returns a procedure that returns a new event for the pre-evaluated and repeated cached data from events.
     events is evaluated only once, regardless of how many copies are created with the result procedure"
    (let*
      ( (cache (sp-events->block channels events)) (events-end (seq-events-end events))
        (event-f
          (l (t offset size output event)
            (each
              (l (out in)
                (each-integer size
                  (l (index)
                    (sp-samples-set! out (+ offset index)
                      (float-sum (sp-samples-ref out (+ offset index))
                        (sp-samples-ref in (+ t index)))))))
              output cache)
            event)))
      (l* (start #:optional end) (seq-event-new start (or end (+ start events-end)) event-f))))

  (define-syntax-rule (sp-fm-synth-event* start duration operator ...)
    (sp-fm-synth-event start duration (qq (operator ...))))

  (define (sp-fm-synth-event start duration operators)
    "for each operator it allows control of amplitude, wavelength and phase offset per channel.
     operator config elements are (id modulator-of (amplitude-path ...) (wavelength-path ...) (phase-offset ...))
     (sp-fm-synth-event* start duration
       (1 0 (0.9) (2000) (0))
       (4 1 (0.7) (3000) (0)))"
    (define (evaluate time offset size output phases wavelengths amplitudes . modulators)
      "-> (value . state)"
      (let*
        ( (modulator-output
            (map
              (l (a) (apply evaluate time 0 size (map (l a (sp-samples-new size)) amplitudes) a))
              modulators))
          (modulator-samples (map first modulator-output))
          (phases
            (apply map
              (l (out phs wvl amp . mod)
                (fold-integers size phs
                  (l (index phs)
                    (let*
                      ( (t (+ time index)) (wvl (wvl t))
                        (wvl
                          (inexact->exact
                            (round
                              (/ 96000
                                (apply float-sum wvl
                                  (map (l (a) (* (sp-samples-ref a index) wvl)) mod))))))
                        (phs (sp-phase phs wvl 96000)))
                      (sp-samples-set! out (+ offset index)
                        (float-sum (sp-samples-ref out (+ offset index)) (* (amp t) (sp-sine~ phs))))
                      phs))))
              output phases wavelengths amplitudes modulator-samples)))
        (pairs output phases wavelengths amplitudes (map tail modulator-output))))
    (define (state-new carrier operators)
      "used to create a list of nested lists used and updated by evaluate.
       nested lists are state values for dependent modulators"
      (apply
        (l (carrier-id modulator-of amplitudes wavelengths phases)
          (apply-values
            (l (modulators operators)
              (pairs phases (map sp-path-procedure-fast wavelengths)
                (map sp-path-procedure-fast amplitudes)
                (map (l (a) (state-new a operators)) modulators)))
            (partition (l (a) (eqv? carrier-id (second a))) operators)))
        carrier))
    (seq-event-new start (+ start duration)
      (l (time offset size output event)
        (seq-event-state-update event
          (map (l (a) (tail (apply evaluate time offset size output a))) (seq-event-state event))))
      (apply-values (l (carriers operators) (map (l (a) (state-new a operators)) carriers))
        (partition (l (a) (zero? (second a))) operators)))))
