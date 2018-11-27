(library (sph sp sequencer)
  (export
    seq
    seq-default-mixer
    seq-event
    seq-event-custom
    seq-event-f
    seq-event-groups
    seq-event-list->events
    seq-event-name
    seq-event-new
    seq-event-start
    seq-event-update
    seq-events-merge
    seq-index-data
    seq-index-end
    seq-index-events
    seq-index-f
    seq-index-f-new
    seq-index-i-f
    seq-index-i-f-new
    seq-index-i-next
    seq-index-new
    seq-index-next
    seq-index-start
    seq-index-update
    seq-output
    seq-output-new
    seq-state-add-events
    seq-state-event-states
    seq-state-index
    seq-state-index-i
    seq-state-input
    seq-state-new
    seq-state-options
    seq-state-output
    seq-state-update
    seq-state-user-value)
  (import
    (guile)
    (sph)
    (sph alist)
    (sph list)
    (sph list other)
    (sph number)
    (sph vector)
    (only (sph sp) sp-clip)
    (only (srfi srfi-1) zip partition))

  (define sph-sp-sequencer-description
    "seq calls sample generating functions at customisable times, shares state values between them and
     mixes output values to create a single result. seq can return single sample values or sample arrays,
     this depends completely on the user supplied events-f and event-f functions")

  (define seq-index-data (vector-accessor 1))
  (define seq-index-end (vector-accessor 2))
  (define seq-index-events (vector-accessor 3))
  (define seq-index-f (vector-accessor 4))
  (define seq-index-i-f (vector-accessor 5))
  (define seq-index-start (vector-accessor 6))

  (define (seq-index-new data end events f i-f start)
    (vector (q seq-index) data end events f i-f start))

  (define* (seq-index-update a #:key data end events f i-f start)
    (vector (or data (seq-index-data a)) (or end (seq-index-end a))
      (or events (seq-index-events a)) (or f (seq-index-f a))
      (or i-f (seq-index-i-f a)) (or start (seq-index-start a))))

  (define (seq-index-next index time state)
    (let (index-f (seq-index-f index))
      (if index-f (index-f time state) (seq-index-f-new time state))))

  (define (seq-index-i-next index time) ((seq-index-i-f index) time (seq-index-start index)))

  (define (seq-index-add-events index events) "seq-index seq-events"
    (if (null? events) events
      (seq-index-update index #:events (seq-events-merge (seq-index-events index) events))))

  (define (seq-index-i-f-new duration index-size)
    "vector number number -> procedure
     return a procedure that returns an index in index-data for a time value"
    (let (slot-duration (/ duration index-size))
      (l (time index-start) (inexact->exact (truncate (/ (- time index-start) slot-duration))))))

  (define (seq-index-f-new time state)
    "number seq-state -> procedure
     create a procedure that returns new event index for a time and state using
     the procedure events-f from the state to get the events"
    (let*
      ( (options (seq-state-options state)) (duration (alist-ref-q options index-duration))
        (size (inexact->exact (round (* duration (alist-ref-q options index-size-factor)))))
        (index-i-f (seq-index-i-f-new duration size)) (events-f (seq-state-events-f state)))
      (letrec
        ( (index-f
            (l (time state) "number seq-state -> seq-index"
              "create an alist where the key is the target index and the value are events for this index.
              then add empty entries for intermediate indexes, remove keys and create an index-data vector
              and the index-events list for out-of-index events"
              (let*
                ( (end (+ time duration)) (events-1 (events-f time end state))
                  (events-2
                    (if (seq-state-index state) (seq-index-events (seq-state-index state)) null))
                  (slots
                    (group (append events-1 events-2)
                      (l (a)
                        ; use zero for events outside index
                        (let (index-i (index-i-f (seq-event-start a) time))
                          (if (< index-i size) (+ 1 index-i) 0)))))
                  (slots (list-sort-with-accessor < first slots))
                  (slots
                    (let loop ((rest slots) (last-id 0))
                      ; add empty entries for inbetween slots
                      (if (null? rest) rest
                        (let* ((a (first rest)) (id (first a)) (diff (- id last-id)))
                          (if (> diff 1)
                            (append (make-list (- diff 1) (list null))
                              (pair a (loop (tail rest) id)))
                            (pair a (loop (tail rest) id))))))))
                (if (null? slots) (seq-index-new (vector) end null index-f index-i-f time)
                  (let*
                    ( (slots (if (zero? (first (first slots))) slots (pair (list 0) slots)))
                      (events-1 (map tail (tail slots))) (events-2 (tail (first slots)))
                      (events-1-length (length events-1))
                      (events-1
                        ; add empty entries for trailing slots
                        (if (< events-1-length size)
                          (append events-1 (make-list (- size events-1-length) null)) events-1)))
                    (seq-index-new (apply vector events-1) end events-2 index-f index-i-f time)))))))
        index-f)))

  (define-as seq-state-options-default alist-q index-duration 4 index-size-factor 4)
  (define seq-state-user-value (vector-accessor 1))
  (define seq-state-events-f (vector-accessor 2))
  (define seq-state-event-states (vector-accessor 3))
  (define seq-state-index (vector-accessor 4))
  (define seq-state-index-i (vector-accessor 5))
  (define seq-state-input (vector-accessor 6))
  (define seq-state-mixer (vector-accessor 7))
  (define seq-state-options (vector-accessor 8))
  (define seq-state-output (vector-accessor 9))

  (define*
    (seq-state-new events-f #:key event-states user-value index index-i input mixer options output)
    "procedure [#:event-f-list list #:user-value alist] -> seq-state
     create a new state object.
     seq-state: (results event-f-list index-i index index-f user-value)
     index-f: -> (procedure:index-i-f . vector:index)
     user-value: ((symbol . any) ...)
     event-f-list: list"
    (vector (q seq-state) (or user-value null)
      events-f (or event-states null)
      index (or index-i 0)
      (or input null) (or mixer seq-default-mixer)
      (or (and options (alist-merge seq-state-options-default options)) seq-state-options-default)
      (or output null)))

  (define*
    (seq-state-update a #:key user-value events-f event-states index index-i input mixer options
      output)
    (vector (q seq-state) (or user-value (seq-state-user-value a))
      (or events-f (seq-state-events-f a)) (or event-states (seq-state-event-states a))
      (or index (seq-state-index a)) (or index-i (seq-state-index-i a))
      (or input (seq-state-input a)) (or mixer (seq-state-mixer a))
      (or options (seq-state-options a)) (or output (seq-state-output a))))

  (define seq-event-state (vector-accessor 1))
  (define seq-event-f (vector-accessor 2))
  (define seq-event-groups (vector-accessor 3))
  (define seq-event-name (vector-accessor 4))
  (define seq-event-start (vector-accessor 5))

  (define* (seq-event-new f #:optional name start event-state groups)
    "procedure #:key integer (symbol ...) any -> vector"
    (vector (q seq-event) (or event-state null) f groups (or name (q unnamed)) (or start 0)))

  (define-syntax-rule (seq-event name f optional ...) (seq-event-new f (q name) optional ...))

  (define* (seq-event-update a #:key f start name groups event-state)
    (vector (or start (seq-event-start a)) (or f (seq-event-f a))
      (or name (seq-event-name a)) (or groups (seq-event-groups a))
      (or event-state (seq-event-state a))))

  (define (seq-event-list->events a) (if (null? a) a (if (vector? (first a)) (map list a) a)))

  (define (seq-events-merge a b) "events:target events:source -> events"
    (if (null? b) a
      (pair (if (null? a) (first b) (append (first a) (first b)))
        (seq-events-merge (if (null? a) a (tail a)) (tail b)))))

  (define (seq-state-add-events state . events)
    "seq-state seq-event-list/seq-events ... -> state
     add new events to either the current input list or the index"
    (let*
      ( (index (seq-state-index state))
        (events
          (map
            (let (index-end (seq-index-end index))
              (l (event-list)
                (apply-values pair (partition (l (a) (< (seq-event-start a) index-end)) event-list))))
            (seq-event-list->events events))))
      (let ((soon (map first events)) (later (map tail events)))
        (seq-state-update state #:input
          (seq-events-merge (seq-state-input state) soon) #:index (seq-index-add-events index later)))))

  (define seq-output-name (vector-accessor 1))
  (define seq-output-data (vector-accessor 2))
  (define seq-output-event-state (vector-accessor 3))
  (define seq-output-event (vector-accessor 4))

  (define* (seq-output-new name data event-state event)
    (vector (q seq-output) name data event-state event))

  (define* (seq-output data state #:optional event-state)
    "symbol integer/vector/any seq-state list list:alist -> list
     create the output structure that event-f must return"
    (pairs data state (or event-state null)))

  (define (seq-default-mixer output)
    "combines multiple event-f results, seq-output objects, into one seq result, which should be sample values.
     sum the samples of each channel, clip and return a vector with one sample per channel"
    (if (null? output) (vector)
      (list->vector
        (map sp-clip
          (map-apply float-sum
            (apply zip (map (compose vector->list any->vector seq-output-data) output)))))))

  (define seq
    (letrec
      ( (index-next
          (l (time state)
            "-> seq-state
            call index-f to get the next index and add the
            input event-f from the next index"
            (let (index (seq-state-index state))
              (and-let*
                ( (index
                    (if index (seq-index-next index time state)
                      ((seq-index-f-new time state) time state)))
                  (data (seq-index-data index))
                  (input
                    (if (zero? (vector-length data)) null
                      (seq-event-list->events (vector-first data)))))
                (seq-state-update state #:input
                  (seq-events-merge (seq-state-input state) input) #:index index #:index-i 0)))))
        (index-ensure
          (l (time state)
            "-> seq-state
            ensure that current index data is available and eventually call index-f to get it"
            (let (index (seq-state-index state)) (if index state (index-next time state)))))
        (input-extend
          (l (state index index-i)
            "-> seq-state
            add event-f from index to the current input list"
            (let (input (seq-event-list->events (vector-ref (seq-index-data index) index-i)))
              (seq-state-update state #:input
                (seq-events-merge (seq-state-input state) input) #:index-i index-i))))
        (index-advance
          (l (time state)
            "-> seq-state
            eventually update the input event-f list from the current or a new index"
            (let (index (seq-state-index state))
              (let*
                ((index-i (seq-state-index-i state)) (index-i-new (seq-index-i-next index time)))
                (if (= index-i-new index-i) state
                  (if (< time (seq-index-end index)) (input-extend state index index-i-new)
                    (index-next time state)))))))
        (clear-output (l (state) (seq-state-update state #:output null)))
        (execute-event-list
          (l (time state event-list event-states c)
            "number seq-state event-list procedure:{state event-list -> any:result} -> any
            check every event for if it is due and eventually execute it, update state from its
            result and remove it from the event-list if its event-f returns false"
            (if (null? event-list) (c state event-list event-states)
              ; for each event in input event list
              (let loop
                ( (state state) (rest event-list) (result-event-list null)
                  (result-event-states null))
                (if (null? rest) (c state result-event-list result-event-states)
                  (let (event (first rest))
                    ; check if event is due
                    (if (<= (seq-event-start event) time)
                      (let*
                        ( (event-name (seq-event-name event))
                          (event-result
                            ( (seq-event-f event) time state
                              event (- time (seq-event-start event))
                              (or (alist-ref event-states event-name) (seq-event-state event)))))
                        (if event-result
                          (list-bind event-result (data state . event-state)
                            (loop
                              (seq-state-update state #:output
                                (pair (seq-output-new event-name data event-state event)
                                  (seq-state-output state)))
                              (tail rest) (pair event result-event-list)
                              (pair (pair event-name event-state) result-event-states)))
                          (loop state (tail rest) result-event-list result-event-states)))
                      (loop state (tail rest) (pair event result-event-list) result-event-states))))))))
        (execute-events
          (l (time state)
            "number seq-state -> seq-state
            execute event-lists, update state and eventually leave out empty result event-lists"
            (let loop
              ( (rest (seq-state-input state)) (state state) (result-events null)
                (result-event-states null))
              (if (null? rest)
                (seq-state-update state #:input
                  (reverse result-events) #:event-states result-event-states)
                (execute-event-list time state
                  (first rest) (seq-state-event-states state)
                  (l (state event-list event-states)
                    (loop (tail rest) state
                      (if (null? event-list) result-events (pair event-list result-events))
                      (append result-event-states event-states)))))))))
      (l (time state c)
        "integer list procedure:{results state -> any:seq-result} -> any:seq-result
        the given procedure receives the results and the updated state and creates the final result of the call to seq"
        (or
          (and-let*
            ( (state (index-ensure time state)) (state (index-advance time state))
              (state (execute-events time state)))
            (c ((seq-state-mixer state) (seq-state-output state)) (clear-output state)))
          (c 0 state))))))
