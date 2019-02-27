(library (sph sp sequencer)
  (export
    seq
    seq-assoc-bind
    seq-assoc-ref
    seq-assoc-refq
    seq-assoc-set
    seq-assoc-set*
    seq-assoc-setq
    seq-assoc-setq*
    seq-event-f
    seq-event-list->events
    seq-event-new
    seq-event-start
    seq-events-merge
    seq-index-data
    seq-index-end
    seq-index-events
    seq-index-f-new
    seq-index-i-f
    seq-index-i-f-new
    seq-index-start
    seq-state-add-events
    seq-state-custom
    seq-state-events
    seq-state-index
    seq-state-index-f
    seq-state-index-i
    seq-state-index-slot-count
    seq-state-index-slot-duration
    seq-state-new
    seq-state-rate)
  (import
    (sph)
    (sph hashtable)
    (only (guile)
      compose
      inexact->exact
      make-list)
    (only (sph list) list-sort-with-accessor map-integers)
    (only (sph list other) group)
    (only (sph number) float-sum)
    (only (sph sp) sp-samples-new)
    (only (sph vector)
      vector-accessor
      vector-setter
      vector-first)
    (only (srfi srfi-1) zip partition))

  (define sph-sp-sequencer-description
    "seq calls sample generating functions at customisable times with custom data state.
     * seq is supposed to return a samples vector of custom length
     * time is counted in samples for exactness as seconds dont always map exactly to samples
     * seq-state-rate is provided to access the sample-rate value provided with seq-state-new
     * index is a vector for a time span where events are distributed over to not have to check all registered events every time
     * events-f is the main user function and will be called for index durations when new events are needed")

  (define (seq-assoc-ref a key) (ht-ref a key))
  (define (seq-assoc-set a key value) (ht-set! a key value))
  (define (seq-assoc-set* a . key/value) (apply ht-set-multiple! a key/value))
  (define (seq-assoc-new) (ht-make-eqv))
  (define-syntax-rule (seq-assoc-refq a key) (seq-assoc-ref a (quote key)))

  (define-syntax-rule (seq-assoc-setq* a key/value ...)
    (apply seq-assoc-update a (quote-odd key/value ...)))

  (define-syntax-rule (seq-assoc-setq a key value) (seq-assoc-set a (quote key) value))
  (define-syntax-rule (seq-assoc-bind a (id ...) body ...) (ht-bind a (id ...) body ...))
  (define seq-state-events-f (vector-accessor 1))
  (define seq-state-events (vector-accessor 2))
  (define seq-state-index (vector-accessor 3))
  (define seq-state-index-i (vector-accessor 4))
  (define seq-state-index-f (vector-accessor 5))
  (define seq-state-rate (vector-accessor 6))
  (define seq-state-custom (vector-accessor 7))
  (define seq-state-events-set! (vector-setter 2))
  (define seq-state-index-set! (vector-setter 3))
  (define seq-state-index-i-set! (vector-setter 4))
  (define seq-index-start (vector-accessor 1))
  (define seq-index-end (vector-accessor 2))
  (define seq-index-data (vector-accessor 3))
  (define seq-index-events (vector-accessor 4))
  (define seq-index-i-f (vector-accessor 5))
  (define seq-event-start (vector-accessor 1))
  (define seq-event-f (vector-accessor 2))
  (define seq-event-state (vector-accessor 3))

  (define (seq-index-i-f-new slot-duration)
    "integer -> procedure:{integer integer -> integer}
     return a procedure that returns an index in index-data for a time value"
    (l (time index-start) (inexact->exact (truncate (/ (- time index-start) slot-duration)))
      (inexact->exact (truncate (/ (- time index-start) slot-duration)))))

  (define (seq-index-f-new events-f slot-duration slot-count)
    (let
      ((index-duration (* slot-duration slot-count)) (index-i-f (seq-index-i-f-new slot-duration)))
      (l (t s)
        "integer seq-state -> seq-index
         get a new index for time and state with events distributed over vector elements"
        (let*
          ( (end (+ t index-duration))
            ; new events
            (events-1 (events-f t s))
            ; events scheduled in the state object
            (events-2 (if (seq-state-index s) (seq-index-events (seq-state-index s)) null))
            (slots
              (group (append events-1 events-2)
                ; -> ((integer:index-i event ...) ...)
                (l (a)
                  ; use zero for events outside current index time range
                  (let (index-i (index-i-f (seq-event-start a) t))
                    (if (< index-i slot-count) (+ 1 index-i) 0)))))
            (slots (list-sort-with-accessor < first slots))
            (slots
              (let loop ((rest slots) (last-id 0))
                ; add empty entries for inbetween slots
                (if (null? rest) rest
                  (let* ((a (first rest)) (id (first a)) (diff (- id last-id)))
                    (if (> diff 1)
                      (append (map-integers (- diff 1) (list null)) (pair a (loop (tail rest) id)))
                      (pair a (loop (tail rest) id))))))))
          ; create an alist where key is the numeric target index and the value are the events for a time range.
          ; then fill with empty lists for intermediate indices, remove the alist keys and create an index-data vector
          ; and an index-events list for out-of-index events
          (if (null? slots) (vector (q seq-index) t end (vector) null (l a 0))
            (begin
              (let*
                ( (slots (if (zero? (first (first slots))) slots (pair (list 0) slots)))
                  (events-1 (map tail (tail slots))) (events-2 (tail (first slots)))
                  (events-1-length (length events-1))
                  (events-1
                    ; add empty entries for trailing slots
                    (if (< events-1-length slot-count)
                      (append events-1 (make-list (- slot-count events-1-length) null)) events-1)))
                (vector (q seq-index) t end (apply vector events-1) events-2 index-i-f))))))))

  (define*
    (seq-state-new events-f #:key events index index-i index-f index-slot-duration index-slot-count
      rate
      custom)
    "procedure _ ... -> seq-state
     create a new state object"
    (vector (q seq-state) events-f
      (or events null) index
      (or index-i 0)
      (or index-f (seq-index-f-new events-f (or index-slot-duration 10000) (or index-slot-count 4)))
      rate custom))

  (define* (seq-event-new f #:optional start event-state)
    "procedure [integer false/seq-assoc] -> vector"
    (vector (q seq-event) (or start 0) f (or event-state (seq-assoc-new))))

  (define (seq-event-list->events a) (if (null? a) a (if (vector? (first a)) (map list a) a)))

  (define (seq-events-merge a b) "events:target events:source -> events"
    (if (null? b) a
      (pair (if (null? a) (first b) (append (first a) (first b)))
        (seq-events-merge (if (null? a) a (tail a)) (tail b)))))

  (define (seq-state-add-events s . events)
    "seq-state seq-event-list/seq-events ... -> state
     add new events to either the active events list or the index"
    (let*
      ( (index (seq-state-index s))
        (events
          (map
            (let (index-end (seq-index-end index))
              (l (event-list)
                (apply-values pair (partition (l (a) (< (seq-event-start a) index-end)) event-list))))
            (seq-event-list->events events))))
      (let ((soon (map first events)) (later (map tail events)))
        (seq-state-events-set! (seq-events-merge (seq-state-events s) soon))
        (seq-state-index-set! (seq-events-merge (seq-index-events index) events)))))

  (define seq
    (let*
      ( (index-next
          (l (t s)
            (and-let* ((index ((seq-state-index-f s) t s)) (data (seq-index-data index)))
              (if (< 0 (vector-length data))
                (seq-state-events-set! s
                  (seq-events-merge (seq-state-events s)
                    (seq-event-list->events (vector-first data)))))
              (seq-state-index-set! s index) (seq-state-index-i-set! s 0) #t)))
        (index-update
          (l (t s)
            "integer seq-state -> seq-state
            maybe update events from the current index or a new index"
            (let (index (seq-state-index s))
              (if index
                (let
                  ( (index-i (seq-state-index-i s))
                    (index-i-new ((seq-index-i-f index) t (seq-index-start index))))
                  (or (= index-i-new index-i)
                    (and (< t (seq-index-end index))
                      (begin
                        ; add more events to current events
                        (seq-state-events-set! s
                          (seq-events-merge (seq-state-events s)
                            (seq-event-list->events (vector-ref (seq-index-data index) index-i))))
                        (seq-state-index-i-set! s index-i) #t))
                    (index-next t s)))
                (index-next t s))))))
      (l* (t s size) "integer:sample-offset seq-state integer -> samples"
        (if (index-update t s)
          (let (output (sp-samples-new size))
            (seq-state-events-set! s
              (map
                (l (event-list)
                  (filter
                    (l (a)
                      (or (not (>= t (seq-event-start a)))
                        ((seq-event-f a) t s output a (- t (seq-event-start a)) (seq-event-state a))))
                    event-list))
                (seq-state-events s)))
            output)
          #f)))))
