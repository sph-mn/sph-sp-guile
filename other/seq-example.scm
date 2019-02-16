(import (sph) (sph list) (sph vector) (sph sp) (sph sp sequencer))

(define (sound-a time state event duration custom)
  "duration is time passed since the start of the event"
  (and (< duration 1000) (seq-output (* 0.5 (sp-sine-of-width time 400)) state)))

(define (sound-b time state event duration custom)
  (and (< duration 1000) (seq-output (* 0.25 (sp-sine-of-width time 100)) state)))

(define (sound-c time state event duration custom)
  (and (< duration 1000) (seq-output (* 0.125 (sp-sine-of-width time 25)) state)))

(define (events-f time end seq-state)
  "this returns a list of next event objects to register.
   events-f is called every (seq-state-duration seq-state) seconds.
   this is so that not all events have to be created at once but can be returned near the time they
   are starting"
  (list (seq-event sound-a 0) (seq-event sound-b 150) (seq-event sound-c 400)))

(let*
  ( (seq-state (seq-state-new events-f)) (sample-rate 1000)
    (results
      ; seq can return single samples or sample vectors for all channels.
      ; in this example it returns one sample per channel.
      (map-integers sample-rate (l (time) (seq time seq-state (l (results seq-state) results))))))
  (sp-plot-samples (sp-samples-from-list (map vector-first results))))

;-- same with sp-generate

(define (sample-f env time result seq-state . custom)
  "-> (#(number:sample-for-channel ...) list:generate-result seq-state any ...)
   this maps time to sample value in sp-generate.
   all parameters after time are for custom state values given to sp-generate"
  (seq time seq-state (l (result-data seq-state) (pairs result-data result seq-state custom))))

(define (segment-f env time segment result . custom)
  "-> #(vector:samples-for-channel ...)
   this maps time and samples to a new samples vector.
   the sample vector will have been processed by sample-f if sample-f was passed to sp-generate.
   the env object passed to segment-f/sample-f is (vector sample-rate sample-duration channel-count)"
  (pair (pair segment result) custom))

(define (run) "result-segments: (#(sample-vector:channel-samples ...) ...)"
  (let*
    ( (duration 1) (sample-rate 1000) (channel-count 1)
      (seq-state (seq-state-new events-f))
      (result-states
        (sp-generate sample-rate channel-count
          duration segment-f sample-f
          ; custom state arguments follow
          null seq-state))
      (result-segments (reverse (first result-states))))
    (sp-plot-segments result-segments "/tmp/sp-plot" 0)))
