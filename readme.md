basic guile scheme sound synthesis and processing toolset

* create sample arrays and write them to files or alsa sound output using a generic port object
* various exemplary utilities, for example for convolution and filters

# features
* generic port object for reading or writing to files or alsa devices
* most other features of [sph-sp](https://github.com/sph-mn/sph-sp)
* generator function that maps time to samples
* sequencer for custom generator functions with designated shared state and unlimited multi-stage cross modulation
* path drawing with line, bezier curve, elliptical arc and gap segments for adsr envelopes and more
* the scheme code is purely functional

# dependencies
* run-time
  * [sph-sp](https://github.com/sph-mn/sph-sp)
  * [sph-lib](https://github.com/sph-mn/sph-lib)
  * libc
  * guile >= 2.2
  * linux or compatible
* quick build
  * gcc and shell for the provided compile script
* development build
  * [sph-sc](https://github.com/sph-mn/sph-sc)
  * clang-format

# setup
* install dependencies
* then in the project directory execute

```
./exe/compile-c
./exe/install
```

first argument to `exe/install` can be the destination path prefix, for example `./exe/install /tmp`.
there is also `exe/install-extended` which can symlink files

installed files
* /usr/lib/libguile-sph-sp.so
* /usr/share/guile/site/sph/*
* /usr/share/guile/site/test/sph/*

# usage
```scheme
(import (sph sp))

(define sample-rate 16000)
(define channel-count 1)

;-- basic io
(define latency 4096)
(define input-port? #f)
(define dac (sp-alsa-open "default" input-port? channel-count sample-rate latency))
(sp-port-write dac (list (f64vector 1 2 3 4)))
(sp-port-close dac)

(define file (sp-file-open "tmp/sp-file.wav" channel-count sample-rate))
(sp-port-write file (list (f64vector 1 2 3 4)))
(sp-port-close file)

;-- sp-generate

(define time-start 0)
(define duration-seconds 2)

(let*
  ( (result-states
      (sp-generate sample-rate time-start duration-seconds
        ; segment-f - maps segments with samples eventually set by sample-f
        (l (env time segment result . states)
          (pair (pair segment result) states))
        ; sample-f - sets samples in segments for time
        (l (env time . states)
          (pair (* 0.5 (sp-sine time)) states))
        ; all following arguments are passed to segment-f/sample-f in "states"
        (list)))
    (result-segments (reverse (first result-states))))
  (sp-segments->alsa result-segments))
```

sequencer usage example (tested 2018-10-04). three sines with different frequencies that start 0.5 seconds apart and last 0.5 seconds. this example assumes gnuplot is installed to display a plot of the result.

```scheme
(import (sph) (sph sp) (sph sp sequencer))

(define (sound-a time state event duration custom)
  "duration is time passed since the start of the event"
  (and (< duration 0.5) (seq-output (* 0.25 (sp-sine time 100)) state)))

(define (sound-b time state event duration custom)
  (and (< duration 0.5) (seq-output (* 0.5 (sp-sine time 200)) state)))

(define (sound-c time state event duration custom)
  (and (< duration 0.5) (seq-output (* 0.75 (sp-sine time 400)) state)))

(define (events-f time end seq-state)
  "this returns a list of next event objects to register.
   events-f is called every (seq-state-duration seq-state) seconds.
   this is so that not all events have to be created at once but can be returned near the time they
   are starting"
  (list (seq-event a sound-a 0) (seq-event b sound-b 0.5) (seq-event c sound-c 1)))

(define (sample-f env time result seq-state . custom)
  "-> (#(number:sample-for-channel ...) list:generate-result seq-state any ...)
   this maps time to sample value in sp-generate.
   all parameters after time are for custom state values given to sp-generate"
  (seq time seq-state (l (result-data seq-state)
      (pairs result-data result seq-state custom))))

(define (segment-f env time segment result . custom)
  "-> #(vector:samples-for-channel ...)
  this maps time and samples to a new samples vector.
  the sample vector will have been processed by sample-f if sample-f was passed to sp-generate.
  the env object passed to segment-f/sample-f is (vector sample-rate sample-duration channel-count)"
  (pair (pair segment result) custom))

(define (run) "result-segments: (#(sample-vector:channel-samples ...) ...)"
  (let*
    ( (duration 3) (sample-rate 8000) (seq-state (seq-state-new events-f))
      (result-states
        (sp-generate sample-rate 1
          0 duration segment-f sample-f
          ; custom state values
          null seq-state))
      (result-segments (reverse (first result-states))))
    ;(sp-segments->file result-segments "/tmp/sp-example.wav" sample-rate)
    (sp-segments->plot-render result-segments "/tmp/sp-plot" 0)))

(run)
```

sp-generate expects sample-f to return single sample numbers or vectors with one sample per channel

# modules
## (sph sp)
```
differences :: a ->
f32vector-sum :: f32vector [start end] -> number
f64-nearly-equal? :: a b c ->
f64vector-sum :: f64vector [start end] -> number
sp-alsa-open :: device-name mode channel-count sample-rate latency -> sp-port
sp-asymmetric-moving :: procedure real integer list -> (any:result-value . state)
sp-asymmetric-moving-average :: real integer list -> (result-value . state)
sp-asymmetric-moving-median :: real integer list -> (result-value . state)
sp-asymmetric-moving-out :: procedure real integer (real:previous-value ...) -> (any:result-value previous-value ...):state
sp-change-limiter :: real integer real list -> (real:result-value real ...):state
sp-clip~ :: a ->
sp-convolution-filter! :: a b c d e ->
sp-convolve :: a b [carryover carryover-len] ->
sp-convolve! :: out a b carryover [carryover-len] -> unspecified
sp-duration->sample-count :: seconds sample-rate ->
sp-factor->rads :: a sample-rate ->
sp-fft-resynth :: f a ->
sp-fftr :: sample-vector:values-at-times -> sample-vector:frequencies
sp-fftri :: sample-vector:frequencies -> sample-vector:values-at-times
sp-file-open :: path mode [channel-count sample-rate] -> sp-port
sp-filter-bank :: samples ((cutoff-l cutoff-h transition-l transition-h) ...) list -> ((samples ...) . state)
sp-float-sum :: a ... ->
sp-fold-file :: procedure integer string any ... -> any
sp-fold-file-overlap :: procedure integer real string any ... -> any
sp-fold-integers :: integer procedure any ... -> (any ...)
sp-generate :: integer integer procedure false/procedure any ... -> (any ...):states
sp-hz->rads :: real -> real:radians-per-second
sp-moving-average :: sample-vector false/sample-vector false/sample-vector integer [integer/false integer/false] -> sample-vector
sp-moving-average! :: result source previous next radius [start end] -> unspecified
sp-noise-band :: size center width state #:transition #:noise-f ->
sp-noise-exponential~ :: [state] ->
sp-noise-normal~ :: [state] ->
sp-noise-uniform~ :: [state] ->
sp-overlap :: false/samples false/samples [real] -> false/samples
sp-path :: number path-state [procedure -> result
sp-path-new :: sample-rate (symbol param ...) ...
sp-path-new-p :: number ((symbol:type any:parameter ...) ...) -> path-state
sp-phase :: number number number -> number
sp-phase-cycle :: integer integer false/previous-result -> (result _ ...):state
sp-phase-sine-width :: integer:sample-count integer:sample-count -> real
sp-pi
sp-plot-fftr :: a ->
sp-plot-fftr->file :: a path ->
sp-plot-fftr-display-file :: file-path #:type #:color ->
sp-plot-samples :: a display-args ... ->
sp-plot-samples->file :: a path ->
sp-plot-samples-display-file :: file-path #:type #:color ->
sp-plot-segments :: a path channel ->
sp-plot-segments->file :: (#(vector:channel ...) ...) string ->
sp-plot-spectrum :: a ->
sp-plot-spectrum->file :: a path ->
sp-plot-spectrum-display-file :: file-path #:type #:color ->
sp-port-channel-count :: sp-port -> integer
sp-port-close :: sp-port -> boolean
sp-port-input? :: sp-port -> boolean
sp-port-mode-read
sp-port-mode-read-write
sp-port-mode-write
sp-port-position :: sp-port -> integer
sp-port-position-set :: sp-port integer:sample-offset -> boolean
sp-port-position? :: sp-port -> boolean
sp-port-read :: sp-port integer:sample-count -> (sample-vector ...):channel-data
sp-port-sample-rate :: sp-port -> integer
sp-port-write :: sp-port (sample-vector ...):channel-data [integer:sample-count] -> unspecified
sp-rads->factor :: real integer -> real
sp-rads->hz :: real -> real:hertz
sp-rectangle~ :: integer:sample-count ... -> real:sample
sp-rectangular :: integer:sample-count ... -> real:sample
sp-sample-align :: procedure procedure integer integer any ... -> (result-value . (x width custom ...))
sp-sample-align-list :: size f update-f custom ... ->
sp-sample-count->duration :: sample-count sample-rate ->
sp-sample-format
sp-sample-sum :: a ... ->
sp-samples->list :: v ->
sp-samples-apply-blackman-window :: a ->
sp-samples-apply-hanning-window :: a ->
sp-samples-copy :: xvector -> xvector
sp-samples-copy-zero :: a ->
sp-samples-copy-zero* :: a c ->
sp-samples-divide :: a divisor ->
sp-samples-from-list :: elts ->
sp-samples-length :: v ->
sp-samples-map :: procedure:{any:element ... -> any} xvector ... -> xvector
sp-samples-map! :: procedure:{any:element ... -> any} xvector ... -> unspecified
sp-samples-map-with :: procedure:{any:variable any:element ... -> any} any:variable xvector -> xvector
sp-samples-map-with-index :: procedure:{index any:element ... -> any} xvector ... -> xvector
sp-samples-multiply :: a factor ->
sp-samples-new :: length [value] ->
sp-samples-ref :: v i ->
sp-samples-set! :: v i x ->
sp-samples? :: obj ->
sp-segment :: integer integer false/procedure:{index states ... -> number/vector} -> (#(vector:channel ...) . states)
sp-segments->alsa :: (#(vector:channel ...) ...) -> unspecified
sp-segments->file :: (#(#(sample ...):channel ...):segment ...) string -> unspecified
sp-sinc :: a ->
sp-sine-of-width :: x width ->
sp-sines~ :: number:radians number:radians-per-s ... -> real:0..1:sample
sp-sine~ :: real:radians:phase-offset real:radians-per-s -> real:sample
sp-spectral-inversion :: a ->
sp-spectral-reversal :: a ->
sp-spectrum :: samples -> #(real ...)
sp-triangle :: x a b height ->
sp-triangle~ :: integer:sample-count ... -> real:sample
sp-window-blackman :: real width -> real
sp-window-hanning :: offset size ->
sp-windowed-sinc-bp-br :: samples real real real boolean false/convolution-filter-state -> samples
sp-windowed-sinc-bp-br! :: a b c d e f g h ->
sp-windowed-sinc-bp-br-ir :: a b c d e ->
sp-windowed-sinc-lp-hp :: samples real real boolean false/convolution-filter-state -> samples
sp-windowed-sinc-lp-hp! :: a b c d e f ->
sp-windowed-sinc-lp-hp-ir :: a b c ->
```

## (sph sp sequencer)
```
seq :: integer list procedure:{results state -> any:seq-result} -> any:seq-result
seq-default-mixer :: output ->
seq-event :: name f optional ...
seq-event-f :: a ->
seq-event-groups :: a ->
seq-event-list->events :: a ->
seq-event-name :: a ->
seq-event-new :: procedure #:key integer (symbol ...) any -> vector
seq-event-start :: a ->
seq-event-update :: a #:f #:start #:name #:groups #:event-state ->
seq-events-merge :: events:target events:source -> events
seq-index-data :: a ->
seq-index-end :: a ->
seq-index-events :: a ->
seq-index-f :: a ->
seq-index-f-new :: number seq-state -> procedure
seq-index-i-f :: a ->
seq-index-i-f-new :: vector number number -> procedure
seq-index-i-next :: index time ->
seq-index-new :: data end events f i-f start ->
seq-index-next :: index time state ->
seq-index-start :: a ->
seq-index-update :: a #:data #:end #:events #:f #:i-f #:start ->
seq-output :: symbol integer/vector/any seq-state list list:alist -> list
seq-output-new :: name data event-state event ->
seq-state-add-events :: seq-state seq-event-list/seq-events ... -> state
seq-state-event-states :: a ->
seq-state-index :: a ->
seq-state-index-i :: a ->
seq-state-input :: a ->
seq-state-new :: procedure [#:event-f-list list #:user-value alist] -> seq-state
seq-state-options :: a ->
seq-state-output :: a ->
seq-state-update :: a #:user-value #:events-f #:event-states #:index #:index-i #:input #:mixer #:options #:output ->
seq-state-user-value :: a ->
```
