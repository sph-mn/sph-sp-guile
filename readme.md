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
(import (sph sp) (sph sp generate))

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
(import (sph sp generate))

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

sequencer usage example

```scheme
(import (sph sp) (sph sp generate) (sph sp generate sequencer))

(define (sound-a time state event duration custom)
  (and (< duration 1) (seq-output (sp-sine time 100) state)))

(define (sound-b time state event duration custom)
  (seq-output (* 0.5 (sp-sine time 200)) state (alist-q freq time)))

(define (sound-c time state event duration custom) (seq-output (* 0.5 (sp-sine time 400)) state))

(define (events-f time end seq-state)
  ; this returns a list of next event objects to register.
  ; events-f is called every (seq-state-duration seq-state) seconds
  ; this is so that not all events have to be created at once.
  (list
    (seq-event a sound-a 0)
    (seq-event b sound-b 1)
    (seq-event c sound-c 1.5)
    (seq-event c sound-b (+ time 1))
    (seq-event d sound-b (+ time 20))))

(define (sample-f env time gen-result seq-state . custom)
  ; this maps time to sample value in sp-generate
  (seq time seq-state (l (data seq-state) (pairs data gen-result seq-state custom))))

(define (segment-f env time segment gen-result . custom)
  ; this maps time and a sample array to a new sample array
  ; the sample array was first processed by sample-f and can be empty
  (pair (pair (list segment) gen-result) custom))

(define (run)
  (let*
    ( (seq-state (seq-state-new events-f))
      (result-states
        (sp-generate sample-rate 0 duration segment-f sample-f
          ; custom state values
          null seq-state))
      (result-segments (reverse (first result-states))))
    (sp-segments->alsa result-segments sample-rate "default" 4096)))
```

## modules
(sph sp)

```
f32vector-sum :: f32vector [start end] -> number
f64vector-sum :: f64vector [start end] -> number
float-nearly-equal? :: a b c ->
sp-alsa-open :: device-name input? channel-count sample-rate latency -> sp-port
sp-duration->sample-count :: seconds sample-rate ->
sp-file-open :: path channel-count sample-rate -> sp-port
sp-pi
sp-plot-render :: file-path ->
sp-port-channel-count :: sp-port -> integer
sp-port-close :: sp-port -> boolean
sp-port-input? :: sp-port -> boolean
sp-port-position :: sp-port -> integer/boolean
sp-port-position? :: sp-port -> boolean
sp-port-read :: sp-port integer:sample-count -> (f32vector ...):channel-data
sp-port-sample-rate :: sp-port -> integer/boolean
sp-port-set-position :: sp-port integer:sample-offset -> boolean
sp-port-write :: sp-port (f32vector ...):channel-data [integer:sample-count] -> boolean
sp-port? :: sp-port -> boolean
sp-sample-count->duration :: sample-count sample-rate ->
sp-segments->alsa :: ((vector ...) ...) ->
sp-segments->file :: ((vector ...) ...) string ->
sp-segments->plot :: ((vector ...) ...) string ->
sp-segments->plot-render :: a path ->
sp-sine! :: data len sample-duration freq phase amp -> unspecified
sp-sine-lq! :: data len sample-duration freq phase amp -> unspecified
```

(sph sp generate)
```
sp-clip :: a ->
sp-fold-integers :: start end f states ... ->
sp-generate :: integer number number procedure procedure any ... -> (any ...):states
sp-noise :: integer [{random-state -> real} random-state] -> f64vector
sp-path :: number path-state [procedure -> result]
sp-path-new :: sample-rate (symbol:segment-type param ...) ...
sp-path-new-p :: number ((symbol:type any:parameter ...) ...) -> path-state
sp-segment :: integer procedure -> (vector . states)
sp-sine :: time freq -> number
```

(sph sp generate sequencer)
```
seq :: integer list -> integer/vector:sample-data list:state
seq-default-mixer :: output ->
seq-event :: name f optional ...
seq-event-custom :: a ->
seq-event-f :: a ->
seq-event-groups :: a ->
seq-event-list->events :: a ->
seq-event-name :: a ->
seq-event-new :: procedure #:key integer (symbol ...) any -> vector
seq-event-start :: a ->
seq-event-update :: a #:f #:start #:name #:groups #:custom ->
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
seq-output-new :: name data custom event ->
seq-state-add-events :: seq-state seq-event-list/seq-events ... -> state
seq-state-custom :: a ->
seq-state-events-custom :: a ->
seq-state-index :: a ->
seq-state-index-i :: a ->
seq-state-input :: a ->
seq-state-new :: procedure [#:event-f-list list #:custom alist] -> seq-state
seq-state-options :: a ->
seq-state-output :: a ->
seq-state-update :: a #:custom #:events-f #:events-custom #:index #:index-i #:input #:mixer #:options #:output -> state
```
