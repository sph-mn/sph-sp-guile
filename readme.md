experimental guile scheme sound synthesis and processing toolset

* read/write samples from/to audio files
* most features of [sph-sp](https://github.com/sph-mn/sph-sp)
  * filters and convolution that work on blocks of data series
  * fast fourier transform
  * file output
* experimental utilities for sequencing, synthesis and analysis

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
(define sample-rate 44100)
(define channel-count 1)

;-- basic io
(define file (sp-file-open "/tmp/sp-file.wav" sp-port-mode-write channel-count sample-rate))

; list with one sample vector per channel
(sp-port-write file (list (f64vector 1 2 3 4)) 4)
(sp-port-close file)
```

# modules
## (sph sp)
~~~
f32vector-sum :: f32vector [start end] -> number
f64-nearly-equal? :: a b c ->
f64vector-sum :: f64vector [start end] -> number
sp-asymmetric-moving :: procedure real integer list -> (any:result-value . state)
sp-asymmetric-moving-average :: real integer list -> (result-value . state)
sp-asymmetric-moving-median :: real integer list -> (result-value . state)
sp-asymmetric-moving-out :: procedure real integer (real:previous-value ...) -> (any:result-value previous-value ...):state
sp-block-new :: channels size ->
sp-block-overlap :: false/samples false/samples [real] -> false/samples
sp-call-with-output-file :: path channels sample-rate f ->
sp-change-limiter :: real integer real list -> (real:result-value real ...):state
sp-convolution-filter! :: a b c d e ->
sp-convolve :: a b [carryover carryover-len] ->
sp-convolve! :: out a b carryover [carryover-len] -> unspecified
sp-fft :: #(complex ...) -> #(complex ...)
sp-fft-resynth :: samples procedure:{#(complex ...) -> #(complex ...)} -> samples
sp-ffti :: #(complex ...) -> #(complex ...)
sp-fftr :: samples -> #(complex ...)
sp-fftri :: #(complex ...) -> samples
sp-file-channel-count :: sp-file -> integer
sp-file-close :: sp-file -> boolean
sp-file-fold :: procedure integer string any ... -> any
sp-file-fold-overlap :: procedure integer real string any ... -> any
sp-file-input? :: sp-file -> boolean
sp-file-mode-read
sp-file-mode-read-write
sp-file-mode-write
sp-file-open :: path mode [channel-count sample-rate] -> sp-file
sp-file-position :: sp-file -> integer
sp-file-position-set :: sp-file integer:sample-offset -> boolean
sp-file-position? :: sp-file -> boolean
sp-file-read :: sp-file integer:sample-count -> (sample-vector ...):channel-data
sp-file-sample-rate :: sp-file -> integer
sp-file-write :: sp-file (sample-vector ...):channel-data [integer:sample-count] -> unspecified
sp-filter-bank :: samples ((cutoff-l cutoff-h transition-l transition-h) ...) list -> ((samples ...) . state)
sp-float-sum :: a ... ->
sp-fold-frames :: procedure samples integer real:0..1 any ... -> (any ...):custom
sp-fold-integers :: integer procedure any ... -> (any ...)
sp-grain-map :: samples/integer integer procedure false/state -> (false/samples:output . state)
sp-map-fold-integers :: count f custom ... ->
sp-moving-average :: samples false/samples false/samples integer [integer/false integer/false] -> samples
sp-moving-average! :: result source previous next radius [start end] -> unspecified
sp-pi
sp-plot-samples :: samples [#:type #:color] -> unspecified
sp-plot-samples->file :: a path ->
sp-plot-samples-display-file :: string #:type symbol:lines/points #:color string -> unspecified
sp-plot-spectrum :: a ->
sp-plot-spectrum->file :: a path ->
sp-plot-spectrum-display-file :: path ->
sp-sample-sum :: a ... ->
sp-samples->list :: v ->
sp-samples-copy :: xvector -> xvector
sp-samples-copy-zero :: a ->
sp-samples-copy-zero* :: a c ->
sp-samples-divide :: a divisor ->
sp-samples-each-index :: procedure:{integer integer:a-length -> unspecified} xvector -> unspecified
sp-samples-extract :: integer integer samples -> samples
sp-samples-extract-padded :: samples integer integer -> samples
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
sp-samples-split :: samples integer -> (samples ...)
sp-samples-threshold :: a limit ->
sp-samples? :: obj ->
sp-scheduler :: additions integer state/false -> (output:samples/false state)
sp-sinc :: a ->
sp-spectrum :: samples -> #(real ...)
sp-window-hann :: offset size ->
sp-windowed-sinc-bp-br :: samples real real real boolean false/convolution-filter-state -> samples
sp-windowed-sinc-bp-br! :: a b c d e f g h ->
sp-windowed-sinc-bp-br-ir :: a b c d e ->
sp-windowed-sinc-lp-hp :: samples real real boolean false/convolution-filter-state -> samples
sp-windowed-sinc-lp-hp! :: a b c d e f ->
sp-windowed-sinc-lp-hp-ir :: a b c ->
sph-sp-description
~~~

## (sph sp synthesise)
sequencing and sound synthesis with composable sequencer objects
~~~
seq :: integer integer integer (samples:channel ...) seq-events -> seq-events
seq-block-series :: integer integer integer seq-events procedure:{(samples:channel ...) seq-events custom ... -> (seq-events custom ...)} -> (seq-events custom ...)
seq-block-series->file :: string integer integer seq-events [#:block-size integer #:sample-rate integer] -> seq-events
seq-block-series->list :: -> (events block ...)
seq-event-data :: a ->
seq-event-data-end :: a ->
seq-event-data-f :: a ->
seq-event-data-start :: a ->
seq-event-group :: start end events ->
seq-event-new :: procedure integer [integer any] -> seq-event
seq-event-state :: a ->
seq-event-state-update :: a state ->
seq-events-new :: seq-event ... -> seq-events
seq-parallel :: integer integer integer (samples:channel ...) seq-events -> seq-events
sp-band-event :: integer integer (sp-path ...) sp-path sp-path #:noise procedure #:trn-l sp-path #:trn-h sp-path #:reject boolean -> event
sp-blocks->file :: ((samples:channel ...):block ...) string integer integer integer -> unspecified
sp-clip~ :: a ->
sp-noise-exponential~ :: [state] ->
sp-noise-normal~ :: [state] ->
sp-noise-uniform~ :: [state] ->
sp-phase :: number number number -> number
sp-sine~ :: integer integer -> sample
sp-square~ :: integer integer -> sample
sp-wave-event :: integer integer (partial-config ...) -> seq-event
sph-sp-synthesis-description
~~~

## (sph sp vectorise)
get sine and noise parameters for a sound. needs update
~~~
sp-vectorise :: samples [integer] -> (series-element ...)
~~~
