basic guile scheme sound synthesis and processing toolset

* create sample arrays and write them to files or alsa sound output using a generic port object
* various exemplary utilities, for example for convolution and filters

# features
* generic port object for reading or writing to files or alsa devices
* most features of [sph-sp](https://github.com/sph-mn/sph-sp)
* helpers for sample vector processing

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
(define dac (sp-alsa-open "default" sp-port-mode-write channel-count sample-rate))
(sp-port-write dac (vector (f64vector 1 2 3 4)) 4)
(sp-port-close dac)

(define file (sp-file-open "/tmp/sp-file.wav" sp-port-mode-write channel-count sample-rate))

; vector with one sample vector per channel
(sp-port-write file (vector (f64vector 1 2 3 4)) 4)
(sp-port-close file)
```

# modules
## (sph sp)
```
differences :: a ->
f32vector-sum :: f32vector [start end] -> number
f64-nearly-equal? :: a b c ->
f64vector-sum :: f64vector [start end] -> number
sp-alsa-open :: device-name mode channel-count sample-rate [latency] -> sp-port
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
sp-fftr :: sample-vector:values-at-times -> #(complex ...):frequencies
sp-fftri :: #(complex ...):frequencies -> sample-vector:values-at-times
sp-file-open :: path mode [channel-count sample-rate] -> sp-port
sp-filter-bank :: samples ((cutoff-l cutoff-h transition-l transition-h) ...) list -> ((samples ...) . state)
sp-float-sum :: a ... ->
sp-fold-file :: procedure integer string any ... -> any
sp-fold-file-overlap :: procedure integer real string any ... -> any
sp-fold-frames :: procedure samples integer real:0..1 any ... -> (any ...):custom
sp-fold-integers :: integer procedure any ... -> (any ...)
sp-generate :: integer integer false/procedure false/procedure any ... -> (any ...):states
sp-grain-map :: samples/integer integer procedure false/state -> (false/samples:output . state)
sp-hz->rads :: real -> real:radians-per-second
sp-moving-average :: sample-vector false/sample-vector false/sample-vector integer [integer/false integer/false] -> sample-vector
sp-moving-average! :: result source previous next radius [start end] -> unspecified
sp-noise-band :: size center radius state #:transition #:noise-f #:is-reject ->
sp-noise-exponential~ :: [state] ->
sp-noise-normal~ :: [state] ->
sp-noise-uniform~ :: [state] ->
sp-overlap :: false/samples false/samples [real] -> false/samples
sp-phase :: number number number -> number
sp-phase-cycle :: integer integer false/previous-result -> (result _ ...):state
sp-phase-sine-width :: integer:sample-count integer:sample-count -> real
sp-pi
sp-plot-fft :: a ->
sp-plot-fft->file :: a path ->
sp-plot-fft-display-file :: path ->
sp-plot-samples :: samples [#:type #:color] -> unspecified
sp-plot-samples->file :: a path ->
sp-plot-samples-display-file :: string #:type symbol:lines/points #:color string -> unspecified
sp-plot-segments :: a path channel ->
sp-plot-segments->file :: (#(vector:channel ...) ...) string ->
sp-plot-spectrum :: a ->
sp-plot-spectrum->file :: a path ->
sp-plot-spectrum-display-file :: path ->
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
sp-samples-apply-hann-window :: a ->
sp-samples-copy :: xvector -> xvector
sp-samples-copy-zero :: a ->
sp-samples-copy-zero* :: a c ->
sp-samples-divide :: a divisor ->
sp-samples-extract :: integer integer samples -> samples
sp-samples-extract-padded :: samples integer integer -> samples
sp-samples-from-list :: elts ->
sp-samples-length :: v ->
sp-samples-list-add-offsets :: (samples ...) [integer] -> ((sample-offset samples) ...)
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
sp-window-hann :: offset size ->
sp-windowed-sinc-bp-br :: samples real real real boolean false/convolution-filter-state -> samples
sp-windowed-sinc-bp-br! :: a b c d e f g h ->
sp-windowed-sinc-bp-br-ir :: a b c d e ->
sp-windowed-sinc-lp-hp :: samples real real boolean false/convolution-filter-state -> samples
sp-windowed-sinc-lp-hp! :: a b c d e f ->
sp-windowed-sinc-lp-hp-ir :: a b c ->
```
