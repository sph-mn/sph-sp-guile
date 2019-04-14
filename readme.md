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
basics
~~~
f32vector-sum :: a [b c] ->
f64-nearly-equal? :: a b c ->
f64vector-sum :: a [b c] ->
sp-block-new :: channels size ->
sp-block-overlap :: a b [overlap-factor] ->
sp-call-with-output-file :: path channels sample-rate f ->
sp-convolution-filter! :: a b c d e ->
sp-convolve :: a b [carryover carryover-len] ->
sp-convolve! :: a b c d [e] ->
sp-fft :: a ->
sp-fft-resynth :: f a ->
sp-ffti :: a ->
sp-fftr :: a ->
sp-fftri :: a ->
sp-file-channel-count :: a ->
sp-file-close :: a ->
sp-file-fold :: f segment-size path custom ... ->
sp-file-fold-overlap :: f segment-size overlap-factor path custom ... ->
sp-file-input? :: a ->
sp-file-mode-read
sp-file-mode-read-write
sp-file-mode-write
sp-file-open :: a b [c d] ->
sp-file-position :: a ->
sp-file-position-set :: a b ->
sp-file-position? :: a ->
sp-file-read :: a b ->
sp-file-sample-rate :: a ->
sp-file-write :: a b c ->
sp-float-sum :: a ... ->
sp-fold-frames :: f input frame-size overlap-factor custom ... ->
sp-fold-integers :: count f init ... ->
sp-grain-map :: input count f state ->
sp-map-fold-integers :: count f custom ... ->
sp-moving-average! :: a b c d e [f g h] ->
sp-pi
sp-plot-samples :: a display-args ... ->
sp-plot-samples->file :: a path ->
sp-plot-samples-display-file :: file-path #:type #:color ->
sp-plot-spectrum :: a ->
sp-plot-spectrum->file :: a path ->
sp-plot-spectrum-display-file :: path ->
sp-sample-sum :: a ... ->
sp-samples->list :: v ->
sp-samples-absolute-max :: in [in-start in-count] ->
sp-samples-copy :: a ->
sp-samples-copy-zero :: a ->
sp-samples-copy-zero* :: a c ->
sp-samples-divide :: a divisor ->
sp-samples-each-index :: f a ->
sp-samples-extract :: input start count ->
sp-samples-extract-padded :: input start count ->
sp-samples-from-list :: elts ->
sp-samples-length :: v ->
sp-samples-map :: f a b ... ->
sp-samples-map! :: f a b ... ->
sp-samples-map-with :: f variable a ... ->
sp-samples-map-with! :: f variable a ... ->
sp-samples-map-with-index :: f a b ... ->
sp-samples-multiply :: a factor ->
sp-samples-new :: length [value] ->
sp-samples-passthrough :: out in [in-start in-count out-start] ->
sp-samples-ref :: v i ->
sp-samples-set! :: v i x ->
sp-samples-split :: b count ->
sp-samples-threshold :: a limit ->
sp-samples-zero! :: a [out-start count] ->
sp-samples? :: obj ->
sp-scheduler :: additions output-size state ->
sp-set-unity-gain :: out in in-start in-count out-start ->
sp-sinc :: a ->
sp-spectrum :: a ->
sp-window-hann :: offset size ->
sp-windowed-sinc-bp-br! :: a b c d e f g h [i] j ... ->
sp-windowed-sinc-bp-br-ir :: a b c d e ->
sp-windowed-sinc-lp-hp! :: a b c d e f ->
sp-windowed-sinc-lp-hp-ir :: a b c ->
~~~

## (sph sp synthesise)
sequencing and sound synthesis with composable sequencer objects
~~~
seq :: time offset size output events ->
seq-block-series :: time channels count events f custom #:block-size #:progress #:parallel ->
seq-block-series->file :: path time channels count events #:block-size #:sample-rate #:progress #:parallel ->
seq-block-series->list :: time channels count events #:block-size #:progress #:parallel ->
seq-event-data :: a ->
seq-event-data-end :: a ->
seq-event-data-f :: a ->
seq-event-data-start :: a ->
seq-event-end :: args ... ->
seq-event-group :: start end events ->
seq-event-group-map :: start end f events custom ... ->
seq-event-new :: start end f [state] ->
seq-event-start :: args ... ->
seq-event-state :: a ->
seq-event-state-update :: a state ->
seq-events-end :: a ->
seq-events-from-list :: a ->
seq-events-new :: a ... ->
seq-events-start :: args ... ->
seq-parallel :: time offset size output events ->
sp-block->file :: a path sample-rate [channels] ->
sp-blocks->file :: a path sample-rate [channels] ->
sp-cheap-noise-event :: start end amplitudes cutoff passes type #:q-factor #:noise #:resolution #:repeat-noise ->
sp-clip~ :: a ->
sp-events->block :: channels events [start-offset] ->
sp-noise-event :: start end amplitudes cut-l cut-h #:noise #:trn-l #:trn-h #:reject #:resolution #:repeat-noise ->
sp-noise-exponential~ :: [state] ->
sp-noise-normal~ :: [state] ->
sp-noise-uniform~ :: [state] ->
sp-path :: a #:dimension #:deep #:mapper #:randomise #:repeat #:reverse #:scale #:shift #:stretch ->
sp-path->procedure :: a ->
sp-phase :: y change phase-size ->
sp-precompiled-event :: channels events ->
sp-rectangle :: t width-a width-b min-value max-value ->
sp-rectangle~ :: t a b ->
sp-sawtooth~ :: t [wavelength] ->
sp-sine~ :: t [wavelength] ->
sp-square~ :: t [wavelength] ->
sp-triangle :: t a b height ->
sp-triangle~ :: t [a b] ->
sp-wave-event :: start end amplitudes wavelength #:phase #:generator #:phase-length ->
~~~

## (sph sp filter)
attenuating frequencies. sp-filter!, sp-cheap-filter! and more
~~~
sp-asymmetric-moving :: f current-value width state ->
sp-asymmetric-moving-average :: current-value width state ->
sp-asymmetric-moving-median :: current-value width state ->
sp-asymmetric-moving-out :: f current-value width state ->
sp-cheap-filter! :: type out in cutoff passes state #:q-factor #:in-start #:in-count #:out-start #:unity-gain ->
sp-filter! :: out in cutoff-l cutoff-h transition-l transition-h is-reject state ->
sp-moving-average :: in prev next radius [in-start in-count out-start] ->
sp-multipass! :: f out in passes state in-start in-count out-start ->
sp-multipass-fir! :: transfer-f out in passes state in-start in-count out-start ->
sp-one-pole-hp :: out in cutoff passes state in-start in-count out-start ->
sp-one-pole-lp :: out in cutoff passes state in-start in-count out-start ->
sp-state-variable-filter! :: type out in cutoff q-factor state in-start in-count out-start ->
sp-windowed-sinc-bp-br :: in cutoff-l cutoff-h transition-l transition-h is-reject state ->
sp-windowed-sinc-lp-hp :: in cutoff transition is-high-pass state ->
~~~

## (sph sp vectorise)
get sine and noise parameters from signals. needs update of the used fft bindings
~~~
sp-vectorise :: input #:duration ->
~~~
