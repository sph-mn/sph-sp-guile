(import (sph sp) (sph))
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

;-- sp-generate helper
(let*
  ( (duration-seconds 2)
    (result-states
      (sp-generate sample-rate channel-count duration-seconds
        ; segment-f - maps segments with samples eventually set by sample-f
        (lambda (env time segment result . states)
          (cons (cons segment result) states))
        ; sample-f - sets samples in segments for time
        (lambda (env time . states)
          (cons (* 0.5 (sp-sine~ time 100)) states))
        ; all following arguments are passed to segment-f/sample-f in "states"
        (list)))
    ; (#(f64vector:channel-samples ...) ...)
    (result-segments (reverse (car result-states))))
  (sp-segments->alsa result-segments sample-rate))
