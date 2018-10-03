(library (sph sp)
  (export
    f32vector-sum
    f64-nearly-equal?
    f64vector-sum
    sp-alsa-open
    sp-duration->sample-count
    sp-fftr
    sp-fftri
    sp-file-open
    sp-moving-average
    sp-moving-average!
    sp-pi
    sp-plot-render
    sp-port-channel-count
    sp-port-close
    sp-port-input?
    sp-port-mode-read
    sp-port-mode-read-write
    sp-port-mode-write
    sp-port-position
    sp-port-position-set
    sp-port-position?
    sp-port-read
    sp-port-sample-rate
    sp-port-write
    sp-port?
    sp-sample-count->duration
    sp-sample-format
    sp-samples->list
    sp-samples-copy-zero
    sp-samples-copy-zero*
    sp-samples-from-list
    sp-samples-length
    sp-samples-new
    sp-samples?
    sp-segments->alsa
    sp-segments->file
    sp-segments->plot
    sp-segments->plot-render
    sp-sine!
    sp-sine-lq!
    sp-windowed-sinc!
    sp-windowed-sinc-state)
  (import
    (guile)
    (rnrs bytevectors)
    (sph)
    (sph process)
    (sph string)
    (sph uniform-vector))

  (load-extension "libguile-sph-sp" "sp_guile_init")
  (define sp-pi (* 4 (atan 1)))

  (define-syntax-rule (sp-samples-new-f uv-create uv-make)
    ; a procedure similar to vector-make except that the fill value can be a procedure used to set the elements
    (l (length value) (if (procedure? value) (uv-create length value) (uv-make length value))))

  (define sp-samples-new
    (case sp-sample-format
      ((f64) (sp-samples-new-f f64vector-create make-f64vector))
      ((f32) (sp-samples-new-f f32vector-create make-f32vector))
      ((int32) (sp-samples-new-f s32vector-create make-s32vector))
      ((int16) (sp-samples-new-f s16vector-create make-s16vector))
      ((int8) (sp-samples-new-f s8vector-create make-s8vector))))

  (define sp-samples?
    (case sp-sample-format
      ((f64) f64vector?)
      ((f32) f32vector?)
      ((int32) s32vector?)
      ((int16) s16vector?)
      ((int8) s8vector?)))

  (define sp-samples->list
    (case sp-sample-format
      ((f64) f64vector->list)
      ((f32) f32vector->list)
      ((int32) s32vector->list)
      ((int16) s16vector->list)
      ((int8) s8vector->list)))

  (define sp-samples-from-list
    (case sp-sample-format
      ((f64) list->f64vector)
      ((f32) list->f32vector)
      ((int32) list->s32vector)
      ((int16) list->s16vector)
      ((int8) list->s8vector)))

  (define sp-samples-copy-zero
    (case sp-sample-format
      ((f64) f64vector-copy-zero)
      ((f32) f32vector-copy-zero)
      ((int32) s32vector-copy-zero)
      ((int16) s16vector-copy-zero)
      ((int8) s8vector-copy-zero)))

  (define sp-samples-copy-zero*
    (case sp-sample-format
      ((f64) f64vector-copy-zero*)
      ((f32) f32vector-copy-zero*)
      ((int32) s32vector-copy-zero*)
      ((int16) s16vector-copy-zero*)
      ((int8) s8vector-copy-zero*)))

  (define sp-samples-length
    (case sp-sample-format
      ((f64) f64vector-length)
      ((f32) f32vector-length)
      ((int32) s32vector-length)
      ((int16) s16vector-length)
      ((int8) s8vector-length)))

  (define (sp-duration->sample-count seconds sample-rate) (* seconds sample-rate))
  (define (sp-sample-count->duration sample-count sample-rate) (/ sample-count sample-rate))

  (define (sp-segments->file a path sample-rate) "((vector ...) ...) string ->"
    (if (not (null? a))
      (let (out (sp-file-open path (length (first a)) sample-rate))
        (each (l (a) (sp-port-write out a)) a) (sp-port-close out))))

  (define* (sp-segments->alsa a sample-rate #:optional (device "default") (latency 4096))
    "((vector ...) ...) ->"
    (if (not (null? a))
      (let (out (sp-alsa-open device #f (length (first a)) sample-rate latency))
        (each (l (a) (sp-port-write out a)) a) (sp-port-close out))))

  (define (sp-segments->plot a path)
    "((vector ...) ...) string ->
     write gnuplot compatible sample data to file at path"
    (call-with-output-file path
      (l (file)
        (each
          (l (channel)
            (each
              (l (segment)
                (each (l (sample) (display sample file) (newline file)) (f64vector->list segment)))
              channel)
            (newline file))
          a))))

  (define (sp-plot-render file-path)
    (process-replace-p "gnuplot" "--persist"
      "-e" (string-append "set yrange [-1:1]; plot " (string-quote file-path) " with lines")))

  (define (sp-segments->plot-render a path) (sp-segments->plot a path) (sp-plot-render path))

  (define* (sp-moving-average source prev next distance #:optional start end)
    "sample-vector false/sample-vector false/sample-vector integer [integer/false integer/false] -> sample-vector"
    (sp-samples-copy-zero* source
      (l (a) (sp-moving-average! a source prev next distance start end)))))
