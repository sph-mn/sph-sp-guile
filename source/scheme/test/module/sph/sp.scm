(define-test-module (test module sph sp)
  (import
    (sph uniform-vector)
    (sph number)
    (sph string)
    (sph sp)
    (sph list)
    (rnrs bytevectors))

  (define test-env-file-path "/tmp/sp-test.wav")
  (define channel-count 2)
  (define volume 0.1)
  (define duration 5)
  (define sample-rate 8000)
  (define sample-duration (/ 1 sample-rate))
  (define segment-size (inexact->exact (/ sample-rate 10)))
  (define (adjust-volume volume data) (map (l (a) (f64vector-map* * volume a)) data))
  (define test-samples (sp-samples-new segment-size (l (index) (sin (+ 1 index)))))

  (define (get-sine-segment index sample-offset)
    (let (data (sp-sine segment-size sample-duration 1400 (* sample-offset sample-duration)))
      (list data data)))

  (define-test (sp-file) (if (file-exists? test-env-file-path) (delete-file test-env-file-path))
    (let*
      ( (file-out (sp-file-open test-env-file-path sp-port-mode-write channel-count sample-rate))
        (data
          ; make data for channels filled with samples of value n
          (list->vector
            (map-integers channel-count (l (n) (sp-samples-new segment-size (* (+ n 1) 0.5)))))))
      (assert-and
        (assert-equal "properties" (list #f 0)
          (list (sp-port-input? file-out) (sp-port-position file-out)))
        (assert-true "write"
          (= segment-size (sp-port-write file-out data segment-size) (sp-port-position file-out)))
        (assert-true "close" (and (sp-port-close file-out)))
        (let*
          ( (file-in (sp-file-open test-env-file-path sp-port-mode-read))
            (test-result (assert-true "read" (equal? data (sp-port-read file-in segment-size)))))
          (sp-port-close file-in) test-result))))

  (define-test (sp-alsa)
    (let (out (sp-alsa-open-output "default" channel-count sample-rate))
      (let loop ((index 0) (sample-offset 0))
        (if (< sample-offset (* duration sample-rate))
          (begin
            (sp-alsa-write out segment-size
              (adjust-volume volume (get-sine-segment index sample-offset)))
            (loop (+ 1 index) (+ segment-size sample-offset)))
          (sp-port-close out)))))

  (define-test (sp-fft) (sp-samples? (sp-fftri (sp-fftr test-samples))))

  (define-test (sp-moving-average in ex)
    (apply
      (l (source prev next distance . a)
        (let*
          ( (source (sp-samples-from-list source)) (prev (and prev (sp-samples-from-list prev)))
            (next (and next (sp-samples-from-list next)))
            (result (apply sp-moving-average source prev next distance a)))
          (if (every (l (a b) (f64-nearly-equal? a b 1.0e-6)) ex (sp-samples->list result)) ex
            result)))
      in))

  (define-test (sp-windowed-sinc in ex)
    (list-bind in (in cutoff transition)
      (let* ((in (sp-samples-from-list in)) (out (sp-samples-copy-zero in)))
        (sp-windowed-sinc-lp-hp! out in cutoff transition #f #f)
        (sp-windowed-sinc-lp-hp! out in cutoff transition #t #f)
        (sp-windowed-sinc-bp-br! out in cutoff cutoff transition transition #f #f)
        (sp-windowed-sinc-bp-br! out in cutoff cutoff transition transition #t #f)
        ; check of result data to be implemented
        (assert-and (sp-samples? (sp-windowed-sinc-lp-hp-ir 0.1 0.08 #f))
          (sp-samples? (sp-windowed-sinc-lp-hp-ir 0.1 0.08 #t))
          (sp-samples? (sp-windowed-sinc-bp-br-ir 0.1 0.1 0.08 0.08 #f))
          (sp-samples? (sp-windowed-sinc-bp-br-ir 0.1 0.1 0.08 0.08 #t)))
        #t)))

  (define-test (sp-convolve) "test convolve and its carryover functionality "
    (let*
      ( (ir-list (make-list 5 2.0)) (a-list (make-list 5 1.0)) (ir (sp-samples-from-list ir-list))
        (a (sp-samples-from-list a-list)) (b a)
        (c a)
        (result
          (fold-multiple
            (l (a state result)
              (let (b (sp-convolve a ir state)) (list (tail b) (pair (first b) result))))
            (list a b c) #f null)))
      (equal? (convolve (append a-list a-list a-list) ir-list)
        (apply append (map sp-samples->list (reverse (pair (first result) (second result))))))))

  (define-test (sp-convolution-filter)
    (let*
      ( (a-list (make-list 5 1.0)) (a (sp-samples-from-list a-list)) (b-list (make-list 5 2.0))
        (b (sp-samples-from-list b-list)) (out (sp-samples-new (- (length a-list) 1) 0.0))
        (state (sp-convolution-filter! out a (l a b) (list 0.1 0.08) #f))
        (state (sp-convolution-filter! out a (l a b) (list 0.1 0.08) state)))
      (sp-samples? out)))

  (define-test (sp-samples-extract)
    (let (series (sp-samples-from-list (map-integers 10 identity)))
      (and (equal? (list 8.0) (sp-samples->list (sp-samples-extract-padded series 8 1)))
        (equal? (list 8.0 9.0) (sp-samples->list (sp-samples-extract series 8 6)))
        (equal? (list 8.0 9.0 0.0 0.0 0.0 0.0)
          (sp-samples->list (sp-samples-extract-padded series 8 6))))))

  (define-test (sp-convolution-filter-2)
    "test for the case that the input is smaller than the impulse response"
    (define (test-convolve)
      (let*
        ( (samples (list 1 2 3 4 5)) (ir (sp-samples-from-list (list 2 4 8 16 32 64)))
          (convolve
            (l (samples state)
              (let*
                ( (samples (sp-samples-from-list (any->list samples)))
                  (output (sp-samples-copy-zero samples)))
                (list output (sp-convolution-filter! output samples (l a ir) null state)))))
          (result-a
            (reverse
              (first
                (fold-multiple
                  (l (sample result state)
                    (apply (l (sample state) (list (pair sample result) state))
                      (convolve sample state)))
                  samples null #f))))
          (result-b (first (convolve samples #f)))
          (result-a (sp-samples-from-list (apply append (map sp-samples->list result-a)))))
        (equal? (any->string result-b) (any->string result-a))))
    (define (test-filter)
      (let*
        ( (samples (list 1 2 3 4 5)) (ir (sp-samples-from-list (list 2 4 8 16 32 64)))
          (filter
            (l (samples state)
              (let (samples (sp-samples-from-list (any->list samples)))
                (sp-windowed-sinc-bp-br samples 0.1 0.4 0.08 0.08 #f state))))
          (result-a
            (reverse
              (first
                (fold-multiple
                  (l (sample result state)
                    (apply (l (sample state) (list (pair sample result) state))
                      (filter sample state)))
                  samples null #f))))
          (result-b (first (filter samples #f)))
          (result-a (sp-samples-from-list (apply append (map sp-samples->list result-a)))))
        (equal? (any->string result-b) (any->string result-a))))
    (assert-and (assert-true (test-convolve)) (assert-true (test-filter))))

  (test-execute-procedures-lambda (sp-convolution-filter-2) (sp-convolution-filter)
    (sp-convolve) (sp-windowed-sinc ((2 2 2 2) 0.1 0.08) #t)
    (sp-moving-average
      ; no prev/next
      ((2 2 2 2) #f #f 1) (1.3333333333333333 2.0 2.0 1.3333333333333333)
      ; no prev/next, bigger width
      ((1 2.5 4.0 1.5 -3.0) #f #f 2) (1.5 1.8 1.2 1.0 0.5)
      ; prev/next
      ((2 1 0 3) (8 4) (5 9) 4)
      (2.555555582046509 3.555555582046509 3.555555582046509 2.6666667461395264)
      ; no next but prev
      ((2 1 0 3) (8 4) #f 4) (2.0 2.0 2.0 1.1111111640930176)
      ; no prev but next
      ((2 1 0 3) #f (5 9) 4)
      (1.2222222089767456 2.222222328186035 2.222222328186035 2.222222328186035))
    sp-file sp-fft (sp-samples-extract)))
