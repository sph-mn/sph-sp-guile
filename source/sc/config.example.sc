(pre-define
  (scm-from-sp-sample a) (scm-from-double a)
  (scm->sp-sample a) (scm->double a)
  (scm-c-make-sp-samples len) (scm-make-f64vector (scm-from-sp-sample-count len) (scm-from-uint8 0))
  (scm-c-take-samples a len) (scm-take-f64vector a len)
  (scm-from-sp-channel-count a) (scm-from-uint8 a)
  (scm-from-sp-sample-rate a) (scm-from-uint32 a)
  (scm-from-sp-sample-count a) (scm-from-size-t a)
  (scm-from-sp-float a) (scm-from-double a)
  (scm->sp-channel-count a) (scm->uint8 a)
  (scm->sp-sample-rate a) (scm->uint32 a)
  (scm->sp-sample-count a) (scm->size-t a)
  (scm->sp-float a) (scm->double a))