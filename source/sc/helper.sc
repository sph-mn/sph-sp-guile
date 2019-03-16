(pre-include "libguile.h" "sph-sp.h" "./foreign/sph/helper.c" "./foreign/sph/guile.c" "./config.c")

(pre-define
  status-group-sp-guile "sp-guile"
  (scm-from-sp-file pointer) (scm-make-foreign-object-1 scm-type-file pointer)
  (scm-from-sp-convolution-filter-state pointer)
  (scm-make-foreign-object-1 scm-type-convolution-filter-state pointer) (scm->sp-file a)
  (convert-type (scm-foreign-object-ref a 0) sp-file-t*) (scm->sp-convolution-filter-state a)
  (convert-type (scm-foreign-object-ref a 0) sp-convolution-filter-state-t*) (scm->sp-samples a)
  (convert-type (SCM-BYTEVECTOR-CONTENTS a) sp-sample-t*) (scm->sp-samples-length a)
  (sp-octets->samples (SCM-BYTEVECTOR-LENGTH a)) scm-samples?
  scm-is-bytevector (define-sp-sine! scm-id f)
  (begin
    "defines scm-sp-sine!, scm-sp-sine-lq!"
    (define (scm-id scm-data scm-len scm-sample-duration scm-freq scm-phase scm-amp)
      (SCM SCM SCM SCM SCM SCM SCM)
      (f
        (scm->sp-sample-count scm-len)
        (scm->sp-float scm-sample-duration)
        (scm->sp-float scm-freq)
        (scm->sp-float scm-phase) (scm->sp-float scm-amp) (scm->sp-samples scm-data))
      (return SCM-UNSPECIFIED)))
  ; error handling
  (scm-from-status-error a)
  (scm-c-error a.group (sp-guile-status-name a) (sp-guile-status-description a))
  (scm-c-error group name description)
  (scm-call-1
    scm-rnrs-raise
    (scm-list-4
      (scm-from-latin1-symbol group)
      (scm-from-latin1-symbol name)
      (scm-cons (scm-from-latin1-symbol "description") (scm-from-utf8-string description))
      (scm-cons (scm-from-latin1-symbol "c-routine") (scm-from-latin1-symbol __FUNCTION__))))
  (scm-from-status-return result)
  (return
    (if* status-is-success result
      (scm-from-status-error status)))
  (scm-from-status-dynwind-end-return result)
  (if status-is-success
    (begin
      (scm-dynwind-end)
      (return result))
    (return (scm-from-status-error status))))

(enum (sp-status-id-missing-argument))

(define (sp-guile-status-description a) (uint8-t* status-t)
  "get the description if available for a status"
  (declare b char*)
  (cond
    ( (not (strcmp status-group-sp-guile a.group))
      (case = a.id
        (sp-status-id-missing-argument (set b "missing argument"))
        (else (set b ""))))
    (else (set b (sp-status-description a))))
  (return (convert-type b uint8-t*)))

(define (sp-guile-status-name a) (uint8-t* status-t)
  "get the name if available for a status"
  (declare b char*)
  (cond
    ( (not (strcmp status-group-sp-guile a.group))
      (case = a.id
        (sp-status-id-missing-argument (set b "missing-argument"))
        (else (set b "unknown"))))
    (else (set b (sp-status-name a))))
  (return (convert-type b uint8-t*)))

(declare
  scm-type-file SCM
  scm-type-convolution-filter-state SCM
  scm-rnrs-raise SCM)

(define (scm->channel-data a result-channel-count result-channel-data)
  (status-t SCM sp-channel-count-t* sp-sample-t***)
  "(samples ...):channels ...:block integer output -> status-t
  result is set to null if channel-data is empty"
  status-declare
  (declare
    channel-data sp-sample-t**
    channel-count sp-channel-count-t
    i sp-channel-count-t)
  (set channel-count (scm->size-t (scm-length a)))
  (if (not channel-count)
    (begin
      (set
        *result-channel-data 0
        *result-channel-count 0)
      (goto exit)))
  (status-require (sph-helper-calloc (* channel-count (sizeof sp-sample-t*)) &channel-data))
  (for ((set i 0) (< i channel-count) (set i (+ 1 i)))
    (set
      (array-get channel-data i) (scm->sp-samples (scm-first a))
      a (scm-tail a)))
  (set
    *result-channel-data channel-data
    *result-channel-count channel-count)
  (label exit
    (return status)))

(define (scm-c-take-channel-data a channel-count sample-count)
  (SCM sp-sample-t** sp-channel-count-t sp-sample-count-t)
  "get a guile scheme object for channel data sample arrays. returns a list of sample-vectors.
  eventually frees given data arrays"
  (declare scm-result SCM)
  (set scm-result (scm_c_make_vector channel-count SCM-BOOL-F))
  (while channel-count
    (set channel-count (- channel-count 1))
    (scm-c-vector-set-x
      scm-result channel-count (scm-c-take-samples (array-get a channel-count) sample-count)))
  (free a)
  (return scm-result))