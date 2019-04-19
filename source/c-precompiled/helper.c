#include <libguile.h>
#include <sph-sp.h>
#include "./foreign/sph/helper.c"
#include "./foreign/sph/guile.c"
#include "./config.c"
#define status_group_sp_guile "sp-guile"
#define scm_from_sp_file(pointer) scm_make_foreign_object_1(scm_type_file, pointer)
#define scm_from_sp_convolution_filter_state(pointer) scm_make_foreign_object_1(scm_type_convolution_filter_state, pointer)
#define scm_to_sp_file(a) ((sp_file_t*)(scm_foreign_object_ref(a, 0)))
#define scm_to_sp_convolution_filter_state(a) ((sp_convolution_filter_state_t*)(scm_foreign_object_ref(a, 0)))
/** gives a pointer to the memory region */
#define scm_to_sp_samples(a) ((sp_sample_t*)(SCM_BYTEVECTOR_CONTENTS(a)))
#define scm_to_sp_samples_length(a) sp_octets_to_samples((SCM_BYTEVECTOR_LENGTH(a)))
#define scm_to_sp_sample_counts(a) ((sp_sample_count_t*)(SCM_BYTEVECTOR_CONTENTS(a)))
#define scm_samples_p scm_is_bytevector
/** defines scm-sp-sine!, scm-sp-sine-lq! */
#define define_sp_sine_x(scm_id, f) \
  SCM scm_id(SCM scm_data, SCM scm_len, SCM scm_sample_duration, SCM scm_freq, SCM scm_phase, SCM scm_amp) { \
    f((scm_to_sp_sample_count(scm_len)), (scm_to_sp_float(scm_sample_duration)), (scm_to_sp_float(scm_freq)), (scm_to_sp_float(scm_phase)), (scm_to_sp_float(scm_amp)), (scm_to_sp_samples(scm_data))); \
    return (SCM_UNSPECIFIED); \
  }
#define scm_from_status_error(a) scm_c_error((a.group), (sp_guile_status_name(a)), (sp_guile_status_description(a)))
#define scm_c_error(group, name, description) scm_call_1(scm_rnrs_raise, (scm_list_4((scm_from_latin1_symbol(group)), (scm_from_latin1_symbol(name)), (scm_cons((scm_from_latin1_symbol("description")), (scm_from_utf8_string(description)))), (scm_cons((scm_from_latin1_symbol("c-routine")), (scm_from_latin1_symbol(__FUNCTION__)))))))
#define scm_from_status_return(result) return ((status_is_success ? result : scm_from_status_error(status)))
#define scm_from_status_dynwind_end_return(result) \
  if (status_is_success) { \
    scm_dynwind_end(); \
    return (result); \
  } else { \
    return ((scm_from_status_error(status))); \
  }
enum { sp_status_id_missing_argument };
/** get the description if available for a status */
uint8_t* sp_guile_status_description(status_t a) {
  char* b;
  if (!strcmp(status_group_sp_guile, (a.group))) {
    if (sp_status_id_missing_argument == a.id) {
      b = "missing argument";
    } else {
      b = "";
    };
  } else {
    b = sp_status_description(a);
  };
  return (((uint8_t*)(b)));
};
/** get the name if available for a status */
uint8_t* sp_guile_status_name(status_t a) {
  char* b;
  if (!strcmp(status_group_sp_guile, (a.group))) {
    if (sp_status_id_missing_argument == a.id) {
      b = "missing-argument";
    } else {
      b = "unknown";
    };
  } else {
    b = sp_status_name(a);
  };
  return (((uint8_t*)(b)));
};
SCM scm_type_file;
SCM scm_type_convolution_filter_state;
SCM scm_rnrs_raise;
/** (samples ...):channels ...:block integer output -> status-t
  result is set to null if channel-data is empty */
status_t scm_to_channel_data(SCM a, sp_channel_count_t* result_channel_count, sp_sample_t*** result_channel_data) {
  status_declare;
  sp_sample_t** channel_data;
  sp_channel_count_t channel_count;
  sp_channel_count_t i;
  channel_count = scm_to_size_t((scm_length(a)));
  if (!channel_count) {
    *result_channel_data = 0;
    *result_channel_count = 0;
    goto exit;
  };
  status_require((sph_helper_calloc((channel_count * sizeof(sp_sample_t*)), (&channel_data))));
  for (i = 0; (i < channel_count); i = (1 + i)) {
    channel_data[i] = scm_to_sp_samples((scm_first(a)));
    a = scm_tail(a);
  };
  *result_channel_data = channel_data;
  *result_channel_count = channel_count;
exit:
  return (status);
};
/** get a guile scheme object for channel data sample arrays. returns a list of sample-vectors.
  eventually frees given channel data.
  all sample vectors must have equal length */
SCM scm_c_take_channel_data(sp_sample_t** a, sp_channel_count_t channel_count, sp_sample_count_t sample_count) {
  SCM scm_result;
  scm_result = SCM_EOL;
  /* sample vectors prepended in reverse order */
  while (channel_count) {
    channel_count = (channel_count - 1);
    scm_result = scm_cons((scm_c_take_samples((a[channel_count]), sample_count)), scm_result);
  };
  free(a);
  return (scm_result);
};