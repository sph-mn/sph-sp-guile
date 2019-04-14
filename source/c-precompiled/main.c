#include "./helper.c"
#include "./foreign/sph/float.c"
SCM scm_sp_file_channel_count(SCM scm_a) { return ((scm_from_sp_channel_count(((scm_to_sp_file(scm_a))->channel_count)))); };
SCM scm_sp_file_sample_rate(SCM scm_a) { return ((scm_from_sp_sample_rate(((scm_to_sp_file(scm_a))->sample_rate)))); };
SCM scm_sp_file_position_p(SCM scm_a) { return ((scm_from_bool((sp_file_bit_position & (scm_to_sp_file(scm_a))->flags)))); };
SCM scm_sp_file_input_p(SCM scm_a) { return ((scm_from_bool((sp_file_bit_input & (scm_to_sp_file(scm_a))->flags)))); };
/** returns the current file position offset in number of samples */
SCM scm_sp_file_position(SCM scm_a) {
  sp_sample_count_t position;
  sp_file_position((scm_to_sp_file(scm_a)), (&position));
  return ((scm_from_sp_sample_count(position)));
};
SCM scm_sp_file_close(SCM a) {
  status_declare;
  status = sp_file_close((scm_to_sp_file(a)));
  scm_from_status_return(SCM_UNSPECIFIED);
};
SCM scm_sp_file_position_set(SCM scm_file, SCM scm_sample_offset) {
  status_declare;
  status = sp_file_position_set((scm_to_sp_file(scm_file)), (scm_to_size_t(scm_sample_offset)));
  scm_from_status_return(SCM_UNSPECIFIED);
};
SCM scm_sp_convolve_x(SCM out, SCM a, SCM b, SCM carryover, SCM carryover_len) {
  sp_sample_count_t a_len;
  sp_sample_count_t b_len;
  sp_sample_count_t c_len;
  a_len = scm_to_sp_samples_length(a);
  b_len = scm_to_sp_samples_length(b);
  c_len = (scm_is_integer(carryover_len) ? scm_to_sp_sample_count(carryover_len) : scm_to_sp_samples_length(carryover));
  if (c_len < (b_len - 1)) {
    scm_c_error(status_group_sp_guile, "invalid-argument-size", ("carryover argument bytevector must be at least (- (length b) 1)"));
  };
  sp_convolve((scm_to_sp_samples(a)), a_len, (scm_to_sp_samples(b)), b_len, c_len, (scm_to_sp_samples(carryover)), (scm_to_sp_samples(out)));
  return (SCM_UNSPECIFIED);
};
SCM scm_sp_windowed_sinc_lp_hp_x(SCM scm_out, SCM scm_in, SCM scm_cutoff, SCM scm_transition, SCM scm_is_high_pass, SCM scm_state) {
  status_declare;
  sp_convolution_filter_state_t* state;
  boolean is_high_pass;
  is_high_pass = scm_is_true(scm_is_high_pass);
  state = (scm_is_true(scm_state) ? scm_to_sp_convolution_filter_state(scm_state) : 0);
  status_require((sp_windowed_sinc_lp_hp((scm_to_sp_samples(scm_in)), (scm_to_sp_samples_length(scm_in)), (scm_to_sp_float(scm_cutoff)), (scm_to_sp_float(scm_transition)), is_high_pass, (&state), (scm_to_sp_samples(scm_out)))));
  if (!scm_is_true(scm_state)) {
    scm_state = scm_from_sp_convolution_filter_state(state);
  };
exit:
  scm_from_status_return(scm_state);
};
/** uses a rest argument because c functions for guile are limited to 10 arguments */
SCM scm_sp_windowed_sinc_bp_br_x(SCM scm_out, SCM scm_in, SCM scm_cutoff_l, SCM scm_cutoff_h, SCM scm_transition_l, SCM scm_transition_h, SCM scm_is_reject, SCM scm_state, SCM scm_in_start, SCM scm_rest) {
  status_declare;
  sp_convolution_filter_state_t* state;
  boolean is_reject;
  sp_sample_count_t in_start;
  sp_sample_count_t in_count;
  sp_sample_count_t out_start;
  is_reject = scm_is_true(scm_is_reject);
  state = (scm_is_true(scm_state) ? scm_to_sp_convolution_filter_state(scm_state) : 0);
  in_start = (scm_is_undefined(scm_in_start) ? 0 : scm_to_sp_sample_count(scm_in_start));
  in_count = (scm_is_null(scm_rest) ? (scm_to_sp_samples_length(scm_in) - in_start) : scm_to_sp_sample_count((scm_first(scm_rest))));
  out_start = ((scm_is_null(scm_rest) || scm_is_null((scm_tail(scm_rest)))) ? 0 : scm_to_sp_sample_count((scm_first((scm_tail(scm_rest))))));
  status_require((sp_windowed_sinc_bp_br((in_start + scm_to_sp_samples(scm_in)), in_count, (scm_to_sp_float(scm_cutoff_l)), (scm_to_sp_float(scm_cutoff_h)), (scm_to_sp_float(scm_transition_l)), (scm_to_sp_float(scm_transition_h)), is_reject, (&state), (out_start + scm_to_sp_samples(scm_out)))));
  if (!scm_is_true(scm_state)) {
    scm_state = scm_from_sp_convolution_filter_state(state);
  };
exit:
  scm_from_status_return(scm_state);
};
SCM scm_sp_windowed_sinc_lp_hp_ir(SCM scm_cutoff, SCM scm_transition, SCM scm_is_high_pass) {
  sp_sample_t* ir;
  sp_sample_count_t ir_len;
  status_declare;
  status_require((sp_windowed_sinc_lp_hp_ir((scm_to_sp_float(scm_cutoff)), (scm_to_sp_float(scm_transition)), (scm_is_true(scm_is_high_pass)), (&ir), (&ir_len))));
exit:
  scm_from_status_return((scm_c_take_samples(ir, ir_len)));
};
SCM scm_sp_windowed_sinc_bp_br_ir(SCM scm_cutoff_l, SCM scm_cutoff_h, SCM scm_transition_l, SCM scm_transition_h, SCM scm_is_reject) {
  sp_sample_t* ir;
  sp_sample_count_t ir_len;
  status_declare;
  status_require((sp_windowed_sinc_bp_br_ir((scm_to_sp_float(scm_cutoff_l)), (scm_to_sp_float(scm_cutoff_h)), (scm_to_sp_float(scm_transition_l)), (scm_to_sp_float(scm_transition_h)), (scm_is_true(scm_is_reject)), (&ir), (&ir_len))));
exit:
  scm_from_status_return((scm_c_take_samples(ir, ir_len)));
};
/** an sp ir-f that takes a scm procedure and arguments it is to be called with as a list.
  the procedure should return a sample vector as a single argument */
status_t scm_c_sp_ir_f(void* arguments, sp_sample_t** out_ir, sp_sample_count_t* out_len) {
  status_declare;
  SCM scm_f;
  SCM scm_arguments;
  SCM scm_ir;
  sp_sample_count_t ir_len;
  sp_sample_t* ir;
  scm_f = *((SCM*)(arguments));
  scm_arguments = *(1 + ((SCM*)(arguments)));
  scm_ir = scm_apply_0(scm_f, scm_arguments);
  if (!scm_samples_p(scm_ir)) {
    status_set_both_goto(status_group_sp_guile, sp_status_id_undefined);
  };
  /* copy data as it will be owned by convolution-filter state */
  ir_len = scm_to_sp_samples_length(scm_ir);
  status_require((sph_helper_malloc((ir_len * sizeof(sp_sample_t)), (&ir))));
  memcpy(ir, (scm_to_sp_samples(scm_ir)), (ir_len * sizeof(sp_sample_t)));
  *out_ir = ir;
  *out_len = ir_len;
exit:
  return (status);
};
SCM scm_sp_convolution_filter_x(SCM scm_out, SCM scm_in, SCM scm_ir_f, SCM scm_ir_f_arguments, SCM scm_state) {
  status_declare;
  sp_convolution_filter_state_t* state;
  SCM ir_f_arguments[(2 * sizeof(SCM))];
  state = (scm_is_true(scm_state) ? scm_to_sp_convolution_filter_state(scm_state) : 0);
  ir_f_arguments[0] = scm_ir_f;
  ir_f_arguments[1] = scm_ir_f_arguments;
  status_require((sp_convolution_filter((scm_to_sp_samples(scm_in)), (scm_to_sp_samples_length(scm_in)), scm_c_sp_ir_f, ir_f_arguments, 2, (&state), (scm_to_sp_samples(scm_out)))));
  if (!scm_is_true(scm_state)) {
    scm_state = scm_from_sp_convolution_filter_state(state);
  };
exit:
  scm_from_status_return(scm_state);
};
/** start/end are indexes counted from 0 */
SCM scm_sp_moving_average_x(SCM scm_out, SCM scm_in, SCM scm_prev, SCM scm_next, SCM scm_radius, SCM scm_in_start, SCM scm_in_count, SCM scm_out_start) {
  status_declare;
  sp_sample_t* in;
  sp_sample_t* in_end;
  sp_sample_t* prev;
  sp_sample_t* prev_end;
  sp_sample_t* next;
  sp_sample_t* next_end;
  sp_sample_count_t in_start;
  sp_sample_count_t in_count;
  sp_sample_count_t out_start;
  in = scm_to_sp_samples(scm_in);
  in_end = (scm_to_sp_samples_length(scm_in) + in);
  in_start = (scm_is_undefined(scm_in_start) ? 0 : scm_to_sp_sample_count(scm_in_start));
  in_count = (scm_is_undefined(scm_in_count) ? (in_end - in - in_start) : scm_to_sp_sample_count(scm_in_count));
  out_start = (scm_is_undefined(scm_out_start) ? 0 : scm_to_sp_sample_count(scm_out_start));
  if (scm_is_true(scm_prev)) {
    prev = scm_to_sp_samples(scm_prev);
    prev_end = (scm_to_sp_samples_length(scm_prev) + prev);
  } else {
    prev = 0;
    prev_end = 0;
  };
  if (scm_is_true(scm_next)) {
    next = scm_to_sp_samples(scm_next);
    next_end = (scm_to_sp_samples_length(scm_next) + next);
  } else {
    next = 0;
    next_end = 0;
  };
  status_require((sp_moving_average(in, in_end, (in_start + in), ((in_start + in_count) + in), prev, prev_end, next, next_end, (scm_to_sp_sample_count(scm_radius)), (out_start + scm_to_sp_samples(scm_out)))));
exit:
  scm_from_status_return(SCM_UNSPECIFIED);
};
/** display a sample array in one line */
void debug_display_sample_array(sp_sample_t* a, sp_sample_count_t len) {
  sp_sample_count_t i;
  printf(("%.17g"), (a[0]));
  for (i = 1; (i < len); i = (1 + i)) {
    printf((" %.17g"), (a[i]));
  };
  printf("\n");
};
SCM scm_sp_fft(SCM scm_input) {
  status_declare;
  sp_sample_count_t i;
  sp_sample_count_t input_len;
  sp_sample_t* input_or_output_real;
  sp_sample_t* input_or_output_imag;
  SCM scm_output;
  scm_dynwind_begin(0);
  input_len = scm_c_vector_length(scm_input);
  status_require((sph_helper_malloc((input_len * sizeof(sp_sample_t)), (&input_or_output_real))));
  status_require((sph_helper_malloc((input_len * sizeof(sp_sample_t)), (&input_or_output_imag))));
  scm_dynwind_free(input_or_output_real);
  scm_dynwind_free(input_or_output_imag);
  for (i = 0; (i < input_len); i = (1 + i)) {
    input_or_output_real[i] = scm_to_sp_sample((scm_real_part((scm_c_vector_ref(scm_input, i)))));
    input_or_output_imag[i] = scm_to_sp_sample((scm_imag_part((scm_c_vector_ref(scm_input, i)))));
  };
  status_require((sp_fft(input_len, input_or_output_real, input_or_output_imag)));
  scm_output = scm_c_make_vector(input_len, SCM_BOOL_F);
  for (i = 0; (i < input_len); i = (1 + i)) {
    scm_c_vector_set_x(scm_output, i, (scm_c_make_rectangular((input_or_output_real[i]), (input_or_output_imag[i]))));
  };
exit:
  scm_from_status_dynwind_end_return(scm_output);
};
SCM scm_sp_ffti(SCM scm_input) {
  status_declare;
  sp_sample_count_t i;
  sp_sample_count_t input_len;
  sp_sample_t* input_or_output_real;
  sp_sample_t* input_or_output_imag;
  SCM scm_output;
  scm_dynwind_begin(0);
  input_len = scm_c_vector_length(scm_input);
  status_require((sph_helper_malloc((input_len * sizeof(sp_sample_t)), (&input_or_output_real))));
  status_require((sph_helper_malloc((input_len * sizeof(sp_sample_t)), (&input_or_output_imag))));
  scm_dynwind_free(input_or_output_real);
  scm_dynwind_free(input_or_output_imag);
  for (i = 0; (i < input_len); i = (1 + i)) {
    input_or_output_real[i] = scm_to_sp_sample((scm_real_part((scm_c_vector_ref(scm_input, i)))));
    input_or_output_imag[i] = scm_to_sp_sample((scm_imag_part((scm_c_vector_ref(scm_input, i)))));
  };
  status_require((sp_ffti(input_len, input_or_output_real, input_or_output_imag)));
  scm_output = scm_c_make_vector(input_len, SCM_BOOL_F);
  for (i = 0; (i < input_len); i = (1 + i)) {
    scm_c_vector_set_x(scm_output, i, (scm_c_make_rectangular((input_or_output_real[i]), (input_or_output_imag[i]))));
  };
exit:
  scm_from_status_dynwind_end_return(scm_output);
};
SCM scm_sp_file_open(SCM scm_path, SCM mode, SCM scm_channel_count, SCM scm_sample_rate) {
  status_declare;
  uint8_t* path;
  SCM scm_result;
  sp_file_t* file;
  scm_dynwind_begin(0);
  path = scm_to_locale_string(scm_path);
  scm_dynwind_free(path);
  file = scm_gc_malloc_pointerless((sizeof(sp_file_t)), "sp-file");
  status_require((sp_file_open(path, (scm_to_uint8(mode)), (scm_is_undefined(scm_channel_count) ? 0 : scm_to_sp_channel_count(scm_channel_count)), (scm_is_undefined(scm_sample_rate) ? 0 : scm_to_sp_sample_rate(scm_sample_rate)), file)));
  scm_result = scm_from_sp_file(file);
exit:
  scm_from_status_dynwind_end_return(scm_result);
};
SCM scm_f64vector_sum(SCM a, SCM start, SCM end) { return ((scm_from_double((f64_sum(((scm_is_undefined(start) ? 0 : scm_to_size_t(start)) + ((double*)(SCM_BYTEVECTOR_CONTENTS(a)))), ((scm_is_undefined(end) ? (SCM_BYTEVECTOR_LENGTH(a) / sizeof(double)) : (end - (1 + start))) * sizeof(double))))))); };
SCM scm_f32vector_sum(SCM a, SCM start, SCM end) { return ((scm_from_double((f32_sum(((scm_is_undefined(start) ? 0 : scm_to_size_t(start)) + ((float*)(SCM_BYTEVECTOR_CONTENTS(a)))), ((scm_is_undefined(end) ? (SCM_BYTEVECTOR_LENGTH(a) / sizeof(float)) : (end - (1 + start))) * sizeof(float))))))); };
SCM scm_f64_nearly_equal_p(SCM a, SCM b, SCM margin) { return ((scm_from_bool((f64_nearly_equal((scm_to_double(a)), (scm_to_double(b)), (scm_to_double(margin))))))); };
SCM scm_sp_file_read(SCM scm_file, SCM scm_sample_count) {
  status_declare;
  sp_channel_count_t channel_count;
  sp_sample_t** channel_data;
  sp_file_t* file;
  sp_sample_count_t result_sample_count;
  sp_sample_count_t sample_count;
  SCM scm_result;
  channel_data = 0;
  file = scm_to_sp_file(scm_file);
  sample_count = scm_to_sp_sample_count(scm_sample_count);
  channel_count = file->channel_count;
  status_require((sp_alloc_channel_array(channel_count, sample_count, (&channel_data))));
  status_require((sp_file_read(file, sample_count, channel_data, (&result_sample_count))));
  scm_result = scm_c_take_channel_data(channel_data, channel_count, sample_count);
exit:
  if (status_is_failure) {
    if (channel_data) {
      sp_channel_data_free(channel_data, channel_count);
    };
    if (sp_status_id_eof == status.id) {
      status.id = status_id_success;
      scm_result = SCM_EOF_VAL;
    };
  };
  scm_from_status_return(scm_result);
};
SCM scm_sp_file_write(SCM scm_file, SCM scm_channel_data, SCM scm_sample_count) {
  status_declare;
  sp_sample_t** channel_data;
  sp_channel_count_t channel_count;
  sp_sample_count_t result_sample_count;
  sp_sample_count_t sample_count;
  SCM scm_result;
  channel_data = 0;
  sample_count = scm_to_sp_sample_count(scm_sample_count);
  status_require((scm_to_channel_data(scm_channel_data, (&channel_count), (&channel_data))));
  status_require((sp_file_write((scm_to_sp_file(scm_file)), channel_data, sample_count, (&result_sample_count))));
  scm_result = scm_from_sp_sample_count(result_sample_count);
  scm_remember_upto_here_1(scm_channel_data);
exit:
  if (channel_data) {
    free(channel_data);
  };
  scm_from_status_return(scm_result);
};
SCM scm_sp_window_blackman(SCM a, SCM width) { scm_from_sp_float((sp_window_blackman((scm_to_sp_float(a)), (scm_to_sp_sample_count(width))))); };
void scm_sp_convolution_filter_state_finalize(SCM a) { sp_convolution_filter_state_free((scm_to_sp_convolution_filter_state(a))); };
void sp_guile_init() {
  SCM type_slots;
  SCM scm_symbol_data;
  SCM m;
  m = scm_c_resolve_module("sph sp");
  scm_rnrs_raise = scm_c_public_ref("rnrs exceptions", "raise");
  scm_symbol_data = scm_from_latin1_symbol("data");
  type_slots = scm_list_1(scm_symbol_data);
  scm_type_file = scm_make_foreign_object_type((scm_from_latin1_symbol("sp-file")), type_slots, 0);
  scm_type_convolution_filter_state = scm_make_foreign_object_type((scm_from_latin1_symbol("sp-convolution-filter-state")), type_slots, scm_sp_convolution_filter_state_finalize);
  scm_c_module_define(m, "sp-file-mode-read", (scm_from_uint8(sp_file_mode_read)));
  scm_c_module_define(m, "sp-file-mode-write", (scm_from_uint8(sp_file_mode_write)));
  scm_c_module_define(m, "sp-file-mode-read-write", (scm_from_uint8(sp_file_mode_read_write)));
  scm_c_define_procedure_c_init;
  scm_c_define_procedure_c("sp-convolve!", 4, 1, 0, scm_sp_convolve_x, ("out a b carryover [carryover-len] -> unspecified"));
  scm_c_define_procedure_c("sp-window-blackman", 2, 0, 0, scm_sp_window_blackman, ("real width -> real"));
  scm_c_define_procedure_c("sp-windowed-sinc-lp-hp!", 6, 0, 0, scm_sp_windowed_sinc_lp_hp_x, ("out in cutoff transition is-high-pass state -> state\n    samples samples real:0..0.5 real:0..0.5 boolean convolution-filter-state -> unspecified\n    apply a windowed-sinc low-pass or high-pass filter to \"in\", write to \"out\" and return\n    an updated state object.\n    if state object is false, create a new state.\n    cutoff and transition are as a fraction of the sampling-rate"));
  scm_c_define_procedure_c("sp-windowed-sinc-bp-br!", 8, 1, 1, scm_sp_windowed_sinc_bp_br_x, ("out in cutoff-l cutoff-h transition-l transition-h is-reject state in-start in-end out-start -> state\n    samples samples real:0..0.5 real real:0..0.5 real boolean convolution-filter-state -> unspecified\n    like sp-windowed-sinc-lp-hp! but as a band-pass or band-reject filter.\n    if state is false then a new state object will be returned.\n    optimised to become a low-pass or high-pass at the ends"));
  scm_c_define_procedure_c("sp-windowed-sinc-lp-hp-ir", 3, 0, 0, scm_sp_windowed_sinc_lp_hp_ir, ("real real boolean -> samples\n    cutoff transition is-high-pass -> ir\n    get an impulse response kernel for a low-pass or high-pass filter"));
  scm_c_define_procedure_c("sp-windowed-sinc-bp-br-ir", 5, 0, 0, scm_sp_windowed_sinc_bp_br_ir, ("real real real real boolean -> samples\n    cutoff-l cutoff-h transition-l transition-h is-reject -> ir\n    get an impulse response kernel for a band-pass or band-reject filter"));
  scm_c_define_procedure_c("sp-convolution-filter!", 5, 0, 0, scm_sp_convolution_filter_x, ("out in ir-f ir-f-arguments state -> state\n     samples samples procedure list false/sp-convolution-filter-state -> sp-convolution-filter-state\n     if state is false then a new state object will be returned"));
  scm_c_define_procedure_c("sp-moving-average!", 5, 3, 0, scm_sp_moving_average_x, ("out in previous next radius [in-start in-count out-start] -> unspecified\n     samples samples samples samples integer [integer integer integer] -> unspecified"));
  scm_c_define_procedure_c("sp-fft", 1, 0, 0, scm_sp_fft, ("#(complex ...) -> #(complex ...)"));
  scm_c_define_procedure_c("sp-ffti", 1, 0, 0, scm_sp_ffti, ("#(complex ...) -> #(complex ...)\n    inverse discrete fourier transform"));
  scm_c_define_procedure_c("sp-file-open", 2, 2, 0, scm_sp_file_open, ("path mode [channel-count sample-rate] -> sp-file"));
  scm_c_define_procedure_c("sp-file-close", 1, 0, 0, scm_sp_file_close, ("sp-file -> boolean"));
  scm_c_define_procedure_c("sp-file-input?", 1, 0, 0, scm_sp_file_input_p, ("sp-file -> boolean"));
  scm_c_define_procedure_c("sp-file-position?", 1, 0, 0, scm_sp_file_position_p, ("sp-file -> boolean"));
  scm_c_define_procedure_c("sp-file-position", 1, 0, 0, scm_sp_file_position, ("sp-file -> integer"));
  scm_c_define_procedure_c("sp-file-channel-count", 1, 0, 0, scm_sp_file_channel_count, ("sp-file -> integer"));
  scm_c_define_procedure_c("sp-file-sample-rate", 1, 0, 0, scm_sp_file_sample_rate, ("sp-file -> integer"));
  scm_c_define_procedure_c("f32vector-sum", 1, 2, 0, scm_f32vector_sum, ("f32vector [start end] -> number"));
  scm_c_define_procedure_c("f64vector-sum", 1, 2, 0, scm_f64vector_sum, ("f64vector [start end] -> number"));
  scm_c_define_procedure_c("f64-nearly-equal?", 3, 0, 0, scm_f64_nearly_equal_p, ("a b margin -> boolean\n    number number number -> boolean"));
  scm_c_define_procedure_c("sp-file-read", 2, 0, 0, scm_sp_file_read, ("sp-file integer:sample-count -> (sample-vector ...):channel-data"));
  scm_c_define_procedure_c("sp-file-write", 3, 0, 0, scm_sp_file_write, ("sp-file (sample-vector ...):channel-data [integer:sample-count] -> unspecified\n  write sample data to the channels of file"));
  scm_c_define_procedure_c("sp-file-position-set", 2, 0, 0, scm_sp_file_position_set, ("sp-file integer:sample-offset -> boolean\n    sample-offset can be negative, in which case it is from the end of the file"));
};