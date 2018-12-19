#include "./helper.c"
#include "./foreign/sph/float.c"
SCM scm_sp_port_channel_count(SCM scm_a) { return ((scm_from_sp_channel_count(((scm_to_sp_port(scm_a))->channel_count)))); };
SCM scm_sp_port_sample_rate(SCM scm_a) { return ((scm_from_sp_sample_rate(((scm_to_sp_port(scm_a))->sample_rate)))); };
SCM scm_sp_port_position_p(SCM scm_a) { return ((scm_from_bool((sp_port_bit_position & (scm_to_sp_port(scm_a))->flags)))); };
SCM scm_sp_port_input_p(SCM scm_a) { return ((scm_from_bool((sp_port_bit_input & (scm_to_sp_port(scm_a))->flags)))); };
/** returns the current port position offset in number of samples */
SCM scm_sp_port_position(SCM scm_a) {
  sp_sample_count_t position;
  sp_port_position((scm_to_sp_port(scm_a)), (&position));
  return ((scm_from_sp_sample_count(position)));
};
SCM scm_sp_port_close(SCM a) {
  status_declare;
  status = sp_port_close((scm_to_sp_port(a)));
  scm_from_status_return(SCM_UNSPECIFIED);
};
SCM scm_sp_port_position_set(SCM scm_port, SCM scm_sample_offset) {
  status_declare;
  status = sp_port_position_set((scm_to_sp_port(scm_port)), (scm_to_size_t(scm_sample_offset)));
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
SCM scm_sp_windowed_sinc_bp_br_x(SCM scm_out, SCM scm_in, SCM scm_cutoff_l, SCM scm_cutoff_h, SCM scm_transition_l, SCM scm_transition_h, SCM scm_is_reject, SCM scm_state) {
  status_declare;
  sp_convolution_filter_state_t* state;
  boolean is_reject;
  is_reject = scm_is_true(scm_is_reject);
  state = (scm_is_true(scm_state) ? scm_to_sp_convolution_filter_state(scm_state) : 0);
  status_require((sp_windowed_sinc_bp_br((scm_to_sp_samples(scm_in)), (scm_to_sp_samples_length(scm_in)), (scm_to_sp_float(scm_cutoff_l)), (scm_to_sp_float(scm_cutoff_h)), (scm_to_sp_float(scm_transition_l)), (scm_to_sp_float(scm_transition_h)), is_reject, (&state), (scm_to_sp_samples(scm_out)))));
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
  memcpy(ir, (scm_to_sp_samples(scm_ir)), ir_len);
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
SCM scm_sp_moving_average_x(SCM scm_result, SCM scm_source, SCM scm_prev, SCM scm_next, SCM scm_radius, SCM scm_start, SCM scm_end) {
  status_declare;
  sp_sample_count_t source_len;
  sp_sample_count_t prev_len;
  sp_sample_count_t next_len;
  sp_sample_t* prev;
  sp_sample_t* next;
  sp_sample_count_t start;
  sp_sample_count_t end;
  source_len = scm_to_sp_samples_length(scm_source);
  start = ((!scm_is_undefined(scm_start) && scm_is_true(scm_start)) ? scm_to_sp_sample_count(scm_start) : 0);
  end = ((!scm_is_undefined(scm_end) && scm_is_true(scm_end)) ? scm_to_sp_sample_count(scm_end) : (source_len - 1));
  if (scm_is_true(scm_prev)) {
    prev = scm_to_sp_samples(scm_prev);
    prev_len = scm_to_sp_samples_length(scm_prev);
  } else {
    prev = 0;
    prev_len = 0;
  };
  if (scm_is_true(scm_next)) {
    next = scm_to_sp_samples(scm_next);
    next_len = scm_to_sp_samples_length(scm_next);
  } else {
    next = 0;
    next_len = 0;
  };
  status_require((sp_moving_average((scm_to_sp_samples(scm_source)), source_len, prev, prev_len, next, next_len, (scm_to_sp_sample_count(scm_radius)), start, end, (scm_to_sp_samples(scm_result)))));
exit:
  scm_from_status_return(SCM_UNSPECIFIED);
};
SCM scm_sp_fftr(SCM scm_input) {
  status_declare;
  sp_sample_count_t input_len;
  sp_sample_count_t output_len;
  SCM scm_output;
  input_len = scm_to_sp_samples_length(scm_input);
  output_len = (2 * sp_fftr_output_len(input_len));
  scm_output = scm_c_make_sp_samples(output_len);
  status_require((sp_fftr((scm_to_sp_samples(scm_input)), input_len, (scm_to_sp_samples(scm_output)))));
exit:
  scm_from_status_return(scm_output);
};
SCM scm_sp_fftri(SCM scm_input) {
  status_declare;
  sp_sample_count_t output_len;
  SCM scm_output;
  output_len = sp_fftri_output_len((scm_to_sp_samples_length(scm_input)));
  scm_output = scm_c_make_sp_samples(output_len);
  status_require((sp_fftri((scm_to_sp_samples(scm_input)), (scm_to_sp_samples_length(scm_input)), (scm_to_sp_samples(scm_output)))));
exit:
  scm_from_status_return(scm_output);
};
SCM scm_sp_alsa_open(SCM scm_device_name, SCM scm_mode, SCM scm_channel_count, SCM scm_sample_rate, SCM scm_latency) {
  status_declare;
  uint8_t* device_name;
  SCM scm_result;
  sp_port_t* port;
  scm_dynwind_begin(0);
  device_name = scm_to_locale_string(scm_device_name);
  scm_dynwind_free(device_name);
  port = scm_gc_malloc_pointerless((sizeof(sp_port_t)), "sp-port");
  status_require((sp_alsa_open(device_name, (scm_to_uint8(scm_mode)), (scm_to_sp_channel_count(scm_channel_count)), (scm_to_sp_sample_rate(scm_sample_rate)), (scm_to_sp_sample_count(scm_latency)), port)));
  scm_result = scm_from_sp_port(port);
exit:
  scm_from_status_dynwind_end_return(scm_result);
};
SCM scm_sp_file_open(SCM scm_path, SCM mode, SCM scm_channel_count, SCM scm_sample_rate) {
  status_declare;
  uint8_t* path;
  SCM scm_result;
  sp_port_t* port;
  scm_dynwind_begin(0);
  path = scm_to_locale_string(scm_path);
  scm_dynwind_free(path);
  port = scm_gc_malloc_pointerless((sizeof(sp_port_t)), "sp-port");
  status_require((sp_file_open(path, (scm_to_uint8(mode)), (scm_is_undefined(scm_channel_count) ? 0 : scm_to_sp_channel_count(scm_channel_count)), (scm_is_undefined(scm_sample_rate) ? 0 : scm_to_sp_sample_rate(scm_sample_rate)), port)));
  scm_result = scm_from_sp_port(port);
exit:
  scm_from_status_dynwind_end_return(scm_result);
};
SCM scm_f64vector_sum(SCM a, SCM start, SCM end) { return ((scm_from_double((f64_sum(((scm_is_undefined(start) ? 0 : scm_to_size_t(start)) + ((f64*)(SCM_BYTEVECTOR_CONTENTS(a)))), ((scm_is_undefined(end) ? (SCM_BYTEVECTOR_LENGTH(a) / sizeof(f64)) : (end - (1 + start))) * sizeof(f64))))))); };
SCM scm_f32vector_sum(SCM a, SCM start, SCM end) { return ((scm_from_double((f32_sum(((scm_is_undefined(start) ? 0 : scm_to_size_t(start)) + ((f32*)(SCM_BYTEVECTOR_CONTENTS(a)))), ((scm_is_undefined(end) ? (SCM_BYTEVECTOR_LENGTH(a) / sizeof(f32)) : (end - (1 + start))) * sizeof(f32))))))); };
SCM scm_f64_nearly_equal_p(SCM a, SCM b, SCM margin) { return ((scm_from_bool((f64_nearly_equal((scm_to_double(a)), (scm_to_double(b)), (scm_to_double(margin))))))); };
SCM scm_sp_port_read(SCM scm_port, SCM scm_sample_count) {
  status_declare;
  sp_channel_count_t channel_count;
  sp_sample_t** channel_data;
  sp_port_t* port;
  sp_sample_count_t result_sample_count;
  sp_sample_count_t sample_count;
  SCM scm_result;
  channel_data = 0;
  port = scm_to_sp_port(scm_port);
  sample_count = scm_to_sp_sample_count(scm_sample_count);
  channel_count = port->channel_count;
  status_require((sp_alloc_channel_array(channel_count, sample_count, (&channel_data))));
  status_require((sp_port_read(port, sample_count, channel_data, (&result_sample_count))));
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
SCM scm_sp_port_write(SCM scm_port, SCM scm_channel_data, SCM scm_sample_count) {
  status_declare;
  sp_sample_t** channel_data;
  sp_channel_count_t channel_count;
  sp_sample_count_t result_sample_count;
  sp_sample_count_t sample_count;
  SCM scm_result;
  channel_data = 0;
  sample_count = scm_to_sp_sample_count(scm_sample_count);
  status_require((scm_to_channel_data(scm_channel_data, (&channel_count), (&channel_data))));
  status_require((sp_port_write((scm_to_sp_port(scm_port)), channel_data, sample_count, (&result_sample_count))));
  scm_result = scm_from_sp_sample_count(result_sample_count);
  scm_remember_upto_here_1(scm_channel_data);
exit:
  if (channel_data) {
    free(channel_data);
  };
  scm_from_status_return(scm_result);
};
SCM scm_sp_window_blackman(SCM a, SCM width) { scm_from_sp_float((sp_window_blackman((scm_to_sp_float(a)), (scm_to_sp_sample_count(width))))); };
SCM scm_sp_sample_format() {
  if (sp_sample_format_f64 == sp_sample_format) {
    return ((scm_from_latin1_symbol("f64")));
  } else if (sp_sample_format_f32 == sp_sample_format) {
    return ((scm_from_latin1_symbol("f32")));
  } else if (sp_sample_format_int32 == sp_sample_format) {
    return ((scm_from_latin1_symbol("int32")));
  } else if (sp_sample_format_int16 == sp_sample_format) {
    return ((scm_from_latin1_symbol("int16")));
  } else if (sp_sample_format_int8 == sp_sample_format) {
    return ((scm_from_latin1_symbol("int8")));
  };
};
void scm_sp_convolution_filter_state_finalize(SCM a) { sp_convolution_filter_state_free((scm_to_sp_convolution_filter_state(a))); };
void sp_guile_init() {
  SCM type_slots;
  SCM scm_symbol_data;
  SCM m;
  m = scm_c_resolve_module("sph sp");
  scm_rnrs_raise = scm_c_public_ref("rnrs exceptions", "raise");
  scm_symbol_data = scm_from_latin1_symbol("data");
  type_slots = scm_list_1(scm_symbol_data);
  scm_type_port = scm_make_foreign_object_type((scm_from_latin1_symbol("sp-port")), type_slots, 0);
  scm_type_convolution_filter_state = scm_make_foreign_object_type((scm_from_latin1_symbol("sp-convolution-filter-state")), type_slots, scm_sp_convolution_filter_state_finalize);
  scm_c_module_define(m, "sp-sample-format", (scm_sp_sample_format(sp_sample_format)));
  scm_c_module_define(m, "sp-port-mode-read", (scm_from_uint8(sp_port_mode_read)));
  scm_c_module_define(m, "sp-port-mode-write", (scm_from_uint8(sp_port_mode_write)));
  scm_c_module_define(m, "sp-port-mode-read-write", (scm_from_uint8(sp_port_mode_read_write)));
  scm_c_define_procedure_c_init;
  scm_c_define_procedure_c("sp-convolve!", 4, 1, 0, scm_sp_convolve_x, ("out a b carryover [carryover-len] -> unspecified"));
  scm_c_define_procedure_c("sp-window-blackman", 2, 0, 0, scm_sp_window_blackman, ("real width -> real"));
  scm_c_define_procedure_c("sp-windowed-sinc-lp-hp!", 6, 0, 0, scm_sp_windowed_sinc_lp_hp_x, ("out in cutoff transition is-high-pass state -> state\n    samples samples real:0..0.5 real:0..0.5 boolean convolution-filter-state -> unspecified\n    apply a windowed-sinc low-pass or high-pass filter to \"in\", write to \"out\" and return\n    an updated state object.\n    if state object is false, create a new state.\n    cutoff and transition are as a fraction of the sampling-rate"));
  scm_c_define_procedure_c("sp-windowed-sinc-bp-br!", 8, 0, 0, scm_sp_windowed_sinc_bp_br_x, ("out in  cutoff-l cutoff-h transition-l transition-h is-reject state -> state\n    samples samples real:0..0.5 real real:0..0.5 real boolean convolution-filter-state -> unspecified\n  like sp-windowed-sinc-lp-hp! but as a band-pass or band-reject filter"));
  scm_c_define_procedure_c("sp-windowed-sinc-lp-hp-ir", 3, 0, 0, scm_sp_windowed_sinc_lp_hp_ir, ("real real boolean -> samples\n    cutoff transition is-high-pass -> ir\n    get an impulse response kernel for a low-pass or high-pass filter"));
  scm_c_define_procedure_c("sp-windowed-sinc-bp-br-ir", 5, 0, 0, scm_sp_windowed_sinc_bp_br_ir, ("real real real real boolean -> samples\n    cutoff-l cutoff-h transition-l transition-h is-reject -> ir\n    get an impulse response kernel for a band-pass or band-reject filter"));
  scm_c_define_procedure_c("sp-convolution-filter!", 5, 0, 0, scm_sp_convolution_filter_x, ("out in ir-f ir-f-arguments state -> state\n     samples samples procedure list sp-convolution-filter-state -> sp-convolution-filter-state"));
  scm_c_define_procedure_c("sp-moving-average!", 5, 2, 0, scm_sp_moving_average_x, ("result source previous next radius [start end] -> unspecified\n    sample-vector sample-vector sample-vector sample-vector integer integer integer [integer]"));
  scm_c_define_procedure_c("sp-fftr", 1, 0, 0, scm_sp_fftr, ("sample-vector:values-at-times -> sample-vector:frequencies\n    discrete fourier transform on the input data. only the real part"));
  scm_c_define_procedure_c("sp-fftri", 1, 0, 0, scm_sp_fftri, ("sample-vector:frequencies -> sample-vector:values-at-times\n    inverse discrete fourier transform on the input data. only the real part"));
  scm_c_define_procedure_c("sp-alsa-open", 5, 0, 0, scm_sp_alsa_open, ("device-name mode channel-count sample-rate latency -> sp-port"));
  scm_c_define_procedure_c("sp-file-open", 2, 2, 0, scm_sp_file_open, ("path mode [channel-count sample-rate] -> sp-port"));
  scm_c_define_procedure_c("sp-port-close", 1, 0, 0, scm_sp_port_close, ("sp-port -> boolean"));
  scm_c_define_procedure_c("sp-port-input?", 1, 0, 0, scm_sp_port_input_p, ("sp-port -> boolean"));
  scm_c_define_procedure_c("sp-port-position?", 1, 0, 0, scm_sp_port_position_p, ("sp-port -> boolean"));
  scm_c_define_procedure_c("sp-port-position", 1, 0, 0, scm_sp_port_position, ("sp-port -> integer"));
  scm_c_define_procedure_c("sp-port-channel-count", 1, 0, 0, scm_sp_port_channel_count, ("sp-port -> integer"));
  scm_c_define_procedure_c("sp-port-sample-rate", 1, 0, 0, scm_sp_port_sample_rate, ("sp-port -> integer"));
  scm_c_define_procedure_c("f32vector-sum", 1, 2, 0, scm_f32vector_sum, ("f32vector [start end] -> number"));
  scm_c_define_procedure_c("f64vector-sum", 1, 2, 0, scm_f64vector_sum, ("f64vector [start end] -> number"));
  scm_c_define_procedure_c("f64-nearly-equal?", 3, 0, 0, scm_f64_nearly_equal_p, ("a b margin -> boolean\n    number number number -> boolean"));
  scm_c_define_procedure_c("sp-port-read", 2, 0, 0, scm_sp_port_read, ("sp-port integer:sample-count -> (sample-vector ...):channel-data"));
  scm_c_define_procedure_c("sp-port-write", 3, 0, 0, scm_sp_port_write, ("sp-port (sample-vector ...):channel-data [integer:sample-count] -> unspecified\n  write sample data to the channels of port"));
  scm_c_define_procedure_c("sp-port-position-set", 2, 0, 0, scm_sp_port_position_set, ("sp-port integer:sample-offset -> boolean\n    sample-offset can be negative, in which case it is from the end of the port"));
};