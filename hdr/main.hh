#ifndef MAIN_HH
#define MAIN_HH
#include "proto.hh"

void debug_init();
void set_debug(bool);
void do_scores();
void add_score(Input_score * s);
void set_default_output(String s);
Input_score* current_iscore_l();
String find_file(String);
const char *get_version();
extern Source* source_l_g;
extern bool only_midi;

extern String default_out_fn;

#endif
