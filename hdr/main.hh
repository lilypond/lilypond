#ifndef MAIN_HH
#define MAIN_HH
#include "proto.hh"

void debug_init();
void set_debug(bool);
void do_scores();
void add_score(Input_score * s);
void set_default_output(String s);
const char *get_version();

#endif
