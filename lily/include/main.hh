#ifndef MAIN_HH
#define MAIN_HH
#include "lily-proto.hh"

void debug_init();
void set_debug(bool);
void do_scores();
void add_score(Score* s);
void set_default_output(String s);
Input_score* current_iscore_l();
String find_file(String);
String get_version_str();
extern Sources* source_l_g;
extern bool only_midi;
extern int exit_status_i_;

extern String default_out_fn;

#endif
