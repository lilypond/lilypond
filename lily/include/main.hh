/*
  main.hh -- declare global entry points

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#ifndef MAIN_HH
#define MAIN_HH
#include "lily-proto.hh"

void debug_init();
void set_debug (bool);
void do_scores();
void clear_scores();
void add_score (Score* s);
void set_default_output (String s);
String find_file (String);
String get_version_str();
String get_version_number_str();
void call_constructors ();
extern Sources* source_global_l;
extern bool no_paper_global_b;
extern bool no_timestamps_global_b;
extern int exit_status_i_;
extern bool experimental_features_global_b;
extern bool postscript_global_b;

extern String default_outname_base_global;
extern String default_outname_suffix_global;
extern int default_count_global;
#endif
