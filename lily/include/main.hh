/*
  main.hh -- declare global entry points

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MAIN_HH
#define MAIN_HH

#include "lily-proto.hh"
#include "array.hh"

void debug_init ();
void set_debug (bool);
void do_scores ();
void clear_scores ();
void add_score (Score* s);
void set_default_output (String s);
String find_file (String);
void call_constructors ();

Array<String> get_inclusion_names ();
void set_inclusion_names (Array<String>);


extern String init_name_global;

/* FIXME: in warn.cc */
void progress_indication (String);

/* options */
extern Array<String> dump_header_fieldnames_global;
extern bool no_paper_global_b;
extern String output_format_global;
extern String output_name_global;
extern bool safe_global_b;
extern bool verbose_global_b;
extern bool store_locations_global_b;
extern bool internal_type_checking_global_b;


/* misc */
extern Array<String> failed_files;
extern int exit_status_global;
extern File_path global_path;

#endif /* MAIN_HH */
