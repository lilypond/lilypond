/*
  main.hh -- declare global entry points

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MAIN_HH
#define MAIN_HH

#include "lily-proto.hh"

void debug_init ();
void set_debug (bool);
void do_scores ();
void clear_scores ();
void add_score (Score* s);
void set_default_output (String s);
String find_file (String);
void call_constructors ();
extern Array<String> get_inclusion_names ();
extern void set_inclusion_names (Array<String>);

/* options */
extern bool dependency_global_b;
extern String dependency_prefix_global;
extern Array<String> dump_header_fieldnames_global;
extern bool no_paper_global_b;
extern String output_format_global;
extern String output_name_global;
extern bool safe_global_b;
extern bool verbose_global_b;
extern bool store_locations_global_b;

/* misc */
extern All_font_metrics *all_fonts_global_p;
extern int exit_status_global;
extern File_path global_path;
extern int score_count_global;
extern Sources* source_global_l;

#include <iostream.h> /* gcc 3.0 */


#endif /* MAIN_HH */
