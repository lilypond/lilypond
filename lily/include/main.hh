/*
  main.hh -- declare global entry points

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#ifndef MAIN_HH
#define MAIN_HH

#include "lily-proto.hh"
#include "array.hh"

void debug_init ();
void set_debug (bool);
void do_scores ();
void clear_scores ();
void add_score (Score *s);
void set_default_output (String s);
String find_file (String);
void call_constructors ();
Array<String> get_inclusion_names ();
void set_inclusion_names (Array<String>);

extern String init_name_global;

/* options */
extern Array<String> dump_header_fieldnames_global;
extern String output_backend_global;
extern String output_name_global;
extern bool be_safe_global;
extern bool be_verbose_global;
extern bool do_internal_type_checking_global;
extern bool is_pango_format_global;
extern bool is_TeX_format_global;
extern bool point_and_click_global;
extern String prefix_directory;
extern bool use_object_keys;

/*
  todo: collect in Output_option struct?
*/
extern String output_format_global;

extern bool make_preview;
extern bool make_print;

/* misc */
extern Array<String> failed_files;
extern int exit_status_global;
extern File_path global_path;
extern const char *LILYPOND_DATADIR;


/*
  Debugging options: switch on

  Cannot switch on -DNDEBUG, because it causes weird errors if you mix
  DNDEBUG and normal builds.
*/

#define DEBUG_SLUR_SCORING 1

#endif /* MAIN_HH */
