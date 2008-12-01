/*
  main.hh -- declare global entry points

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#ifndef MAIN_HH
#define MAIN_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "std-string.hh"

void debug_init ();
void set_debug (bool);
void do_scores ();
void clear_scores ();
void add_score (Score *s);
void set_default_output (string s);
string find_file (string);
void call_constructors ();
vector<string> get_inclusion_names ();
void set_inclusion_names (vector<string>);

extern string init_name_global;

/* options */
extern vector<string> dump_header_fieldnames_global;
extern vector<string> start_environment_global;
extern string output_backend_global;
extern string output_name_global;
extern bool be_safe_global;
extern bool be_verbose_global;
extern bool do_internal_type_checking_global;
extern bool is_pango_format_global;
extern bool is_TeX_format_global;
extern bool point_and_click_global;
extern string lilypond_datadir;
extern bool use_object_keys;
extern bool strict_infinity_checking;
extern string init_scheme_code_global;
extern string init_scheme_variables_global;

/*
  todo: collect in Output_option struct?
*/
extern string output_format_global;

/* misc */
extern vector<string> failed_files;
extern int exit_status_global;
extern File_path global_path;
extern const char *LILYPOND_DATADIR;


/*
  Debugging options: switch on

  Cannot switch on -DNDEBUG, because it causes weird errors if you mix
  DNDEBUG and normal builds.
*/

#define DEBUG_SLUR_SCORING 1
#define DEBUG_TIE_SCORING 1
#define DEBUG_BEAM_SCORING 1

#endif /* MAIN_HH */
