/*
  scm-option.cc -- implement option setting from Scheme

  source file of the GNU LilyPond music typesetter

  (c) 2001--2005  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "program-option.hh"

#include <cstdio>
#include <string.h>

#include "string-convert.hh"
#include "protected-scm.hh"
#include "parse-scm.hh"
#include "warn.hh"
#include "main.hh"


/* Write midi as formatted ascii stream? */
bool do_midi_debugging_global;

/*
  Backwards compatibility.
*/
bool lily_1_8_relative = false;
bool lily_1_8_compatibility_used = false;

/*
  crash if internally the wrong type is used for a grob property.
*/
bool do_internal_type_checking_global;


static Lilypond_option_init options[] = {
  {"point-and-click", "#t",
   "use point & click"},
  {"midi-debug", "#f",
   "generate human readable MIDI",},
  {"internal-type-checking", "#f",
   "check every property assignment for types"},
  {"parse-protect", "#t",
   "continue when finding errors in inline\n" 
   "scheme are caught in the parser. If off, halt \n"
   "on errors, and print a stack trace."},
  {"old-relative", "#f",
   "relative for simultaneous music works\n"
   "similar to chord syntax"},
  {"resolution", "90",
   "resolution for generating bitmaps"},
  {"preview-include-book-title", "#t",
   "include book-titles in preview images."},
  {"gs-font-load", "#f",
   "load fonts via Ghostscript."},
  {"delete-intermediate-files", "#f",
   "delete unusable PostScript files"},
  {"verbose", "#f", "value for the --verbose flag"},
  {"ttf-verbosity", "0",
   "how much verbosity for TTF font embedding?"},
  {"debug-gc", "#f",
   "dump GC protection info"}, 
  {0,0,0},
};

Protected_scm option_hash_;

void internal_set_option (SCM var, SCM val)
{
  scm_hashq_set_x (option_hash_, var, val);
  
  if (var == ly_symbol2scm ("midi-debug"))
    {
      do_midi_debugging_global = to_boolean (val);
      val = scm_from_bool (to_boolean (val));
    }
  else if (var == ly_symbol2scm ("point-and-click"))
    {
      point_and_click_global = to_boolean (val);
      val = scm_from_bool (to_boolean (val));
    }
  else if (var == ly_symbol2scm ("parse-protect"))
    {
      parse_protect_global = to_boolean (val);
      val = scm_from_bool (to_boolean (val));
    }
  else if (var == ly_symbol2scm ("internal-type-checking"))
    {
      do_internal_type_checking_global = to_boolean (val);
      val = scm_from_bool (to_boolean (val));
    }
  else if (var == ly_symbol2scm ("old-relative"))
    {
      lily_1_8_relative = to_boolean (val);
      /*  Needs to be reset for each file that uses this option.  */
      lily_1_8_compatibility_used = to_boolean (val);
      val = scm_from_bool (to_boolean (val));
    }
}

const int HELP_INDENT = 30; 
const int INDENT = 2; 
const int SEPARATION = 5; 

static String
get_help_string ()
{
  String help ("Options supported by ly:set-option\n\n");
  for (Lilypond_option_init *p = options; p->name_; p ++)
    {
      String opt_spec =
	String_convert::char_string (' ', INDENT)
	+ String (p->name_)
	+ " ("
	+ String (p->init_)
	+ ")";
	

      if (opt_spec.length () + SEPARATION > HELP_INDENT)
	{
	  opt_spec += "\n"
	    + String_convert::char_string (' ', HELP_INDENT);
	}
      else
	opt_spec += String_convert::char_string (' ', HELP_INDENT - opt_spec.length ());
      
      String opt_help = p->descr_;
      opt_help.substitute (String ("\n"),
			   String ("\n")
			   + String_convert::char_string (' ', HELP_INDENT));
      
      help += opt_spec + opt_help + "\n";
    }
  
  help += String ("\n");
  return help;
}

static void
init_program_options ()
{
  option_hash_ = scm_c_make_hash_table (11);

  for (Lilypond_option_init *p = options; p->name_; p ++)
    {
      SCM sym = ly_symbol2scm (p->name_);
      SCM val = scm_c_eval_string (p->init_);

      internal_set_option (sym, val);
    }

  String help = get_help_string ();



  internal_set_option (ly_symbol2scm ("help"),
		       scm_makfrom0str (help.to_str0 ()));
}

ADD_SCM_INIT_FUNC(scm_option, init_program_options);


/*
  This interface to option setting is meant for setting options are
  useful to a limited audience. The reason for this interface is that
  making command line options clutters up the command-line option name
  space.

*/

Protected_scm command_line_settings = SCM_EOL;

LY_DEFINE (ly_option_usage, "ly:option-usage", 0, 0, 0, (),
	   "Print ly:set-option usage")
{
  SCM scm_stdout = scm_current_output_port();
  scm_display (ly_get_option (ly_symbol2scm ("help")), scm_stdout);
  exit (0);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_set_option, "ly:set-option", 1, 1, 0, (SCM var, SCM val),
	   "Set a program option. Try setting 'help for a help string.")
{
  SCM_ASSERT_TYPE (scm_is_symbol (var), var, SCM_ARG1,
		   __FUNCTION__,  "symbol");

  if (ly_symbol2scm ("help") == var)
    {
      ly_option_usage ();
    }

  if (val == SCM_UNDEFINED)
    val = SCM_BOOL_T;

  String  varstr = ly_scm2string (scm_symbol_to_string (var));
  if (varstr.left_string (3) == String ("no-"))
    {
      var = ly_symbol2scm (varstr.nomid_string (0, 3).to_str0 ());
      val = scm_from_bool (!to_boolean (val));
    }
  
  SCM handle = scm_hashq_get_handle (option_hash_, var);
  if (handle == SCM_BOOL_F)
    {
      warning (_f ("no such internal option: %s", varstr.to_str0 ()));
    }

  internal_set_option (var, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_get_option, "ly:get-option", 1, 0, 0, (SCM var),
	   "Get a global option setting.")
{
  SCM_ASSERT_TYPE (scm_is_symbol (var), var,
		   SCM_ARG1, __FUNCTION__,  "symbol");
  return scm_hashq_ref (option_hash_, var, SCM_BOOL_F);
}  
