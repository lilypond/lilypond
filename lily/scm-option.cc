/*
  scm-option.cc --  implement option setting from Scheme

  source file of the GNU LilyPond music typesetter

  (c) 2001--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "scm-option.hh"

#include <cstdio>

#include "parse-scm.hh"
#include "warn.hh"
#include "main.hh"

/*
  This interface to option setting is meant for setting options are
  useful to a limited audience. The reason for this interface is that
  making command line options clutters up the command-line option name
  space.


  preferably, also dont use TESTING_LEVEL_GLOBAL, since it defeats
  another purpose of this very versatile interface, which is to
  support multiple debug/testing options concurrently.

 */


/* Write midi as formatted ascii stream? */
bool midi_debug_global_b;

int preview_resolution_global = 90;


/* General purpose testing flag */
int testing_level_global;

/*
  Backwards compatibility.
 */
bool lily_1_8_relative = false;
bool lily_1_8_compatibility_used = false;

/*
  crash if internally the wrong type is used for a grob property.
 */
bool do_internal_type_checking_global;


/*
  What is this function for ? 
 */
LY_DEFINE (ly_option_usage, "ly:option-usage", 0, 0, 0, (SCM),
		  "Print ly:set-option usage")
{
  printf ( _("lilypond -e EXPR means:").to_str0 ());
  puts ("");
  printf (_ ("  Evalute the Scheme EXPR before parsing any .ly files.").to_str0 ());
  puts ("");
  printf (_ ("  Multiple -e options may be given, they will be evaluated sequentially.").to_str0 ());
  puts ("");
  printf (_("  The function ly:set-option allows for access to some internal variables.").to_str0 ());
  puts ("\n");
  printf (_ ("Usage: lilypond -e \"(ly:set-option SYMBOL VAL)\"").to_str0 ());
  puts ("\n");
  printf (_ ("Use help as  SYMBOL to get online help.").to_str0 ());

  exit (0);
  return SCM_UNSPECIFIED;
}

/* Add these as well:

@item -T,--no-timestamps
don't timestamp the output

@item -t,--test
Switch on any experimental features.  Not for general public use.

*/


LY_DEFINE (ly_set_option, "ly:set-option", 1, 1, 0, (SCM var, SCM val),
	    "Set a global option value.  Supported options include\n"
"\n"
"@table @code\n"
"@item help\n"
"List all options.\n"
"@item midi-debug\n"
"If set to true, generate human readable MIDI\n"
"@item internal-type-checking\n"
"Set paranoia for property assignments\n"
"@item parse-protect\n"
"If protection is switched on, errors in inline scheme are caught in the parser. \n"
"If off, GUILE will halt on errors, and give a stack trace. Default is protected evaluation. \n"
"@item old-relative\n"
"Relative for simultaneous music functions similar to chord syntax\n"
"@item new-relative\n"
"Relative for simultaneous music functions similar to sequential music\n"
"@end table\n"
"\n"
"This function is useful to call from the command line: @code{lilypond -e\n"
"\"(ly:set-option 'midi-debug #t)\"}.\n")
{
  if (val == SCM_UNDEFINED)
    val = SCM_BOOL_T;

  if (var == ly_symbol2scm ("help"))
    /* lilypond -e "(ly:set-option 'help #t)" */
    ly_option_usage (SCM_EOL);
  else if (var == ly_symbol2scm ("midi-debug"))
    midi_debug_global_b = to_boolean (val);
  else if (var == ly_symbol2scm ("testing-level"))
    testing_level_global = scm_to_int (val);
  else if (var == ly_symbol2scm ("parse-protect" ))
    parse_protect_global = to_boolean (val);
  else if (var == ly_symbol2scm ("internal-type-checking"))
    do_internal_type_checking_global = to_boolean (val);
  else if (var == ly_symbol2scm ("old-relative"))
    {
      lily_1_8_relative = true;
      /*  Needs to be reset for each file that uses this option.  */
      lily_1_8_compatibility_used = false;
    }
  else if (var == ly_symbol2scm ("resolution"))
    preview_resolution_global = robust_scm2int (val, 90);
  else if (var == ly_symbol2scm ("new-relative"))
    lily_1_8_relative = false;
  else
    {
      if (scm_is_symbol (var))
	var = scm_symbol_to_string (var);
      
      warning (_f ("No such internal option: %s", ly_scm2string (var)));
    }
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_get_option, "ly:get-option", 1, 0, 0, (SCM var),
	    "Get a global option setting.  Supported options include\n"
	   "@table @code\n"
	   "@item old-relative-used\n"
	   "Report whether old-relative compatibility mode is necessary\n"
	   "@item old-relative\n"
	   "Report whether old-relative compatibility mode is used\n"
	   "@item verbose\n"
	   "Report whether we are running in verbose mode\n"
	   "@item resolution\n"
	   "Resolution for the PNG output."
	   "@end table\n"
	   "\n")
{
  SCM o = SCM_UNSPECIFIED;
  
  if (var == ly_symbol2scm ("safe")) // heavily used; put in front. 
    o = ly_bool2scm (be_safe_global);
  else  if (var == ly_symbol2scm ("old-relative-used"))
    o = ly_bool2scm (lily_1_8_compatibility_used);
  else if (var == ly_symbol2scm ("old-relative"))
    o = ly_bool2scm (lily_1_8_relative);
  else if (var == ly_symbol2scm ("verbose"))
    o = ly_bool2scm (be_verbose_global);
  else if ( var == ly_symbol2scm ("resolution"))
    o = scm_from_int (preview_resolution_global);
  else
    {
      if (scm_is_symbol (var))
	var = scm_symbol_to_string (var);

      String s = ly_scm2string (var);
      
      warning (_f ("No such internal option: %s", s.to_str0() ));
    }
  return o;
}
