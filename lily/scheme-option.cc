/*   
  scheme-option.cc --  implement option setting from Scheme
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lily-guile.hh"


bool midi_debug_global_b;


/*

  Todo: print help text.


  other interesting stuff to add:

@item -T,--no-timestamps
don't timestamp the output

@item -t,--test
Switch on any experimental features.  Not for general public use.

 */

SCM
set_lily_option (SCM var, SCM val)
{
  if (var == ly_symbol2scm ("midi-debug"))
    {
      midi_debug_global_b = to_boolean (val);
    }
  else if (var == ly_symbol2scm ("find-old-relative"))
    {
      /*
	Seems to have been broken for some time!
	
	@item  -Q,--find-old-relative
	show all changes needed to convert a file to  relative octave syntax.


	
      */

      ;
      
    }

  return SCM_UNSPECIFIED;
}


static void
init_functions ()
{
  scm_make_gsubr ("set-lily-option", 2, 0, 0, (Scheme_function_unknown)set_lily_option);
}


ADD_SCM_INIT_FUNC(init_functions_sopt, init_functions);


