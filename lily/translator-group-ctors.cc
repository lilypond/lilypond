/*
  translator-group-ctors.cc -- implement Translator_group factory.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score-engraver.hh"
#include "score-performer.hh"
#include "warn.hh"
#include "international.hh"

/*
  Quick & dirty.
*/
Translator_group *
get_translator_group (SCM sym)
{
  if (sym == ly_symbol2scm ("Engraver_group"))
    return new Engraver_group ();
  else if (sym == ly_symbol2scm ("Performer_group"))
    return new Performer_group ();
  else if (sym == ly_symbol2scm ("Score_engraver"))
    return new Score_engraver ();
  else if (sym == ly_symbol2scm ("Score_performer"))
    return new Score_performer ();

  error (_f ("fatal error. Couldn't find type: %s",
	     ly_symbol2string (sym).c_str ()));
  scm_flush (scm_current_error_port ());
  scm_display (sym, scm_current_error_port ());
  scm_flush (scm_current_error_port ());
  
  exit (2);
  
  return 0;
}
