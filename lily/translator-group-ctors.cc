/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
