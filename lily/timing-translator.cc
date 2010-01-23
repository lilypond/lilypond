/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "timing-translator.hh"

#include "warn.hh"
#include "translator-group.hh"
#include "global-context.hh"
#include "multi-measure-rest.hh"

void
Timing_translator::stop_translation_timestep ()
{
  Global_context *global = get_global_context ();

  if (to_boolean (get_property ("timing"))
      && !to_boolean (get_property ("skipBars")))
    {
      Moment barleft = (measure_length () - measure_position (context ()));
      Moment now = now_mom ();

      if (barleft > Moment (0))
	{
	  Moment nextmom = now + barleft;
	  nextmom.grace_part_ = Rational (0);
	  global->add_moment_to_process (nextmom);
	}
    }
}

void
Timing_translator::initialize ()
{
  context ()->add_alias (ly_symbol2scm ("Timing"));
  context ()->set_property ("currentBarNumber", scm_from_int (1));
  context ()->set_property ("internalBarNumber", scm_from_int (1));

  context ()->set_property ("timeSignatureFraction",
			    scm_cons (scm_from_int (4), scm_from_int (4)));
  /*
    Do not init measurePosition; this should be done from global
    context.
  */
  context ()->set_property ("measureLength",
			    Moment (Rational (1)).smobbed_copy ());
  context ()->set_property ("beatLength",
			    Moment (Rational (1, 4)).smobbed_copy ());
}

Rational
Timing_translator::measure_length () const
{
  SCM l = get_property ("measureLength");
  if (unsmob_moment (l))
    return unsmob_moment (l)->main_part_;
  else
    return Rational (1);
}

Timing_translator::Timing_translator ()
{
}

void
Timing_translator::start_translation_timestep ()
{
  Global_context *global = get_global_context ();

  Moment now = global->now_mom ();
  Moment dt = now - global->previous_moment ();
  if (dt < Moment (0))
    {
      programming_error ("moving backwards in time");
      dt = 0;
    }
  else if (dt.main_part_.is_infinity ())
    {
      programming_error ("moving infinitely to future");
      dt = 0;
    }

  if (!dt.to_bool ())
    return;

  Moment measposp;

  SCM s = get_property ("measurePosition");
  if (unsmob_moment (s))
    measposp = *unsmob_moment (s);
  else
    {
      measposp = now;
      context ()->set_property ("measurePosition",
				measposp.smobbed_copy ());
    }

  measposp += dt;

  int current_barnumber = robust_scm2int (get_property ("currentBarNumber"), 0);
  int internal_barnumber = robust_scm2int (get_property ("internalBarNumber"), 0);

  SCM cad = get_property ("timing");
  bool c = to_boolean (cad);

  Rational len = measure_length ();
  while (c && measposp.main_part_ >= len)
    {
      measposp.main_part_ -= len;
      current_barnumber ++;
      internal_barnumber ++;
    }

  context ()->set_property ("currentBarNumber", scm_from_int (current_barnumber));
  context ()->set_property ("internalBarNumber", scm_from_int (internal_barnumber));
  context ()->set_property ("measurePosition", measposp.smobbed_copy ());
}

#include "translator.icc"

ADD_TRANSLATOR (Timing_translator,
		/* doc */
		"This engraver adds the alias @code{Timing} to its containing"
		" context.  Responsible for synchronizing timing information"
		" from staves.  Normally in @code{Score}.  In order to create"
		" polyrhythmic music, this engraver should be removed from"
		" @code{Score} and placed in @code{Staff}.",

		/* create */
		"",

		/* read */
		"internalBarNumber "
		"currentBarNumber "
		"measureLength "
		"measurePosition ",

		/* write */
		"internalBarNumber "
		"currentBarNumber "
		"measurePosition "
		);
