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

#include "engraver.hh"
#include "grob.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "pointer-group-interface.hh"

class Vertically_spaced_contexts_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Vertically_spaced_contexts_engraver);
protected:
  virtual void initialize ();
  DECLARE_ACKNOWLEDGER (vertically_spaceable);

private:
  Grob *system_;
};

Vertically_spaced_contexts_engraver::Vertically_spaced_contexts_engraver ()
{
  system_ = 0;
}

void
Vertically_spaced_contexts_engraver::initialize ()
{
  system_ = unsmob_grob (get_property ("rootSystem"));
}

void
Vertically_spaced_contexts_engraver::acknowledge_vertically_spaceable (Grob_info gi)
{
  if (Axis_group_interface::has_interface (gi.grob ()))
    {
      SCM spaceable = get_property ("verticallySpacedContexts");
      Context *orig = gi.origin_contexts (this)[0];

      if (scm_memq (ly_symbol2scm (orig->context_name ().c_str ()),
		    spaceable) != SCM_BOOL_F)
	{
	  Pointer_group_interface::add_unordered_grob (system_,
						       ly_symbol2scm ("spaceable-staves"),
						       gi.grob ());
	}
    }
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Vertically_spaced_contexts_engraver, vertically_spaceable);
ADD_TRANSLATOR (Vertically_spaced_contexts_engraver,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"verticallySpacedContexts ",

		/* write */
		"verticallySpacedContexts "
		);
