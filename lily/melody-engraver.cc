/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "item.hh"
#include "melody-spanner.hh"
#include "pointer-group-interface.hh"

class Melody_engraver : public Engraver
{
  Grob *melody_item_;
  Grob *stem_; 
protected:
  
  DECLARE_ACKNOWLEDGER (stem);
  DECLARE_ACKNOWLEDGER (slur);
  TRANSLATOR_DECLARATIONS (Melody_engraver);
  void stop_translation_timestep ();
  void process_music ();
};


Melody_engraver::Melody_engraver ()
{
  stem_ = 0;
  melody_item_ = 0;
}

void
Melody_engraver::process_music ()
{
  if (scm_is_string (get_property ("whichBar")))
    melody_item_ = 0;
}
  
void
Melody_engraver::stop_translation_timestep ()
{
  if (stem_
      && !is_direction (stem_->get_property_data ("neutral-direction")))
    {
      extract_grob_set (stem_, "rests", rests);
      if (rests.size ())
	melody_item_ = 0;
      else
	{
	  if (!melody_item_)
	    melody_item_ = make_item ("MelodyItem", stem_->self_scm ());

	  Melody_spanner::add_stem (melody_item_, stem_);
	}
    }
  stem_ = 0;
}


void
Melody_engraver::acknowledge_slur (Grob_info /* info */)
{
  melody_item_ = 0;
}


void
Melody_engraver::acknowledge_stem (Grob_info info)
{
  stem_ = info.grob ();
}



#include "translator.icc"

ADD_ACKNOWLEDGER (Melody_engraver, stem);
ADD_ACKNOWLEDGER (Melody_engraver, slur);

ADD_TRANSLATOR (Melody_engraver,
		/* doc */
		"Create information for context dependent typesetting"
		" decisions.",

		/* create */
		"MelodyItem ",

		/* read */
		"",

		/* write */
		""
		);
		
