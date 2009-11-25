/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys

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

#include "protected-scm.hh"
#include "note-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "spanner.hh"
#include "text-interface.hh"
#include "item.hh"

class Ottava_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Ottava_spanner_engraver);
protected:
  virtual void finalize ();

  DECLARE_ACKNOWLEDGER (note_column);

  void process_music ();
  void stop_translation_timestep ();
  virtual void derived_mark () const;
private:
  Spanner *span_;
  Spanner *finished_;

  SCM last_ottavation_;

  void typeset_all ();
};

void
Ottava_spanner_engraver::derived_mark () const
{
  scm_gc_mark (last_ottavation_);
}

Ottava_spanner_engraver::Ottava_spanner_engraver ()
{
  finished_ = 0;
  span_ = 0;
  last_ottavation_ = SCM_EOL;
}

void
Ottava_spanner_engraver::process_music ()
{
  SCM ott = get_property ("ottavation");
  if (ott != last_ottavation_)
    {
      finished_ = span_;
      span_ = 0;
      if (Text_interface::is_markup (ott))
	{
	  span_ = make_spanner ("OttavaBracket", SCM_EOL);
	  span_->set_property ("text", ott);

	  SCM offset (get_property ("middleCOffset"));
	  if (robust_scm2double (offset, 0) > 0)
	    span_->set_property ("direction", scm_from_int (DOWN));
	}
    }
  last_ottavation_ = ott;
}

void
Ottava_spanner_engraver::acknowledge_note_column (Grob_info info)
{
  Item *it = info.item ();
  if (span_ && it)
    {
      Side_position_interface::add_support (span_, it);

      if (!span_->get_bound (LEFT))
	span_->set_bound (LEFT, it);
      span_->set_bound (RIGHT, it);
    }
}

void
Ottava_spanner_engraver::typeset_all ()
{
  if (finished_)
    {
      Direction d = LEFT;
      do
	{
	  if (!finished_->get_bound (RIGHT))
	    {
	      Grob *e = unsmob_grob (get_property ("currentMusicalColumn"));
	      finished_->set_bound (d, e);
	    }
	}
      while (flip (&d) != LEFT);

      finished_ = 0;
    }
}

void
Ottava_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob *e = unsmob_grob (get_property ("currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
}

void
Ottava_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    finished_ = span_;
  typeset_all ();
  last_ottavation_ = SCM_EOL;
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Ottava_spanner_engraver, note_column);

ADD_TRANSLATOR (Ottava_spanner_engraver,
		/* doc */
		"Create a text spanner when the ottavation property changes.",

		/* create */
		"OttavaBracket ",

		/* read */
		"ottavation "
		"originalMiddleCPosition "
		"currentMusicalColumn ",
		
		/* write */
		""
		);
