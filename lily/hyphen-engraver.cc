/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Glen Prideaux <glenprideaux@iname.com>,
  Han-Wen Nienhuys <hanwen@xs4all.nl>,
  Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "international.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Hyphen_engraver : public Engraver
{
  Stream_event *ev_;
  Stream_event *finished_ev_;

  Spanner *hyphen_;
  Spanner *finished_hyphen_;
  bool current_lyric_is_skip_;

public:
  TRANSLATOR_DECLARATIONS (Hyphen_engraver);

protected:

  DECLARE_ACKNOWLEDGER (lyric_syllable);
  DECLARE_TRANSLATOR_LISTENER (hyphen);

  virtual void finalize ();

  void stop_translation_timestep ();
  void process_music ();
};

Hyphen_engraver::Hyphen_engraver ()
{
  current_lyric_is_skip_ = false;
  hyphen_ = 0;
  finished_hyphen_ = 0;
  finished_ev_ = 0;
  ev_ = 0;
}

void
Hyphen_engraver::acknowledge_lyric_syllable (Grob_info i)
{
  Item *item = i.item ();
  SCM text = item->get_property ("text");
  current_lyric_is_skip_ = ly_is_equal (text, scm_from_locale_string (" "));
  
  if (!hyphen_ && !current_lyric_is_skip_)
    hyphen_ = make_spanner ("LyricSpace", item->self_scm ());

  if (hyphen_)
    hyphen_->set_bound (LEFT, item);
      
  if (finished_hyphen_ && !current_lyric_is_skip_)
    finished_hyphen_->set_bound (RIGHT, item);
}

IMPLEMENT_TRANSLATOR_LISTENER (Hyphen_engraver, hyphen);
void
Hyphen_engraver::listen_hyphen (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (ev_, ev);
}

void
completize_hyphen (Spanner *sp)
{
  if (!sp->get_bound (RIGHT))
    {
      extract_item_set (sp, "heads", heads);
      if (heads.size ())
	sp->set_bound (RIGHT, heads.back ());
    }
}

void
Hyphen_engraver::finalize ()
{
  if (hyphen_)
    {
      completize_hyphen (hyphen_);

      if (!hyphen_->get_bound (RIGHT))
	{
	  hyphen_->warning (_ ("removing unterminated hyphen"));
	  hyphen_->suicide ();
	}

      hyphen_ = 0;
    }

  if (finished_hyphen_)
    {
      completize_hyphen (finished_hyphen_);

      if (!finished_hyphen_->get_bound (RIGHT))
	{
	  if (finished_ev_)
	    finished_hyphen_->warning (_ ("unterminated hyphen; removing"));
	  finished_hyphen_->suicide ();
	}
      finished_hyphen_ = 0;
    }
}

void
Hyphen_engraver::process_music ()
{
  if (ev_)
    hyphen_ = make_spanner ("LyricHyphen", ev_->self_scm ());
}

void
Hyphen_engraver::stop_translation_timestep ()
{
  if (finished_hyphen_ && finished_hyphen_->get_bound (RIGHT))
    {
      finished_hyphen_ = 0;
      finished_ev_ = 0;
    }
  
  if (finished_hyphen_ && hyphen_ && !current_lyric_is_skip_)
    {
      programming_error ("hyphen not finished yet");
      finished_hyphen_ = 0;
      finished_ev_ = 0;
    }

  if (hyphen_)
    {
      finished_hyphen_ = hyphen_;
      finished_ev_ = ev_;
    }
  
  hyphen_ = 0;
  ev_ = 0;
}

ADD_ACKNOWLEDGER (Hyphen_engraver, lyric_syllable);

ADD_TRANSLATOR (Hyphen_engraver,
		/* doc */
		"Create lyric hyphens and distance constraints between words.",

		/* create */
		"LyricHyphen "
		"LyricSpace ",

		/* read */
		"",

		/* write */
		""
		);
