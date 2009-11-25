/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/*
  this is C&P from beam_performer.
*/

class Slur_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Slur_performer);

protected:
  void start_translation_timestep ();
  void process_music ();
  void set_melisma (bool);
  
  DECLARE_TRANSLATOR_LISTENER (slur);
private:
  Stream_event *start_ev_;
  Stream_event *now_stop_ev_;
  bool slur_;
};

Slur_performer::Slur_performer ()
{
  slur_ = false;
  start_ev_ = 0;
  now_stop_ev_ = 0;
}

void
Slur_performer::process_music ()
{
  if (now_stop_ev_)
    {
      slur_ = false;
      set_melisma (false);
    }

  if (start_ev_)
    {
      slur_ = true;
      set_melisma (true);
    }
}

void
Slur_performer::set_melisma (bool ml)
{
  context ()->set_property ("slurMelismaBusy", ml ? SCM_BOOL_T : SCM_BOOL_F);
}

void
Slur_performer::start_translation_timestep ()
{
  start_ev_ = 0;
  now_stop_ev_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Slur_performer, slur);
void
Slur_performer::listen_slur (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));

  if (d == START)
    start_ev_ = ev;
  else if (d == STOP)
    now_stop_ev_ = ev;
}

ADD_TRANSLATOR (Slur_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
