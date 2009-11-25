/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2009 Juergen Reuter <reuter@ipd.uka.de>

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

#ifndef LIGATURE_ENGRAVER_HH
#define LIGATURE_ENGRAVER_HH

#include "engraver.hh"
#include "moment.hh"

class Ligature_engraver : public Engraver
{
protected:
  Ligature_engraver ();
  void stop_translation_timestep ();
  virtual void finalize ();

  DECLARE_ACKNOWLEDGER (rest);
  DECLARE_ACKNOWLEDGER (note_head);
  virtual void listen_ligature (Stream_event *ev);
  void process_music ();
  virtual Spanner *create_ligature_spanner () = 0;
  virtual void typeset_ligature (Spanner *ligature,
				 vector<Grob_info> primitives) = 0;
  virtual Spanner *current_ligature ();
  SCM brew_ligature_primitive_proc;

public:
  // no TRANSLATOR_DECLARATIONS (Ligature_engraver) needed since this
  // class is abstract

private:
  Drul_array<Stream_event *> events_drul_;

  Spanner *ligature_;
  vector<Grob_info> primitives_;

  Spanner *finished_ligature_;
  vector<Grob_info> finished_primitives_;

  Stream_event *prev_start_event_;

  // moment where ligature started.
  Moment ligature_start_mom_;

  Grob *last_bound_;
};

#endif // LIGATURE_ENGRAVER_HH
