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

#ifndef SCORE_ENGRAVER_HH
#define SCORE_ENGRAVER_HH

#include "engraver-group.hh"

class Score_engraver : public Engraver_group
{
  System *system_;

  vector<Grob*> elems_;
  Paper_score *pscore_;

  void typeset_all ();

protected:
  DECLARE_LISTENER (finish);
  DECLARE_LISTENER (prepare);
  DECLARE_LISTENER (one_time_step);

  /* Engraver_group_engraver interface */
  virtual void connect_to_context (Context *);
  virtual void disconnect_from_context ();
  virtual void initialize ();
  virtual void finalize ();
  virtual void announce_grob (Grob_info);
  void stop_translation_timestep ();

  /*
    Translator interface
  */
  virtual void derived_mark () const;

public:
  Score_engraver ();
};

#endif /* SCORE_ENGRAVER_HH */
