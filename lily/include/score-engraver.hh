/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#if GUILEV2
#include <gc.h>
#endif

class Score_engraver : public Engraver_group
{
  System *system_;
#if GUILEV2
  GC_word last_;
#endif
  std::vector<Grob *> elems_;
  Paper_score *pscore_;

  void typeset_all ();

protected:
  void finish (SCM);
  void prepare (SCM);
  void one_time_step (SCM);

  /* Engraver_group_engraver interface */
  void connect_to_context (Context *) override;
  void disconnect_from_context () override;
  void initialize () override;
  void finalize () override;
  void announce_grob (Grob_info, Direction dir,
                      Context *reroute_context = 0) override;
  void stop_translation_timestep ();

  /*
    Translator interface
  */
  void derived_mark () const override;

public:
  Score_engraver ();
};

#endif /* SCORE_ENGRAVER_HH */
