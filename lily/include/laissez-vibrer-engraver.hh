/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                2017 David Kastrup <dak@gnu.org>

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

#ifndef LAISSEZ_VIBRER_ENGRAVER_HH
#define LAISSEZ_VIBRER_ENGRAVER_HH

#include "engraver.hh"

#include <vector>

class Laissez_vibrer_engraver : public Engraver
{
  Stream_event *event_;
  Grob *lv_column_;
  std::vector<Grob *> lv_ties_;

  virtual bool is_my_event_class (Stream_event *ev);
  virtual Grob *make_my_tie (SCM cause);
  virtual Grob *make_my_column (SCM cause);

protected:
  void stop_translation_timestep ();
  void acknowledge_note_head (Grob_info);
  void listen_laissez_vibrer (Stream_event *);

public:
  TRANSLATOR_DECLARATIONS (Laissez_vibrer_engraver);
};

#endif // LAISSEZ_VIBRER_ENGRAVER_HH
