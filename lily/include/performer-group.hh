/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#ifndef PERFORMER_GROUP_HH
#define PERFORMER_GROUP_HH

#include "performer.hh"
#include "translator-group.hh"

#include <vector>

typedef void (Performer::*Performer_method) (void);

class Performer_group : public Translator_group
{
public:
  OVERRIDE_CLASS_NAME (Performer_group);

  void do_announces ();
  virtual void announce_element (Audio_element_info);

protected:
  std::vector<Audio_element_info> announce_infos_;
  virtual void acknowledge_audio_elements ();
};

#endif /* PERFORMER_GROUP_HH */
