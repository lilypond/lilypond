/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef AUDIO_ITEM_INFO_HH
#define AUDIO_ITEM_INFO_HH

#include "lily-proto.hh"

#include <vector>

/**
   Data container for broadcasts
*/
class Audio_element_info
{
public:
  Audio_element *elem_ = nullptr;
  Stream_event *event_ = nullptr;
  Translator *origin_trans_ = nullptr;

  std::vector<Context *> origin_contexts (Translator *) const;

  Audio_element_info () = default;

  Audio_element_info (Audio_element *elem, Stream_event *event)
    : elem_ (elem),
      event_ (event)
  {
  }
};

#endif
