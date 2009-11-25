/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "audio-element-info.hh"
#include "grob-info.hh"
#include "translator.hh"

/* Convert a music definition into a audio representation.
   A baseclass.  */
class Performer : public Translator
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator, Performer);
  friend class Performer_group;
  Performer_group *get_daddy_performer () const;

protected:
  virtual void announce_element (Audio_element_info);
  virtual void acknowledge_audio_element (Audio_element_info);
  virtual void create_audio_elements ();
};

#endif /* PERFORMER_HH */

