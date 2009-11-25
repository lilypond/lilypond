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

#ifndef TYPESWALLOW_TRANSLATOR_HH
#define TYPESWALLOW_TRANSLATOR_HH

#include "translator.hh"

/** eat a certain type of event
    (Duh, it's good for your skin)
*/
class Type_swallow_translator : public Translator
{
protected:
  string swallow_string_;
  bool try_music (Music *);
public:
  VIRTUAL_COPY_CONS (Translator);
};

#define DECLARE_EVENT_SWALLOWER(TYPE)					\
  struct TYPE ## _swallow_translator : public Type_swallow_translator	\
  {									\
    TRANSLATOR_DECLARATIONS (TYPE ## _swallow_translator);		\
  };									\
  TYPE ## _swallow_translator ::TYPE ## _swallow_translator ()		\
  {									\
    swallow_string_ = #TYPE;						\
  }									\
  ADD_TRANSLATOR (TYPE ## _swallow_translator,				\
		  "Swallow events of " #TYPE " type.",			\
		  "",							\
		  "general-music",					\
		  "",							\
		  "",							\
		  "");

#endif // TYPESWALLOW_TRANSLATOR_HH

