/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

#include "diagnostics.hh"
#include "stream-event.hh"
#include "virtual-methods.hh"

class Audio_element : public Diagnostics
{
private:
  Stream_event *cause_ = nullptr;

private:
  // Performance keeps Audio_elements alive and calls gc_mark() to ensure that
  // their causing events survive garbage collection.
  friend class Performance;
  void set_cause (Stream_event *cause) { cause_ = cause; }

public:
  Audio_element ();
  virtual ~Audio_element ();

  VIRTUAL_CLASS_NAME (Audio_element);
  Stream_event *cause () const { return cause_; }
  void gc_mark () const;
  virtual char const *name () const;
  Input *origin () const final { return cause_ ? cause_->origin () : nullptr; }
};

#endif // AUDIO_ELEMENT_HH
