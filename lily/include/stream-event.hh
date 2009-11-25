/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005-2006 Erik Sandberg <mandolaerik@gmail.com>

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

#ifndef STREAM_EVENT_HH
#define STREAM_EVENT_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "prob.hh"

class Stream_event : public Prob
{
public:
  Stream_event ();
  VIRTUAL_COPY_CONSTRUCTOR (Stream_event, Stream_event);

  Stream_event (SCM event_class, SCM mutable_props=SCM_EOL);
  Stream_event (SCM class_name, Input *);

  Input *origin () const;
  void set_spot (Input *i);
  bool internal_in_event_class (SCM class_name);

  virtual SCM copy_mutable_properties () const;

  DECLARE_SCHEME_CALLBACK (undump, (SCM));
  DECLARE_SCHEME_CALLBACK (dump, (SCM));
};

#define in_event_class(class_name) internal_in_event_class (ly_symbol2scm (class_name))

Stream_event *unsmob_stream_event (SCM);
DECLARE_TYPE_P (Stream_event);
SCM ly_event_deep_copy (SCM);

#endif /* STREAM_EVENT_HH */
