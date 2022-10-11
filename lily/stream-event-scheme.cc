/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Erik Sandberg  <mandolaerik@gmail.com>

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

#include "stream-event.hh"

LY_DEFINE (ly_stream_event_p, "ly:stream-event?", 1, 0, 0, (SCM obj),
           R"(
Is @var{obj} a @code{Stream_event} object?
           )")
{
  return to_scm (static_cast<bool> (unsmob<Stream_event> (obj)));
}

LY_DEFINE (ly_make_stream_event, "ly:make-stream-event", 1, 1, 0,
           (SCM cl, SCM proplist),
           R"(
Create a stream event of class @var{cl} with the given mutable property list.
           )")
{
  LY_ASSERT_TYPE (ly_is_pair, cl, 1);

  LY_ASSERT_TYPE (ly_cheap_is_list, proplist, 2);

  Stream_event *e = new Stream_event (cl, proplist);
  return e->unprotect ();
}

LY_DEFINE (ly_event_property, "ly:event-property", 2, 1, 0,
           (SCM sev, SCM sym, SCM val),
           R"(
Get the property @var{sym} of stream event @var{sev}.  If @var{sym} is
undefined, return @var{val} or @code{'()} if @var{val} is not specified.
           )")
{
  LY_ASSERT_SMOB (Stream_event, sev, 1);
  return ly_prob_property (sev, sym, val);
}

LY_DEFINE (ly_event_set_property_x, "ly:event-set-property!", 3, 0, 0,
           (SCM ev, SCM sym, SCM val),
           R"(
Set property @var{sym} in event @var{ev} to @var{val}.
           )")
{
  LY_ASSERT_SMOB (Stream_event, ev, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  return ly_prob_set_property_x (ev, sym, val);
}

LY_DEFINE (ly_event_deep_copy, "ly:event-deep-copy", 1, 0, 0, (SCM m),
           R"(
Copy @var{m} and all sub-expressions of@tie{}@var{m}.
           )")
{
  SCM copy = m;
  if (Stream_event *ev = unsmob<Stream_event> (m))
    {
      ev = ev->clone ();
      copy = ev->unprotect ();
    }
  else if (scm_is_pair (m))
    copy = scm_cons (ly_event_deep_copy (scm_car (m)),
                     ly_event_deep_copy (scm_cdr (m)));

  return copy;
}
