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

#include "moment.hh"

#include "lily-guile.hh"
#include "warn.hh"

using std::string;

const char *const Moment::type_p_name_ = "ly:moment?";

int
Moment::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Mom ", port);
  scm_puts (to_string ().c_str (), port);
  scm_puts (">", port);

  return 1;
}

SCM
Moment::equal_p (SCM a, SCM b)
{
  Moment *m1 = unsmob<Moment> (a);
  Moment *m2 = unsmob<Moment> (b);

  return (*m1 == *m2) ? SCM_BOOL_T : SCM_BOOL_F;
}

int
compare (Moment const &a, Moment const &b)
{
  return Moment::compare (a, b);
}

int
Moment::compare (Moment const &a, Moment const &b)
{
  int c = Rational::compare (a.main_part_, b.main_part_);
  if (c)
    return c;

  return Rational::compare (a.grace_part_, b.grace_part_);
}

Moment &
Moment::operator+= (Moment const &src)
{
  main_part_ += src.main_part_;
  grace_part_ += src.grace_part_;
  return *this;
}

Moment &
Moment::operator-= (Moment const &src)
{
  main_part_ -= src.main_part_;
  grace_part_ -= src.grace_part_;
  return *this;
}

Moment &
Moment::operator*= (Rational const &r)
{
  main_part_ *= r;
  grace_part_ *= r;
  return *this;
}

Moment &
Moment::operator/= (Rational const &r)
{
  main_part_ /= r;
  grace_part_ /= r;
  return *this;
}

Moment &
Moment::operator%= (Rational const &r)
{
  main_part_ %= r;
  grace_part_ %= r;
  return *this;
}

string
Moment::to_string () const
{
  string s = main_part_.to_string ();
  if (grace_part_)
    s += "G" + grace_part_.to_string ();
  return s;
}

Moment
Moment::operator- () const
{
  Moment m;
  m.grace_part_ = -grace_part_;
  m.main_part_ = -main_part_;
  return m;
}

#ifdef STREAM_SUPPORT
ostream &
operator<< (ostream &os, Moment const &m)
{
  os << m.to_string ();
  return os;
}
#endif

bool
moment_less (SCM a, SCM b)
{
  return *unsmob<Moment> (a) < *unsmob<Moment> (b);
}
