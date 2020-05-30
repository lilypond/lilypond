/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MOMENT_HH
#define MOMENT_HH

#include "smobs.hh"
#include "rational.hh"

/**
   Musical timing (Main-timing, grace-timing) with glue for
   Guilification;
*/
class Moment : public Simple_smob<Moment>
{
public:
  static SCM equal_p (SCM, SCM);
  int print_smob (SCM, scm_print_state *) const;
  static const char *const type_p_name_;
  Moment ();
  Moment (int m);

  Moment (Rational, Rational);
  explicit Moment (Rational m);

  Moment operator - () const;

  Moment &operator += (Moment const &m);
  Moment &operator -= (Moment const &m);

  Moment operator + (Moment const &m) const { return Moment (*this) += m; }
  Moment operator - (Moment const &m) const { return Moment (*this) -= m; }

  Moment &operator += (Rational const &r) { main_part_ += r; return *this; }
  Moment &operator -= (Rational const &r) { main_part_ -= r; return *this; }
  Moment &operator *= (Rational const &); // affects main and grace parts
  Moment &operator /= (Rational const &); // affects main and grace parts
  Moment &operator %= (Rational const &); // affects main and grace parts

  Moment operator + (Rational const &r) const { return Moment (*this) += r; }
  Moment operator - (Rational const &r) const { return Moment (*this) -= r; }
  Moment operator * (Rational const &r) const { return Moment (*this) *= r; }
  Moment operator / (Rational const &r) const { return Moment (*this) /= r; }
  Moment operator % (Rational const &r) const { return Moment (*this) %= r; }

  Rational main_part_;
  Rational grace_part_;

  bool to_bool () const;
  I64 den () const;
  I64 num () const;

  std::string to_string () const;
  static int compare (Moment const &, Moment const &);
};

int compare (Moment const &, Moment const &);
INSTANTIATE_COMPARE (Moment const &, Moment::compare);

Moment robust_scm2moment (SCM, Moment);

#ifdef STREAM_SUPPORT
ostream &operator << (ostream &, Moment const &);
#endif

bool moment_less (SCM a, SCM b);

inline std::string
to_string (Moment const &m)
{
  return m.to_string ();
}

#endif /* MOMENT_HH */
