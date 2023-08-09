/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2023 Juergen Reuter <reuter@ipd.uka.de>

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

#ifndef TUPLET_DESCRIPTION_HH
#define TUPLET_DESCRIPTION_HH

#include "lily-guile.hh"
#include "moment.hh"
#include "rational.hh"
#include "stream-event.hh"
#include "spanner.hh"

struct Tuplet_description : public Smob<Tuplet_description>
{
  Stream_event *const event_;
  Spanner *bracket_ = nullptr;
  Spanner *number_ = nullptr;

  bool full_length_ = false;
  bool full_length_note_ = false;

  Moment start_moment_;
  Moment stop_moment_; // Tuplet_engraver needs to modify this for some reason

  Tuplet_description *parent_ = nullptr;

  unsigned numerator_ = 0;
  unsigned denominator_ = 0;

  static const char *const type_p_name_;
  Tuplet_description (Stream_event *, const Moment &);
  virtual ~Tuplet_description ();

  static SCM equal_p (SCM, SCM);

  SCM mark_smob () const;

  Rational const &tuplet_start () const;
  Rational const &tuplet_stop () const;
  Rational tuplet_length () const;

  friend bool operator== (Tuplet_description const &,
                          Tuplet_description const &);
  friend bool operator!= (Tuplet_description const &,
                          Tuplet_description const &);
};

#endif // TUPLET_DESCRIPTION_HH
