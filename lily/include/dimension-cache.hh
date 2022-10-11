/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "lily-proto.hh"

/*
  XY offset/refpoint/extent structure.
*/
class Dimension_cache
{
  // A value plus a validity flag.  The interface was chosen with the hope of
  // replacing this with C++17 std::optional someday.  The implementation
  // differs fundamentally from std::optional, but not in any way that matters
  // for current uses in Dimension_cache.
  template <class T>
  class Optional
  {
  private:
    bool has_value_;
    T value_;

  public:
    Optional ()
      : has_value_ (false),
        value_ ()
    {
    }

    Optional &operator= (const T &value)
    {
      has_value_ = true;
      value_ = value;
      return *this;
    }

    void reset () { has_value_ = false; }

    explicit operator bool () const { return has_value_; }

    T &operator* () { return value_; }
    const T &operator* () const { return value_; }
  };

  Optional<Interval> extent_;
  Optional<Real> offset_;
  Grob *parent_;

  friend class Grob;

  Dimension_cache ()
    : parent_ (0)
  {
  }

  // The compiler-generated copy constructor, assignment operator, and
  // destructor should be OK.

  void clear ()
  {
    extent_.reset ();
    offset_.reset ();
    // note that parent_ is not nullified
  }
};

#endif /* DIMENSION_CACHE_HH */
