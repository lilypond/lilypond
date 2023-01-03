/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2023 Han-Wen Nienhuys <hanwen@lilypond.org>
      2007--2008 Rune Zedeler
      2008       Joe Neeman <joeneeman@gmail.com>

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

#include "scale.hh"
#include "lily-imports.hh"

#include <limits>

using std::vector;

const char *const Scale::type_p_name_ = "ly:note-scale?";

/*
  todo: put string <-> pitch here too.
*/
LY_DEFINE (ly_make_scale, "ly:make-scale", 1, 0, 0, (SCM steps),
           R"(
Create a scale.  The argument is a vector of rational numbers, each of which
represents the number of 200-cent tones of a pitch above the tonic.
           )")
{
  bool type_ok = scm_is_vector (steps);
  vector<Rational> tones;

  if (type_ok)
    {
      vsize len = scm_c_vector_length (steps);
      tones.reserve (len);
      for (vsize i = 0; i < len; i++)
        {
          SCM step = scm_c_vector_ref (steps, i);
          type_ok = is_scm<Rational> (step);

          if (!type_ok)
            break;

          tones.push_back (from_scm<Rational> (step));
        }
    }

  SCM_ASSERT_TYPE (type_ok, steps, SCM_ARG1, __FUNCTION__,
                   "vector of rational");

  return (new Scale (tones))->unprotect ();
}

int
Scale::step_count () const
{
  return static_cast<int> (step_tones_.size ());
}

Rational
Scale::tones_at_step (int step, int octave) const
{
  int normalized_step = normalize_step (step);

  octave += (step - normalized_step) / step_count ();

  // There are 6 tones in an octave.
  return step_tones_[normalized_step] + Rational (octave * 6);
}

Rational
Scale::step_size (int step) const
{
  int normalized_step = normalize_step (step);

  // Wrap around if we are asked for the final note of the
  // scale (6 is the number of tones of the octave above the
  // first note).
  if (normalized_step + 1 == step_count ())
    return Rational (6) - step_tones_[normalized_step];

  return step_tones_[normalized_step + 1] - step_tones_[normalized_step];
}

int
Scale::normalize_step (int step) const
{
  int ret = step % step_count ();
  if (ret < 0)
    ret += step_count ();

  return ret;
}

Scale::Scale (vector<Rational> const &tones)
{
  assert (tones.size () <= std::numeric_limits<int>::max ());
  step_tones_ = tones;

  smobify_self ();
}

Scale::Scale (Scale const &src)
  : Smob<Scale> ()
{
  step_tones_ = src.step_tones_;
  smobify_self ();
}

Scale::~Scale ()
{
}
