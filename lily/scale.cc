/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>
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
#include "protected-scm.hh"

#include <limits>

using std::vector;

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
      for (vsize i = 0; i < len; i++)
        {
          SCM step = scm_c_vector_ref (steps, i);
          type_ok = type_ok && is_scm<Rational> (step);
          if (type_ok)
            {
              Rational from_c (from_scm<int> (scm_numerator (step)),
                               from_scm<int> (scm_denominator (step)));
              tones.push_back (from_c);
            }
        }
    }

  SCM_ASSERT_TYPE (type_ok, steps, SCM_ARG1, __FUNCTION__,
                   "vector of rational");

  return (new Scale (tones))->unprotect ();
}

Scale *default_global_scale = 0;
Protected_scm default_global_scale_scm (SCM_BOOL_F);

// TODO: This is somewhat fishy: pitches protect their scale via a
// mark_smob hook.  But since pitches are of Simple_smob variety, they
// are unknown to GUILE unless a smobbed_copy has been created.  So
// changing the default scale might cause some existing pitches to
// lose their scale's protection.

LY_DEFINE (ly_default_scale, "ly:default-scale", 0, 0, 0, (),
           R"(
Get the global default scale.
           )")
{
  return default_global_scale_scm;
}

LY_DEFINE (ly_set_default_scale, "ly:set-default-scale", 1, 0, 0, (SCM scale),
           R"(
Set the global default scale.  This determines the tuning of pitches with no
accidentals or key signatures.  The first pitch is C.  Alterations are
calculated relative to this scale.  The number of pitches in this scale
determines the number of scale steps that make up an octave.  Usually the
7-note major scale.
           )")
{
  auto *const s = LY_ASSERT_SMOB (Scale, scale, 1);

  default_global_scale_scm = scale;
  default_global_scale = s;

  return SCM_UNSPECIFIED;
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
