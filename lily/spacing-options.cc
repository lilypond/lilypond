/*
  spacing-options.cc -- implement Spacing_options 

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@lilypond.org>

*/

#include "spacing-spanner.hh"
#include "grob.hh"

void
Spacing_options::init_from_grob (Grob *me)
{
  increment_ = robust_scm2double (me->get_property ("spacing-increment"), 1);

  packed_ = to_boolean (me->get_property ("packed-spacing"));
  stretch_uniformly_ = to_boolean (me->get_property ("uniform-stretching"));
  float_nonmusical_columns_
    = to_boolean (me->get_property ("strict-note-spacing"));
  float_grace_columns_
    = to_boolean (me->get_property ("strict-grace-spacing"));
  shortest_duration_space_ = robust_scm2double (me->get_property ("shortest-duration-space"), 1);
}

Spacing_options::Spacing_options ()
{
  increment_ = 1.2;
  packed_ = false;
  stretch_uniformly_ = false;
  float_nonmusical_columns_ = false;
  float_grace_columns_ = false;
  shortest_duration_space_ = 2.0;

  global_shortest_ = Rational (1, 8);
}
