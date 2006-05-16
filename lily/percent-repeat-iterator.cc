/*
  percent-repeat-iterator.cc -- implement Percent_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2006  Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg <mandolaerik@gmail.com>
*/

#include "percent-repeat-iterator.hh"

#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "repeated-music.hh"

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);

Percent_repeat_iterator::Percent_repeat_iterator ()
{
  child_list_ = SCM_EOL;
}

void
Percent_repeat_iterator::construct_children ()
{
  /* TODO: Distinction between percent and slash */
  Music *mus = get_music ();
  Music *child = Repeated_music::body (mus);
  SCM length = child->get_length ().smobbed_copy ();
  child_list_ = SCM_EOL;

  int repeats = scm_to_int (mus->get_property ("repeat-count"));
  for (int i = repeats; i > 1; i--)
  {
    Music *percent = make_music_by_name (ly_symbol2scm ("PercentEvent"));
    percent->set_spot (*mus->origin ());
    percent->set_property ("length", length);
    if (repeats > 1)
      percent->set_property ("repeat-count", scm_int2num (i - 1));
    Music *percent_chord = make_music_by_name (ly_symbol2scm ("EventChord"));
    percent_chord->set_spot (*mus->origin ());
    percent_chord->set_property ("elements", scm_list_1 (percent->self_scm ()));
    child_list_ = scm_cons (percent_chord->self_scm (), child_list_);
    percent->unprotect ();
    percent_chord->unprotect ();
  }
  child_list_ = scm_cons (child->self_scm (), child_list_);
  
  Sequential_iterator::construct_children ();
}

SCM
Percent_repeat_iterator::get_music_list () const
{
  return child_list_;
}

void
Percent_repeat_iterator::derived_mark () const
{
  scm_gc_mark (child_list_);
  Sequential_iterator::derived_mark ();
}
