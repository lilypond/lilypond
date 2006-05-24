/*
  percent-repeat-iterator.cc -- implement Percent_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2006  Han-Wen Nienhuys <hanwen@xs4all.nl>
                  Erik Sandberg <mandolaerik@gmail.com>
*/

#include "percent-repeat-iterator.hh"
#include "input.hh"
#include "music.hh"
#include "repeated-music.hh"

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);

Percent_repeat_iterator::Percent_repeat_iterator ()
{
}

SCM
Percent_repeat_iterator::get_music_list () const
{
  /* TODO: Distinction between percent, double-percent and slash */
  Music *mus = get_music ();
  Music *child = Repeated_music::body (mus);
  SCM length = child->get_length ().smobbed_copy ();
  SCM child_list = SCM_EOL;

  int repeats = scm_to_int (mus->get_property ("repeat-count"));
  for (int i = repeats; i > 1; i--)
  {
    Music *percent = make_music_by_name (ly_symbol2scm ("PercentEvent"));
    percent->set_spot (*mus->origin ());
    percent->set_property ("length", length);
    if (repeats > 1)
      percent->set_property ("repeat-count", scm_int2num (i - 1));
    child_list = scm_cons (percent->unprotect (), child_list);
  }
  child_list = scm_cons (child->self_scm (), child_list);

  return child_list;
}
