/*
  percent-repeat-iterator.cc -- implement Percent_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
                  Erik Sandberg <mandolaerik@gmail.com>
*/

#include "input.hh"
#include "repeated-music.hh"
#include "sequential-iterator.hh"

class Percent_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_CLASSNAME (Percent_repeat_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Percent_repeat_iterator ();
protected:
  virtual SCM get_music_list () const;
};

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
      percent->set_property ("repeat-count", scm_int2num (i));
    
    child_list = scm_cons (percent->unprotect (), child_list);
  }
  
  child_list = scm_cons (child->self_scm (), child_list);

  return child_list;
}
