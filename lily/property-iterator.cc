/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "property-iterator.hh"
#include "translation-property.hh"
#include "translator-group.hh"

/**
  There is no real processing to a property: just lookup the
  translation unit, and set the property.
  */
void
Property_iterator::do_process_and_next (Moment m)
{
  SCM sym = music_l_->get_mus_property ("symbol");
  if (gh_symbol_p(sym))
    report_to_l ()->set_property (sym, music_l_->get_mus_property ("value"));
  Music_iterator::do_process_and_next (m);
}


void
Push_property_iterator::do_process_and_next (Moment m)
{
  SCM syms = music_l_->get_mus_property ("symbols");
  SCM  eprop = music_l_->get_mus_property ("element-property");
  SCM val = music_l_->get_mus_property ("element-value");

  for (SCM s = syms; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM sym = gh_car (s);
      if (gh_symbol_p(sym))
      {
	SCM prev = report_to_l ()->get_property (sym);

	prev = gh_cons (gh_cons (eprop, val), prev);
	report_to_l ()->set_property (gh_car (s), prev);
      }
    }
  Music_iterator::do_process_and_next (m);
}

void
Pop_property_iterator::do_process_and_next (Moment m)
{
  SCM syms = music_l_->get_mus_property ("symbols");
  SCM eprop = music_l_->get_mus_property ("element-property");
  for (SCM s = syms; gh_pair_p (s); s = gh_cdr (s)) 
    {
    SCM sym = gh_car (s);
    if (gh_symbol_p(sym))
      {
	SCM prev = report_to_l ()->get_property (sym);

	SCM newprops= SCM_EOL ;
	while (gh_pair_p (prev) && gh_caar (prev) != eprop)
	  {
	    newprops = gh_cons (gh_car (prev), newprops);
	    prev = gh_cdr (prev);
	  }

	newprops = scm_reverse_x (newprops, gh_cdr (prev));
	report_to_l ()->set_property (sym, newprops);
      }
    }
  Music_iterator::do_process_and_next (m);
}
