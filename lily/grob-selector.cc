/*
  grob-selector.cc -- implement Grob selection.

  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "context-selector.hh"
#include "context.hh"
#include "grob-selector.hh"
#include "grob.hh"
#include "paper-column.hh"
#include "scm-hash.hh"

Scheme_hash_table *Grob_selector::grobs_ = 0;

void
Grob_selector::register_grob (Context *context, Grob *grob)
{
  if (!grobs_)
    grobs_ = new Scheme_hash_table ();
  int count = 0;
  if (Grob *first = retrieve_grob (identify_grob (context, grob, 0)))
    {
      count = robust_scm2int (first->get_property ("max"), 0);
      count++;
      SCM s = scm_int2num (count);
      first->set_property ("max", s);
      grob->set_property ("count", s);
    }
  grob->set_property ("context", context->self_scm ());
  grobs_->set (identify_grob (context, grob, count), grob->self_scm ());
}

SCM
Grob_selector::identify_grob (Context *context, Grob *grob, int count)
{
  return ly_symbol2scm ((ly_symbol2string (Context_selector::identify_context (context))
			 + ","
			 + grob->name ()
#if 0	// "when" not defined yet?
			 + ","
			 + Paper_column::when_mom (((Item*)grob)->get_column ()).to_string (),
#endif			 
			 + ","
			 + to_string (count)).to_str0 ());
}

SCM
Grob_selector::identify_grob (Grob *grob)
{
  return identify_grob (unsmob_context (grob->get_property ("context")),
			grob,
			robust_scm2int (grob->get_property ("count"), 0));
}

Grob *
Grob_selector::retrieve_grob (SCM key)
{
  return unsmob_grob (grobs_->get (key));
}

LY_DEFINE (ly_grob_id, "ly:grob-id",
	   1, 0, 0, (SCM grob_scm),
	   "Return grob id.")
{
  Grob *grob = unsmob_grob (grob_scm);
  SCM s = Grob_selector::identify_grob (grob);
  return s;
}
