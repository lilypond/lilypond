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
#include "warn.hh"

Scheme_hash_table *Grob_selector::grobs_ = 0;
Protected_scm Grob_selector::tweaks_ = SCM_EOL;

void
Grob_selector::register_grob (Context *context, Grob *grob)
{
  if (!grobs_)
    grobs_ = new Scheme_hash_table ();
  int count = 0;
  Moment m = context->now_mom ();
  if (Grob *first = retrieve_grob (identify_grob (context, m, grob, 0)))
    {
      count = robust_scm2int (first->get_property ("tweak-count"), 0);
      count++;
      SCM s = scm_int2num (count);
      first->set_property ("tweak-count", s);
      grob->set_property ("tweak-rank", s);
    }
  grob->set_property ("context", context->self_scm ());
  SCM grob_id = identify_grob (context, m, grob, count);
  store_grob (grob_id, grob);
  SCM tweak = ly_assoc_get (grob_id, tweaks_, SCM_BOOL_F);
  if (tweak != SCM_BOOL_F)
    grob->set_property (ly_symbol2string (scm_car (tweak)).to_str0 (),
			scm_cadr (tweak));
}

SCM
Grob_selector::identify_grob (Context *context, Moment m, Grob *grob, int count)
{
  return scm_list_4 (Context_selector::identify_context (context),
		     scm_makfrom0str (m.to_string ().to_str0 ()),
		     scm_makfrom0str (grob->name ().to_str0 ()),
		     scm_int2num (count));
}

SCM
Grob_selector::identify_grob (Grob *grob)
{
  Moment m;
  return identify_grob (unsmob_context (grob->get_property ("context")),
			Paper_column::when_mom (((Item*) grob)->get_column ()),
			grob,
			robust_scm2int (grob->get_property ("tweak-rank"), 0));
}

void
Grob_selector::store_grob (SCM grob_id, Grob *grob)
{
  grobs_->set (ly_to_symbol (grob_id), grob->self_scm ());
}

Grob *
Grob_selector::retrieve_grob (SCM grob_id)
{
  return unsmob_grob (grobs_->get (ly_to_symbol (grob_id)));
}

void
Grob_selector::set_tweaks (SCM tweaks)
{
  tweaks_ = tweaks;
}

LY_DEFINE (ly_grob_id, "ly:grob-id",
	   1, 0, 0, (SCM grob_scm),
	   "Return grob id.")
{
  Grob *grob = unsmob_grob (grob_scm);
  SCM_ASSERT_TYPE (grob, grob_scm, SCM_ARG1, __FUNCTION__, "grob");
  return Grob_selector::identify_grob (grob);
}
