/*
  engraver-group-engraver.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver-group-engraver.hh"

#include "warn.hh"
#include "paper-score.hh"
#include "grob.hh"
#include "context.hh"
#include "translator-dispatch-list.hh"

void
Engraver_group_engraver::announce_grob (Grob_info info)
{
  announce_infos_.push (info);

  Engraver_group_engraver *dad_eng
    = context_->get_parent_context ()
    ? dynamic_cast<Engraver_group_engraver *> (context_->get_parent_context ()->implementation ())
    : 0;
  if (dad_eng)
    dad_eng->announce_grob (info);
}

void
Engraver_group_engraver::acknowledge_grobs ()
{
  if (!announce_infos_.size ())
    return;

  SCM name_sym = ly_symbol2scm ("name");
  SCM meta_sym = ly_symbol2scm ("meta");

  for (int j = 0; j < announce_infos_.size (); j++)
    {
      Grob_info info = announce_infos_[j];

      SCM meta = info.grob ()->internal_get_property (meta_sym);
      SCM nm = scm_assoc (name_sym, meta);
      if (scm_is_pair (nm))
	nm = scm_cdr (nm);
      else
	continue;

      SCM acklist = scm_hashq_ref (acknowledge_hash_table_, nm, SCM_BOOL_F);
      Engraver_dispatch_list *dispatch
	= Engraver_dispatch_list::unsmob (acklist);

      if (acklist == SCM_BOOL_F)
	{
	  SCM ifaces
	    = scm_cdr (scm_assoc (ly_symbol2scm ("interfaces"), meta));
	  acklist = Engraver_dispatch_list::create (get_simple_trans_list (),
						    ifaces);

	  dispatch
	    = Engraver_dispatch_list::unsmob (acklist);

	  scm_hashq_set_x (acknowledge_hash_table_, nm, acklist);
	}

      if (dispatch)
	dispatch->apply (info);
    }
}

/*
  Ugh. This is slightly expensive. We could/should cache the value of
  the group count?
*/
int
Engraver_group_engraver::pending_grob_count () const
{
  int count = announce_infos_.size ();
  for (SCM s = context_->children_contexts ();
       scm_is_pair (s); s = scm_cdr (s))
    {
      Context *c = unsmob_context (scm_car (s));
      Engraver_group_engraver *group
	= dynamic_cast<Engraver_group_engraver *> (c->implementation ());

      if (group)
	count += group->pending_grob_count ();
    }
  return count;
}

void
Engraver_group_engraver::do_announces ()
{
  do
    {
      for (SCM s = context ()->children_contexts ();
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Context *c = unsmob_context (scm_car (s));
	  Engraver_group_engraver *group
	    = dynamic_cast<Engraver_group_engraver *> (c->implementation ());
	  if (group)
	    group->do_announces ();
	}

      while (1)
	{
	  precomputed_translator_foreach (PROCESS_ACKNOWLEDGED);
	  if (announce_infos_.size () == 0)
	    break;

	  acknowledge_grobs ();
	  announce_infos_.clear ();
	}
    }
  while (pending_grob_count () > 0);
}

Engraver_group_engraver::Engraver_group_engraver ()
{
  acknowledge_hash_table_ = SCM_EOL;
  acknowledge_hash_table_ = scm_c_make_hash_table (61);
}

#include "translator.icc"

ADD_TRANSLATOR_GROUP (Engraver_group_engraver,
		      /* doc */ "A group of engravers taken together",
		      /* create */ "",
		      /* accept */ "",
		      /* read */ "",
		      /* write */ "");


void
Engraver_group_engraver::derived_mark () const
{
  scm_gc_mark (acknowledge_hash_table_);
}
