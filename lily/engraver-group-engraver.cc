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

  Engraver_group_engraver * dad_eng =
    context_->get_parent_context ()
    ? dynamic_cast<Engraver_group_engraver*> (context_->get_parent_context ()->implementation ())
    : 0;
  if (dad_eng)
    dad_eng->announce_grob (info);
}

SCM find_acknowledge_engravers (SCM gravlist, SCM meta);
SCM find_accept_engravers (SCM gravlist, SCM music_descr);

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
	{
	  /*
	    it's tempting to put an assert for
	    immutable_property_alist_ == '(), but in fact, some
	    engravers (clef-engraver) add some more information to the
	    immutable_property_alist_ (after it has been '()-ed).

	    We ignore the grob anyway. He who has no name, shall not
	    be helped.  */

	  continue;
	}

      SCM acklist = scm_hashq_ref (acknowledge_hash_table_, nm, SCM_UNDEFINED);
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

	  if (dispatch)
	    scm_hashq_set_x (acknowledge_hash_table_, nm, acklist);
	}


      if (dispatch)
	{
	  dispatch->apply (info);
	}
      else
	{
	  if (acklist == SCM_BOOL_F)
	    {
	      acklist = find_acknowledge_engravers (get_simple_trans_list (),
						    meta);
	      scm_hashq_set_x (acknowledge_hash_table_, nm, acklist);
	    }

	  for (SCM p = acklist; scm_is_pair (p); p = scm_cdr (p))
	    {
	      Translator *t = unsmob_translator (scm_car (p));
	      Engraver *eng = dynamic_cast<Engraver *> (t);
	      if (eng && eng != info.origin_translator ())
		eng->acknowledge_grob (info);
	    }
	}
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
		      /* descr */ "A group of engravers taken together",
		      /* creats*/ "",
		      /* accepts */ "",
		      /* acks  */ "",
		      /* reads */ "",
		      /* write */ "");


bool
engraver_valid (Translator *tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("interfaces-acked"),
			   tr->translator_description ());
  ack_ifs = scm_cdr (ack_ifs);
  for (SCM s = ifaces; scm_is_pair (s); s = scm_cdr (s))
    if (scm_c_memq (scm_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}

SCM
find_acknowledge_engravers (SCM gravlist, SCM ifaces)
{
  SCM l = SCM_EOL;
  for (SCM s = gravlist; scm_is_pair (s); s = scm_cdr (s))
    {
      Translator *tr = unsmob_translator (scm_car (s));
      if (engraver_valid (tr, ifaces))
	l = scm_cons (tr->self_scm (), l);
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}

void
Engraver_group_engraver::derived_mark () const
{
  scm_gc_mark (acknowledge_hash_table_);
}
