/*
  engraver-group.cc -- implement Engraver_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "dispatcher.hh"
#include "engraver-group.hh"
#include "grob.hh"
#include "paper-score.hh"
#include "translator-dispatch-list.hh"
#include "warn.hh"

IMPLEMENT_LISTENER (Engraver_group, override);
void
Engraver_group::override (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  
  sloppy_general_pushpop_property (context (),
				   ev->get_property ("symbol"),
				   ev->get_property ("property-path"),
				   ev->get_property ("value"));
}

IMPLEMENT_LISTENER (Engraver_group, revert);
void
Engraver_group::revert (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  
  sloppy_general_pushpop_property (context (),
				   ev->get_property ("symbol"),
				   ev->get_property ("property-path"),
				   SCM_UNDEFINED);
}

void
Engraver_group::connect_to_context (Context *c)
{
  Translator_group::connect_to_context (c);
  c->event_source ()->add_listener (GET_LISTENER (override), ly_symbol2scm ("Override"));
  c->event_source ()->add_listener (GET_LISTENER (revert), ly_symbol2scm ("Revert"));
}

void
Engraver_group::disconnect_from_context ()
{
  context ()->event_source ()->remove_listener (GET_LISTENER (override), ly_symbol2scm ("Override"));
  context ()->event_source ()->remove_listener (GET_LISTENER (revert), ly_symbol2scm ("Revert"));
  Translator_group::disconnect_from_context ();
}

void
Engraver_group::announce_grob (Grob_info info)
{
  announce_infos_.push_back (info);

  Engraver_group *dad_eng
    = context_->get_parent_context ()
    ? dynamic_cast<Engraver_group *> (context_->get_parent_context ()->implementation ())
    : 0;

  if (dad_eng)
    dad_eng->announce_grob (info);
}

void
Engraver_group::acknowledge_grobs ()
{
  if (!announce_infos_.size ())
    return;

  SCM name_sym = ly_symbol2scm ("name");
  SCM meta_sym = ly_symbol2scm ("meta");

  for (vsize j = 0; j < announce_infos_.size (); j++)
    {
      Grob_info info = announce_infos_[j];
      
      SCM meta = info.grob ()->internal_get_property (meta_sym);
      SCM nm = scm_assoc (name_sym, meta);
      if (scm_is_pair (nm))
	nm = scm_cdr (nm);
      else
	continue;

      SCM acklist = scm_hashq_ref (acknowledge_hash_table_drul_[info.start_end ()],
				   nm, SCM_BOOL_F);
      
      Engraver_dispatch_list *dispatch
	= Engraver_dispatch_list::unsmob (acklist);

      if (acklist == SCM_BOOL_F)
	{
	  SCM ifaces
	    = scm_cdr (scm_assoc (ly_symbol2scm ("interfaces"), meta));
	  acklist = Engraver_dispatch_list::create (get_simple_trans_list (),
						    ifaces, info.start_end ());

	  dispatch
	    = Engraver_dispatch_list::unsmob (acklist);

	  scm_hashq_set_x (acknowledge_hash_table_drul_[info.start_end ()], nm, acklist);
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
Engraver_group::pending_grob_count () const
{
  int count = announce_infos_.size ();
  for (SCM s = context_->children_contexts ();
       scm_is_pair (s); s = scm_cdr (s))
    {
      Context *c = unsmob_context (scm_car (s));
      Engraver_group *group
	= dynamic_cast<Engraver_group *> (c->implementation ());

      if (group)
	count += group->pending_grob_count ();
    }
  return count;
}

void
Engraver_group::do_announces ()
{
  do
    {
      /*
	DOCME: why is this inside the loop? 
       */
      for (SCM s = context ()->children_contexts ();
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Context *c = unsmob_context (scm_car (s));
	  Engraver_group *group
	    = dynamic_cast<Engraver_group *> (c->implementation ());
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

Engraver_group::Engraver_group ()
{
  acknowledge_hash_table_drul_[LEFT] 
    = acknowledge_hash_table_drul_[RIGHT] 
    = SCM_EOL;
  
  acknowledge_hash_table_drul_[LEFT] = scm_c_make_hash_table (61);
  acknowledge_hash_table_drul_[RIGHT] = scm_c_make_hash_table (61);
}

#include "translator.icc"

ADD_TRANSLATOR_GROUP (Engraver_group,
		      /* doc */
		      "A group of engravers taken together.",

		      /* create */
		      "",

		      /* read */
		      "",

		      /* write */
		      ""
		      );

void
Engraver_group::derived_mark () const
{
  scm_gc_mark (acknowledge_hash_table_drul_[LEFT]);
  scm_gc_mark (acknowledge_hash_table_drul_[RIGHT]);
}
