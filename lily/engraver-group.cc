/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "context.hh"
#include "dispatcher.hh"
#include "engraver-group.hh"
#include "global-context.hh"
#include "grob.hh"
#include "grob-properties.hh"
#include "paper-score.hh"
#include "translator-dispatch-list.hh"
#include "warn.hh"

void
Engraver_group::override (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  SCM sym = get_property (ev, "symbol");
  Grob_property_info gpi (context (), sym);

  if (from_scm<bool> (get_property (ev, "once")))
    {
      SCM token = gpi.temporary_override (get_property (ev, "property-path"),
                                          get_property (ev, "value"));
      if (scm_is_pair (token))
        {
          Global_context *g = find_global_context (context ());
          g->add_finalization (ly_list (ly_context_matched_pop_property_proc,
                                        context ()->self_scm (), sym, token));
        }
    }
  else
    gpi.push (get_property (ev, "property-path"), get_property (ev, "value"));
}

void
Engraver_group::revert (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  SCM sym = get_property (ev, "symbol");
  Grob_property_info gpi (context (), sym);

  if (from_scm<bool> (get_property (ev, "once")))
    {
      SCM token = gpi.temporary_revert (get_property (ev, "property-path"));
      if (scm_is_pair (token))
        {
          Global_context *g = find_global_context (context ());
          g->add_finalization (ly_list (ly_context_matched_pop_property_proc,
                                        context ()->self_scm (), sym, token));
        }
    }
  else
    gpi.pop (get_property (ev, "property-path"));
}

void
Engraver_group::connect_to_context (Context *c)
{
  Translator_group::connect_to_context (c);
  c->event_source ()->add_listener (GET_LISTENER (this, override),
                                    ly_symbol2scm ("Override"));
  c->event_source ()->add_listener (GET_LISTENER (this, revert),
                                    ly_symbol2scm ("Revert"));
}

void
Engraver_group::disconnect_from_context ()
{
  context ()->event_source ()->remove_listener (GET_LISTENER (this, override),
                                                ly_symbol2scm ("Override"));
  context ()->event_source ()->remove_listener (GET_LISTENER (this, revert),
                                                ly_symbol2scm ("Revert"));
  Translator_group::disconnect_from_context ();
}

void
Engraver_group::announce_grob (Grob_info info, Direction dir,
                               Context *reroute_context)
{
  announce_infos_.push_back (Announce_grob_info (info, dir));

  Context *dad_con
    = reroute_context ? reroute_context : context_->get_parent ();

  Engraver_group *dad_eng
    = dad_con ? dynamic_cast<Engraver_group *> (dad_con->implementation ()) : 0;

  if (dad_eng)
    dad_eng->announce_grob (info, dir);
}

void
Engraver_group::acknowledge_grobs ()
{
  if (!announce_infos_.size ())
    return;

  SCM name_sym = ly_symbol2scm ("name");

  // N.B. announce_infos_ can grow during this loop.
  for (vsize j = 0; j < announce_infos_.size (); j++)
    {
      Announce_grob_info info = announce_infos_[j];

      SCM meta = get_property (info.grob (), "meta");
      SCM nm = ly_assoc (name_sym, meta);
      if (scm_is_pair (nm))
        nm = scm_cdr (nm);
      else
        continue;

      // A grob's interface list depends on its definition, but also
      // on its class (e.g., System adds system-interface and spanner-interface).
      nm = scm_cons (ly_symbol2scm (info.grob ()->class_name ()), nm);

      SCM ackhandle = scm_hash_create_handle_x (
        acknowledge_hash_table_drul_[info.start_end ()], nm, SCM_BOOL_F);

      SCM acklist = scm_cdr (ackhandle);

      if (scm_is_false (acklist))
        {
          SCM ifaces = info.grob ()->interfaces ();
          acklist = Engraver_dispatch_list::create (get_simple_trans_list (),
                                                    ifaces, info.start_end ());

          scm_set_cdr_x (ackhandle, acklist);
        }

      Engraver_dispatch_list *dispatch
        = unsmob<Engraver_dispatch_list> (acklist);

      if (dispatch)
        dispatch->apply (info);
    }
}

/*
  Ugh. This is slightly expensive. We could/should cache the value of
  the group count?
*/
bool
Engraver_group::pending_grobs () const
{
  if (!announce_infos_.empty ())
    return true;
  for (SCM s = context_->children_contexts (); scm_is_pair (s); s = scm_cdr (s))
    {
      Context *c = unsmob<Context> (scm_car (s));
      Engraver_group *group
        = dynamic_cast<Engraver_group *> (c->implementation ());

      if (group && group->pending_grobs ())
        return true;
    }
  return false;
}

void
Engraver_group::do_announces ()
{
  do
    {
      /*
        DOCME: why is this inside the loop?
       */
      for (SCM s = context ()->children_contexts (); scm_is_pair (s);
           s = scm_cdr (s))
        {
          Context *c = unsmob<Context> (scm_car (s));
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
  while (pending_grobs ());
}

Engraver_group::Engraver_group ()
{
  acknowledge_hash_table_drul_[LEFT] = scm_c_make_hash_table (61);
  acknowledge_hash_table_drul_[RIGHT] = scm_c_make_hash_table (61);
}

#include "translator.icc"

ADD_TRANSLATOR_GROUP (Engraver_group,
                      /* doc */
                      R"(
A group of engravers taken together.
                )",

                      /* create */
                      R"(

                )",

                      /* read */
                      R"(

                )",

                      /* write */
                      R"(

                )");

void
Engraver_group::derived_mark () const
{
  scm_gc_mark (acknowledge_hash_table_drul_[LEFT]);
  scm_gc_mark (acknowledge_hash_table_drul_[RIGHT]);
}
