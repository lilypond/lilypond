/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>

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

#include "translator-group.hh"

#include "context-def.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "engraver.hh"
#include "engraver-group.hh"
#include "international.hh"
#include "listener.hh"
#include "ly-smob-list.hh"
#include "music.hh"
#include "output-def.hh"
#include "performer.hh"
#include "performer-group.hh"
#include "scheme-engraver.hh"
#include "scm-hash.hh"
#include "warn.hh"

using std::vector;

void
translator_each (SCM list, SCM method)
{
  for (SCM p = list; scm_is_pair (p); p = scm_cdr (p))
    ly_call (method, scm_car (p));
}

void
Translator_group::initialize ()
{
  precompute_method_bindings ();
}

void
Translator_group::connect_to_context (Context *c)
{
  if (context_)
    {
      programming_error ("translator group is already connected to context "
                         + context_->context_name ());
    }

  context_ = c;
  c->event_source ()->add_listener (
    GET_LISTENER (this, create_child_translator),
    ly_symbol2scm ("AnnounceNewContext"));
  for (SCM tr_list = simple_trans_list_; scm_is_pair (tr_list);
       tr_list = scm_cdr (tr_list))
    {
      Translator *tr = unsmob<Translator> (scm_car (tr_list));
      tr->connect_to_context (c);
    }
}

void
Translator_group::disconnect_from_context ()
{
  for (SCM tr_list = simple_trans_list_; scm_is_pair (tr_list);
       tr_list = scm_cdr (tr_list))
    {
      Translator *tr = unsmob<Translator> (scm_car (tr_list));
      tr->disconnect_from_context (context_);
    }
  context_->event_source ()->remove_listener (
    GET_LISTENER (this, create_child_translator),
    ly_symbol2scm ("AnnounceNewContext"));
  context_ = 0;
  protected_events_ = SCM_EOL;
}

void
Translator_group::finalize ()
{
}

/*
  Protects the parameter from being garbage collected. The object is
  protected until the next disconnect_from_context call.

  Whenever a child translator hears an event, the event is added to
  this list. This eliminates the need for derived_mark methods in most
  translators; all incoming events are instead protected by the
  translator group.

  TODO: Should the list also be flushed at the beginning of each new
  moment?
 */
void
Translator_group::protect_event (SCM ev)
{
  protected_events_ = scm_cons (ev, protected_events_);
}

/*
  Create a new translator for a newly created child context. Triggered
  by AnnounceNewContext events.
 */
void
Translator_group::create_child_translator (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  // get from AnnounceNewContext
  SCM cs = get_property (ev, "context");
  Context *new_context = unsmob<Context> (cs);
  Context_def *def = unsmob<Context_def> (new_context->get_definition ());
  SCM ops = new_context->get_definition_mods ();

  Translator_group *g
    = get_translator_group (def->get_translator_group_type ());
  ly_scm_list trans_list;
  auto tail = trans_list.begin ();

  for (SCM trans : ly_scm_list (def->get_translator_names (ops)))
    {
      SCM arg = trans; // in case we want to print the original value below

      if (ly_is_symbol (trans))
        trans = get_translator_creator (trans);
      if (ly_is_procedure (trans))
        trans = ly_call (trans, cs);
      if (ly_cheap_is_list (trans))
        trans = (new Scheme_engraver (trans, new_context))->unprotect ();
      Translator *instance = unsmob<Translator> (trans);
      if (!instance)
        {
          warning (
            _f ("cannot find: `%s'", ly_scm_write_string (arg).c_str ()));
          continue;
        }

      if (instance->must_be_last () || trans_list.empty ())
        {
          tail = trans_list.insert_before (tail, trans);
          ++tail;
        }
      else
        {
          trans_list.insert_before (trans_list.begin (), trans);
        }
    }

  /* Filter unwanted translator types. Required to make
     \with { \consists "..." } work. */
  if (dynamic_cast<Engraver_group *> (g))
    trans_list.remove_if (
      [] (SCM s) { return !unsmob<Translator> (s)->is_layout (); });
  else if (dynamic_cast<Performer_group *> (g))
    trans_list.remove_if (
      [] (SCM s) { return !unsmob<Translator> (s)->is_midi (); });

  g->simple_trans_list_ = trans_list.begin_scm ();

  // TODO: scrap Context::implementation
  new_context->implementation_ = g;

  g->connect_to_context (new_context);
  g->unprotect ();

  recurse_over_translators (new_context, MFP_WRAP (&Translator::initialize),
                            MFP_WRAP (&Translator_group::initialize), DOWN);
}

SCM
Translator_group::get_simple_trans_list ()
{
  return simple_trans_list_;
}

void
precomputed_recurse_over_translators (Context *c,
                                      Translator_precompute_index idx,
                                      Direction dir)
{
  Translator_group *tg = c->implementation ();

  if (tg && dir == DOWN)
    {
      tg->precomputed_translator_foreach (idx);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s); s = scm_cdr (s))
    precomputed_recurse_over_translators (unsmob<Context> (scm_car (s)), idx,
                                          dir);

  if (tg && dir == UP)
    {
      tg->precomputed_translator_foreach (idx);
    }
}

void
recurse_over_translators (Context *c, SCM ptr, SCM tg_ptr, Direction dir)
{
  Translator_group *tg = c->implementation ();
  SCM tg_scm = tg ? tg->self_scm () : SCM_UNDEFINED;

  if (tg && dir == DOWN)
    {
      ly_call (tg_ptr, tg_scm);
      translator_each (tg->get_simple_trans_list (), ptr);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s); s = scm_cdr (s))
    recurse_over_translators (unsmob<Context> (scm_car (s)), ptr, tg_ptr, dir);

  if (tg && dir == UP)
    {
      translator_each (tg->get_simple_trans_list (), ptr);

      ly_call (tg_ptr, tg_scm);
    }
}

Translator_group::Translator_group ()
{
  simple_trans_list_ = SCM_EOL;
  protected_events_ = SCM_EOL;
  context_ = 0;
  smobify_self ();
}

void
Translator_group::derived_mark () const
{
}

void
Translator_group::precompute_method_bindings ()
{
  for (SCM s = simple_trans_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Translator *tr = unsmob<Translator> (scm_car (s));
      SCM ptrs[TRANSLATOR_METHOD_PRECOMPUTE_COUNT];
      tr->fetch_precomputable_methods (ptrs);

      assert (tr);
      for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
        {
          if (!SCM_UNBNDP (ptrs[i]))
            precomputed_method_bindings_[i].push_back (
              Method_instance (ptrs[i], tr));
        }
    }
}

void
Translator_group::precomputed_translator_foreach (
  Translator_precompute_index idx)
{
  vector<Method_instance> &bindings (precomputed_method_bindings_[idx]);
  for (vsize i = 0; i < bindings.size (); i++)
    bindings[i]();
}

Translator_group::~Translator_group ()
{
}

const char *const Translator_group::type_p_name_ = "ly:translator-group?";

int
Translator_group::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Translator_group ", port);
  scm_puts (class_name (), port);
  scm_display (simple_trans_list_, port);
  scm_puts (" >", port);
  return 1;
}

SCM
Translator_group::mark_smob () const
{
  derived_mark ();
  scm_gc_mark (protected_events_);
  return simple_trans_list_;
}
