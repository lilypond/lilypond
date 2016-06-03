/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>,
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
#include "main.hh"
#include "music.hh"
#include "output-def.hh"
#include "performer.hh"
#include "performer-group.hh"
#include "scheme-engraver.hh"
#include "scm-hash.hh"
#include "warn.hh"

void
translator_each (SCM list, SCM method)
{
  for (SCM p = list; scm_is_pair (p); p = scm_cdr (p))
    scm_call_1 (method, scm_car (p));
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
  c->event_source ()->add_listener (GET_LISTENER (Translator_group, create_child_translator),
                                    ly_symbol2scm ("AnnounceNewContext"));
  for (SCM tr_list = simple_trans_list_; scm_is_pair (tr_list); tr_list = scm_cdr (tr_list))
    {
      Translator *tr = unsmob<Translator> (scm_car (tr_list));
      tr->connect_to_context (c);
    }
}

void
Translator_group::disconnect_from_context ()
{
  for (SCM tr_list = simple_trans_list_; scm_is_pair (tr_list); tr_list = scm_cdr (tr_list))
    {
      Translator *tr = unsmob<Translator> (scm_car (tr_list));
      tr->disconnect_from_context (context_);
    }
  context_->event_source ()->remove_listener (GET_LISTENER (Translator_group, create_child_translator),
                                              ly_symbol2scm ("AnnounceNewContext"));
  context_ = 0;
  protected_events_ = SCM_EOL;
}

void
Translator_group::finalize ()
{
}

/*
  Both filter_performers and filter_engravers used to use a direct dynamic_cast
  on the unsmobbed translator to be filtered, i.e.,

  if (unsmob<Performer> (scm_car (*tail)))

  but this caused mysterious optimisation issues in several GUB builds.  See
  issue #818 for the background to this change.
*/
SCM
filter_performers (SCM ell)
{
  SCM *tail = &ell;
  for (SCM p = ell; scm_is_pair (p); p = scm_cdr (p))
    {
      if (unsmob<Performer> (scm_car (*tail)))
        *tail = scm_cdr (*tail);
      else
        tail = SCM_CDRLOC (*tail);
    }
  return ell;
}

SCM
filter_engravers (SCM ell)
{
  SCM *tail = &ell;
  for (SCM p = ell; scm_is_pair (p); p = scm_cdr (p))
    {
      if (unsmob<Engraver> (scm_car (*tail)))
        *tail = scm_cdr (*tail);
      else
        tail = SCM_CDRLOC (*tail);
    }
  return ell;
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
  SCM cs = ev->get_property ("context");
  Context *new_context = unsmob<Context> (cs);
  Context_def *def = unsmob<Context_def> (new_context->get_definition ());
  SCM ops = new_context->get_definition_mods ();

  SCM trans_names = def->get_translator_names (ops);

  Translator_group *g = get_translator_group (def->get_translator_group_type ());
  SCM trans_list = SCM_EOL;

  for (SCM s = trans_names; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM definition = scm_car (s);
      bool is_scheme = false;

      Translator *type = 0;
      if (ly_is_symbol (definition))
        type = get_translator (definition);
      else if (ly_is_pair (definition))
        {
          is_scheme = true;
        }
      else if (ly_is_procedure (definition))
        {
          // `definition' is a procedure, which takes the context as
          // an argument and evaluates to an a-list scheme engraver
          // definition.
          definition = scm_call_1 (definition, cs);
          is_scheme = true;
        }

      if (!is_scheme && !type)
        warning (_f ("cannot find: `%s'", ly_symbol2string (scm_car (s)).c_str ()));
      else
        {
          Translator *instance = is_scheme ? new Scheme_engraver (definition)
            : type->clone ();

          SCM str = instance->self_scm ();

          if (instance->must_be_last ())
            {
              SCM cons = scm_cons (str, SCM_EOL);
              if (scm_is_pair (trans_list))
                scm_set_cdr_x (scm_last_pair (trans_list), cons);
              else
                trans_list = cons;
            }
          else
            trans_list = scm_cons (str, trans_list);

          instance->daddy_context_ = new_context;
          instance->unprotect ();
        }
    }

  /* Filter unwanted translator types. Required to make
     \with { \consists "..." } work. */
  if (dynamic_cast<Engraver_group *> (g))
    g->simple_trans_list_ = filter_performers (trans_list);
  else if (dynamic_cast<Performer_group *> (g))
    g->simple_trans_list_ = filter_engravers (trans_list);

  // TODO: scrap Context::implementation
  new_context->implementation_ = g;

  g->connect_to_context (new_context);
  g->unprotect ();

  recurse_over_translators
    (new_context,
     Callback0_wrapper::make_smob<Translator, &Translator::initialize> (),
     Callback0_wrapper::make_smob<Translator_group, &Translator_group::initialize> (),
     DOWN);
}

SCM
Translator_group::get_simple_trans_list ()
{
  return simple_trans_list_;
}

void
precomputed_recurse_over_translators (Context *c, Translator_precompute_index idx, Direction dir)
{
  Translator_group *tg
    = dynamic_cast<Translator_group *> (c->implementation ());

  if (tg && dir == DOWN)
    {
      tg->precomputed_translator_foreach (idx);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    precomputed_recurse_over_translators (unsmob<Context> (scm_car (s)), idx, dir);

  if (tg && dir == UP)
    {
      tg->precomputed_translator_foreach (idx);
    }
}

void
recurse_over_translators (Context *c, SCM ptr,
                          SCM tg_ptr, Direction dir)
{
  Translator_group *tg = c->implementation ();
  SCM tg_scm = tg ? tg->self_scm () : SCM_UNDEFINED;

  if (tg && dir == DOWN)
    {
      scm_call_1 (tg_ptr, tg_scm);
      translator_each (tg->get_simple_trans_list (), ptr);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    recurse_over_translators (unsmob<Context> (scm_car (s)), ptr, tg_ptr, dir);

  if (tg && dir == UP)
    {
      translator_each (tg->get_simple_trans_list (),
                       ptr);

      scm_call_1 (tg_ptr, tg_scm);
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
            precomputed_method_bindings_[i].push_back (Method_instance (ptrs[i], tr));
        }
    }

}

void
Translator_group::precomputed_translator_foreach (Translator_precompute_index idx)
{
  vector<Method_instance> &bindings (precomputed_method_bindings_[idx]);
  for (vsize i = 0; i < bindings.size (); i++)
    bindings[i]();
}

Translator_group::~Translator_group ()
{
}


const char * const Translator_group::type_p_name_ = "ly:translator-group?";

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
