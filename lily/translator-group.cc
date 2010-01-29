/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>,
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
#include "engraver-group.hh"
#include "international.hh"
#include "main.hh"
#include "music.hh"
#include "output-def.hh"
#include "performer-group.hh"
#include "scheme-engraver.hh"
#include "scm-hash.hh"
#include "warn.hh"

void
translator_each (SCM list, Translator_method method)
{
  for (SCM p = list; scm_is_pair (p); p = scm_cdr (p))
    (unsmob_translator (scm_car (p))->*method) ();
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
			 +  context_->context_name ());
    }
  
  context_ = c;
  c->event_source ()->add_listener (GET_LISTENER (create_child_translator),
				    ly_symbol2scm ("AnnounceNewContext"));
  for (SCM tr_list = simple_trans_list_; scm_is_pair (tr_list); tr_list = scm_cdr (tr_list))
    {
      Translator *tr = unsmob_translator (scm_car (tr_list));
      tr->connect_to_context (c);
    }
}

void
Translator_group::disconnect_from_context ()
{
  for (SCM tr_list = simple_trans_list_; scm_is_pair (tr_list); tr_list = scm_cdr (tr_list))
    {
      Translator *tr = unsmob_translator (scm_car (tr_list));
      tr->disconnect_from_context (context_);
    }
  context_->event_source ()->remove_listener (GET_LISTENER (create_child_translator),
					      ly_symbol2scm ("AnnounceNewContext"));
  context_ = 0;
  protected_events_ = SCM_EOL;
}

void
Translator_group::finalize ()
{
}

SCM
filter_performers (SCM ell)
{
  SCM *tail = &ell;
  for (SCM p = ell; scm_is_pair (p); p = scm_cdr (p))
    {
      if (dynamic_cast<Performer *> (unsmob_translator (scm_car (*tail))))
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
      if (dynamic_cast<Engraver *> (unsmob_translator (scm_car (*tail))))
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
IMPLEMENT_LISTENER (Translator_group, create_child_translator);
void
Translator_group::create_child_translator (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  // get from AnnounceNewContext
  SCM cs = ev->get_property ("context");
  Context *new_context = unsmob_context (cs);
  Context_def *def = unsmob_context_def (new_context->get_definition ());
  SCM ops = new_context->get_definition_mods ();
  
  SCM trans_names = def->get_translator_names (ops);

  Translator_group *g = get_translator_group (def->get_translator_group_type ());
  SCM trans_list = SCM_EOL;

  for (SCM s = trans_names; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM definition = scm_car (s);

      Translator *type = 0;
      Translator *instance = type;
      if (ly_is_symbol (definition))
	{
	  type = get_translator (definition);
	  instance = type->clone ();
	}
      else if (ly_is_pair (definition))
	{
	  type = get_translator (ly_symbol2scm ("Scheme_engraver"));
	  instance = type->clone ();
	  dynamic_cast<Scheme_engraver*> (instance)->init_from_scheme (definition);
	}
	 
      if (!type)
	warning (_f ("cannot find: `%s'", ly_symbol2string (scm_car (s)).c_str ()));
      else
	{
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

  recurse_over_translators (new_context,
			    &Translator::initialize,
			    &Translator_group::initialize,
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
      tg->call_precomputed_self_method (idx);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    precomputed_recurse_over_translators (unsmob_context (scm_car (s)), idx, dir);

  if (tg && dir == UP)
    {
      tg->precomputed_translator_foreach (idx);
      tg->call_precomputed_self_method (idx);
    }
}

void
recurse_over_translators (Context *c, Translator_method ptr,
			  Translator_group_method tg_ptr, Direction dir)
{
  Translator_group *tg
    = dynamic_cast<Translator_group *> (c->implementation ());

  if (tg && dir == DOWN)
    {
      (tg->*tg_ptr) ();
      translator_each (tg->get_simple_trans_list (), ptr);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    recurse_over_translators (unsmob_context (scm_car (s)), ptr, tg_ptr, dir);

  if (tg && dir == UP)
    {
      translator_each (tg->get_simple_trans_list (),
		       ptr);

      (tg->*tg_ptr) ();
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
      Translator *tr = unsmob_translator (scm_car (s));
      Translator_void_method_ptr ptrs[TRANSLATOR_METHOD_PRECOMPUTE_COUNT];
      tr->fetch_precomputable_methods (ptrs);

      assert (tr);
      for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
	{
	  if (ptrs[i])
	    precomputed_method_bindings_[i].push_back (Translator_method_binding (tr, ptrs[i]));
	}
    }

  fetch_precomputable_methods (precomputed_self_method_bindings_);
}

void
Translator_group::precomputed_translator_foreach (Translator_precompute_index idx)
{
  vector<Translator_method_binding> &bindings (precomputed_method_bindings_[idx]);
  for (vsize i = 0; i < bindings.size (); i++)
    bindings[i].invoke ();
}

void
Translator_group::fetch_precomputable_methods (Translator_group_void_method ptrs[])
{
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    ptrs[i] = 0;
}

void
Translator_group::call_precomputed_self_method (Translator_precompute_index idx)
{
  if (precomputed_self_method_bindings_[idx])
    (*precomputed_self_method_bindings_[idx]) (this);
}

Translator_group::~Translator_group ()
{
}

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Translator_group);
IMPLEMENT_DEFAULT_EQUAL_P (Translator_group);
IMPLEMENT_TYPE_P (Translator_group, "ly:translator-group?");

int
Translator_group::print_smob (SCM s, SCM port, scm_print_state *)
{
  Translator_group *me = (Translator_group *) SCM_CELL_WORD_1 (s);
  scm_puts ("#<Translator_group ", port);
  scm_puts (me->class_name (), port);
  scm_display (me->simple_trans_list_, port);
  scm_puts (" >", port);
  return 1;
}

SCM
Translator_group::mark_smob (SCM smob)
{
  Translator_group *me = (Translator_group *)SCM_CELL_WORD_1 (smob);

  me->derived_mark ();
  scm_gc_mark (me->protected_events_);
  return me->simple_trans_list_;
}
