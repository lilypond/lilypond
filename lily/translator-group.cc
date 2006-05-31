/*
  translator-group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>
*/

#include "translator-group.hh"

#include "context-def.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "international.hh"
#include "main.hh"
#include "music.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "stream-event.hh"
#include "warn.hh"

Translator_group *
Translator_group::get_daddy_translator () const
{
  return context ()->get_parent_context ()->implementation ();
}

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
    programming_error ("already connected to a context");
  context_ = c;
  c->event_source ()->add_listener (GET_LISTENER (eat_event), ly_symbol2scm ("MusicEvent"));
}

void
Translator_group::finalize ()
{
}

bool
translator_accepts_any_of (Translator *tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("events-accepted"),
			   tr->translator_description ());
  ack_ifs = scm_cdr (ack_ifs);
  for (SCM s = ifaces; scm_is_pair (s); s = scm_cdr (s))
    if (scm_c_memq (scm_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}

SCM
find_accept_translators (SCM gravlist, SCM ifaces)
{
  SCM l = SCM_EOL;
  for (SCM s = gravlist; scm_is_pair (s); s = scm_cdr (s))
    {
      Translator *tr = unsmob_translator (scm_car (s));
      if (translator_accepts_any_of (tr, ifaces))
	l = scm_cons (tr->self_scm (), l);
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}

IMPLEMENT_LISTENER (Translator_group, eat_event);
void
Translator_group::eat_event (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  SCM sm = ev->get_property ("music");
  Music *m = unsmob_music (sm);
  try_music (m);
}

bool
Translator_group::try_music (Music *m)
{
  SCM name = scm_sloppy_assq (ly_symbol2scm ("name"),
			      m->get_property_alist (false));

  if (!scm_is_pair (name))
    return false;

  name = scm_cdr (name);
  SCM accept_list = scm_hashq_ref (accept_hash_table_, name, SCM_UNDEFINED);
  if (accept_list == SCM_BOOL_F)
    {
      accept_list = find_accept_translators (get_simple_trans_list (),
					     m->get_property ("types"));
      scm_hashq_set_x (accept_hash_table_, name, accept_list);
    }

  for (SCM p = accept_list; scm_is_pair (p); p = scm_cdr (p))
    {
      Translator *t = unsmob_translator (scm_car (p));
      if (t && t->try_music (m))
	return true;
    }
    
  // We couldn't swallow the event in this context. Try parent.
  Context *p = context ()->get_parent_context ();
  // Global context's translator group is a dummy, so don't try it.
  if (p->get_parent_context())
    // ES todo: Make Translators listeners directly instead.
    return p->implementation ()->try_music (m);
  else
    // We have tried all possible contexts. Give up.
    m->origin ()->warning (_f ("junking event: `%s'", m->name ()));
  return false;
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

  if (dir == DOWN)
    {
      tg->precomputed_translator_foreach (idx);
      tg->call_precomputed_self_method (idx);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    precomputed_recurse_over_translators (unsmob_context (scm_car (s)), idx, dir);

  if (dir == UP)
    {
      tg->precomputed_translator_foreach (idx);
      tg->call_precomputed_self_method (idx);
    }
}

void
recurse_over_translators (Context *c, Translator_method ptr, Translator_group_method tg_ptr, Direction dir)
{
  Translator_group *tg
    = dynamic_cast<Translator_group *> (c->implementation ());

  if (dir == DOWN)
    {
      (tg->*tg_ptr) ();
      translator_each (tg->get_simple_trans_list (), ptr);
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    recurse_over_translators (unsmob_context (scm_car (s)), ptr, tg_ptr, dir);

  if (dir == UP)
    {
      translator_each (tg->get_simple_trans_list (),
		       ptr);

      (tg->*tg_ptr) ();
    }
}

Translator_group::Translator_group ()
{
  simple_trans_list_ = SCM_EOL;
  accept_hash_table_ = SCM_EOL;
  context_ = 0;
  smobify_self ();

  accept_hash_table_ = scm_c_make_hash_table (19);
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
  scm_gc_mark (me->accept_hash_table_);
  return me->simple_trans_list_;
}
