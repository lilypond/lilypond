/*
  translator-group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"

#include "output-def.hh"
#include "warn.hh"
#include "scm-hash.hh"
#include "context-def.hh"
#include "context.hh"
#include "main.hh"
#include "music.hh"

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
  SCM tab = scm_make_vector (scm_int2num (19), SCM_BOOL_F);
  context ()->set_property ("acceptHashTable", tab);
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

bool
Translator_group::try_music (Music *m)
{
  SCM tab = get_property ("acceptHashTable");
  SCM name = scm_sloppy_assq (ly_symbol2scm ("name"),
			      m->get_property_alist (false));

  if (!scm_is_pair (name))
    return false;

  name = scm_cdr (name);
  SCM accept_list = scm_hashq_ref (tab, name, SCM_UNDEFINED);
  if (accept_list == SCM_BOOL_F)
    {
      accept_list = find_accept_translators (get_simple_trans_list (),
					     m->get_property ("types"));
      scm_hashq_set_x (tab, name, accept_list);
    }

  for (SCM p = accept_list; scm_is_pair (p); p = scm_cdr (p))
    {
      Translator *t = unsmob_translator (scm_car (p));
      if (t && t->try_music (m))
	return true;
    }
  return false;
}

SCM
Translator_group::get_simple_trans_list ()
{
  return simple_trans_list_;
}

void
recurse_over_translators (Context *c, Translator_method ptr, Direction dir)
{
  Translator_group *tg
    = dynamic_cast<Translator_group *> (c->implementation ());

  /*
    Top down:
  */
  if (dir == DOWN)
    {
      translator_each (tg->get_simple_trans_list (),
		       ptr);

      (tg->*ptr) ();
    }

  for (SCM s = c->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    {
      recurse_over_translators (unsmob_context (scm_car (s)), ptr, dir);
    }

  if (dir == UP)
    {
      translator_each (tg->get_simple_trans_list (),
		       ptr);

      (tg->*ptr) ();
    }
}

Translator_group::Translator_group ()
{
  simple_trans_list_ = SCM_EOL;
}

void
Translator_group::derived_mark () const
{
  scm_gc_mark (simple_trans_list_);
}
