/*
  Translator_group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-output-def.hh"
#include "translator-group.hh"
#include "translator.hh"
#include "warn.hh"
#include "moment.hh"
#include "scm-hash.hh"
#include "context-def.hh"
#include "context.hh"
#include "main.hh"
#include "music.hh"

Translator_group*
Translator_group::get_daddy_translator () const
{
  Translator *t
    = unsmob_translator (get_parent_context ()->get_parent_context ()->implementation_);
  return dynamic_cast<Translator_group*> (t);
}


void
translator_each (SCM list, Translator_method method)
{
  for (SCM p = list; is_pair (p); p = ly_cdr (p))
    (unsmob_translator (ly_car (p))->*method) ();
}


void
Translator_group::initialize ()
{
  SCM tab = scm_make_vector (scm_int2num (19), SCM_BOOL_F);
  get_parent_context ()->set_property ("acceptHashTable", tab);
}


bool
translator_accepts_any_of (Translator*tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("events-accepted"),
			   tr->translator_description ());
  ack_ifs = ly_cdr (ack_ifs);
  for (SCM s = ifaces; is_pair (s); s = ly_cdr (s))
    if (scm_c_memq (ly_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}

SCM
find_accept_translators (SCM gravlist, SCM ifaces)
{
  SCM l = SCM_EOL;
  for (SCM s = gravlist; is_pair (s);  s = ly_cdr (s))
    {
      Translator* tr = unsmob_translator (ly_car (s));
      if (translator_accepts_any_of (tr, ifaces))
	l = scm_cons (tr->self_scm (), l); 
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}

bool
Translator_group::try_music (Music* m)
{
  SCM tab = get_property ("acceptHashTable");
  SCM name = scm_sloppy_assq (ly_symbol2scm ("name"),
			      m->get_property_alist (false));

  if (!is_pair (name))
    return false;

  name = ly_cdr (name);
  SCM accept_list = scm_hashq_ref (tab, name, SCM_UNDEFINED);
  if (accept_list == SCM_BOOL_F)
    {
      accept_list = find_accept_translators (get_simple_trans_list (),
					     m->get_property ("types"));
      scm_hashq_set_x (tab, name, accept_list);
    }

  for (SCM p = accept_list; is_pair (p); p = ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
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
recurse_over_translators (Context * c, Translator_method ptr, Direction dir)
{
  Translator_group * tg
    = dynamic_cast<Translator_group*> (unsmob_translator (c->implementation_));


  /*
    Top down: 
   */
  if (dir == DOWN)
    {
      translator_each (tg->get_simple_trans_list (),
			  ptr);

      (tg->*ptr) ();
    }

  for (SCM s = c->children_contexts () ; is_pair (s);
       s =ly_cdr (s))
    {
      recurse_over_translators (unsmob_context (ly_car (s)), ptr, dir);
    }

  if (dir == UP)
    {
      translator_each (tg->get_simple_trans_list (),
		     ptr);

      (tg->*ptr) ();
    }
}
