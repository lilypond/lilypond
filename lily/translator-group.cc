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
#include "main.hh"
#include "music.hh"

Translator_group::Translator_group (Translator_group const&s)
  : Translator (s)
{
  iterator_count_ =0;
  
  Scheme_hash_table * tab =  new Scheme_hash_table (*s.properties_dict ());
  properties_scm_ = tab->self_scm ();
  scm_gc_unprotect_object (tab->self_scm ());
}

Scheme_hash_table*
Translator_group::properties_dict () const
{
  return Scheme_hash_table::unsmob (properties_scm_);
}

Translator_group::~Translator_group ()
{
  
  //assert (is_removable ());
}


Translator_group::Translator_group ()
{
  iterator_count_  = 0;
  Scheme_hash_table *tab = new Scheme_hash_table ;
  properties_scm_ = tab->self_scm ();

  scm_gc_unprotect_object (tab->self_scm ());
}

void
Translator_group::check_removal ()
{
  SCM next = SCM_EOL; 
  for (SCM p = trans_group_list_; gh_pair_p (p); p = next)
    {
      next = ly_cdr (p);

      Translator_group *trg =  dynamic_cast<Translator_group*> (unsmob_translator (ly_car (p)));

      trg->check_removal ();
      if (trg->is_removable ())
	terminate_translator (trg);
    }
}

SCM
Translator_group::add_translator (SCM list, Translator *t)
{
  /*
    Must append, since list ordering must be preserved.
   */
  list = gh_append2 (list, gh_cons (t->self_scm (), SCM_EOL));
  t->daddy_trans_ = this;
  t->output_def_ = output_def_;

  return list;
}


void
Translator_group::add_used_group_translator (Translator *t)
{
  trans_group_list_ = add_translator (trans_group_list_,t);
}


void
Translator_group::add_fresh_group_translator (Translator*t)
{
  Translator_group*tg = dynamic_cast<Translator_group*> (t);
  trans_group_list_ = add_translator (trans_group_list_,t);
  scm_gc_unprotect_object (t->self_scm ());

  Context_def * td = unsmob_context_def (tg->definition_);

  /*
    this can not move before add_translator(), because \override
    operations require that we are in the hierarchy.
   */
  td->apply_default_property_operations (tg);

  t->initialize ();
}

bool
Translator_group::try_music (Music* m)
{
  bool hebbes_b = try_music_on_nongroup_children (m);
  
  if (!hebbes_b && daddy_trans_)
    hebbes_b = daddy_trans_->try_music (m);
  
  return hebbes_b ;
}

void
Translator_group::terminate_translator (Translator*r)
{
  r->finalize ();
  /*
    Return value ignored. GC does the rest.
   */
  remove_translator (r);
}


/**
   Remove a translator from the hierarchy.
 */
Translator *
Translator_group::remove_translator (Translator*trans)
{
  assert (trans);

  trans_group_list_ = scm_delq_x (trans->self_scm (), trans_group_list_);
  trans->daddy_trans_ = 0;
  return trans;
}


static void
static_each (SCM list, Method_pointer method)
{
  for (SCM p = list; gh_pair_p (p); p = ly_cdr (p))
    (unsmob_translator (ly_car (p))->*method) ();
  
}

void
Translator_group::each (Method_pointer method) 
{
  static_each (get_simple_trans_list (), method);
  static_each (trans_group_list_, method);
}




/*
  STUBS
*/
void
Translator_group::stop_translation_timestep ()
{
  each (&Translator::stop_translation_timestep);
}

void
Translator_group::start_translation_timestep ()
{
  each (&Translator::start_translation_timestep);
}

void
Translator_group::do_announces ()
{
  each (&Translator::do_announces);
}

void
Translator_group::initialize ()
{
  SCM tab = scm_make_vector (gh_int2scm (19), SCM_BOOL_F);
  set_property ("acceptHashTable", tab);
  each (&Translator::initialize);
}

void
Translator_group::finalize ()
{
  each (&Translator::finalize);
}

bool
translator_accepts_any_of (Translator*tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("events-accepted"),
			   tr->translator_description());
  ack_ifs = gh_cdr (ack_ifs);
  for (SCM s = ifaces; ly_pair_p (s); s = ly_cdr (s))
    if (scm_memq (ly_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}

SCM
find_accept_translators (SCM gravlist, SCM ifaces)
{
  SCM l = SCM_EOL;
  for (SCM s = gravlist; ly_pair_p (s);  s = ly_cdr (s))
    {
      Translator* tr = unsmob_translator (ly_car (s));
      if (translator_accepts_any_of (tr, ifaces))
	l = scm_cons (tr->self_scm (), l); 
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}

bool
Translator_group::try_music_on_nongroup_children (Music *m )
{
  SCM tab = get_property ("acceptHashTable");
  SCM name = scm_sloppy_assq (ly_symbol2scm ("name"),
			      m->get_property_alist (false));

  if (!gh_pair_p (name))
    return false;

  name = gh_cdr (name);
  SCM accept_list = scm_hashq_ref (tab, name, SCM_UNDEFINED);
  if (accept_list == SCM_BOOL_F)
    {
      accept_list = find_accept_translators (get_simple_trans_list (),
					     m->get_mus_property ("types"));
      scm_hashq_set_x (tab, name, accept_list);
    }

  for (SCM p = accept_list; gh_pair_p (p); p = ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      if (t && t->try_music (m))
	return true;
    }
  return false;
}

SCM
Translator_group::properties_as_alist () const
{
  return properties_dict()->to_alist();
}

String
Translator_group::context_name () const
{
  Context_def * td = unsmob_context_def (definition_ );
  return ly_symbol2string (td->get_context_name ());
}



SCM
names_to_translators (SCM namelist, Translator_group*tg)
{
  SCM l = SCM_EOL;
  for (SCM s = namelist; gh_pair_p (s) ; s = ly_cdr (s))
    {
      Translator * t = get_translator (ly_car (s));
      if (!t)
	warning (_f ("can't find: `%s'", s));
      else
	{
	  Translator * tr = t->clone ();
	  SCM str = tr->self_scm ();
	  l = gh_cons (str, l);

	  tr->daddy_trans_ = tg;
	  tr->output_def_  = tg->output_def_;

	  scm_gc_unprotect_object (str);
	}
    }
  return l;
}


SCM
Translator_group::get_simple_trans_list ()
{
  return simple_trans_list_;
}

