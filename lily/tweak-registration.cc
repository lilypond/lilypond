/*
  tweak-registration.cc --  implement Tweak_registry

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include "object-key-undumper.hh"
#include "tweak-registration.hh"
#include "object-key.hh"
#include "grob.hh"

#include "ly-smobs.icc"


void
Tweak_registry::clear ()
{
  tweaks_.clear ();
  undumper_ = new Object_key_undumper();
  scm_gc_unprotect_object (undumper_->self_scm ());
}

void
Tweak_registry::insert_tweak_from_file (SCM tweak)
{
  SCM skey = scm_car (tweak);

  assert(scm_is_pair (skey) &&
	 scm_car (skey) == ly_symbol2scm ("key"));

  Object_key const * key =  undumper_->get_key (scm_to_int (scm_cadr (skey)));
  if (tweaks_.find (key) == tweaks_.end())
    {
      tweaks_[key] = SCM_EOL;
    }

  tweaks_[key] = scm_cons (scm_cdr (tweak), tweaks_[key]);
}


void
Tweak_registry::insert_grob_tweak (Grob *g, SCM tweak)
{
  Object_key const * key =  g->get_key ();
  if (tweaks_.find (key) == tweaks_.end())
    {
      tweaks_[key] = SCM_EOL;
    }

  tweaks_[key] = scm_cons (tweak, tweaks_[key]);
}


SCM
Tweak_registry::get_tweaks (Grob *g) 
{
  Object_key const *key = g->get_key();
  if (tweaks_.find (key) == tweaks_.end())
    {
      return SCM_EOL;
    }
  return tweaks_[key];
}

Tweak_registry::Tweak_registry ()
{
  undumper_ = 0;
  smobify_self();
  undumper_ = new Object_key_undumper();
  scm_gc_unprotect_object (undumper_->self_scm ());
}

Tweak_registry::~Tweak_registry ()
{
}

SCM
Tweak_registry::list_tweaks ()
{
  SCM retval = SCM_EOL;
  for (Tweak_map::const_iterator i (tweaks_.begin ());
       i != tweaks_.end();
       i++)
    {
      const Object_key *  key = (*i).first;
      for (SCM t = (*i).second; scm_is_pair (t); t = scm_cdr (t))
	{
	  retval = scm_cons (scm_cons (key->self_scm(), scm_car (t)), retval);
	}
    }

  return retval;
}

SCM
Tweak_registry::mark_smob (SCM smob)
{
  Tweak_registry *me = (Tweak_registry*) SCM_CELL_WORD_1(smob);

  for (Tweak_map::const_iterator i (me->tweaks_.begin ());
       i != me->tweaks_.end();
       i++)
    {
      scm_gc_mark ((*i).first->self_scm());
      scm_gc_mark ((*i).second);
    }

  if (me->undumper_)
    scm_gc_mark (me->undumper_->self_scm());
		      
  return SCM_EOL;
}

int
Tweak_registry::print_smob (SCM smob, SCM port, scm_print_state*)
{
  scm_puts ("#<Tweak_registry>", port); 
  return 1;
}

Object_key_undumper*
Tweak_registry::undumper () const
{
  return undumper_;
}

IMPLEMENT_DEFAULT_EQUAL_P(Tweak_registry);
IMPLEMENT_SMOBS(Tweak_registry);


Tweak_registry  * global_registry_;

void
init_global_tweak_registry()
{
  global_registry_ = new Tweak_registry();
}


LY_DEFINE(ly_clear_tweak_registry, "ly:tweak-clear-registry",
	  0,0,0,(),
	  "Clear global tweak registry"
	  )
{
  global_registry_->clear ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE(ly_insert_tweak, "ly:insert-tweak",
	  2,0,0,
	  (SCM grob, SCM tweak),
	  "add new tweak for grob."
	  )
{
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE(gr, grob, SCM_ARG1, __FUNCTION__, "Grob");
  SCM_ASSERT_TYPE(scm_list_p (tweak) == SCM_BOOL_T
		  && ly_c_procedure_p (scm_car (tweak)),
		  tweak, SCM_ARG2, __FUNCTION__, "Tweak");
  
  global_registry_->insert_grob_tweak (gr, tweak);
  return SCM_UNSPECIFIED;
}


LY_DEFINE(ly_tweak_read_keys, "ly:tweak-define-keys",
	  1,0,0,(SCM keys),
	  "Read keys"
	  )
{
  global_registry_->undumper ()->parse_contents (keys); 
  return SCM_UNSPECIFIED;
}


LY_DEFINE(ly_all_tweaks, "ly:all-tweaks",
	  0,0,0,(),
	  "all tweaks"
	  )
{
  return global_registry_->list_tweaks();
}


LY_DEFINE(ly_tweak_read_tweaks, "ly:tweak-define-tweaks",
	  1,0,0,(SCM tweaks),
	  "Read  tweaks"
	  )
{
  for (SCM s = tweaks; scm_is_pair (s); s = scm_cdr (s))
    {
      global_registry_->insert_tweak_from_file (scm_car (s));
    }
  return SCM_UNSPECIFIED;
}
