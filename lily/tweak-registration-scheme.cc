/*
  tweak-registration-scheme.cc -- implement Tweak_registry bindings

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tweak-registration.hh"
#include "grob.hh"
#include "object-key-undumper.hh"

LY_DEFINE (ly_clear_tweak_registry, "ly:tweak-clear-registry",
	   0, 0, 0, (),
	   "Clear global tweak registry")
{
  global_registry_->clear ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_insert_tweak, "ly:grob-insert-tweak",
	   2, 0, 0,
	   (SCM grob, SCM tweak),
	   "add new tweak for grob.")
{
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE (gr, grob, SCM_ARG1, __FUNCTION__, "Grob");
  SCM_ASSERT_TYPE (scm_list_p (tweak) == SCM_BOOL_T
		   && ly_is_procedure (scm_car (tweak)),
		   tweak, SCM_ARG2, __FUNCTION__, "Tweak");

  global_registry_->insert_grob_tweak (gr, tweak);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_grob_replace_tweak, "ly:grob-replace-tweak",
	   2, 0, 0,
	   (SCM grob, SCM tweak),
	   "Replace tweak for grob.")
{
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE (gr, grob, SCM_ARG1, __FUNCTION__, "Grob");
  SCM_ASSERT_TYPE (scm_list_p (tweak) == SCM_BOOL_T
		   && ly_is_procedure (scm_car (tweak)),
		   tweak, SCM_ARG2, __FUNCTION__, "Tweak");

  global_registry_->replace_grob_tweak (gr, tweak);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_tweak_read_keys, "ly:tweak-define-keys",
	   1, 0, 0, (SCM keys),
	   "Read keys")
{
  global_registry_->undumper ()->parse_contents (keys);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_all_tweaks, "ly:all-tweaks",
	   0, 0, 0, (),
	   "all tweaks")
{
  return global_registry_->list_tweaks ();
}

LY_DEFINE (ly_tweak_read_tweaks, "ly:tweak-define-tweaks",
	   1, 0, 0, (SCM tweaks),
	   "Read tweaks")
{
  for (SCM s = tweaks; scm_is_pair (s); s = scm_cdr (s))
    global_registry_->insert_tweak_from_file (scm_car (s));
  return SCM_UNSPECIFIED;
}
