/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "translator.hh"
#include "warn.hh"
#include "translator-group.hh"
#include "context-def.hh"

#include "moment.hh"
#include "ly-smobs.icc"


Translator::~Translator ()
{
}

void
Translator::init ()
{
  simple_trans_list_ = SCM_BOOL_F;
  trans_group_list_ = SCM_EOL;
  properties_scm_ = SCM_EOL;
  definition_ = SCM_EOL;
  daddy_trans_ =0;
  accepts_list_ = SCM_EOL;
}

Translator::Translator ()
{
  self_scm_ = SCM_EOL;
  init ();
  output_def_ = 0;
  smobify_self ();
}

Translator::Translator (Translator const &s)
{
  self_scm_ = SCM_EOL;
  init ();
  output_def_ = s.output_def_;

  smobify_self ();
}

bool
Translator::is_alias (SCM sym) const
{
  return unsmob_context_def (definition_)->is_alias (sym);
}

bool
Translator::try_music (Music *)
{
  return false;
}
			    

Moment
Translator::now_mom () const
{
  return daddy_trans_->now_mom ();
}

void
Translator::do_announces ()
{
}

Music_output_def *
Translator::get_output_def () const
{
  return output_def_;
}

SCM
Translator::internal_get_property (SCM sym) const
{
  return daddy_trans_->internal_get_property (sym);
}

void
Translator::stop_translation_timestep ()
{
}

/*
  this function has 2 properties

  - It is called before try_music()

  - It is called before any user information enters the translators.
  (i.e. any \property or event is not processed yet.)

  */
void
Translator::start_translation_timestep ()
{
}

void
Translator::initialize ()
{
}

void
Translator::finalize ()
{
}


/*

  SMOBS

*/
SCM
Translator::mark_smob (SCM sm)
{
  Translator * me = (Translator*) SCM_CELL_WORD_1 (sm);
  scm_gc_mark (me->simple_trans_list_);
  scm_gc_mark (me->trans_group_list_);
  scm_gc_mark (me->definition_);  
  scm_gc_mark (me->properties_scm_);  
  scm_gc_mark (me->accepts_list_);

  return me->properties_scm_;
}

SCM
Translator::translator_description () const
{
  return SCM_EOL;
}

SCM
Translator::static_translator_description ()const
{
  return SCM_EOL;
}


IMPLEMENT_SMOBS (Translator);
IMPLEMENT_DEFAULT_EQUAL_P (Translator);
IMPLEMENT_TYPE_P(Translator,"ly:translator?");

SCM
Translator::get_simple_trans_list()
{
  return SCM_EOL;
}
