/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "translator.hh"
#include "debug.hh"
#include "translator-group.hh"

#include "moment.hh"
#include "ly-smobs.icc"

char const*
Translator::name() const
{
  return classname(this);
}

Translator::~Translator ()
{
}

void
Translator::init ()
{
  simple_trans_list_ = SCM_EOL;
  trans_group_list_ = SCM_EOL;
  properties_scm_ = SCM_EOL;
  definition_ = SCM_EOL;
  daddy_trans_l_ =0;
}

Translator::Translator ()
{
  init ();
  output_def_l_ = 0;
  smobify_self ();

}

Translator::Translator (Translator const &s)
  : Input (s)
{
  init ();
  output_def_l_ = s.output_def_l_;
  type_str_ = s.type_str_;

  smobify_self ();
}

bool
Translator::is_alias_b (String s) const
{
  return s == type_str_;
}

bool
Translator::try_music (Music *)
{
  return false;
}
			    

Moment
Translator::now_mom () const
{
  return daddy_trans_l_->now_mom ();
}


void
Translator::add_processing ()
{
  do_add_processing ();
}

void
Translator::do_add_processing ()
{
}


void
Translator::post_move_processing ()
{
  start_translation_timestep ();
}

void
Translator::removal_processing ()
{
  do_removal_processing ();
}


void
Translator::announces ()
{
  do_announces ();
}


void
Translator::pre_move_processing ()
{
  stop_translation_timestep ();
}



Music_output_def *
Translator::output_def_l () const
{
  return output_def_l_;
}

SCM
Translator::get_property (char const * id) const
{
  return daddy_trans_l_->get_property (ly_symbol2scm (id));
}

SCM
Translator::get_property (SCM sym) const
{
  return daddy_trans_l_->get_property (sym);
}

void
Translator:: stop_translation_timestep ()
{
}

void
Translator::start_translation_timestep ()
{
}

void
Translator::do_announces ()
{
}

void
Translator::do_creation_processing ()
{
}

void
Translator::do_removal_processing ()
{
}


/*

  SMOBS

*/
SCM
Translator::mark_smob (SCM sm)
{
  Translator * me = (Translator*) SCM_CELL_WORD_1(sm);
  scm_gc_mark (me->simple_trans_list_);
  scm_gc_mark (me->trans_group_list_);
  scm_gc_mark (me->definition_);  
  scm_gc_mark (me->properties_scm_);  

  return me->properties_scm_;
}


int
Translator::print_smob (SCM s, SCM port, scm_print_state *)
{
  Translator *sc = (Translator *) gh_cdr (s);
     
  scm_puts ("#<Translator ", port);
  scm_puts ((char *)sc->name (), port);
  scm_display (sc->simple_trans_list_, port);
  /*
    don't try to print properties, that is too much hassle.
   */
  scm_puts (" >", port);

  
  
  return 1;
}



IMPLEMENT_UNSMOB(Translator, translator);
IMPLEMENT_SMOBS(Translator);
IMPLEMENT_DEFAULT_EQUAL_P(Translator);
