/*
  paper-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-score.hh"

#include "all-font-metrics.hh"
#include "book.hh"
#include "gourlay-breaking.hh"
#include "international.hh"
#include "main.hh"
#include "misc.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-column.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"
#include "constrained-breaking.hh"

Paper_score::Paper_score (Output_def *layout)
{
  layout_ = layout;
  system_ = 0;
  systems_ = SCM_EOL;
  paper_systems_ = SCM_EOL;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}

void
Paper_score::derived_mark () const
{
  if (layout_)
    scm_gc_mark (layout_->self_scm ());
  scm_gc_mark (systems_);
  scm_gc_mark (paper_systems_);
}

void
Paper_score::typeset_system (System *system)
{
  if (!system_)
    system_ = system;

  systems_ = scm_cons (system->self_scm (), systems_);
  system->pscore_ = this;
  system->layout_ = layout_;
  system->unprotect ();
}


vector<int>
Paper_score::find_break_indices () const
{
  vector<Grob*> all = root_system ()->columns ();
  vector<int> retval;

  for (vsize i = 0; i < all.size (); i++)
    if (Item::is_breakable (all[i]))
      retval.push_back (i);

  return retval;
}


vector<Column_x_positions>
Paper_score::calc_breaking ()
{
  Break_algorithm *algorithm = 0;
  vector<Column_x_positions> sol;
  
  int system_count = robust_scm2int (layout ()->c_variable ("system-count"), 0);
  if (system_count)
    {
      Constrained_breaking *b = new Constrained_breaking;
      b->resize (system_count);
      algorithm = b;
    }
  else
    algorithm = new Gourlay_breaking;
  
  algorithm->set_pscore (this);
  sol = algorithm->solve ();
  delete algorithm;

  return sol;
}

void
Paper_score::process ()
{
  if (be_verbose_global)
    message (_f ("Element count %d (spanners %d) ",
		 system_->element_count (),
		 system_->spanner_count ()));

  message (_ ("Preprocessing graphical objects...") + " ");

  /* FIXME: Check out why we need this - removing gives assertion failures
     down the road.

     doubly, also done in Score_engraver */
  vector<Grob*> pc (system_->columns ());
  pc[0]->set_property ("breakable", SCM_BOOL_T);
  pc.back ()->set_property ("breakable", SCM_BOOL_T);

  system_->pre_processing ();

  vector<Column_x_positions> breaking = calc_breaking ();
  system_->break_into_pieces (breaking);

  paper_systems_ = system_->get_paper_systems ();
}

System *
Paper_score::root_system () const
{
  return system_;
}

Output_def *
Paper_score::layout () const
{
  return layout_;
}

SCM
Paper_score::get_paper_systems () const
{
  return paper_systems_;
}

