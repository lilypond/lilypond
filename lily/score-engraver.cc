/*
  score-engraver.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "all-font-metrics.hh"
#include "afm.hh"
#include "warn.hh"
#include "main.hh"
#include "system.hh"
#include "item.hh"
#include "score-engraver.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "axis-group-interface.hh"
#include "context-def.hh"
#include "staff-spacing.hh"
#include "note-spacing.hh"
#include "context.hh"
#include "global-context.hh"




/*
  TODO: the column creation logic is rather hairy. Revise it.
 */
Score_engraver::Score_engraver ()
{
  system_ =0;
  command_column_ =0;
  musical_column_ =0;
  breaks_ =0;
  pscore_ = 0;
}

void
Score_engraver::make_columns ()
{
  /*
    ugh.
   */
  if (!command_column_)
    {
      SCM nmp
	= updated_grob_properties (get_parent_context (),
				   ly_symbol2scm ("NonMusicalPaperColumn"));
      SCM pc = updated_grob_properties (get_parent_context (),
					ly_symbol2scm ("PaperColumn"));
      
      set_columns (new Paper_column (nmp), new Paper_column (pc));

      Grob_info i1;
      i1.grob_ = command_column_;
      i1.origin_trans_ = this;
  
      announce_grob (i1);

      Grob_info i2;
      i2.grob_ = musical_column_;
      i2.origin_trans_ = this;

      announce_grob (i2);
    }
}

void
Score_engraver::prepare (Moment m)
{
  /*
    TODO: don't make columns when skipTypesetting is true.
   */
  make_columns ();

  SCM w = m.smobbed_copy ();
  command_column_->set_property ("when", w);
  musical_column_->set_property ("when", w);
  
  recurse_over_translators (get_parent_context (), &Translator::start_translation_timestep, DOWN);
}

void
Score_engraver::finish ()
{
  if ((breaks_%8))
    progress_indication ("[" + to_string (breaks_) + "]");

  recurse_over_translators (get_parent_context (), &Translator::finalize, UP);
}

/*
  use start/finish?
 */
void
Score_engraver::initialize ()
{
  Font_metric *fm = all_fonts_global->find_afm ("feta20");
  if (!fm)
    error (_f ("can't find `%s'", "feta20.afm")
	   + "\n" +_ ("Fonts have not been installed properly.  Aborting"));
   

  pscore_ = new Paper_score;
  pscore_->paper_ = dynamic_cast<Paper_def*> (get_output_def ());

  SCM props = updated_grob_properties (get_parent_context (), ly_symbol2scm ("System"));

  pscore_->typeset_line (new System (props));
  
  make_columns ();
  system_ = pscore_->system_;
  system_->set_bound (LEFT, command_column_);
  command_column_->set_property ("breakable", SCM_BOOL_T);

  Engraver_group_engraver::initialize ();
}


void
Score_engraver::finalize ()
{
  Score_translator::finalize ();

  Grob * cc
    = unsmob_grob (get_property ("currentCommandColumn"));
  system_->set_bound (RIGHT, cc);
  cc->set_property ("breakable", SCM_BOOL_T);
  
  typeset_all ();
}


void
Score_engraver::one_time_step ()
{
  if (!to_boolean (get_property ("skipTypesetting")))
    {
      recurse_over_translators (get_parent_context (), &Engraver::process_music, UP);
      recurse_over_translators (get_parent_context (), &Engraver::do_announces, UP);
    }
  
  recurse_over_translators (get_parent_context (), &Translator::stop_translation_timestep, UP);
}

void
Score_engraver::announce_grob (Grob_info info)
{
  announce_infos_.push (info);
  pscore_->system_->typeset_grob (info.grob_);
}

void
Score_engraver::typeset_grob (Grob *elem)
{
  if (!elem)
    programming_error ("Score_engraver: empty elt\n");
  else
    elems_.push (elem);
}

void
Score_engraver::typeset_all ()
{
  for (int i =0; i < elems_.size (); i++) 
    {
      Grob * elem = elems_[i];
      
      if (Spanner *s = dynamic_cast <Spanner *> (elem))
	{
	  /*
	    do something sensible if spanner not 
	    spanned on 2 items.
	  */
	  Direction d = LEFT;
	  do {
	    if (!s->get_bound (d))
	      {
		Grob * cc
		  = unsmob_grob (get_property ("currentCommandColumn"));
		s->set_bound (d, cc);
		/* don't warn for empty/suicided spanners,
		   it makes real warningsinvisible.
		   maybe should be junked earlier? */
		if (elem->live ())
		  elem->warning (_f ("unbound spanner `%s'", s->name ().to_str0 ()));
	      }
	  }
	  while (flip (&d) != LEFT);

	  if (dynamic_cast<Item*> (s->get_parent (Y_AXIS)))
	    programming_error ("Spanner Y-parent is an item.");
	}
      else 
	{
	  if (!elem->get_parent (X_AXIS))
	    {
	      bool br = to_boolean (elem->get_property ("breakable"));
	      Axis_group_interface::add_element (br ? command_column_ : musical_column_, elem);

	    }
	}
      if (!elem->get_parent (Y_AXIS))
	Axis_group_interface::add_element (system_, elem);
    }
  elems_.clear ();
}

void
Score_engraver::stop_translation_timestep ()
{
  // this generates all items.
  Engraver_group_engraver::stop_translation_timestep ();
  
  typeset_all ();
  if (to_boolean (command_column_->get_property ("breakable")))
    {
      breaks_ ++;
      if (! (breaks_%8))
	progress_indication ("[" + to_string (breaks_) + "]");
    }


  system_->add_column (command_column_);
  system_->add_column (musical_column_);
  
  command_column_ = 0;
  musical_column_ = 0;
}

void
Score_engraver::set_columns (Paper_column *new_command, 
			     Paper_column *new_musical)
{
  assert (!command_column_ && !musical_column_);

  command_column_ = new_command;
  musical_column_ = new_musical;
  if (new_command)
    {
      get_parent_context ()->set_property ("currentCommandColumn", new_command->self_scm ());  
    }
  
  if (new_musical)
    {
      get_parent_context ()->set_property ("currentMusicalColumn", new_musical->self_scm ());
    }
}

Music_output*
Score_engraver::get_output ()
{
  Music_output *o = pscore_;
  ///FIXME WTF?  pscore_ = 0;
  return o;
}

bool
Score_engraver::try_music (Music *m)
{
  if (Engraver_group_engraver::try_music (m))
    return true;

  if (m->is_mus_type ("break-event"))
    {
      SCM pen = command_column_->get_property ("penalty");
      Real total_penalty = ly_c_number_p (pen) ? ly_scm2double (pen) : 0.0;

      SCM mpen = m->get_property ("penalty");
      if (ly_c_number_p (mpen))
	total_penalty += ly_scm2double (mpen);

      command_column_->set_property ("penalty", scm_make_real (total_penalty));

      /* ugh.  arbitrary, hardcoded */
      if (total_penalty > 10000.0)
	forbid_breaks ();

      SCM page_pen = command_column_->get_property ("page-penalty");
      Real total_pp = ly_c_number_p (page_pen) ? ly_scm2double (page_pen) : 0.0;
      SCM mpage_pen = m->get_property ("page-penalty");
      if (ly_c_number_p (mpage_pen))
	total_pp += ly_scm2double (mpage_pen);
      
      command_column_->set_property ("page-penalty", scm_make_real (total_pp));
      return true;
    }
  return false;
}

void
Score_engraver::forbid_breaks ()
{
  if (command_column_)
    command_column_->set_property ("breakable", SCM_EOL);
}
  
void
Score_engraver::acknowledge_grob (Grob_info gi)
{
  if (Staff_spacing::has_interface (gi.grob_))
    {
      Pointer_group_interface::add_grob (command_column_,
					 ly_symbol2scm ("spacing-wishes"),
					 gi.grob_);
    }
  if (Note_spacing::has_interface (gi.grob_))
    {
      Pointer_group_interface::add_grob (musical_column_,
					 ly_symbol2scm ("spacing-wishes"),
					 gi.grob_);
    }
}



ENTER_DESCRIPTION (Score_engraver,
/* descr */       "Top level engraver. Takes care of generating columns and the complete  system (ie. System) "
"\n\n "
"This engraver decides whether a column is breakable. The default is "
"that a column is always breakable. However, when every Bar_engraver "
"that does not have a barline at a certain point will call "
"Score_engraver::forbid_breaks to stop linebreaks.  In practice, this "
"means that you can make a breakpoint by creating a barline (assuming "
"that there are no beams or notes that prevent a breakpoint.) "
,
/* creats*/       "System PaperColumn NonMusicalPaperColumn", 
/* accepts */     "break-event",
/* acks  */       "note-spacing-interface staff-spacing-interface",
/* reads */       "currentMusicalColumn currentCommandColumn",
/* write */       "");
