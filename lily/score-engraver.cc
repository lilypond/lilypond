/*
  score-engraver.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#include "command-request.hh"
#include "paper-def.hh"
#include "axis-group-interface.hh"
#include "translator-def.hh"

#include "staff-spacing.hh"
#include "note-spacing.hh"

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
      set_columns (new Paper_column (get_property ("NonMusicalPaperColumn")),
		   new Paper_column (get_property ("PaperColumn")));
  
      command_column_->set_grob_property ("breakable", SCM_BOOL_T);


      Grob_info i1 (command_column_);
      i1.origin_trans_ = this;
  
      Grob_info i2 (musical_column_);
      i2.origin_trans_ = this;

  
      announce_grob (i1);
      announce_grob (i2);

      
    }
}

void
Score_engraver::prepare (Moment w)
{
  Global_translator::prepare (w);

  /*
    TODO: don't make columns when skipTypesetting is true.
   */
  make_columns ();
  
  command_column_->set_grob_property ("when", now_mom_.smobbed_copy ());
  musical_column_->set_grob_property ("when", now_mom_.smobbed_copy ());

  
  Translator_group::start_translation_timestep();
}

void
Score_engraver::finish ()
{
  if ((breaks_%8))
    progress_indication ("[" + to_string (breaks_) + "]");
   
  check_removal ();
  removal_processing ();

}

/*
  use start/finish?
 */
void
Score_engraver::initialize ()
{
  Font_metric *fm =
    all_fonts_global->find_afm("feta20");
  if (!fm)
    error (_("Could not find feta20.afm. Fonts have not been installed properly; Aborting"));
   
  unsmob_translator_def (definition_)->apply_property_operations (this);

  assert (dynamic_cast<Paper_def *> (output_def_));
  assert (!daddy_trans_);
  pscore_ = new Paper_score;
  pscore_->paper_ = dynamic_cast<Paper_def*> (output_def_);

  SCM props = get_property ("System");

  pscore_->typeset_line (new System (props));
  
  make_columns ();
  system_ = pscore_->system_;
  system_->set_bound (LEFT, command_column_);
  command_column_->set_grob_property ("breakable", SCM_BOOL_T);

  Engraver_group_engraver::initialize ();
}


void
Score_engraver::finalize ()
{
  Engraver_group_engraver::finalize ();

  Grob * cc
    = unsmob_grob (get_property ("currentCommandColumn"));
  system_->set_bound (RIGHT, cc);
  cc->set_grob_property ("breakable", SCM_BOOL_T);
  
  typeset_all ();
}

void
Score_engraver::one_time_step ()
{
  if (!to_boolean (get_property ("skipTypesetting")))
    {
      process_music ();
      do_announces ();
    }
  
  stop_translation_timestep ();
  check_removal ();


  for (int i = announce_infos_.size(); i--;)
    {
      Grob *g = announce_infos_[i].grob_;
      if (!dynamic_cast<Paper_column*> (g)) // ugh.
	{
      
	  String msg= "Grob "
	    + g->name()
	    + " was created too late!";
	  g->programming_error (msg);
	}
    }
  announce_infos_.clear ();
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
		s->set_bound (d, command_column_);
		/* don't warn for empty/suicided spanners,
		   it makes real warningsinvisible.
		   maybe should be junked earlier? */
		if (!elem->live())
		  ; // gdb hook
		else
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
	      bool br = to_boolean (elem->get_grob_property ("breakable"));
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
  if (to_boolean (command_column_->get_grob_property ("breakable")))
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
      set_property ("currentCommandColumn", new_command->self_scm ());  
    }
  
  if (new_musical)
    {
      set_property ("currentMusicalColumn", new_musical->self_scm ());
    }
}

Music_output*
Score_engraver::get_output ()
{
  Music_output * o = pscore_;
  pscore_=0;
  return o;
}

bool
Score_engraver::try_music (Music*r)
{
  bool gotcha = Engraver_group_engraver::try_music (r);  

  if (!gotcha)
    {
      if (Break_req* b = dynamic_cast<Break_req *> (r))
	{
	  gotcha = true;


	  SCM pen = command_column_->get_grob_property ("penalty");
	  Real total_penalty = gh_number_p (pen)
	    ? gh_scm2double (pen)
	    : 0.0;

	  SCM rpen = b->get_mus_property ("penalty");
	  if (gh_number_p (rpen))
	    total_penalty +=  gh_scm2double (rpen);
	  
	  if (total_penalty > 10000.0) //  ugh. arbitrary.
	    forbid_breaks ();

	  command_column_->set_grob_property ("penalty",
					       gh_double2scm (total_penalty));
	}
    }
   return gotcha;
}

/*
  TODO:  use property Score.breakForbidden = #t
 */
void
Score_engraver::forbid_breaks ()
{
  /*
    result is junked.
   */
  if (command_column_)
    command_column_->set_grob_property ("breakable", SCM_EOL);
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



ENTER_DESCRIPTION(Score_engraver,
/* descr */       "Top level engraver. Takes care of generating columns and the complete  system (ie. System)


This engraver decides whether a column is breakable. The default is
that a column is always breakable. However, when every Bar_engraver
that does not have a barline at a certain point will call
Score_engraver::forbid_breaks to stop linebreaks.  In practice, this
means that you can make a breakpoint by creating a barline (assuming
that there are no beams or notes that prevent a breakpoint.)


",
/* creats*/       "System PaperColumn NonMusicalPaperColumn",
/* acks  */       "note-spacing-interface staff-spacing-interface",
/* reads */       "currentMusicalColumn currentCommandColumn",
/* write */       "");
