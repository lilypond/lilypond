/*
  score-engraver.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"

#include "line-of-score.hh"
#include "item.hh"
#include "score-engraver.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "command-request.hh"
#include "paper-def.hh"
#include "axis-group-interface.hh"
#include "translator-def.hh"


Score_engraver::Score_engraver()
{
  scoreline_l_ =0;
  command_column_l_ =0;
  musical_column_l_ =0;
  breaks_i_ =0;
  pscore_p_ = 0;
}

void
Score_engraver::make_columns (Moment w)
{
  /*
    ugh.
   */
  if (!command_column_l_
      || *unsmob_moment (command_column_l_->get_grob_property ("when")) != w)
    {
      set_columns (new Paper_column (get_property (ly_symbol2scm ("NonMusicalPaperColumn"))),
		   new Paper_column (get_property (ly_symbol2scm ("PaperColumn"))));
  
      command_column_l_->set_grob_property ("when", w.smobbed_copy());
      musical_column_l_->set_grob_property ("when", w.smobbed_copy());
      command_column_l_->set_grob_property ("breakable", SCM_BOOL_T);

      Grob_info i1(command_column_l_, 0), i2 (musical_column_l_,0);

      i1.origin_trans_l_ = this;
      i2.origin_trans_l_ = this;
      announce_grob (i1);
      announce_grob (i2);
    }
}

void
Score_engraver::prepare (Moment w)
{
  Global_translator::prepare (w);
  make_columns (w);

  post_move_processing();
}

void
Score_engraver::finish()
{
  if ((breaks_i_%8))
    progress_indication ("[" + to_str ( breaks_i_) + "]");
   
  check_removal();
  removal_processing();
}

/*
  use start/finish?
 */
void
Score_engraver::initialize ()
{
  unsmob_translator_def (definition_)->apply_property_operations (this);

  assert (dynamic_cast<Paper_def *> (output_def_l_));
  assert (!daddy_trans_l_);
  pscore_p_ = new Paper_score;
  pscore_p_->paper_l_ = dynamic_cast<Paper_def*>(output_def_l_);

  SCM props = get_property (ly_symbol2scm ("LineOfScore"));

  pscore_p_->typeset_line (new Line_of_score (props));
  
  make_columns (Moment (0));
  scoreline_l_ = pscore_p_->line_l_;

  scoreline_l_->set_bound(LEFT, command_column_l_);
  
  command_column_l_->set_grob_property ("breakable", SCM_BOOL_T);

  Engraver_group_engraver::initialize();
}


void
Score_engraver::finalize()
{
  Engraver_group_engraver::finalize();
  scoreline_l_->set_bound(RIGHT,command_column_l_);
  command_column_l_->set_grob_property ("breakable", SCM_BOOL_T);
  
  typeset_all ();

  set_columns (0,0);
}

void
Score_engraver::one_time_step()
{
  process_music();
  announces();
  pre_move_processing();
  check_removal();
}

void
Score_engraver::announce_grob (Grob_info info)
{
  announce_info_arr_.push (info);
  pscore_p_->line_l_->typeset_grob (info.elem_l_);
}

/* All elements are propagated to the top upon announcement. If
   something was created during one run of
   Engraver_group_engraver::do_announces, then
   announce_info_arr_.size() will be nonzero again
*/
/* junkme? Done by Engraver_group_engraver::do_announces ()?
 */
   
void
Score_engraver::do_announces()
{
  //////  do
    Engraver_group_engraver::do_announces();
    //////while (announce_info_arr_.size());
}


void
Score_engraver::typeset_grob (Grob *elem_p)
{
  if (!elem_p)
    programming_error ("Score_engraver: empty elt\n");
  else

    elem_p_arr_.push (elem_p);
}


void
Score_engraver::typeset_all()
{
  for  (int i =0; i < elem_p_arr_.size(); i++) 
    {
      Grob * elem_p = elem_p_arr_[i];
      
      if (Spanner *s = dynamic_cast <Spanner *> (elem_p))
	{
	    /*
	    do something sensible if spanner not 
	    spanned on 2 items.
	   */
	  Direction d = LEFT;
	  do {
	    if (!s->get_bound (d))
	      {
		s->set_bound(d, command_column_l_);
		::warning (_f ("unbound spanner `%s'", s->name().ch_C()));
	      }
	  } while (flip(&d) != LEFT);
	}
      else 
	{
	  if (!elem_p->parent_l (X_AXIS))
	    {
	      bool br = to_boolean (elem_p->get_grob_property ("breakable"));
	      Axis_group_interface::add_element (br ? command_column_l_ : musical_column_l_, elem_p);

	    }
	}
      if (!elem_p->parent_l(Y_AXIS))
	Axis_group_interface::add_element (scoreline_l_, elem_p);
    }
  elem_p_arr_.clear();
}

void
Score_engraver::stop_translation_timestep()
{
  // this generates all items.
  Engraver_group_engraver::stop_translation_timestep();
  
  typeset_all();
  if (to_boolean (command_column_l_->get_grob_property ("breakable")))
    {
      breaks_i_ ++;
      if (! (breaks_i_%8))
	progress_indication ("[" + to_str ( breaks_i_) + "]");
    }
}

void
Score_engraver::set_columns (Paper_column *new_command_l, 
			     Paper_column *new_musical_l)
{
  Paper_column * news[] = {new_command_l, new_musical_l};
  Paper_column **current[] = {&command_column_l_, &musical_column_l_};

  for (int i=00; i< 2; i++) 
    {
      if (*current[i])
	{
	  scoreline_l_->add_column ((*current[i]));
	}
      if (news[i])
	*current[i] = news[i];
    }

  if (new_musical_l)
    set_property ("currentMusicalColumn", new_musical_l->self_scm ());
  if (new_command_l)
    set_property ("currentCommandColumn", new_command_l->self_scm ());  
}

Music_output*
Score_engraver::get_output_p ()
{
  Music_output * o = pscore_p_;
  pscore_p_=0;
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


	  SCM pen = command_column_l_->get_grob_property  ("penalty");
	  Real total_penalty = gh_number_p (pen)
	    ? gh_scm2double(pen)
	    : 0.0;

	  SCM rpen = b->get_mus_property ("penalty");
	  if (gh_number_p (rpen))
	    total_penalty +=  gh_scm2double (rpen);
	  
	  if (total_penalty > 10000.0) //  ugh. arbitrary.
	    forbid_breaks ();

	  command_column_l_->set_grob_property ("penalty",
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
  command_column_l_->remove_grob_property ("breakable");
}

ADD_THIS_TRANSLATOR(Score_engraver);


