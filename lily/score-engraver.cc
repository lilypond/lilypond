/*
  score-grav.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "super-element.hh"
#include "line-of-score.hh"
#include "debug.hh"
#include "item.hh"
#include "score-engraver.hh"
#include "p-score.hh"
#include "musical-request.hh"
#include "score-column.hh"
#include "command-request.hh"
#include "paper-def.hh"


Score_engraver::Score_engraver()
{
  break_penalty_i_ = 0;
  scoreline_l_ =0;
  command_column_l_ =0;
  musical_column_l_ =0;
  breaks_i_ =0;
  pscore_p_ = 0;
}
 
void
Score_engraver::prepare (Moment w)
{
  Global_translator::prepare (w);
  set_columns (new Score_column (w),  new Score_column (w));
  
  break_penalty_i_ = 0;
  post_move_processing();
}

void
Score_engraver::finish()
{
  if ((breaks_i_%8))
    *mlog << "[" << breaks_i_ << "]" << flush;
   
  check_removal();
  removal_processing();
}

void
Score_engraver::do_creation_processing()
{
  scoreline_l_ = pscore_p_->super_elem_l_->line_of_score_l_;
  scoreline_l_->set_bounds(LEFT,get_staff_info().command_pcol_l ());
  command_column_l_->breakable_b_ = true;
  Engraver_group_engraver::do_creation_processing();
}

void
Score_engraver::do_removal_processing()
{
  Engraver_group_engraver::do_removal_processing();
  scoreline_l_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  command_column_l_->breakable_b_ = true;

  typeset_all ();
  set_columns (0,0);
}

void
Score_engraver::process()
{
  process_requests();
  do_announces();
  pre_move_processing();
  check_removal();
}

void
Score_engraver::announce_element (Score_element_info info)
{
  announce_info_arr_.push (info);
  info.origin_grav_l_arr_.push (this);
}

void
Score_engraver::do_announces()
{
  /* All elements are propagated to the top upon announcement. If
     something was created during one run of
     Engraver_group_engraver::do_announces, then
     announce_info_arr_.size() will be nonzero again

     */
  while (announce_info_arr_.size()) 
    {
      for (int i=0; i < announce_info_arr_.size(); i++)
	/*
	  TODO

	  More subtle spacing
	  */
	if (announce_info_arr_[i].req_l_) 
	  {
	    Musical_req *m = announce_info_arr_[i].req_l_->access_Musical_req ();
	    if (m && m->access_Rhythmic_req ()) 
	      {
		musical_column_l_->add_duration (m->duration());
	      }
	  }
      Engraver_group_engraver::do_announces();
    }
}


void
Score_engraver::typeset_element (Score_element *elem_p)
{
  elem_p_arr_.push(elem_p);
}

void
Score_engraver::typeset_all()
{
  for  (int i =0; i < elem_p_arr_.size(); i++) 
    {
      Score_element * elem_p = elem_p_arr_[i];
      if (elem_p->access_Spanner ()) 
	{
	  Spanner *s = elem_p->access_Spanner ();
	  pscore_p_->typeset_unbroken_spanner (s);



	    	  /*
	    do something sensible if spanner not 
	    spanned on 2 items.
	   */
	  Direction d = LEFT;
	  do {
	    if (!s->spanned_drul_[d])
	      {
		s->set_bounds(d, command_column_l_);
		::warning (_f ("Unbound spanner `%s\'", s->name ()));
	      }
	  } while (flip(&d) != LEFT);
	}
      else 
	{
	  Item *item_p = elem_p->access_Item ();
	  pscore_p_->typeset_element (item_p);
	  if (!item_p->axis_group_l_a_[X_AXIS]) {
	    if (item_p->breakable_b_) 
	      command_column_l_->add_element(item_p);
	    else
	      musical_column_l_->add_element(item_p);
	  }
	}
      scoreline_l_->add_element (elem_p);
    }
  elem_p_arr_.clear();
}

void
Score_engraver::do_pre_move_processing()
{
  if (break_penalty_i_ > Break_req::DISALLOW)
    {
      get_staff_info().command_pcol_l ()-> breakable_b_ = true;
      breaks_i_ ++;
      if (! (breaks_i_%8))
	*mlog << "[" << breaks_i_ << "]" << flush;
    }
  // this generates all items.
  Engraver_group_engraver::do_pre_move_processing();
  
  typeset_all();
}

void
Score_engraver::set_columns (Score_column *new_command_l, 
			     Score_column *new_musical_l)
{
  if (command_column_l_ && command_column_l_->linked_b()) 
    {
      pscore_p_->add_column (command_column_l_);
      scoreline_l_->add_element (command_column_l_);
    }
  else 
    {
      delete command_column_l_ ;
      command_column_l_ =0;
    }
  if (new_command_l) 
    {
      command_column_l_ = new_command_l;
      command_column_l_->musical_b_ = false;
    }
  if (musical_column_l_ && musical_column_l_->linked_b()) 
    {
      pscore_p_->add_column (musical_column_l_);
      scoreline_l_->add_element (musical_column_l_);
    }
  else 
    {
      delete musical_column_l_;
      musical_column_l_ = 0;
    }
  
  if (new_musical_l) 
    {
      musical_column_l_ = new_musical_l;
      musical_column_l_->musical_b_ = true;
    }
}


Staff_info
Score_engraver::get_staff_info() const
{
  Staff_info inf = Engraver_group_engraver::get_staff_info();

  inf.command_l_ = command_column_l_;
  inf.musical_l_ = musical_column_l_;
  
  return inf;
}



Music_output*
Score_engraver::get_output_p ()
{
  Music_output * o = pscore_p_;
  pscore_p_=0;
  return o;
}

bool
Score_engraver::do_try_request (Request*r)
{
  bool gotcha = Engraver_group_engraver::do_try_request (r);  

  if (gotcha || !r->access_Command_req ())
    return gotcha;

  Command_req * c = r->access_Command_req ();
  if (c->access_Break_req ())
    {
      Break_req* b = (Break_req*)c->access_Break_req ();
      if (b->penalty_i_ <= Break_req::DISALLOW)
	break_penalty_i_ = b->penalty_i_;
      else if (b->penalty_i_ >= Break_req::FORCE)
	{
	  command_column_l_->break_penalty_i_ = b->penalty_i_;
	  gotcha = true;
	}
    }
  return gotcha;
}

IMPLEMENT_IS_TYPE_B1(Score_engraver,Engraver_group_engraver);
ADD_THIS_TRANSLATOR(Score_engraver);

void
Score_engraver::do_add_processing ()
{
  Translator_group::do_add_processing ();
  assert (output_def_l_->is_type_b (Paper_def::static_name ()));
  assert (!daddy_trans_l_);
  pscore_p_ = new Paper_score;
  pscore_p_->paper_l_ = (Paper_def*)output_def_l_;
}
