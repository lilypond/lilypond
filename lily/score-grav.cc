/*
  score-grav.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "super-elem.hh"
#include "scoreline.hh"
#include "debug.hh"
#include "score-elem.hh"
#include "bar.hh"		// needed for Bar::static_name
#include "score-grav.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "score.hh"
#include "musical-request.hh"
#include "score-column.hh"
#include "command-request.hh"


void
Score_engraver::set_score(Score *s)
{
    Global_translator::set_score(s);
    scoreline_l_ = s->pscore_p_->super_elem_l_->line_of_score_l_;
}

Score_engraver::Score_engraver()
{
    disallow_break_b_ = false;
    scoreline_l_ =0;
    command_column_l_ =0;
    musical_column_l_ =0;
    breaks_i_ =0;
}

 
void
Score_engraver::prepare(Moment w)
{
    set_columns(new Score_column(w),  new Score_column(w));
    
    
    disallow_break_b_ = false;
    post_move_processing();
}

void
Score_engraver::finish()
{
    if ( (breaks_i_%8))
	    *mlog << "[" << breaks_i_ << "]" << flush;
   
    check_removal();
    removal_processing();
}

void
Score_engraver::do_creation_processing()
{
    scoreline_l_->left_col_l_ = get_staff_info().command_pcol_l();
    scoreline_l_->left_col_l_ ->set_breakable();
    Engraver_group_engraver::do_creation_processing();
}

void
Score_engraver::do_removal_processing()
{
    Engraver_group_engraver::do_removal_processing();
    scoreline_l_->right_col_l_ = get_staff_info().command_pcol_l();
    scoreline_l_->right_col_l_ ->set_breakable();
    typeset_all();
    set_columns(0,0);
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
Score_engraver::announce_element(Score_elem_info info)
{
    announce_info_arr_.push(info);
    info.origin_grav_l_arr_.push(this);
        
}
void
Score_engraver::do_announces()
{
    /* All elements are propagated to the top upon announcement. If
      something was created during one run of
      Engraver_group_engraver::do_announces, then
      announce_info_arr_.size() will be nonzero again

      */
    while (announce_info_arr_.size()) {
	for (int i=0; i <announce_info_arr_.size(); i++)
	    /*
	      TODO

	      More subtle spacing
	     */
	    if (announce_info_arr_[i].req_l_) {
		Musical_req *m = announce_info_arr_[i].req_l_->musical();
		if (m && m->rhythmic()) {
		    musical_column_l_->add_duration( m->duration());
		}
	    }
	Engraver_group_engraver::do_announces();
    }
}


void
Score_engraver::typeset_element(Score_elem *elem_p)
{
    if  ( elem_p->item() && elem_p->item()->breakable_b_ ) {
	nobreak_item_p_arr_.push(elem_p->item());
    } else
	musical_item_p_arr_.push(elem_p);
}

void
Score_engraver::typeset_all()
{
    PCol * c= get_staff_info().command_pcol_l();
    Paper_score *ps_l = score_l_->pscore_p_;

    for  (int i =0; i < nobreak_item_p_arr_.size(); i++) {
	ps_l->typeset_item(nobreak_item_p_arr_[i], c);

	// should get rid of this.. .
	scoreline_l_->add_dependency(nobreak_item_p_arr_[i]);
    }
    nobreak_item_p_arr_.clear();
    
    for (int i=0; i < musical_item_p_arr_.size(); i++) {
	PCol* m = get_staff_info().musical_pcol_l();
	Score_elem *elem_p = musical_item_p_arr_[i];

	scoreline_l_->add(elem_p);
	if (elem_p->spanner()) {
	    ps_l->typeset_unbroken_spanner(elem_p->spanner());
	} else if (elem_p->item()) {
	    ps_l->typeset_item(elem_p->item(), m);
	} else
	    assert(false);
    }
    musical_item_p_arr_.clear();
}


void
Score_engraver::do_pre_move_processing()
{
    if ( !disallow_break_b_ ){ 
	get_staff_info().command_pcol_l()->set_breakable();
	breaks_i_ ++;
	if ( ! (breaks_i_%8))
	    *mlog << "[" << breaks_i_ << "]" << flush;
    }
    // this generates all items.
    Engraver_group_engraver::do_pre_move_processing();
    
    typeset_all();
}

void
Score_engraver::set_columns(Score_column *new_command_l, 
			    Score_column *new_musical_l)
{
    if ( command_column_l_ && command_column_l_->used_b() )
	score_l_->pscore_p_->add(command_column_l_);
    else {
	delete command_column_l_ ;
	command_column_l_ =0;
    }
    if (new_command_l) {
	command_column_l_ = new_command_l;
	command_column_l_->musical_b_ = false;
    }
    if ( musical_column_l_ && musical_column_l_->used_b())
	score_l_->pscore_p_->add (musical_column_l_);
    else {
	delete musical_column_l_;
	musical_column_l_ = 0;
    }
    
    if (new_musical_l) {
	musical_column_l_ = new_musical_l;
	musical_column_l_->musical_b_ = true;
    }
}


Staff_info
Score_engraver::get_staff_info()const
{
    Staff_info inf;

    inf.command_l_ = command_column_l_;
    inf.musical_l_ = musical_column_l_;
    return inf;
}

Paper_def*
Score_engraver::paper()const
{
    return score_l_->paper_p_;
}



bool
Score_engraver::do_try_request(Request*r)
{
    bool gotcha = Engraver_group_engraver::do_try_request(r);  
    if ( !gotcha && r->command() && r->command()->disallowbreak())
	    disallow_break_b_ = true;
    return gotcha;
}

IMPLEMENT_IS_TYPE_B1(Score_engraver,Engraver_group_engraver);
ADD_THIS_ENGRAVER(Score_engraver);

