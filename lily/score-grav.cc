/*
  score-reg.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "super-elem.hh"
#include "scoreline.hh"
#include "debug.hh"
#include "score-elem.hh"
#include "bar.hh"		// needed for Bar::static_name
#include "staffline.hh"
#include "score-grav.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "score.hh"
#include "musical-request.hh"
#include "score-column.hh"


void
Score_engraver::set_score(Score *s)
{
    Global_translator::set_score(s);
    scoreline_l_ = s->pscore_p_->super_elem_l_->line_of_score_l_;
}

Score_engraver::Score_engraver()
{
    scoreline_l_ =0;
    command_column_l_ =0;
    musical_column_l_ =0;
}

 
void
Score_engraver::prepare(Moment w)
{
    Score_column* c1 = new Score_column(w);
    Score_column* c2 = new Score_column(w);
    
    c1->musical_b_ = false;
    c2->musical_b_ = true;
    
    score_l_->cols_.bottom().add(c1);
    score_l_->cols_.bottom().add(c2);
    set_cols(c1,c2);


    post_move_processing();
}
void
Score_engraver::finish()
{
    check_removal();
    do_removal_processing();
}

void
Score_engraver::do_creation_processing()
{
    scoreline_l_->left_col_l_ = get_staff_info().command_pcol_l();
    scoreline_l_->left_col_l_ ->set_breakable();
    Engraver_group_engraver::do_creation_processing();
}

void
Score_engraver::set_cols(Score_column*c1,Score_column*c2)
{
    command_column_l_ = c1;
    musical_column_l_ = c2;
}

void
Score_engraver::do_removal_processing()
{
    Engraver_group_engraver::do_removal_processing();
    scoreline_l_->right_col_l_ = get_staff_info().command_pcol_l();
    scoreline_l_->right_col_l_ ->set_breakable();
    typeset_all();
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
    info.origin_grav_l_arr_.push(this);
    if (info.elem_l_->name() == Bar::static_name()) {
	get_staff_info().command_pcol_l()->set_breakable();
    } 
        
    announce_info_arr_.push(info);
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
		if (m&&m->rhythmic()) {
		    musical_column_l_->add_duration( m->duration());
		}
	    }
	Engraver_group_engraver::do_announces();
    }
}


void
Score_engraver::typeset_element(Score_elem *elem_p)
{
    musical_item_p_arr_.push(elem_p);
}

void
Score_engraver::typeset_breakable_item(Item * nobreak_p)
{
    if (nobreak_p) {
	nobreak_item_p_arr_.push(nobreak_p);
    }
}

void
Score_engraver::typeset_all()
{
    PCol * c= get_staff_info().command_pcol_l();
    PScore *ps_l = score_l_->pscore_p_;

    for  (int i =0; i < nobreak_item_p_arr_.size(); i++) {
	ps_l->typeset_item(nobreak_item_p_arr_[i], c, 0);
	scoreline_l_->add_dependency(nobreak_item_p_arr_[i]);
    }
    nobreak_item_p_arr_.set_size(0);
    
    for (int i=0; i < musical_item_p_arr_.size(); i++) {
	PCol* m = get_staff_info().musical_pcol_l();
	Score_elem *elem_p = musical_item_p_arr_[i];

	scoreline_l_->add(elem_p);
	if (elem_p->spanner()) {
	    ps_l->typeset_unbroken_spanner(elem_p->spanner());
	} else if (elem_p->item()) {
	    ps_l->typeset_item(elem_p->item(), m, 0);
	} else
	    assert(false);
    }
    musical_item_p_arr_.set_size(0);
}


void
Score_engraver::do_pre_move_processing()
{
    // this generates all items.
    Engraver_group_engraver::do_pre_move_processing();
    
    typeset_all();
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
    bool gotcha = false;  
    for ( int i =0; !gotcha && i < nongroup_l_arr_.size() ; i++)
	gotcha = nongroup_l_arr_[i]->try_request(r);
  
    return gotcha;
}

IMPLEMENT_IS_TYPE_B1(Score_engraver,Engraver_group_engraver);
IMPLEMENT_STATIC_NAME(Score_engraver);
ADD_THIS_ENGRAVER(Score_engraver);

