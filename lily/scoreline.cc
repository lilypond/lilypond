/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "scoreline.hh"
#include "staffline.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "p-col.hh"
#include "p-score.hh"


/* To do:
   take out hard coded TeX stuff.
   
   */
String
Line_of_score::TeX_string() const
{
     String s("\\hbox{%<- line of score\n");
     if (error_mark_b_)
	 s+= "\\scorelineerrormark";
     
     
     Real lastpos = cols[0]->hpos;
     for (int i=0; i < cols.size();  i++){
	 PCol* col_l= cols[i];
	 // all items in the current line & staff.
	 String chunk_str;
	 Real delta  = col_l->hpos - lastpos;
	    
	    
	 if (col_l->error_mark_b_) {
	     chunk_str += String("\\columnerrormark");
	 }
	 // now output the items.
	 for (iter_top(col_l->its,j); j.ok(); j++) {
	     chunk_str += j->TeX_string();
	 }
	 // spanners.
	 for (iter_top(col_l->starters,j); j.ok(); j++) {
	     if (j->name() != name())
		 chunk_str += j->TeX_string();
	 }
	 if (chunk_str!="") {
	     // moveover
	     if (delta)
		 s +=String( "\\kern ") + print_dimen(delta);
	     s += chunk_str;
	     lastpos = col_l->hpos;
	 }
     }
     s += "}";
     return s;
}


Line_of_score::Line_of_score()
{
    error_mark_b_ = 0;
}


void
Line_of_score::do_substitute_dependency(Score_elem*o, Score_elem*n)
{
    Spanner_elem_group::do_substitute_dependency(o,n);
    
    int i;
    while ((i =line_arr_.find_i((Spanner_elem_group*)o->spanner())) >=0)
	if (n)
	    line_arr_[i] = (Spanner_elem_group*)n->spanner();
	else 
	    line_arr_.del(i);
}


void
Line_of_score::do_post_processing()
{
    Real y_pos=0;
    for (int i=line_arr_.size(); i--; ) {
	Interval y = line_arr_[i]->height() ;
	if (y.empty_b())
	    continue;
	line_arr_[i]->translate(Offset(0, -y[-1] + y_pos));
	y_pos += y.length();
    }
    translate(Offset(0, -y_pos));
}

IMPLEMENT_STATIC_NAME(Line_of_score);

void
Line_of_score::add_line(Spanner_elem_group*e)
{
    add_element(e);
    line_arr_.push(e);
}

bool
Line_of_score::contains_b(PCol const* c)const
{
    return cols.find_l((PCol*)c);
}

void
Line_of_score::do_pre_processing()
{
    left_col_l_ = pscore_l_->cols.top();
    right_col_l_ = pscore_l_->cols.bottom();
    for (int i=0; i < line_arr_.size(); i++){
	line_arr_[i]->left_col_l_ = left_col_l_;
	line_arr_[i]->right_col_l_ = right_col_l_;
    }
}

void
Line_of_score::set_breaking(Array<Col_hpositions> const &breaking)
{
    for (int j=0; j < breaking.size(); j++) {
	const Array<PCol*> &curline(breaking[j].cols);
	const Array<PCol*> &errors(breaking[j].error_col_l_arr_);
	const Array<Real> &config(breaking[j].config);
	
	for (int i=0; i < errors.size(); i++)
	    errors[i]->error_mark_b_ = true;

	Line_of_score *line_p = (Line_of_score*)clone();
	for (int i=0; i < curline.size(); i++){
	    curline[i]->hpos = config[i];
	    curline[i]->line_l_ = (Line_of_score*)line_p;
	}
	((Array<PCol*> &)line_p->cols) = curline;
	line_p->left_col_l_ =  curline[0];
	line_p->right_col_l_= curline.top();
	pscore_l_->typeset_broken_spanner(line_p);
	broken_into_l_arr_.push(line_p);
    }
}

void
Line_of_score::break_into_pieces()
{
    
}

Link_array<Line_of_score>
Line_of_score::get_lines()const
{
    Link_array<Line_of_score> ret;
    assert(broken_into_l_arr_.size());
    for (int i=0; i < broken_into_l_arr_.size(); i++) {
	ret.push((Line_of_score*)broken_into_l_arr_[i]);
    }
    return ret;
}
