/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "scoreline.hh"
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
     String s ("\\hbox{%<- line of score\n");
     if (error_mark_b_)
	 s+= "\\scorelineerrormark";
     
     
     Real lastpos = cols[0]->hpos_f_;
     for (int i=0; i < cols.size();  i++){
	 PCol* col_l= cols[i];
	 // all items in the current line & staff.
	 String chunk_str;

	    
	 Link_array<Score_elem> elems;
	    
	 if (col_l->error_mark_b_) {
	     chunk_str += String ("\\columnerrormark");
	 }
	 
	 // now output the items.
	 for (iter_top (col_l->its,j); j.ok(); j++) {
	     elems.push (j);
	 }

	 // spanners.
	 for (iter_top (col_l->starters,j); j.ok(); j++) {
	     if (j->name() != name ())
		elems.push (j);
	 }
	 
	 for (int j =0; j< elems.size(); j++) {
	     Offset o = elems[j]->absolute_offset();
	     o[X_AXIS] += cols[i]->hpos_f_;
	     s += elems[j]->TeX_string_without_offset (o);
	 }
     }
     s += "}";
     return s;
}


Line_of_score::Line_of_score()
{
    error_mark_b_ = 0;
}





IMPLEMENT_IS_TYPE_B1(Line_of_score,Spanner);

void
Line_of_score::add (Score_elem*e)
{
    // avoid excess dependencies.
    if (!( e->axis_group_l_a_[0] || e->axis_group_l_a_[1]))
	add_dependency (e);
}

bool
Line_of_score::contains_b (PCol const* c)const
{
    return cols.find_l ((PCol*)c);
}

void
Line_of_score::set_breaking (Array<Col_hpositions> const &breaking)
{
    for (int j=0; j < breaking.size(); j++) {
	const Array<PCol*> &curline (breaking[j].cols);
	const Array<PCol*> &errors (breaking[j].error_col_l_arr_);
	const Array<Real> &config (breaking[j].config);
	
	for (int i=0; i < errors.size(); i++)
	    errors[i]->error_mark_b_ = true;

	Line_of_score *line_l=0;
	Line_of_score *line_p =0;
	
	if (breaking.size() >1) {
	    line_p = (Line_of_score*)clone()->spanner ();
	    line_p->copy_dependencies (*this);
	    line_l = line_p;
	} else 
	    line_l =  this;
	
	((Array<PCol*> &)line_l->cols) = curline;
	line_l->left_col_l_ =  curline[0];
	line_l->right_col_l_= curline.top();
	
	if (line_p) {  
	    pscore_l_->typeset_broken_spanner (line_p);
	    broken_into_l_arr_.push (line_p);
	}
	
	for (int i=0; i < curline.size(); i++){
	    curline[i]->hpos_f_ = config[i];
	    curline[i]->line_l_ = (Line_of_score*)line_l;
	}
    }
}

void
Line_of_score::break_into_pieces (bool)
{
}

Link_array<Line_of_score>
Line_of_score::get_lines()const
{
    Link_array<Line_of_score> ret;

    if (broken_into_l_arr_.size())
	for (int i=0; i < broken_into_l_arr_.size(); i++) {
	    ret.push ((Line_of_score*)broken_into_l_arr_[i]);
	}
    else 
	ret.push ((Line_of_score*)this);	// ugh
    
    return ret;
}

void
Line_of_score::do_print()const
{
    Spanner::do_print();
}

Interval
Line_of_score::do_width()const
{ 
    return Spanner::do_width();
}
