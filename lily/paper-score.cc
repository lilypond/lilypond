/*
  p-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "main.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "line-of-score.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "scope.hh"
#include "word-wrap.hh"
#include "gourlay-breaking.hh"
#include "paper-stream.hh"
#include "paper-outputter.hh"
#include "file-results.hh"
#include "misc.hh"

Paper_score::Paper_score ()
{
  protected_scms_ = scm_protect_object (gh_cons (SCM_BOOL_T, SCM_EOL));
  paper_l_ =0;
  outputter_l_ =0;
  Line_of_score * line_p = new Line_of_score;
  typeset_unbroken_spanner (line_p);

  line_l_ = line_p;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}

Paper_score::~Paper_score ()
{
  for (int i=span_p_arr_.size (); --i >=0 ; )
    delete span_p_arr_[i];
  for (int i=elem_p_arr_.size (); --i >=0 ; )
    delete elem_p_arr_[i];

  scm_unprotect_object (protected_scms_);
}

void
Paper_score::typeset_element (Score_element * elem_p)
{
  elem_p_arr_.push (elem_p);
  elem_p->pscore_l_ = this;

  // take over protection.
  SCM_CDR(protected_scms_) = gh_cons (elem_p->element_property_alist_,
				      SCM_CDR (protected_scms_));
  scm_unprotect_object (elem_p->element_property_alist_);
  
  SCM p =  elem_p->remove_elt_property (break_helper_only_scm_sym);
  if (p != SCM_BOOL_F)
    break_helpers_arr_.push (elem_p);
}


void
Paper_score::typeset_unbroken_spanner (Spanner*span_p)
{
  span_p_arr_.push (span_p);
  span_p->pscore_l_=this;

  SCM p =  span_p->remove_elt_property (break_helper_only_scm_sym);
  if (p != SCM_BOOL_F)
    break_helpers_arr_.push (span_p);
}

void
Paper_score::add_column (Paper_column *p)
{
  p->set_rank (col_l_arr_.size ());
  col_l_arr_.push (p);
  typeset_element(p);
}



void
Paper_score::print () const
{
#ifndef NPRINT
  if (!check_debug)
    return ;
  DOUT << "Paper_score { ";
  DOUT << "\n elements: ";
  for (int i=0; i < span_p_arr_.size (); i++)
    span_p_arr_[i]->print ();
  for (int i=0; i < elem_p_arr_.size (); i++)
    elem_p_arr_[i]->print();
  
  DOUT << "}\n";
#endif
}

int
Paper_score::find_col_idx (Paper_column const *c) const
{
  Paper_column const *what = c;

  return col_l_arr_.find_i ((Paper_column*)what);
}

Array<Column_x_positions>
Paper_score::calc_breaking ()
{
  Break_algorithm *algorithm_p=0;
  Array<Column_x_positions> sol;
  bool try_wrap = !paper_l_->get_var ("castingalgorithm");

  if (!try_wrap)
    {
      algorithm_p = new Gourlay_breaking ;
      algorithm_p->set_pscore (this);
      sol = algorithm_p->solve ();
      delete algorithm_p;
      if (! sol.size ())
	{
	  warning (_ ("Can't solve this casting problem exactly; revert to Word_wrap"));
	  try_wrap = true;
	}
    }
  if  (try_wrap)
    {
      algorithm_p = new Word_wrap;
      algorithm_p->set_pscore (this);
      sol = algorithm_p->solve ();
      delete algorithm_p;
    }
  return sol;
}



void
Paper_score::process ()
{
  Dictionary<int> type_stats;
  type_stats["Item"] =0;
  type_stats["Spanner"] =0;
  type_stats["Total"]=0;

  print ();
  *mlog << _ ("Preprocessing elements...") << " " << flush;
  line_l_->breakable_col_processing ();
  line_l_->pre_processing ();
  
  *mlog << '\n' << _ ("Calculating column positions...") << " " << flush;
  line_l_->space_processing ();

  Array<Column_x_positions> breaking = calc_breaking ();


  Paper_stream* paper_stream_p = paper_l_->paper_stream_p ();
  outputter_l_ = paper_l_->paper_outputter_p (paper_stream_p, header_l_, origin_str_);

  Link_array<Line_of_score> lines;
  for (int i=0; i < breaking.size (); i++)
    {
      Line_of_score *line_l = line_l_->set_breaking (breaking, i);
      lines.push (line_l);
      if (line_l != line_l_)
	typeset_element (line_l);
    }

  if (experimental_features_global_b)
    *mlog << elem_p_arr_.size ()  + span_p_arr_.size () << _ (" elements. ");

  *mlog << "\n";
  *mlog << _ ("Line ... ");
  line_l_->break_processing ();
  for (int i=0; i < lines.size (); i++)
    {
      *mlog << '[' << flush;
      
      Line_of_score *line_l = lines[i];

      line_l->post_processing ();
      *mlog << i << flush;
      line_l->output_all (i + 1 == lines.size());
      if (experimental_features_global_b)
	*mlog << '(' << elem_p_arr_.size () + span_p_arr_.size () << ')';
      
      *mlog << ']' << flush;
     }
  
  // huh?
  delete outputter_l_;
  delete paper_stream_p;
  outputter_l_ = 0;


  /*
    todo: sort output
   */
  if (experimental_features_global_b)
    {
      for (Dictionary_iter<int> i(type_stats); i.ok(); i++)
	{
	  *mlog << i.key () << ": " << i.val () << " objects\n";
	}
    }
  *mlog << '\n' << flush;
      
}

Link_array<Item>
Paper_score::broken_col_range (Item const*l, Item const*r) const
{
  Link_array<Item> ret;

  l = l->column_l ();
  r = r->column_l ();
  
  int  start = l
    ? find_col_idx (dynamic_cast<Paper_column*> ((Item*)l))+1
    : 0;

  int stop = r
    ? find_col_idx (dynamic_cast<Paper_column*>((Item*)r))
    : col_l_arr_.size ();

  while (start < stop)
    {
      Paper_column *c = col_l_arr_[start];
      if (c->breakable_b () && !c->line_l_)
	ret.push (c);
      start++;
    }

  return ret;
}
