/*
  p-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "main.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "line-of-score.hh"
#include "pcursor.hh"
#include "plist.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "p-col.hh"
#include "scope.hh"
#include "word-wrap.hh"
#include "gourlay-breaking.hh"
#include "paper-stream.hh"
#include "ps-stream.hh"
#include "tex-stream.hh"
#include "paper-outputter.hh"
#include "ps-outputter.hh"
#include "tex-outputter.hh"
#include "file-results.hh"
#include "misc.hh"

// sucking Cygnus egcs - w32
#include "list.tcc"
#include "cursor.tcc"

Paper_score::Paper_score ()
{
  outputter_l_ =0;
  Line_of_score * line_p = new Line_of_score;
  typeset_unbroken_spanner (line_p);

  line_l_ = line_p;
}

Paper_score::~Paper_score ()
{
#if 0
  for (int i=0; i< line_l_arr_.size (); i++)
    line_l_arr_[i]->unlink_all ();

  for (PCursor<Score_element*> i(elem_p_list_.top()); i.ok(); i++)
    {

      if (i->linked_b())
	i->unlink ();
      assert (! i->linked_b ());
    }
#endif
}

void
Paper_score::typeset_element (Score_element * elem_p)
{
  elem_p_list_.bottom ().add (elem_p);
  elem_p->pscore_l_ = this;
  elem_p->add_processing ();
}

void
Paper_score::typeset_broken_spanner (Spanner*span_p)
{
  typeset_element (span_p);
}


void
Paper_score::typeset_unbroken_spanner (Spanner*span_p)
{
  span_p_list_.bottom ().add (span_p);
  span_p->pscore_l_=this;

  // do not init start/stop fields. These are for broken spans only.
  span_p->add_processing ();
}


void
Paper_score::clean_cols ()
{
  int rank_i = 0;
  for (iter_top (col_p_list_,c); c.ok ();)
    {
      c->set_rank (rank_i++);
      c++;
    }
}

void
Paper_score::add_column (Paper_column *p)
{
  col_p_list_.bottom ().add (p);
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
  for (iter_top (elem_p_list_,cc); cc.ok (); cc++)
    cc->print ();
  DOUT << "\n unbroken spanners: ";
  for (iter (span_p_list_.top (), i); i.ok  (); i++)
    i->print ();

  DOUT << "}\n";
#endif
}

PCursor<Paper_column *>
Paper_score::find_col (Paper_column const *c) const
{
  Paper_column const *what = c;

  return col_p_list_.find ((Paper_column*)what);
}


#if 0
void
Paper_score::set_breaking (Array<Column_x_positions> const &breaking)
{
  for (iter (span_p_list_.top (),i); i.ok  ();)
    {
      Spanner *span_p = i.remove_p ();
      if (span_p->broken_b ()
	  || !((Score_element*)span_p)->line_l ())
	{
	  span_p->unlink ();
	  delete span_p;
	}
      else 
	{
	  typeset_broken_spanner (span_p);
	}
    }
  for (iter (elem_p_list_.top (),i); i.ok  () ;)
    {
      Item *i_l =i->access_Item ();
      if (i_l && !i_l->line_l ())
	{
	  i_l->unlink ();
	  Score_element * item_p= i.remove_p ();
	  delete item_p;
	}
      else
	i++;
    }
}
#endif


Array<Column_x_positions>
Paper_score::calc_breaking ()
{
  Break_algorithm *algorithm_p=0;
  Array<Column_x_positions> sol;
  bool try_wrap = ! paper_l_->get_var ("castingalgorithm");

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
  clean_cols ();
  print ();
  *mlog << _ ("Preprocessing elements...") << " " << flush;
      line_l_->breakable_col_processing ();
      line_l_->pre_processing ();
  
      *mlog << '\n' << _ ("Calculating column positions...") << " " << flush;
      line_l_->space_processing ();

  Array<Column_x_positions> breaking = calc_breaking ();

  Paper_stream* paper_stream_p = global_lookup_l->paper_stream_p ();
  outputter_l_ = global_lookup_l->paper_outputter_p (paper_stream_p, paper_l_, header_l_, origin_str_);

  Link_array<Line_of_score> lines;
  for (int i=0; i < breaking.size (); i++)
    {
      Line_of_score *line_l = line_l_->set_breaking (breaking, i);
      lines.push (line_l);
      if (line_l != line_l_)
	typeset_broken_spanner (line_l);
      
    }

  *mlog << "\nLine ... ";
  for (int i=0; i < lines.size (); i++)
    {
      *mlog << '[' << flush;
      
      Line_of_score *line_l = lines[i];
      line_l->break_processing ();
      line_l->post_processing ();
	*mlog << i << flush;
      line_l->output_all ();
	*mlog << ']' << flush;
      remove_line (line_l);
    }
  
  // huh?
  delete outputter_l_;
  delete paper_stream_p;
  outputter_l_ = 0;

  *mlog << '\n' << flush;
}

void
Paper_score::remove_line (Line_of_score *l)
{
  Link_array<Score_element> to_remove;
  for (PCursor<Score_element*> i(elem_p_list_.top ()); i.ok (); )
    {
      if (i->line_l () == l)
	to_remove.push (i.remove_p ());
      else
	i++;
    }

  for (PCursor<Spanner*> i (span_p_list_.top ()); i.ok (); )
    {
      Score_element *e = i.ptr ();
      if (e->line_l () == l)
	to_remove.push (i.remove_p ());
      else
	i++;
    }

  //  l->unlink_all ();
  for (int i=0; i < to_remove.size (); i++)
    {
      to_remove[i]->unlink ();
      assert (!to_remove[i]->linked_b ());
      delete to_remove [i];
    }
}

/** Get all breakable columns between l and r, (not counting l and r).  */
Link_array<Paper_column>
Paper_score::breakable_col_range (Paper_column*l, Paper_column*r) const
{
  Link_array<Paper_column> ret;

  PCursor<Paper_column*> start (l ? find_col (l)+1 : col_p_list_.top ());
  PCursor<Paper_column*> stop (r ? find_col (r) : col_p_list_.bottom ());

  /*
    ugh! windows-suck-suck-suck.
    */
  while (PCursor<Paper_column*>::compare (start,stop) < 0)
    {
      if (start->breakable_b_)
	ret.push (start);
      start++;
    }

  return ret;
}


Link_array<Paper_column>
Paper_score::col_range (Paper_column*l, Paper_column*r) const
{
  Link_array<Paper_column> ret;

  PCursor<Paper_column*> start (l ? find_col (l)+1 : col_p_list_.top ());
  PCursor<Paper_column*> stop (r ? find_col (r) : col_p_list_.bottom ());
  ret.push (l);

  /*
    ugh! windows-suck-suck-suck.
    */
  while (PCursor<Paper_column*>::compare (start,stop) < 0)
    ret.push (start++);
  ret.push (r);
  return ret;
}

Link_array<Item>
Paper_score::broken_col_range (Item const*l_item_l, Item const*r_item_l) const
{
  Link_array<Item> ret;
  Item const*l=l_item_l;
  Item const*r=r_item_l;

  while (! l->is_type_b(Paper_column::static_name ()))
    l = l->axis_group_l_a_[X_AXIS]->access_Score_element ()->access_Item ();

  while (! r->is_type_b(Paper_column::static_name ()))
    r = r->axis_group_l_a_[X_AXIS]->access_Score_element ()->access_Item ();

  PCursor<Paper_column*> start (l ? find_col ((Paper_column*)l)+1 : col_p_list_.top ());
  PCursor<Paper_column*> stop (r ? find_col ((Paper_column*)r) : col_p_list_.bottom ());

  /*
    ugh! windows-suck-suck-suck.
    */
  while (PCursor<Paper_column*>::compare (start,stop) < 0)
    {
      if (start->breakable_b_ && !start->line_l_)
	ret.push (start);
      start++;
    }

  return ret;
}
