/*
  dynamic-reg.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "crescendo.hh"
#include "dynamic-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "score-column.hh"
#include "staff-sym.hh"
#include "note-column.hh"

Dynamic_engraver::Dynamic_engraver()
{
  do_post_move_processing();
  dir_ = CENTER;
  dynamic_p_ =0;
  to_end_cresc_p_ = cresc_p_ = 0;
  cresc_req_l_ = 0;
}

void
Dynamic_engraver::do_post_move_processing()
{
  dynamic_req_l_arr_.clear();
}

bool
Dynamic_engraver::do_try_music (Music * r)
{
  if(Dynamic_req * d = dynamic_cast <Dynamic_req *> (r))
    {
      for (int i=0; i < dynamic_req_l_arr_.size (); i++)
	if (d->equal_b (dynamic_req_l_arr_[i]))
	  return true;
      
      dynamic_req_l_arr_.push (d);
      return true;
    }
  return false;
}
void
Dynamic_engraver::do_process_requests()
{
  Crescendo*  new_cresc_p=0;
  for (int i=0; i < dynamic_req_l_arr_.size(); i++)
    {
      Dynamic_req *dreq_l = dynamic_req_l_arr_[i];
      if (Absolute_dynamic_req *absd = dynamic_cast<Absolute_dynamic_req *> (dreq_l))
	{

	  if (dynamic_p_)
	    {
	      dynamic_req_l_arr_[i]->warning (_("Got a dynamic already.  Continuing dazed and confused"));
	      continue;
	    }
	  
	  Text_def * td_p = new Text_def;
	  String loud = absd->loudness_str ();
	  td_p->text_str_ =  paper ()->lookup_l (0)->dynamic (loud).str_; // ugh
	  td_p->style_str_ = "dynamic";
	  td_p->align_dir_ = RIGHT;
	  Real nw_f = paper ()->note_width () * 0.8;

	  dynamic_p_ = new Text_item (td_p);
	  dynamic_p_->translate (Offset (nw_f, 0));

	  announce_element (Score_element_info (dynamic_p_, dreq_l));
	}
      else if (Span_dynamic_req *span_l = dynamic_cast <Span_dynamic_req *> (dreq_l))
	{
	  if (span_l->spantype_ == STOP)
	    {
	      if (!cresc_p_)
		{
		  span_l->warning (_ ("can't find (de)crescendo to end"));
		}
	      else
		{
		  assert (!to_end_cresc_p_);
		  to_end_cresc_p_ =cresc_p_;
		  cresc_p_ = 0;
		}
	    }
	  else if (span_l->spantype_ == START)
	    {
	      cresc_req_l_ = span_l;
	      assert (!new_cresc_p);
	      new_cresc_p  = new Crescendo;
	      new_cresc_p->grow_dir_ = span_l->dynamic_dir_;
	      announce_element (Score_element_info (new_cresc_p, span_l));
	    }
	}
    }

  if (new_cresc_p)
    {
      if (cresc_p_)
	{
	  ::warning (_ ("Too many crescendi here"));
	  typeset_element (cresc_p_);
	  cresc_p_ = 0;
	}
      
      cresc_p_ = new_cresc_p;
      cresc_p_->set_bounds(LEFT,get_staff_info().musical_l ());
      if (dynamic_p_)
	{
	  cresc_p_->dyn_b_drul_[LEFT] = true;
	}
    }
}

void
Dynamic_engraver::do_pre_move_processing()
{
  Staff_symbol* s_l = get_staff_info().staff_sym_l_;
  if (to_end_cresc_p_)
    {
      if (dynamic_p_)
	to_end_cresc_p_->dyn_b_drul_[RIGHT]=true;


      Scalar prop = get_property ("dynamicdir");
      if (prop.isnum_b ())
       {
         to_end_cresc_p_->dir_ = (Direction) (int) prop;
       }
      to_end_cresc_p_->set_bounds(RIGHT,get_staff_info().musical_l ());
      to_end_cresc_p_->add_support (s_l);
      typeset_element (to_end_cresc_p_);
      to_end_cresc_p_ = 0;
    }
  if (dynamic_p_)
    {
      Scalar prop = get_property ("dynamicdir");
      if (prop.isnum_b ())
       {
         dynamic_p_->dir_ = (Direction) (int) prop;
       }
      
      dynamic_p_->add_support (s_l);
      typeset_element (dynamic_p_);
      dynamic_p_ = 0;
    }
}



ADD_THIS_TRANSLATOR(Dynamic_engraver);

void
Dynamic_engraver::do_removal_processing ()
{
  if (cresc_p_)
    {
      typeset_element (cresc_p_ );
      
      cresc_req_l_->warning (_ ("unended crescendo"));
      cresc_p_ =0;
    }
  if (to_end_cresc_p_)
    {
      typeset_element (to_end_cresc_p_);
      to_end_cresc_p_ =0;
    }
  if (dynamic_p_)
    {
      typeset_element (dynamic_p_);
      dynamic_p_ =0;
    }
}

void
Dynamic_engraver::acknowledge_element (Score_element_info i)
{
  if (dynamic_cast<Note_column *> (i.elem_l_))
    {
      if (dynamic_p_)
	dynamic_p_->add_support (i.elem_l_);

      if (to_end_cresc_p_)
	to_end_cresc_p_->add_support (i.elem_l_);

      if (cresc_p_)
	cresc_p_->add_support (i.elem_l_);
    }
}
