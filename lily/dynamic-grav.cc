/*
  dynamic-reg.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "debug.hh"
#include "crescendo.hh"
#include "dynamic-grav.hh"
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
Dynamic_engraver::do_try_request (Request * r)
{
  Musical_req * m = r->musical();
  if (!m)
    return false;
  Dynamic_req * d = m->dynamic ();
  if (!d)
    return false;

  if (cresc_p_ && d->span_dynamic ()
      && d->span_dynamic ()->spantype == Span_req::START)
    return false;
  
  dynamic_req_l_arr_.push (m->dynamic());
  return true;
}
void
Dynamic_engraver::do_process_requests()
{
  Crescendo*  new_cresc_p=0;
  for (int i=0; i < dynamic_req_l_arr_.size(); i++)
    {
      Dynamic_req *dreq_l = dynamic_req_l_arr_[i];
      if (dreq_l->absdynamic())
	{
	  Text_def * td_p = new Text_def;
	  td_p->align_i_ = CENTER;
	  String loud = dreq_l->absdynamic()->loudness_str ();
	  td_p->text_str_ = paper()->lookup_l ()->dynamic (loud).tex_;
	  td_p->style_str_ = "dynamic";

	  assert (!dynamic_p_) ; // TODO

	  dynamic_p_ = new Text_item (td_p);
	  announce_element (Score_elem_info (dynamic_p_, dreq_l));
	}
      else if (dreq_l->span_dynamic())
	{

	  Span_dynamic_req* span_l = dreq_l->span_dynamic();
	  if (span_l->spantype == Span_req::STOP)
	    {
	      if (!cresc_p_)
		{
		  span_l->warning (_("Can't find cresc to end "));
		}
	      else
		{
		  assert (!to_end_cresc_p_);
		  to_end_cresc_p_ =cresc_p_;
		  cresc_p_ = 0;
		}
	    }
	  else if (span_l->spantype == Span_req::START)
	    {
	      cresc_req_l_ = span_l;
	      assert (!new_cresc_p);
	      new_cresc_p  = new Crescendo;
	      new_cresc_p->grow_dir_ = span_l->dynamic_dir_;
	      announce_element (Score_elem_info (new_cresc_p, span_l));
	    }
	}
    }

  if (new_cresc_p)
    {
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

      to_end_cresc_p_->set_bounds(RIGHT,get_staff_info().musical_l ());
      to_end_cresc_p_->add_support (s_l);
      typeset_element (to_end_cresc_p_);
      to_end_cresc_p_ = 0;
    }
  if (dynamic_p_)
    {
      
      dynamic_p_->add_support (s_l);
      typeset_element (dynamic_p_);
      dynamic_p_ = 0;
    }
}


IMPLEMENT_IS_TYPE_B1(Dynamic_engraver,Engraver);
ADD_THIS_TRANSLATOR(Dynamic_engraver);

void
Dynamic_engraver::do_removal_processing ()
{
  if (cresc_p_)
    {
      typeset_element (cresc_p_ );
      
      cresc_req_l_->warning (_("unended crescendo"));
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
Dynamic_engraver::acknowledge_element (Score_elem_info i)
{
  if (i.elem_l_->is_type_b (Note_column::static_name ()))
    {
      if (dynamic_p_) dynamic_p_->add_support (i.elem_l_);
      if (to_end_cresc_p_)
	to_end_cresc_p_->add_support (i.elem_l_);
      if (cresc_p_)
	cresc_p_->add_support (i.elem_l_);
    }
}
