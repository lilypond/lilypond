/*
  dynamic-reg.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "crescendo.hh"
#include "dynamic-engraver.hh"
#include "musical-request.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "score-column.hh"
#include "staff-sym.hh"
#include "note-column.hh"
#include "g-text-item.hh"
#include "g-staff-side.hh"
#include "engraver.hh"
#include "stem.hh"
#include "note-head.hh"

/**
   print text & hairpin dynamics.
 */
class Dynamic_engraver : public Engraver
{
  G_text_item * text_p_;
  G_staff_side_item * staff_side_p_;
  
  Crescendo * to_end_cresc_p_;
  Crescendo * cresc_p_;
  Span_dynamic_req * cresc_req_l_;
  Array<Dynamic_req*> dynamic_req_l_arr_;
  void  typeset_all ();
public:
  VIRTUAL_COPY_CONS(Translator);
  Dynamic_engraver();
  
protected:
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
};



Dynamic_engraver::Dynamic_engraver()
{
  do_post_move_processing();
  text_p_ =0;
  staff_side_p_ =0;
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
	  if (text_p_)
	    {
	      dynamic_req_l_arr_[i]->warning (_("Got a dynamic already.  Continuing dazed and confused"));
	      continue;
	    }
	  
	  String loud = absd->loudness_str_;

	  text_p_ = new G_text_item;
	  text_p_->text_str_ =  loud; // ugh
	  Scalar prop = get_property ("dynamicStyle", 0);

	  text_p_->style_str_ = prop.length_i () ? prop :  "dynamic";

	  staff_side_p_ = new G_staff_side_item;
	  staff_side_p_->set_victim (text_p_);
	  staff_side_p_->axis_ = Y_AXIS;
	  

	  prop = get_property ("dynamicDir", 0);
	  if (prop.isnum_b ())
	    {
	      staff_side_p_->dir_ = (Direction) (int) prop;
	    }


	  announce_element (Score_element_info (text_p_, dreq_l));
	  announce_element (Score_element_info (staff_side_p_, dreq_l));
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
		  Scalar prop = get_property ("dynamicDir", 0);
		  if (prop.isnum_b ())
		    {
		      to_end_cresc_p_->dir_ = (Direction) (int) prop;
		    }
		  
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
      if (text_p_)
	{
	  cresc_p_->dyn_b_drul_[LEFT] = true;
	  if (to_end_cresc_p_)
	    to_end_cresc_p_->dyn_b_drul_[RIGHT] = true;
	}
    }
}

void
Dynamic_engraver::do_pre_move_processing()
{
  Staff_symbol* s_l = get_staff_info().staff_sym_l_;
  if (to_end_cresc_p_)
    to_end_cresc_p_->add_support (s_l);
  if (staff_side_p_)
    {
      staff_side_p_->add_support (s_l);
      //      staff_side_p_->dim_cache_[Y_AXIS].parent_l_ =  &s_l->dim_cache_[Y_AXIS];
    }

  typeset_all ();
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
  typeset_all ();
}


void
Dynamic_engraver::typeset_all ()
{  
  if (to_end_cresc_p_)
    {
      to_end_cresc_p_->set_bounds(RIGHT,get_staff_info().musical_l ());
      typeset_element (to_end_cresc_p_);
      to_end_cresc_p_ =0;
    }
  if (text_p_)
    {
      typeset_element (text_p_);
      typeset_element (staff_side_p_);
      text_p_ =0;
      staff_side_p_ =0;
    }
}


void
Dynamic_engraver::acknowledge_element (Score_element_info i)
{
  if (dynamic_cast<Stem *> (i.elem_l_)
      || dynamic_cast<Note_head *> (i.elem_l_)
      )
    {
      if (staff_side_p_)
	staff_side_p_->add_support (i.elem_l_);

      if (to_end_cresc_p_)
	to_end_cresc_p_->add_support (i.elem_l_);

      if (cresc_p_)
	cresc_p_->add_support (i.elem_l_);
    }
}
