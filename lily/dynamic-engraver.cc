/*
  dynamic-reg.cc -- implement Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "crescendo.hh"
#include "dynamic-engraver.hh"
#include "musical-request.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "score-column.hh"
#include "staff-symbol.hh"
#include "note-column.hh"
#include "text-item.hh"
#include "staff-side.hh"
#include "engraver.hh"
#include "stem.hh"
#include "note-head.hh"

/**
   print text & hairpin dynamics.
 */
class Dynamic_engraver : public Engraver
{
  Text_item * text_p_;
  Staff_side_item * staff_side_p_;
  Staff_side_spanner * ss_span_p_;
  Staff_side_spanner * to_end_ss_span_p_;
  
  
  Crescendo * to_end_cresc_p_;
  Crescendo * cresc_p_;
  Span_req * cresc_req_l_;
  Array<Request*> dynamic_req_l_arr_;
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
  ss_span_p_ = to_end_ss_span_p_=0;
  cresc_req_l_ = 0;
}

void
Dynamic_engraver::do_post_move_processing()
{
  dynamic_req_l_arr_.clear();
}

bool
Dynamic_engraver::do_try_music (Music * m)
{
  Request * r = dynamic_cast<Request*> (m);

  if(Text_script_req * d = dynamic_cast <Text_script_req *> (r))
    {
      if (d->style_str_ != "dynamic")
	return false;
    }
  else if (Span_req * s =  dynamic_cast <Span_req*> (r))
    {
      if (s-> span_type_str_ != "crescendo"
	  && s->span_type_str_ != "decrescendo")
	return false;
    }
  else
    return false;
  
  for (int i=0; i < dynamic_req_l_arr_.size (); i++)
    if (r->equal_b (dynamic_req_l_arr_[i]))
      return true;
  
  dynamic_req_l_arr_.push (r);
  return true;
}


void
Dynamic_engraver::do_process_requests()
{
  Crescendo*  new_cresc_p=0;
  Staff_side_spanner * new_sss_p =0;
  for (int i=0; i < dynamic_req_l_arr_.size(); i++)
    {
      if (Text_script_req *absd =
	  dynamic_cast<Text_script_req *> ( dynamic_req_l_arr_[i]))
	{
	  if (text_p_)
	    {
	      dynamic_req_l_arr_[i]->warning (_("Got a dynamic already.  Continuing dazed and confused"));
	      continue;
	    }
	  
	  String loud = absd->text_str_;

	  text_p_ = new Text_item;
	  text_p_->text_str_ =  loud; // ugh
	  Scalar prop = get_property ("dynamicStyle", 0);

	  text_p_->style_str_ = prop.length_i () ? prop :  "dynamic";

	  staff_side_p_ = new Staff_side_item;
	  staff_side_p_->set_elt_property (script_priority_scm_sym,
					   gh_int2scm (100));
					   
	  staff_side_p_->set_victim (text_p_);
	  staff_side_p_->axis_ = Y_AXIS;
	  

	  
	  prop = get_property ("verticalDirection", 0);
	  if (prop.isdir_b())
	    {
	      staff_side_p_->dir_ = Direction (int (prop));
	    }

	  prop = get_property ("dynamicDir", 0);
	  if (prop.isnum_b ())
	    {
	      staff_side_p_->dir_ = (Direction) (int) prop;
	    }
	  if (absd->dir_)
	    {
	      staff_side_p_->dir_ = absd->dir_;
	    }

	  prop = get_property ("dynamicPadding", 0);
	  if (prop.isnum_b ())
	    {
	      staff_side_p_->set_elt_property (padding_scm_sym, 
					       gh_double2scm(Real(prop)));
	    }
	  announce_element (Score_element_info (text_p_, absd));
	  announce_element (Score_element_info (staff_side_p_, absd));
	}
      else if (Span_req *span_l
	       = dynamic_cast <Span_req *> (dynamic_req_l_arr_[i]))
	{
	  if (span_l->span_dir_ == STOP)
	    {
	      if (!cresc_p_)
		{
		  span_l->warning (_ ("can't find (de)crescendo to end"));
		}
	      else
		{
		  assert (!to_end_cresc_p_);
		  to_end_cresc_p_ =cresc_p_;
		  to_end_ss_span_p_ = ss_span_p_ ;
		  
		  cresc_p_ = 0;
		  ss_span_p_ =0;



		  
		  Scalar prop = get_property ("verticalDirection", 0);
		  if (prop.isdir_b())
		    {
		      to_end_ss_span_p_->dir_ = Direction (int (prop));
		    }
		  prop = get_property ("dynamicDir", 0);
		  if (prop.isdir_b ())
		    {
		      to_end_ss_span_p_->dir_ = (Direction) (int) prop;
		    }
		  prop = get_property ("dynamicPadding", 0);
		  if (prop.isnum_b ())
		    {
		      to_end_ss_span_p_->set_elt_property (padding_scm_sym, 
							   gh_double2scm(Real(prop)));
		    }
		}
	    }
	  else if (span_l->span_dir_ == START)
	    {
	      cresc_req_l_ = span_l;
	      assert (!new_cresc_p);
	      new_cresc_p  = new Crescendo;
	      new_cresc_p->grow_dir_ = (span_l->span_type_str_ == "crescendo")  ? BIGGER : SMALLER;
	      announce_element (Score_element_info (new_cresc_p, span_l));

	      new_sss_p = new Staff_side_spanner;
	      new_sss_p->set_victim (new_cresc_p);
	      new_sss_p->axis_ = Y_AXIS;
	      // UGH.!
	      // new_sss_p->set_elt_property (dangling_scm_sym, SCM_BOOL_T);
	      announce_element (Score_element_info (new_sss_p, span_l));
	    }
	}
    }

  if (new_cresc_p)
    {
      if (cresc_p_)
	{
	  ::warning (_ ("Too many crescendi here"));
	  typeset_element (cresc_p_);
	  typeset_element (ss_span_p_);
	  cresc_p_ = 0;
	  ss_span_p_ =0;
	}
      
      cresc_p_ = new_cresc_p;
      ss_span_p_ = new_sss_p;
      cresc_p_->set_bounds(LEFT,get_staff_info().musical_pcol_l ());
      ss_span_p_->set_bounds (LEFT,get_staff_info().musical_pcol_l ());
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
  typeset_all ();
}



ADD_THIS_TRANSLATOR(Dynamic_engraver);

void
Dynamic_engraver::do_removal_processing ()
{
  if (cresc_p_)
    {
      typeset_element (cresc_p_ );
      typeset_element (ss_span_p_);
      ss_span_p_ =0;
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
      to_end_cresc_p_->set_bounds(RIGHT,get_staff_info().musical_pcol_l ());
      to_end_ss_span_p_->set_bounds(RIGHT,get_staff_info().musical_pcol_l ());
      typeset_element (to_end_cresc_p_);
      typeset_element (to_end_ss_span_p_);
      to_end_cresc_p_ =0;
      to_end_ss_span_p_ =0;
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

      if (to_end_ss_span_p_)
	to_end_ss_span_p_->add_support (i.elem_l_);

      if (ss_span_p_)
	ss_span_p_->add_support (i.elem_l_);
    }
}
