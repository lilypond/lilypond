/*
  span-bar-grav.cc -- implement Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "lily-guile.hh"
#include "span-bar.hh"
#include "engraver.hh"

/** 

  Make bars that span multiple "staffs". Catch bars, and span a
  Span_bar over them if we find more than 2 bars.  Vertical alignment
  of staffs changes the appearance of spanbars.  It is up to the
  aligner (Vertical_align_engraver, in this case, to add extra
  dependencies to the spanbars.

  */
class Span_bar_engraver : public Engraver
{
  Span_bar * spanbar_p_;
  Link_array<Bar> bar_l_arr_;

public:
  VIRTUAL_COPY_CONS(Translator);
  Span_bar_engraver();
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual Span_bar* get_span_bar_p(SCM) const;
};


Span_bar_engraver::Span_bar_engraver()
{
  spanbar_p_ =0;
}

Span_bar*
Span_bar_engraver::get_span_bar_p(SCM s) const
{
  Span_bar * sp= new Span_bar (s);
  return sp;
}


void
Span_bar_engraver::acknowledge_element (Score_element_info i)
{
  int depth = i.origin_trans_l_arr (this).size();
  if (depth > 1
      && dynamic_cast<Bar *> (i.elem_l_)) 
    {
      bar_l_arr_.push (dynamic_cast<Bar *> (i.elem_l_));

      if (bar_l_arr_.size() >= 2 && !spanbar_p_) 
	{
	  spanbar_p_ = get_span_bar_p (get_property ("basicSpanBarProperties"));
	  spanbar_p_->set_elt_property ("glyph", bar_l_arr_[0]->get_elt_property ("glyph"));
	  spanbar_p_->set_elt_property ("visibility-lambda",
					bar_l_arr_[0]->get_elt_property ("visibility-lambda"));	  
					
	  spanbar_p_->set_parent (bar_l_arr_[0], Y_AXIS);
	  spanbar_p_->set_parent (bar_l_arr_[0], X_AXIS);

	  announce_element (Score_element_info (spanbar_p_,0));
	}
    }
}
void
Span_bar_engraver::do_pre_move_processing()
{
  if (spanbar_p_) 
    {
      for (int i=0; i < bar_l_arr_.size() ; i++)
	spanbar_p_->add_bar (bar_l_arr_[i]);
      typeset_element (spanbar_p_);
      spanbar_p_ =0;
    }
  bar_l_arr_.set_size (0);
}



ADD_THIS_TRANSLATOR(Span_bar_engraver);



