/*   
  line-number-engraver.cc --  implement  Line_number_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "note-head.hh"
#include "stem.hh"

/**
   Annotate output with line numbers. Creates text-items when it
   catches note heads.  */
class Line_number_engraver : public Engraver
{
  Array<Score_element_info> interesting_;
  Link_array<Score_element> support_;
  Item * text_item_p_;
  String last_text_;
public:
  Line_number_engraver ();
  VIRTUAL_COPY_CONS (Translator);
protected:
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
};

void
Line_number_engraver::process_acknowledged ()
{
  if (!text_item_p_ && interesting_.size ())
    {
      text_item_p_ = new Item (get_property ("basicTextProperties") );
      Side_position_interface si (text_item_p_);
      si.set_axis (Y_AXIS);
      text_item_p_->set_parent (interesting_[0].elem_l_, Y_AXIS);

      si.set_direction (UP);
      announce_element (Score_element_info (text_item_p_, 0));
    }
}

void
Line_number_engraver::acknowledge_element (Score_element_info inf)
{
  if (dynamic_cast<Note_head*> (inf.elem_l_))
    {
      interesting_.push (inf);
      support_.push (inf.elem_l_);
    }
  if (dynamic_cast<Stem*> (inf.elem_l_))
    {
      support_.push (inf.elem_l_);
    }
}

void
Line_number_engraver::do_pre_move_processing ()
{
  if (text_item_p_)
    {
      String s;
      Side_position_interface si (text_item_p_);
      for (int i=0; i < interesting_.size (); i++)
	{
	  if (i)
	    s += ",";
	  
	  s += interesting_[i].req_l_->line_number_str ();
	  
	}

      for (int j= support_.size (); j--; )
	{
	  si.add_support (support_[j]);
	}
      if (s != last_text_)
	{
	  text_item_p_->set_elt_property ("text", ly_str02scm (s.ch_C()));
	  last_text_ =s;
	}
      
      typeset_element (text_item_p_);
      text_item_p_ =0;
    }
  interesting_.clear ();
  support_.clear ();
}

Line_number_engraver::Line_number_engraver ()
{
  text_item_p_ =0;
}

ADD_THIS_TRANSLATOR(Line_number_engraver);
