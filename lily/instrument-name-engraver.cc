/*   
  new-staff-margin-engraver.cc --  implement Instrument_name_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "text-item.hh"
#include "bar.hh"
#include "span-bar.hh"

class Instrument_name_engraver : public Engraver
{
  Text_item *text_;

  void create_text (SCM s);
public:
  VIRTUAL_COPY_CONS(Translator);
  Instrument_name_engraver ();

  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();
};

ADD_THIS_TRANSLATOR(Instrument_name_engraver);

Instrument_name_engraver::Instrument_name_engraver ()
{
  text_ = 0;
}


void
Instrument_name_engraver::do_pre_move_processing ()
{
  if (text_)
    {
      typeset_element (text_);
      text_ = 0;
    }
}

void
Instrument_name_engraver::create_text (SCM txt)
{
  if(!text_)
    {
      text_ = new Text_item;
      text_->set_elt_property ("text", txt);
      text_->set_elt_property ("breakable", SCM_BOOL_T);

      /*
	TODO: use more lispish names for break-align-symbols
       */
      text_->set_elt_property ("break-align-symbol", ly_symbol2scm ("Instrument_name"));
      text_->set_elt_property ("visibility-lambda",
			       scm_eval (ly_symbol2scm ("begin-of-line-visible")));

      announce_element (Score_element_info (text_,0));
    }
}

void
Instrument_name_engraver::acknowledge_element (Score_element_info i)
{
  SCM s = get_property ("instrument");
  
  if (now_mom () > Moment (0))
    s = get_property ("instr");

  if (gh_string_p (s))
    {
      if (Bar* b= dynamic_cast<Bar*> (i.elem_l_))
	{
	  create_text (s);
	  if (Span_bar* s= dynamic_cast<Span_bar*> (b))
	    {
	      text_->set_parent (s, Y_AXIS);
	    }
	}
    }
}




