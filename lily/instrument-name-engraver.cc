/*   
  new-staff-margin-engraver.cc --  implement Instrument_name_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "item.hh"
#include "bar.hh"
#include "system-start-delimiter.hh"
#include "side-position-interface.hh"
#include "align-interface.hh"

class Instrument_name_engraver : public Engraver
{
  Item *text_;
  Grob * delim_ ;

  void create_text (SCM s);
public:
  VIRTUAL_COPY_CONS(Translator);
  Instrument_name_engraver ();

  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
};

ADD_THIS_TRANSLATOR(Instrument_name_engraver);

Instrument_name_engraver::Instrument_name_engraver ()
{
  text_ = 0;
  delim_ =0;
}


void
Instrument_name_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      typeset_grob (text_);
      text_ = 0;
    }
}

void
Instrument_name_engraver::create_text (SCM txt)
{
  if(!text_)
    {
      text_ = new Item (get_property ("InstrumentName"));
      
      if (text_->get_grob_property ("text") != txt)
	text_->set_grob_property ("text", txt);
     
      if (delim_)
        text_->set_parent (delim_, Y_AXIS);
      
      announce_grob (text_,0);
    }
}

void
Instrument_name_engraver::acknowledge_grob (Grob_info i)
{
  if (Bar::has_interface (i.elem_l_))
    {
      SCM s = get_property ("instrument");
  
      if (now_mom () > Moment (0))
	s = get_property ("instr");

      /*
	FIXME: use markup_p () to check type.
      */
      if (gh_string_p (s) || gh_pair_p (s))
	create_text (s);
	  
    }

  if (Align_interface::has_interface (i.elem_l_)
      && Align_interface::axis  (i.elem_l_) == Y_AXIS      
      //System_start_delimiter::has_interface (i.elem_l_)
      && i.origin_trans_l_->daddy_trans_l_ == daddy_trans_l_)
    {
      delim_ = i.elem_l_;
    }
}




