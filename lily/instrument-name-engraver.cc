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
#include "axis-group-interface.hh"
#include "translator-group.hh"

class Instrument_name_engraver : public Engraver
{
  Item *text_;
  Grob *delim_ ;
  
  void create_text (SCM s);
public:
  VIRTUAL_COPY_CONS (Translator);
  Instrument_name_engraver ();
  virtual void initialize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
};

ADD_THIS_TRANSLATOR (Instrument_name_engraver);

Instrument_name_engraver::Instrument_name_engraver ()
{
  text_ = 0;
  delim_ =0;
}


void
Instrument_name_engraver::initialize ()
{
  daddy_trans_l_->set_property ("instrumentSupport", SCM_EOL); 
}

void
Instrument_name_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      text_->set_grob_property ("side-support-elements",
				get_property ("instrumentSupport"));
      typeset_grob (text_);
      text_ = 0;
    }
}

void
Instrument_name_engraver::create_text (SCM txt)
{
  if (!text_)
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

  if (dynamic_cast<Spanner*> (i.elem_l_)
      && i.elem_l_->has_interface (ly_symbol2scm ("dynamic-interface")))
    return;

  /*
    Hang the instrument names on the staffs, but not on the alignment
    groups enclosing that staff. The alignment has no real location,
    but is only a vehicle for the placement routine it contains, and
    therefore the location of its refpoint won't be very useful.
    
  */
  if (dynamic_cast<Spanner*> (i.elem_l_)
      && ((Axis_group_interface::has_interface (i.elem_l_)
	   && Axis_group_interface::axis_b (i.elem_l_, Y_AXIS)))
      && !Align_interface::has_interface (i.elem_l_))
    {
      SCM nl = gh_cons (i.elem_l_->self_scm (),
			get_property ("instrumentSupport"));

      daddy_trans_l_->set_property ("instrumentSupport", nl);
    }
}




