/*   
  instrument-name-engraver.cc --  implement Instrument_name_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "item.hh"
#include "bar-line.hh"
#include "system-start-delimiter.hh"
#include "side-position-interface.hh"
#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "translator-group.hh"
#include "text-item.hh"

class Instrument_name_engraver : public Engraver
{
  Item *text_;
  Grob *delim_ ;
  
  void create_text ();
public:
  TRANSLATOR_DECLARATIONS(Instrument_name_engraver);

  virtual void initialize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void process_music ();
};



Instrument_name_engraver::Instrument_name_engraver ()
{
  text_ = 0;
  delim_ =0;
}


void
Instrument_name_engraver::initialize ()
{
  daddy_trans_->set_property ("instrumentSupport", SCM_EOL); 
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
Instrument_name_engraver::create_text ()
{
   if (!text_)
    {
      SCM txt = get_property ("instrument");
  
      if (now_mom () > Moment (0))
	txt = get_property ("instr");
      /*
	UGH.
      */
      if (txt == SCM_EOL)
	return ;
      
      text_ = new Item (get_property ("InstrumentName"));
      
      if (text_->get_grob_property ("text") != txt)
	text_->set_grob_property ("text", txt);

      if (delim_)
        text_->set_parent (delim_, Y_AXIS);
      
      announce_grob (text_, SCM_EOL);
    }
}

void
Instrument_name_engraver::acknowledge_grob (Grob_info i)
{
  if (Bar_line::has_interface (i.grob_))
    {
      create_text();
    }

  if (dynamic_cast<Spanner*> (i.grob_)
      && i.grob_->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
    return;

  /*
    Hang the instrument names on the staves, but not on the alignment
    groups enclosing that staff. The alignment has no real location,
    but is only a vehicle for the placement routine it contains, and
    therefore the location of its refpoint won't be very useful.

    We could also just use stavesFound, but lets keep this working
    without staffs as well.
  */
  if (dynamic_cast<Spanner*> (i.grob_)
      && ((Axis_group_interface::has_interface (i.grob_)
	   && Axis_group_interface::axis_b (i.grob_, Y_AXIS)))
      && !Align_interface::has_interface (i.grob_))
    {
      SCM nl = gh_cons (i.grob_->self_scm (),
			get_property ("instrumentSupport"));

      daddy_trans_->set_property ("instrumentSupport", nl);
    }
}

void
Instrument_name_engraver::process_music ()
{
  /*
    Also create text if barlines in other groups. This allows
    a name to be attached to lyrics or chords. 
   */
  if (gh_string_p (get_property ("whichBar")))
    create_text();
}

ENTER_DESCRIPTION(Instrument_name_engraver,
/* descr */       " Prints the name of the instrument (specified by "
" @code{Staff.instrument} and @code{Staff.instr}) "
"at the left of the staff. ",
/* creats*/       "InstrumentName",
/* accepts */     "",
/* acks  */      "bar-line-interface axis-group-interface",
/* reads */       "instrument instr",
/* write */       "");
