/*
  bar-number-engraver.cc -- implement Bar_number_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "lily-guile.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "side-position-interface.hh"
#include "item.hh"
#include "moment.hh"
#include "engraver.hh"
#include "translator-group.hh"

/*
  
TODO: detect the top staff (stavesFound), and acknowledge staff-group
system-start-delims. If we find these, and the top staff is in the
staff-group, add padding to the bar number.

*/


class Bar_number_engraver : public Engraver
{
protected:
  Item* text_;
protected:
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  void create_items ();
  TRANSLATOR_DECLARATIONS(  Bar_number_engraver );
};


void
Bar_number_engraver::process_music ()
{
  // todo include (&&!time->cadenza_b_)

  SCM wb = get_property ("whichBar");
  
  if (gh_string_p (wb))
    {
      SCM smp = get_property ("measurePosition");
      
      Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
      if (mp.main_part_ == Rational (0))
	{
	  SCM bn = get_property ("currentBarNumber");
	  SCM proc = get_property ("barNumberVisibility");
	  if (gh_number_p (bn) && to_boolean(gh_call1(proc, bn)))
	    {
	      create_items ();
	      // guh.
	      text_->set_grob_property
		("text", scm_makfrom0str (to_string (gh_scm2int (bn)).to_str0 ()));
	    }
	}
    }

}



Bar_number_engraver::Bar_number_engraver ()
{
  text_ =0;
}

					       
void
Bar_number_engraver::acknowledge_grob (Grob_info inf)
{
  Grob * s = inf.grob_;
  if (text_
      && dynamic_cast<Item*> (s)
      && s->get_grob_property ("break-align-symbol") == ly_symbol2scm ("left-edge"))
    {
      /*
	By default this would land on the Paper_column -- so why
	doesn't it work when you leave this out?  */
      text_->set_parent (s, X_AXIS);
    }
}

void 
Bar_number_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      text_->set_grob_property ("side-support-elements", get_property ("stavesFound"));
      typeset_grob (text_);
      text_ =0;
    }
}


void
Bar_number_engraver::create_items ()
{
  if (text_)
    return;

  SCM b = get_property ("BarNumber");
  text_ = new Item (b);
  Side_position_interface::set_axis (text_,Y_AXIS);

  announce_grob(text_, SCM_EOL);
}

ENTER_DESCRIPTION(Bar_number_engraver,
/* descr */       "A bar number is created whenever measurePosition is zero. It is
put on top of all staves, and appears only at  left side of the staff.",
/* creats*/       "BarNumber",
/* accepts */     "",
/* acks  */      "break-aligned-interface",
/* reads */       "currentBarNumber stavesFound barNumberVisibility" ,
/* write */       "");
