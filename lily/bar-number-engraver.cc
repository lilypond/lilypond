/*
  bar-number-grav.cc -- implement Bar_number_engraver

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


class Bar_number_engraver : public Engraver
{
protected:
  Item* text_p_;
protected:
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  void create_items ();
  TRANSLATOR_DECLARATIONS(  Bar_number_engraver );
};


/*
  TODO: more advanced formatting via SCM function, perhaps barnumbers
  every 5 measures?  */

void
Bar_number_engraver::process_music ()
{
  // todo include (&&!time->cadenza_b_)

  SCM wb = get_property ("whichBar");
  
  if (gh_string_p (wb))
    {
      SCM bn = get_property ("currentBarNumber");
      SCM smp = get_property ("measurePosition");
      int ibn = gh_number_p (bn) ? gh_scm2int(bn) : 1;
      
      Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
      if (mp.main_part_ == Rational (0)
	  && ibn != 1)
	{
	  create_items ();
	  
	  // guh.
	  text_p_->set_grob_property ("text",
				      ly_str02scm (to_str (gh_scm2int (bn)).ch_C ()));
	}
    }

}



Bar_number_engraver::Bar_number_engraver ()
{
  text_p_ =0;
}

					       
void
Bar_number_engraver::acknowledge_grob (Grob_info inf)
{
  Grob * s = inf.grob_l_;
  if (text_p_
      && dynamic_cast<Item*> (s)
      && s->get_grob_property ("break-align-symbol") == ly_symbol2scm ("Left_edge_item"))
    {
      /*
	By default this would land on the Paper_column -- so why
	doesn't it work when you leave this out?  */
      text_p_->set_parent (s, X_AXIS);
    }
}

void 
Bar_number_engraver::stop_translation_timestep ()
{
  if (text_p_)
    {
      text_p_->set_grob_property ("side-support-elements", get_property ("stavesFound"));
      typeset_grob (text_p_);
      text_p_ =0;
    }
}


void
Bar_number_engraver::create_items ()
{
  if (text_p_)
    return;

  SCM b = get_property ("BarNumber");
  text_p_ = new Item (b);
  Side_position_interface::set_axis (text_p_,Y_AXIS);

  announce_grob(text_p_, SCM_EOL);
}

ENTER_DESCRIPTION(Bar_number_engraver,
/* descr */       "A bar number is created whenever measurePosition is zero. It is
put on top of all staves, and appears only at  left side of the staff.",
/* creats*/       "BarNumber",
/* acks  */       "break-aligned-interface",
/* reads */       "currentBarNumber stavesFound" ,
/* write */       "");
