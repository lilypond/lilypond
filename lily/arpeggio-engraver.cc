/*   
  arpeggio-engraver.cc -- implement Arpeggio_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "engraver.hh"
#include "group-interface.hh"
#include "item.hh"
#include "musical-request.hh"
#include "arpeggio.hh"
#include "stem.hh"
#include "local-key-item.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"

class Arpeggio_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Arpeggio_engraver); 
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music *);

private:
  Item* arpeggio_; 
  Arpeggio_req *arpeggio_req_;
  Link_array <Grob> stems_;
  Link_array<Grob> supports_;
};

Arpeggio_engraver::Arpeggio_engraver ()
{
  arpeggio_ = 0;
  arpeggio_req_ = 0;
}

bool
Arpeggio_engraver::try_music (Music* m)
{
  if (!arpeggio_req_)
    {
      if (Arpeggio_req *a = dynamic_cast<Arpeggio_req*> (m))
	{
	  arpeggio_req_ = a;
	  return true;
	}
    }
  return false;
}

void
Arpeggio_engraver::acknowledge_grob (Grob_info info)
{
  if (arpeggio_req_)
    {
      if (Stem::has_interface (info.grob_l_))
	{
	  stems_.push (info.grob_l_);
	}
      
      /*
	We can't catch local key items (accidentals) from Voice context,
	see Local_key_engraver
      */
      else if (Rhythmic_head::has_interface (info.grob_l_))
	{
	  supports_.push (info.grob_l_);
	}
    }
}

void
Arpeggio_engraver::create_grobs ()
{
  if (!arpeggio_ && !stems_.empty ())
    {
      arpeggio_ = new Item (get_property ("Arpeggio"));
      arpeggio_->set_parent (stems_[0], Y_AXIS);
      
      for (int i = 0; i < stems_.size (); i++)
	{
	  Pointer_group_interface::add_element (arpeggio_, ly_symbol2scm ("stems"), stems_[i]);
	}
      for (int i = 0; i < supports_.size (); i++)
	{
	  Side_position_interface::add_support (arpeggio_, supports_[i]);
	}
      announce_grob (arpeggio_, arpeggio_req_);
    }
}

void
Arpeggio_engraver::stop_translation_timestep ()
{
  if (arpeggio_)
    {
      typeset_grob (arpeggio_);
      arpeggio_ = 0;
    }
  arpeggio_req_ = 0;
  stems_.clear ();
  supports_.clear ();
}




ENTER_DESCRIPTION(Arpeggio_engraver,
/* descr */       "Generate an Arpeggio from a Arpeggio_req",
/* creats*/       "Arpeggio",
/* acks  */       "stem-interface rhythmic-head-interface",
/* reads */       "",
/* write */       "");
