/*   
  arpeggio-engraver.cc -- implement Arpeggio_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
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
  VIRTUAL_COPY_CONS (Translator);
  Arpeggio_engraver ();

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing ();
  virtual bool do_try_music (Music *);

private:
  Item* arpeggio_; 
  Arpeggio_req *arpeggio_req_;
  Link_array <Score_element> stems_;
  Link_array<Score_element> supports_;
};

Arpeggio_engraver::Arpeggio_engraver ()
{
  arpeggio_ = 0;
  arpeggio_req_ = 0;
}

bool
Arpeggio_engraver::do_try_music (Music* m)
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
Arpeggio_engraver::acknowledge_element (Score_element_info info)
{
  if (arpeggio_req_)
    {
      if (Stem::has_interface (info.elem_l_))
	{
	  stems_.push (info.elem_l_);
	}
      
      else

  /*
    We can't catch local key items from Voice context, but let's leave
    it here in case someone moves this stuff around.  */
	if (Rhythmic_head::has_interface (info.elem_l_)
	       || Local_key_item::has_interface (info.elem_l_))
	{
	  supports_.push (info.elem_l_);
	}
    }
}

void
Arpeggio_engraver::process_acknowledged ()
{
  if (!arpeggio_ && !stems_.empty ())
    {
      arpeggio_ = new Item (get_property ("Arpeggio"));
      arpeggio_->set_parent (stems_[0], Y_AXIS);
      
      //      Staff_symbol_referencer::set_interface (arpeggio_);

      for (int i = 0; i < stems_.size (); i++)
	{
	  Pointer_group_interface::add_element (arpeggio_, "stems", stems_[i]);
	  //Side_position::add_support (arpeggio_, stems_[i]);
	}
      for (int i = 0; i < supports_.size (); i++)
	{
	  ;//Side_position::add_support (arpeggio_, supports_[i]);
	}
      announce_element (arpeggio_, arpeggio_req_);
    }
}

void
Arpeggio_engraver::do_pre_move_processing ()
{
  if (arpeggio_)
    {
      typeset_element (arpeggio_);
      arpeggio_ = 0;
    }
  arpeggio_req_ = 0;
  stems_.clear ();
  supports_.clear ();
}


ADD_THIS_TRANSLATOR (Arpeggio_engraver);

