/*   
  arpeggio-engraver.cc -- implement Arpeggio_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "engraver.hh"
#include "group-interface.hh"
#include "item.hh"
#include "arpeggio.hh"
#include "stem.hh"

class Arpeggio_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Arpeggio_engraver ();

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing ();

private:
  Item* arpeggio_; 
  Link_array <Score_element> stems_;
};

Arpeggio_engraver::Arpeggio_engraver ()
{
  arpeggio_ = 0;
}

void
Arpeggio_engraver::acknowledge_element (Score_element_info info)
{
  if (Stem::has_interface (info.elem_l_))
    {
      stems_.push (info.elem_l_);
    }
}

void
Arpeggio_engraver::process_acknowledged ()
{
  if (!arpeggio_ && !stems_.empty ())
    {
      arpeggio_ = new Item (get_property ("basicArpeggioProperties"));
      Pointer_group_interface pgi (arpeggio_, "stems");
      for (int i = 0; i < stems_.size (); i++)
	{
	  pgi.add_element (stems_[i]);
	  arpeggio_->add_dependency (stems_[i]);
	}
      announce_element (arpeggio_, 0);
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
  stems_.clear ();
}


ADD_THIS_TRANSLATOR (Arpeggio_engraver);

