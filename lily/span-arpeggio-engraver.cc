/*
  span-arpeggio-engraver.cc -- implement Span_arpeggio_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "lily-guile.hh"
#include "item.hh"
#include "arpeggio.hh"
#include "span-arpeggio.hh"
#include "group-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"


/** 

  Make arpeggios that span multiple staffs.  Catch arpeggios, and span a
  Span_arpeggio over them if we find more than two arpeggios.
  */
class Span_arpeggio_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Span_arpeggio_engraver ();

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing ();

private:
  Item *span_arpeggio_;
  Link_array<Score_element> arpeggios_;
};


Span_arpeggio_engraver::Span_arpeggio_engraver ()
{
  span_arpeggio_ = 0;
}

void
Span_arpeggio_engraver::acknowledge_element (Score_element_info info)
{
    if (info.origin_trans_l_arr (this).size ()
        && Arpeggio::has_interface (info.elem_l_))
    {
      arpeggios_.push (info.elem_l_);
    }
}

void
Span_arpeggio_engraver::process_acknowledged ()
{
  if (!span_arpeggio_ && arpeggios_.size () > 1
      && to_boolean (get_property ("connectArpeggios")))
    {
      span_arpeggio_ = new Item (get_property ("SpanArpeggio"));
      span_arpeggio_->set_parent (arpeggios_[0], Y_AXIS);
      Side_position::set_axis (span_arpeggio_, X_AXIS);
      Pointer_group_interface pgi (span_arpeggio_, "arpeggios");
      for (int i = 0; i < arpeggios_.size () ; i++)
	{
	  pgi.add_element (arpeggios_[i]);
	  span_arpeggio_->add_dependency (arpeggios_[i]);
	}
      
      announce_element (span_arpeggio_, 0);
    }
}

void
Span_arpeggio_engraver::do_pre_move_processing ()
{
  if (span_arpeggio_) 
    {
      typeset_element (span_arpeggio_);
      span_arpeggio_ = 0;
    }
  arpeggios_.clear ();
}

ADD_THIS_TRANSLATOR (Span_arpeggio_engraver);

