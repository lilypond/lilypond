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
  /*
    connectArpeggios is slightly brusque; we should really read a elt
    property of the caught non-span arpeggios. That way, we can have

    both non-connected and connected arps in one pianostaff.
    

  */
  if (!span_arpeggio_ && arpeggios_.size () > 1
      && to_boolean (get_property ("connectArpeggios")))
    {
      span_arpeggio_ = new Item (get_property ("Arpeggio"));
      announce_element (span_arpeggio_, 0);      
    }
}

void
Span_arpeggio_engraver::do_pre_move_processing ()
{
  if (span_arpeggio_) 
    {
      /*
	we do this very late, to make sure we also catch `extra'
	side-pos support like accidentals.
       */
      for (int i=0; i < arpeggios_.size (); i ++)
	{
	  for (SCM s = arpeggios_[i]->get_elt_property ("stems");
	       gh_pair_p (s); s = gh_cdr (s))
	    Group_interface::add_thing (span_arpeggio_, "stems", gh_car (s));
	  for (SCM s = arpeggios_[i]->get_elt_property ("side-support-elements");
	       gh_pair_p (s); s = gh_cdr (s))
	    Group_interface::add_thing (span_arpeggio_, "side-support-elements", gh_car (s));

	  /*
	    we can't kill the children, since we don't want to the
	    previous note to bump into the span arpeggio; so we make
	    it transparent.  */
	  arpeggios_[i]->set_elt_property ("molecule-callback", SCM_BOOL_T);
	}
      
      typeset_element (span_arpeggio_);
      span_arpeggio_ = 0;
    }
  arpeggios_.clear ();
}

ADD_THIS_TRANSLATOR (Span_arpeggio_engraver);

