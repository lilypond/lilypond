
/*   
  lyric-number-engraver.cc --  implement Stanza_number_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>, Glen Prideaux <glenprideaux@iname.com>
  
  Similar to (and derived from) Instrument_name_engraver.
 */

#include "engraver.hh"
#include "item.hh"
//#include "system-start-delimiter.hh"
//#include "side-position-interface.hh"
//#include "staff-symbol-referencer.hh"
#include "bar.hh"

class Stanza_number_engraver : public Engraver
{
  Item *text_;
  bool bar_b_;;

  void create_text (SCM s);
public:
  VIRTUAL_COPY_CONS(Translator);
  Stanza_number_engraver ();

  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();
};

ADD_THIS_TRANSLATOR(Stanza_number_engraver);

Stanza_number_engraver::Stanza_number_engraver ()
{
  text_ = 0;
  bar_b_ = false;
}

void
Stanza_number_engraver::acknowledge_element(Score_element_info i)
{
  SCM s = get_property ("stanza");
      
  if (now_mom () > Moment (0))
    s = get_property ("stz");
  
  if (gh_string_p (s))
    {
//       if (i.elem_l_->has_interface (ly_symbol2scm ("lyric-syllable-interface")))
        // Tried catching lyric items to generate stanza numbers, but it spoils lyric spacing.
       if (Bar::has_interface (i.elem_l_) || now_mom() == Moment(0))
	// Works, but requires bar_engraver in LyricVoice context apart from at beginning.
	// Is there any score element we can catch that will do the trick?
//       if (! i.elem_l_->has_interface (ly_symbol2scm ("lyric-syllable-interface")) ||
// 	  now_mom() == Moment(0))
	// What happens if we try anything at all EXCEPT a lyric? Is there anything else?
        // Not sure what it's catching, but it still mucks up lyrics.
	create_text (s);
    }
}


void
Stanza_number_engraver::do_pre_move_processing ()
{
  if (text_)
    {
      typeset_element (text_);
      text_ = 0;
    }
}

void
Stanza_number_engraver::create_text (SCM txt)
{
  if(!text_)
    {
      text_ = new Item (get_property ("basicStanzaNumberProperties"));
      text_->set_elt_property ("text", txt);
      announce_element (text_,0);
    }
}




