/*
  chord-name-engraver.cc -- implement Chord_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name-engraver.hh"
#include "chord.hh"
#include "musical-request.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "main.hh"
#include "dimensions.hh"
#include "g-text-item.hh"

ADD_THIS_TRANSLATOR (Chord_name_engraver);

Chord_name_engraver::Chord_name_engraver ()
{
}

void
Chord_name_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_req* n = dynamic_cast<Note_req*> (i.req_l_))
    pitch_arr_.push (n->pitch_);
}

bool
Chord_name_engraver::do_try_music (Music* m)
{
  if (Note_req* n = dynamic_cast<Note_req*> (m))
    {
      pitch_arr_.push (n->pitch_);
      return true;
    }
  return false;
}

void
Chord_name_engraver::do_process_requests ()
{
  if (text_p_arr_.size ())
    return;
  if (!pitch_arr_.size ())
    return;

  /*
   Banter style chord names (almost).
   TODO:
     - move this stuff to new Item class Chord_name
     - switch on property, add american (?) chordNameStyle

  Scalar chordNameStyle = get_property ("chordNameStyle", 0);
  if (chordNameStyle == "Banter")
     chord = pitches_to_banter (pitch_arr_));

   */

  Chord chord (pitch_arr_);
  Musical_pitch* inversion = 0;
  Scalar chord_inversion = get_property ("chordInversion", 0);
  if (chord_inversion.to_bool ())
    {
      int tonic_i = chord.find_tonic_i ();
      if (tonic_i)
	{
	  inversion = &pitch_arr_[0];
	  Scalar preserve = get_property ("chordInversionPreserve", 0);
	  if (preserve.to_bool ())
	    chord.rebuild_from_base (tonic_i);
	  else
	    chord.rebuild_insert_inversion (tonic_i);
	}
    }
    
  G_text_item* item_p =  new G_text_item;

  item_p->text_str_ = chord.banter_str (inversion);
  
  Scalar style = get_property ("textstyle", 0);
  if (style.length_i ())
    item_p->style_str_ = style;
  
  text_p_arr_.push (item_p);
  announce_element (Score_element_info (item_p, 0));
}

void
Chord_name_engraver::do_pre_move_processing ()
{
  for (int i=0; i < text_p_arr_.size (); i++)
    {
      typeset_element (text_p_arr_[i]);
    }
  text_p_arr_.clear ();
  pitch_arr_.clear ();
}
