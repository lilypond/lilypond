/*
  chord-name-engraver.cc -- implement Chord_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "main.hh"
#include "dimensions.hh"

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

  Scalar style = get_property ("textstyle");
  Scalar alignment = get_property ("textalignment");
  Text_def* text_p = new Text_def;
  text_p->align_dir_ = LEFT;
  if (style.length_i ())
    text_p->style_str_ = style;
  if (alignment.isnum_b())
    text_p->align_dir_= (Direction)(int)alignment;

  Musical_pitch tonic = pitch_arr_[0];
  text_p->text_str_ = tonic.str ();
  for (int i=1; i < pitch_arr_.size (); i++)
    {
      Musical_pitch p = pitch_arr_[i];
      int trap = (p.notename_i_ - tonic.notename_i_ + 8) % 8 + 1;
      if (((trap != 3) && (trap != 5)) || (p.accidental_i_))
        {
	  text_p->text_str_ += to_str ((p.notename_i_ - tonic.notename_i_ + 8) % 8 + 1);
	  if (p.accidental_i_)
	    text_p->text_str_ += p.accidental_i_ < 0 ? "-" : "+";
	  if (i + 1 < pitch_arr_.size ())
	    text_p->text_str_ += "/";
	}
    }

  Text_item* item_p =  new Text_item (text_p);
  item_p->dir_ = DOWN;
  item_p->fat_b_ = true;
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

