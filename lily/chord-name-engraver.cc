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

  /*
   Banter style chord names (almost).
   TODO:
     - don't print inclusive scale (i.e. no "9" in c 9/11)
     - handle c7 / cmaj7
     - use #,b iso -es -is on tonica
     - switch on property, add american (?) chordNameStyle

  Scalar chordNameStyle = get_property ("chordNameStyle");
  if (chordNameStyle == "Banner")
     chord = pitches_to_banner (pitch_arr_.size ());

   */

  Scalar style = get_property ("textstyle");
  Scalar alignment = get_property ("textalignment");
  Text_def* text_p = new Text_def;
  text_p->align_dir_ = LEFT;
  if (style.length_i ())
    text_p->style_str_ = style;
  if (alignment.isnum_b())
    text_p->align_dir_= (Direction)(int)alignment;

  Musical_pitch tonic = pitch_arr_[0];

  Array<Musical_pitch> scale;
  scale.push (Musical_pitch (0)); // c
  scale.push (Musical_pitch (1)); // d
  scale.push (Musical_pitch (2)); // e
  scale.push (Musical_pitch (3)); // f
  scale.push (Musical_pitch (4)); // g
  scale.push (Musical_pitch (5)); // a
  // 7 always means 7-...
  scale.push (Musical_pitch (6, -1)); // b

  for (int i = 0; i < scale.size (); i++)
    scale[i].transpose (tonic);

  //urg, should do translation in scheme.
  char const *acc[] = {"\\textflat\\textflat", "\\textflat", "", "\\textsharp" , "\\textsharp\\textsharp"};
  String tonic_str = tonic.str ();
  tonic_str = tonic_str.left_str (1).upper_str ()
    + acc[tonic.accidental_i_ + 2];

  String add_str;
  String sep_str;
  for (int i=1; i < pitch_arr_.size (); i++)
    {
      Musical_pitch p = pitch_arr_[i];
      int trap = p.notename_i_ - tonic.notename_i_ 
        + (p.octave_i_ - tonic.octave_i_) * 7 + 1;
      int accidental = p.accidental_i_ - scale[(trap - 1) % 7].accidental_i_;
      if ((trap == 3) && (accidental == -1))
        tonic_str += "m"; // hmm
      else if (accidental || (!(trap % 2) || ((i + 1 == pitch_arr_.size ()) && (trap > 5))))
        {
	  add_str += sep_str;
          if ((trap == 7) && (accidental == 1))
            add_str += "maj7";
          else
            {
              add_str += to_str (trap);
              if (accidental)
                add_str += accidental < 0 ? "-" : "+";
            }
	  sep_str = "/";
	}
    }

  text_p->text_str_ = tonic_str + "$^{" + add_str + "}$";
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

