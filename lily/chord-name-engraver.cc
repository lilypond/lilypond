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

/*
  UGH.

  Split this routine into neat packets
 */
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

  
  /*
    find tonic: after longest line of triads
   */

  int tonic_i = 0;
  Scalar chord_inversions = get_property ("chordInversion", 0);
  if (chord_inversions.to_bool ())
    {
      int longest_i = 0;
      for (int i = 0; i < pitch_arr_.size (); i++)
	for (int j = 0; j < pitch_arr_.size (); j++)
	  {
	    int gap = pitch_arr_[(i + j + 1) % pitch_arr_.size ()].notename_i_
	      - pitch_arr_[(i + j) % pitch_arr_.size ()].notename_i_;
	    while (gap < 0)
	      gap += 7;
	    gap %= 7;
	    if (gap != 2)
	      {
		if (j > longest_i)
		  {
		    longest_i = j;
		    tonic_i = i;
		  }
		break;
	      }
	  }

      int biggest_i = 0;
      if (!longest_i)
	for (int i = 0; i < pitch_arr_.size (); i++)
	  {
	    int gap = pitch_arr_[i].notename_i_
	      - pitch_arr_[(i - 1 + pitch_arr_.size ()) 
	      % pitch_arr_.size ()].notename_i_;
	    while (gap < 0)
	      gap += 7;
	    gap %= 7;
	    if (gap > biggest_i)
	      {
		biggest_i = gap;
		tonic_i = i;
	      }
	  }
    }

  Musical_pitch inversion = pitch_arr_[0];
  if (tonic_i)
    {
      Musical_pitch last (0, 0, -5);
      Array<Musical_pitch> pitches;
      for (int i = 0; i < pitch_arr_.size (); i++)
	{
	  Musical_pitch p = pitch_arr_[(tonic_i + i) % pitch_arr_.size ()];
	  if (p < last)
	    {
	      p.octave_i_ = last.octave_i_;
	      if (p < last)
		p.octave_i_++;
	    }
	  pitches.push (p);
	  last = p;
	}
      pitch_arr_ = pitches;
    }

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
  char const *acc[] = {"\\textflat\\textflat ", "\\textflat ", "", "\\textsharp " , "\\textsharp\\textsharp "};
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

  String inversion_str;
  if (tonic_i)
    {
      inversion_str = inversion.str ();
      inversion_str = "/" + inversion_str.left_str (1).upper_str ()
	+ acc[tonic.accidental_i_ + 2];

    }
  
  G_text_item* item_p =  new G_text_item;


  item_p->text_str_ = tonic_str + "$^{" + add_str + "}$" + inversion_str;
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

