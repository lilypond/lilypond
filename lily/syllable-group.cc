#include <string.h>

#include "lyric-phrasing-engraver.hh"
#include "note-head.hh"
#include "translator-group.hh"
#include "side-position-interface.hh"
#include "ly-smobs.icc"
#include "spanner.hh"
#include "paper-def.hh"


/*=========================================================================================*/

/** Syllable_group is a class to be smobbed and entered as data in the association list
    member of the Lyric_phrasing_engraver class.
*/

Syllable_group::Syllable_group()
{
  first_in_phrase_b_=true;
  melisma_b_ = false;
  clear();
}

void 
Syllable_group::clear()
{
  notehead_l_=0;
  lyric_list_.clear();
  longest_lyric_l_=0;
  shortest_lyric_l_=0;
  melisma_b_ = false;
  group_translation_f_ = 0.0;
}
  
void
Syllable_group::copy( Syllable_group *from)
{
  notehead_l_ = from->notehead_l_;
  lyric_list_ = from->lyric_list_;
  longest_lyric_l_ = from->longest_lyric_l_;
  shortest_lyric_l_ = from->shortest_lyric_l_;
  melisma_b_ = from->melisma_b_;
  alignment_i_ = from->alignment_i_;
  first_in_phrase_b_ = from->first_in_phrase_b_;
  group_translation_f_ = from->group_translation_f_;
}

void 
Syllable_group::set_first_in_phrase(bool f) 
{ 
  first_in_phrase_b_ = f; 
}

void 
Syllable_group::set_notehead(Score_element * notehead)
{
  if(!notehead_l_) {
    /* there should only be a single notehead, so silently ignore any extras */
    notehead_l_=notehead;
  }
}

void 
Syllable_group::add_lyric(Score_element * lyric)
{
  lyric_list_.push(lyric);
  /* record longest and shortest lyrics */
  if( longest_lyric_l_ ) {
    if(lyric->extent(X_AXIS).length() > (longest_lyric_l_->extent(X_AXIS)).length())
      longest_lyric_l_ = lyric;
    if(lyric->extent(X_AXIS).length() < (shortest_lyric_l_->extent(X_AXIS)).length())
      shortest_lyric_l_ = lyric;
  }
  else
    longest_lyric_l_ = shortest_lyric_l_ = lyric;
}

void 
Syllable_group::add_extender(Score_element * extender)
{
  if(notehead_l_ && melisma_b_) {
    dynamic_cast<Spanner*>(extender)->set_bound (RIGHT, notehead_l_);
    // should the extender finish at the right of the last note of the melisma, or the left?
    // Comments in lyric-extender.hh say left, but right looks better to me. GP.

    // Left:
//     extender->set_elt_property("right-trim-amount", gh_double2scm(0.0));

    // Right:
    Real ss = extender->paper_l ()->get_var ("staffspace");
    extender->set_elt_property("right-trim-amount", 
			       gh_double2scm(-notehead_l_->extent(X_AXIS).length()/ss));
  }
}

bool 
Syllable_group::set_lyric_align(const char *punc, Score_element *default_notehead_l)
{
  if(lyric_list_.size()==0) {
    // No lyrics: nothing to do.
    return true;
  }

  Score_element * lyric;
  alignment_i_ = appropriate_alignment(punc);

  // If there was no notehead in the matching voice context, use the first 
  // notehead caught from any voice context (any port in a storm).
  if(!notehead_l_) {
    notehead_l_ = default_notehead_l;
  }

  group_translation_f_ = amount_to_translate();

  // set the x alignment of each lyric
  for(int l = 0; l < lyric_list_.size(); l++) {
    lyric = lyric_list_[l];
    lyric->set_elt_property("self-alignment-X", gh_int2scm(alignment_i_));
    // centre on notehead ... if we have one. 
    if(notehead_l_) {
      lyric->set_parent(notehead_l_, X_AXIS);
      lyric->add_offset_callback (Side_position::centered_on_parent, X_AXIS);
      // reference is on the right of the notehead; move it left half way, and add translation
      lyric->translate_axis (group_translation_f_-(notehead_l_->extent(X_AXIS)).center(), X_AXIS);
    }
  }
  return (notehead_l_);
}

/** determine the distance to translate lyrics to get correct alignment
    Rules: If alignment is centre, translate = 0
           Otherwise,
	      If (length of longest lyric) < 2 * (length of shortest lyric),
	         - centre longest lyric on notehead
	      Otherwise
	         - move so shortest lyric just reaches notehead centre
*/
Real 
Syllable_group::amount_to_translate()
{
  Real translate = 0.0;
  if(alignment_i_ != CENTER) {
    // FIXME: do we really know the lyric extent here? Some font sizing comes later?
    if((longest_lyric_l_->extent(X_AXIS)).length() <
       (shortest_lyric_l_->extent(X_AXIS)).length() * 2 )
      translate = alignment_i_*(longest_lyric_l_->extent(X_AXIS)).length()/2;
    else
      translate = alignment_i_*(shortest_lyric_l_->extent(X_AXIS)).length();
  }
  return translate;
}


/** determine what alignment we want.
    Rules: if first_in_phrase_b_ is set, then alignment is LEFT.
           otherwise if each syllable ends in punctuation, then alignment is RIGHT
	   otherwise alignment is centre.
*/
int 
Syllable_group::appropriate_alignment(const char *punc)
{
  if(first_in_phrase_b_)
    return LEFT;

  Score_element * lyric;
  bool end_phrase = true;

  for(int l = 0; l < lyric_list_.size() && end_phrase; l++) {
    lyric = lyric_list_[l];
    SCM lyric_scm = lyric->get_elt_property("text");
    String lyric_str = gh_string_p(lyric_scm)?ly_scm2string(lyric_scm):"";
    char lastchar;
    if(lyric_str.length_i()>0) {
      lastchar = lyric_str[lyric_str.length_i()-1];
      /* If it doesn't end in punctuation then it ain't an end of phrase */
      if(! strchr(punc, lastchar)) {
	/* Special case: trailing space. Here examine the previous character and reverse the
	   sense of the test (i.e. trailing space makes a break without punctuation, or 
	   suppresses a break with punctuation).
	   This behaviour can be suppressed by including a space in the 
	   phrasingPunctuation property, in which case trailing space always means 
	   the same as punctuation.

	   FIXME: The extra space throws alignment out a bit.
	*/
	if(lastchar == ' ') {
	  if(lyric_str.length_i()>1) {
	    lastchar = lyric_str[lyric_str.length_i()-2];
	    if(strchr(punc, lastchar))
	      end_phrase=false;
	  }
	}
	else
	  end_phrase=false;
      }
    }
  }
  if(end_phrase)
    return RIGHT;

  return CENTER;
}

/** We don't know about the melisma until after the initial alignment work is done, so go
    back and fix the alignment when we DO know.
*/
void
Syllable_group::adjust_melisma_align()
{
  if(notehead_l_ && lyric_list_.size()) {
    // override any previous offset adjustments
    Real translation = -group_translation_f_;
    // melisma aligning:
    switch (alignment_i_) {
      //  case LEFT: // that's all
    case CENTER: // move right so smallest lyric is left-aligned on notehead
      translation += (shortest_lyric_l_->extent(X_AXIS)).length()/2;
      break;
    case RIGHT: // move right so smallest lyric is left-aligned on notehead
      translation += (shortest_lyric_l_->extent(X_AXIS)).length();
      break;
    }
    group_translation_f_ += translation;
    for(int l = 0; l < lyric_list_.size(); l++) {
      lyric_list_[l]->translate_axis (translation, X_AXIS);
    }
  }
}


bool
Syllable_group::is_empty()
{
  return lyric_list_.size()==0;
}

void
Syllable_group::next_lyric()
{
  first_in_phrase_b_ = (alignment_i_ == RIGHT);
  clear();
}

/* SMOB */
SCM
Syllable_group::mark_smob (SCM)
{
  return SCM_EOL;
}

int
Syllable_group::print_smob (SCM, SCM port, scm_print_state * )
{
  scm_puts ("#<Syllable_group>", port);
  return 1;
}

IMPLEMENT_UNSMOB(Syllable_group, voice_entry);
IMPLEMENT_SIMPLE_SMOBS(Syllable_group);
IMPLEMENT_DEFAULT_EQUAL_P(Syllable_group);

SCM
Syllable_group::make_entry ()
{
  Syllable_group *vi = new Syllable_group;
  return vi->smobbed_self ();
}
