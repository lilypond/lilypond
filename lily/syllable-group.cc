#include <string.h>

#include "lyric-phrasing-engraver.hh"
#include "note-head.hh"
#include "translator-group.hh"
#include "side-position-interface.hh"
#include "ly-smobs.icc"
#include "spanner.hh"
#include "paper-def.hh"


/*
  Syllable_group is a class to be smobbed and entered as data in the
  association list member of the Lyric_phrasing_engraver class.
*/

Syllable_group::Syllable_group ()
{
  first_in_phrase_b_=true;
  melisma_b_ = false;
  clear ();
}

void 
Syllable_group::clear ()
{
  notehead_=0;
  lyrics_.clear ();
  longest_lyric_=0;
  shortest_lyric_=0;
  melisma_b_ = false;
  group_translation_ = 0.0;
}
  
void
Syllable_group::copy (Syllable_group *from)
{
  notehead_ = from->notehead_;
  lyrics_ = from->lyrics_;
  longest_lyric_ = from->longest_lyric_;
  shortest_lyric_ = from->shortest_lyric_;
  melisma_b_ = from->melisma_b_;
  alignment_ = from->alignment_;
  first_in_phrase_b_ = from->first_in_phrase_b_;
  group_translation_ = from->group_translation_;
}

void 
Syllable_group::set_first_in_phrase (bool f) 
{ 
  first_in_phrase_b_ = f; 
}

void 
Syllable_group::set_notehead (Grob * notehead)
{
  if (!notehead_)
    {
      /* there should only be a single notehead, so silently ignore
	 any extras */
      notehead_=notehead;
    }
}

void 
Syllable_group::add_lyric (Grob * lyric)
{
  lyrics_.push (lyric);
  /* record longest and shortest lyrics */
  if (longest_lyric_)
    {
      if (lyric->extent (lyric,X_AXIS).length () > (longest_lyric_->extent (longest_lyric_, X_AXIS)).length ())
	longest_lyric_ = lyric;
      if (lyric->extent (lyric, X_AXIS).length () < (shortest_lyric_->extent (shortest_lyric_, X_AXIS)).length ())
	shortest_lyric_ = lyric;
    }
  else
    longest_lyric_ = shortest_lyric_ = lyric;
}

void 
Syllable_group::add_extender (Grob * extender)
{
  if (notehead_ && melisma_b_)
    {
      dynamic_cast<Spanner*> (extender)->set_bound (RIGHT, notehead_);
      // should the extender finish at the right of the last note of the melisma, or the left?
      // Comments in lyric-extender.hh say left, but right looks better to me. GP.

      // Left:
      //     extender->set_grob_property ("right-trim-amount", gh_double2scm (0.0));

      // Right:
      Real ss = 1.0;
      extender->set_grob_property ("right-trim-amount", 
				   gh_double2scm (-notehead_->extent (notehead_, X_AXIS).length ()/ss));
    }
}

bool 
Syllable_group::set_lyric_align (const char *punc, Grob *default_notehead)
{
  if (lyrics_.size ()<=1)
    {
      // No lyrics or single line: nothing to do.
      return true;
    }

  Grob * lyric;
  alignment_ = appropriate_alignment (punc);
  
  /* If there was no notehead in the matching voice context, use the
   first notehead caught from any voice context (any port in a storm).


   Is this wise? Can't the lyric simply be set on a the paper-column,
   and be done with it. That's just as correct, and won't give strange
   results if the port-in-the-storms happesn to be involved in a
   note-collision? --hwn.
  */
  if (!notehead_)
    {
      notehead_ = default_notehead;
    }

  group_translation_ = amount_to_translate ();

  // set the x alignment of each lyric
  for (int l = 0; l < lyrics_.size (); l++)
    {
      lyric = lyrics_[l];
      lyric->set_grob_property ("self-alignment-X", gh_int2scm (alignment_));
      if (notehead_)
	{
	  /*
	    Centering on parent is done by default (see
	    grob-description.scm); we only have to set the parent.
	  */
	  lyric->set_parent (notehead_, X_AXIS);
	  lyric->translate_axis (group_translation_, X_AXIS);
	}
    }
  return (notehead_);
}

/** determine the distance to translate lyrics to get correct alignment
    Rules: If alignment is centre, translate = 0
    Otherwise,
    If (length of longest lyric) < (property {begin,end}-alignment) * (length of shortest lyric),
    - centre longest lyric on notehead
    Otherwise
    - move so shortest lyric just reaches notehead centre
*/
Real 
Syllable_group::amount_to_translate ()
{
  Real translate = 0.0;
  if (alignment_ != CENTER)
    {
      switch (alignment_)
	{
	  // FIXME: do we really know the lyric extent here? Some font sizing comes later?
	case LEFT: 
	  translate =  longest_lyric_->extent (longest_lyric_, X_AXIS).length () / gh_scm2double (longest_lyric_->get_grob_property("begin-alignment"));
	  break;
	case RIGHT: 
	  translate =   longest_lyric_->extent (longest_lyric_, X_AXIS).length () / gh_scm2double (longest_lyric_->get_grob_property("end-alignment"));
	  break;
	}
      if (!gh_scm2bool(longest_lyric_->get_grob_property("ignore-length-mismatch")))
	{
	  Real l = shortest_lyric_->extent (shortest_lyric_, X_AXIS).length ();
	  translate = l <? translate;
	}
    
      translate *= alignment_ ;
    }
  return translate;
}


/** determine what alignment we want.
    Rules: if property alignment is set it specifies the alignment
    if first_in_phrase_b_ is set, then alignment is LEFT.
    otherwise if each syllable ends in punctuation, then alignment is RIGHT
    otherwise alignment is centre.
*/
int 
Syllable_group::appropriate_alignment (const char *punc)
{
  SCM s=this->longest_lyric_->get_grob_property ("alignment");
  if (s!=SCM_EOL)
    {
      return gh_scm2int (s);
    }

  if (first_in_phrase_b_)
    return LEFT;

  Grob * lyric;
  bool end_phrase = true;

  for (int l = 0; l < lyrics_.size () && end_phrase; l++)
    {
      lyric = lyrics_[l];
      SCM lyric_scm = lyric->get_grob_property ("text");
      String lyric_string = gh_string_p (lyric_scm)?ly_scm2string (lyric_scm):"";
      char lastchar;
      if (lyric_string.length ()>0)
	{
	  lastchar = lyric_string[lyric_string.length ()-1];
	  /* If it doesn't end in punctuation then it ain't an end of phrase */
	  if (! strchr (punc, lastchar))
	    {
	      /*
		FIXME: Document this.
	  
		Special case: trailing space. Here examine the
		previous character and reverse the sense of the test
		(i.e. trailing space makes a break without
		punctuation, or suppresses a break with punctuation).
		This behaviour can be suppressed by including a space
		in the phrasingPunctuation property, in which case
		trailing space always means the same as punctuation.

		FIXME: The extra space throws alignment out a bit.
	      */
	      if (lastchar == ' ')
		{
		  if (lyric_string.length ()>1)
		    {
		      lastchar = lyric_string[lyric_string.length ()-2];
		      if (strchr (punc, lastchar))
			end_phrase=false;
		    }
		}
	      else
		end_phrase=false;
	    }
	}
    }
  if (end_phrase)
    return RIGHT;

  return CENTER;
}

/** We don't know about the melisma until after the initial alignment work is done, so go
    back and fix the alignment when we DO know.
*/
void
Syllable_group::adjust_melisma_align ()
{
  if (notehead_ && lyrics_.size ())
    {
      // override any previous offset adjustments
      Real translation = -group_translation_;
      // melisma aligning:
      switch (alignment_)
	{
	  //  case LEFT: // that's all
	case CENTER: // move right so smallest lyric is left-aligned on notehead
	  translation += (shortest_lyric_->extent (shortest_lyric_, X_AXIS)).length ()/2;
	  break;
	case RIGHT: // move right so smallest lyric is left-aligned on notehead
	  translation += (shortest_lyric_->extent (shortest_lyric_, X_AXIS)).length ();
	  break;
	}
      group_translation_ += translation;
      for (int l = 0; l < lyrics_.size (); l++)
	{
	  lyrics_[l]->translate_axis (translation, X_AXIS);
	}
    }
}


bool
Syllable_group::is_empty ()
{
  return lyrics_.size ()==0;
}

void
Syllable_group::next_lyric ()
{
  first_in_phrase_b_ = (alignment_ == RIGHT);
  clear ();
}

/* SMOB */
SCM
Syllable_group::mark_smob (SCM)
{
  return SCM_EOL;
}

int
Syllable_group::print_smob (SCM, SCM port, scm_print_state *)
{
  scm_puts ("#<Syllable_group>", port);
  return 1;
}


IMPLEMENT_SIMPLE_SMOBS (Syllable_group);
IMPLEMENT_DEFAULT_EQUAL_P (Syllable_group);

SCM
Syllable_group::make_entry ()
{
  Syllable_group *vi = new Syllable_group;
  return vi->smobbed_self ();
}

struct Lyric_syllable
{
  static bool has_interface (Grob*);
};
ADD_INTERFACE (Lyric_syllable,"lyric-syllable-interface",
	       "a single piece of lyrics",
	       "word-space alignment ignore-length-mismatch begin-alignment end-alignment");

