/*
  lyric-phrasing-engraver.cc -- implement Lyric_phrasing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  2000 Glen Prideaux <glenprideaux@iname.com>
*/
#include <string.h>

#include "lyric-phrasing-engraver.hh"
#include "note-head.hh"
#include "translator-group.hh"
#include "side-position-interface.hh"
#include "ly-smobs.icc"
#include "spanner.hh"
#include "paper-def.hh"


String get_context_id(Translator_group * ancestor, const char * type);
String trim_suffix(String &id);

ADD_THIS_TRANSLATOR (Lyric_phrasing_engraver);

/*
  We find start and end of phrases, and align lyrics accordingly.
  Also, lyrics at start of melismata should be left aligned.

  Alignment and melismata

  I've taken [a different] approach:
	  |      |
	  |      |
	 O      O  <-- second note throws a melisma score element
	  \____/

	 ^      ^
	 |      |
       Lyric (None)

  Lyric_phrasing_engraver keeps track of the current and previous notes and
  lyrics for each voice, and when it catches a melisma, it adjusts the
  alignment of the lyrics of the previous note. I hope this isn't
  unnecessarily convoluted.
 */

Lyric_phrasing_engraver::Lyric_phrasing_engraver()
{
  voice_alist_ = SCM_EOL;
  any_notehead_l_ = 0;
}

Lyric_phrasing_engraver::~Lyric_phrasing_engraver()
{
  /*
    No need to delete alist_; that's what Garbage collection is for.
   */
}

Voice_alist_entry * 
Lyric_phrasing_engraver::lookup_context_id(const String &context_id)
{
  SCM key = ly_str02scm(context_id.ch_C());
  if( ! gh_null_p(voice_alist_) ) {
    SCM s = scm_assoc(key, voice_alist_);
    if(! (gh_boolean_p(s) && !to_boolean(s))) {
      /* match found */
      // ( key . ( (alist_entry . old_entry) . previous_entry) )
      if(to_boolean(gh_cdadr(s))) { // it's an old entry ... make it a new one
	SCM val = gh_cons(gh_cons(gh_caadr(s), SCM_BOOL_F), gh_cddr(s)); 
	voice_alist_ = scm_assoc_set_x(voice_alist_, gh_car(s), val);
	return unsmob_voice_entry (gh_caar(val));
      }
      else { // the entry is current ... return it.
	SCM entry_scm = gh_caadr(s);
	return unsmob_voice_entry(entry_scm);
      }
    }
  }
  // ( ( alist_entry . old_entry ) . previous_entry )
  SCM val = gh_cons(gh_cons(Voice_alist_entry::make_entry (), SCM_BOOL_F), 
		    Voice_alist_entry::make_entry ()); 

  voice_alist_ = scm_acons(key, val, voice_alist_);
  return unsmob_voice_entry (gh_caar(val));
}


void 
Lyric_phrasing_engraver::record_notehead(const String &context_id, 
					 Score_element * notehead)
{
  Voice_alist_entry * v = lookup_context_id(context_id);
  v->set_notehead(notehead);
  if(!any_notehead_l_)
    any_notehead_l_ = notehead;
}
  
void 
Lyric_phrasing_engraver::record_lyric(const String &context_id, Score_element * lyric)
{
  Voice_alist_entry * v = lookup_context_id(context_id);
  v->add_lyric(lyric);
}

void 
Lyric_phrasing_engraver::record_extender(const String &context_id, Score_element * extender)
{
  SCM key = ly_str02scm(context_id.ch_C());
  if( ! gh_null_p(voice_alist_) ) {
    SCM s = scm_assoc(key, voice_alist_);
    if(! (gh_boolean_p(s) && !to_boolean(s))) {
      /* match found */
      // ( key . ( (alist_entry . old_entry) . previous_entry) )
      SCM previous_scm = gh_cddr(s);
      if(previous_scm != SCM_EOL) {
	Voice_alist_entry * v = unsmob_voice_entry(previous_scm);
	v->add_extender(extender);
      }
    }
  }
}

void 
Lyric_phrasing_engraver::record_melisma(const String &context_id)
{
  Voice_alist_entry * v = lookup_context_id(context_id);
  v->set_melisma();
}
  
void
Lyric_phrasing_engraver::acknowledge_element(Score_element_info i)
{
  SCM p = get_property("automaticPhrasing");
  if(!to_boolean(p))
    return;


  Score_element *h = i.elem_l_;

  if (Note_head::has_interface(h)) {
    /* caught a note head ... do something with it */
    /* ... but not if it's a grace note ... */
    bool grace= to_boolean (i.elem_l_->get_elt_property ("grace"));
    SCM wg = get_property ("weAreGraceContext");
    bool wgb = to_boolean (wg);
    if (grace != wgb)
      return;

    /* what's its Voice context name? */
    String voice_context_id = get_context_id(i.origin_trans_l_->daddy_trans_l_, "Voice");
    record_notehead(voice_context_id, h);
    return;
  }
  /* now try for a lyric */
  if (h->has_interface (ly_symbol2scm ("lyric-syllable-interface"))) {

    /* what's its LyricVoice context name? */
    String lyric_voice_context_id = 
      get_context_id(i.origin_trans_l_->daddy_trans_l_, "LyricVoice");
    record_lyric(trim_suffix(lyric_voice_context_id), h);
    return;
  }
  /* finally for a melisma */
  if(h->has_interface (ly_symbol2scm ("melisma-interface"))) {
    String voice_context_id = get_context_id(i.origin_trans_l_->daddy_trans_l_, "Voice");
    record_melisma(voice_context_id);
    return;
  }
    /* How about catching any extender items and then if we have a melisma, 
       set the RIGHT item of the extender spanner to the melismatic note in 
       the corresponding context (if any).
       This has the effect of finishing the extender under the last note
       of the melisma, instead of extending it to the next lyric.

       Problem: the extender request is thrown at the same moment as the next lyric,
       by which time we have already passed the last note of the melisma.
       However, the Lyric_phrasing_engraver remembers the last note, so just 
       attach it to that, provided it was melismatic. If it was not melismatic, 
       then ignore it and let the Extender_engraver take care of it (i.e. finish at next
       lyric).
    */
  if(h->has_interface (ly_symbol2scm ("lyric-extender-interface"))) {
    String voice_context_id = get_context_id(i.origin_trans_l_->daddy_trans_l_, "LyricVoice");
    record_extender(trim_suffix(voice_context_id), h);
    return;
  }
}

String 
get_context_id(Translator_group * ancestor, const char *type)
{
  while(ancestor != 0 && ancestor->type_str_ != type) {
    ancestor = ancestor->daddy_trans_l_;
  }

  if(ancestor != 0) {
    return ancestor->id_str_;
  }

  return "";
}

String 
trim_suffix(String &id)
{
  int index = id.index_i('-');
  if(index >= 0) {
    return id.left_str(index);
  }
  return id;
}


void Lyric_phrasing_engraver::process_acknowledged () 
{
  /* iterate through entries in voice_alist_
     for each, call set_lyric_align(alignment). Issue a warning if this returns false.
  */
  String punc;
  SCM sp = get_property("phrasingPunctuation");
  punc = gh_string_p(sp) ? ly_scm2string(sp) : ".,;:?!\""; 
  
  for(SCM v=voice_alist_; gh_pair_p(v); v = gh_cdr(v)) {
    SCM v_entry = gh_cdar(v);
    // ((current . oldflag) . previous)
    if(!to_boolean(gh_cdar(v_entry))) { // not an old entry left over from a prior note ...
      Voice_alist_entry *entry = unsmob_voice_entry(gh_caar(v_entry));
      if(! entry->set_lyric_align(punc.ch_C(), any_notehead_l_))
	warning (_ ("lyrics found without any matching notehead"));

      // is this note melismatic? If so adjust alignment of previous one.
      if(entry->get_melisma()) {
	if(entry->lyric_count())
	  warning (_ ("Huh? Melismatic note found to have associated lyrics."));
	SCM previous_scm = gh_cdr(v_entry);
	if(previous_scm != SCM_EOL) {
	  Voice_alist_entry *previous = unsmob_voice_entry(previous_scm);
	  if (previous->lyric_count())
	    previous->adjust_melisma_align();
	}
      }
    }
  }
}


void
Lyric_phrasing_engraver::do_pre_move_processing ()
{
  for(SCM v=voice_alist_; gh_pair_p(v); v = gh_cdr(v)) {
    SCM entry_scm = gh_cdar(v);
    // ((alist_entry . entry_is_old) . previous_entry)
    Voice_alist_entry * entry = unsmob_voice_entry(gh_caar(entry_scm));

    // set previous_entry, set entry_is_old, and resave it to alist_
    // but only change if this current was not old.
    if(! to_boolean(gh_cdar(entry_scm))) { 
      Voice_alist_entry * previous_entry = unsmob_voice_entry(gh_cdr(entry_scm));
      previous_entry->copy(entry);
      entry_scm = gh_cons(gh_cons(gh_caar(entry_scm), SCM_BOOL_T), gh_cdr(entry_scm));
      voice_alist_ = scm_assoc_set_x(voice_alist_, gh_caar(v), entry_scm);
    }
    entry->next_lyric();
  }
  any_notehead_l_ = 0;
}



/*=========================================================================================*/

/** Voice_alist_entry is a class to be smobbed and entered as data in the association list
    member of the Lyric_phrasing_engraver class.
*/

Voice_alist_entry::Voice_alist_entry()
{
  first_in_phrase_b_=true;
  melisma_b_ = false;
  clear();
}

void 
Voice_alist_entry::clear()
{
  notehead_l_=0;
  lyric_list_.clear();
  longest_lyric_l_=0;
  shortest_lyric_l_=0;
  melisma_b_ = false;
}
  
void
Voice_alist_entry::copy( Voice_alist_entry *from)
{
  notehead_l_ = from->notehead_l_;
  lyric_list_ = from->lyric_list_;
  longest_lyric_l_ = from->longest_lyric_l_;
  shortest_lyric_l_ = from->shortest_lyric_l_;
  melisma_b_ = from->melisma_b_;
  alignment_i_ = from->alignment_i_;
  first_in_phrase_b_ = from->first_in_phrase_b_;
}

void 
Voice_alist_entry::set_first_in_phrase(bool f) 
{ 
  first_in_phrase_b_ = f; 
}

void 
Voice_alist_entry::set_notehead(Score_element * notehead)
{
  if(!notehead_l_) {
    /* there should only be a single notehead, so silently ignore any extras */
    notehead_l_=notehead;
  }
}

void 
Voice_alist_entry::add_lyric(Score_element * lyric)
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
Voice_alist_entry::add_extender(Score_element * extender)
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

void 
Voice_alist_entry::set_melisma()
{
  melisma_b_ = true;
}
 
bool 
Voice_alist_entry::set_lyric_align(const char *punc, Score_element *default_notehead_l)
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
  Real translation = amount_to_translate();
  for(int l = 0; l < lyric_list_.size(); l++) {
    /** set the x alignment of each lyric
     */
    lyric = lyric_list_[l];
    lyric->set_elt_property("self-alignment-X", gh_int2scm(alignment_i_));

    // centre on notehead ... if we have one. 
    if(notehead_l_) {
      /* set the parent of each lyric to the notehead,
	 set the offset callback of each lyric to centered_on_parent,
      */
      lyric->set_parent(notehead_l_, X_AXIS);
      lyric->add_offset_callback (Side_position::centered_on_parent, X_AXIS);
      /* reference is on the right of the notehead; move it left half way, then centralise */
      lyric->translate_axis (translation-(notehead_l_->extent(X_AXIS)).center(), X_AXIS);
    }
  }
  return (notehead_l_);
}

Real 
Voice_alist_entry::amount_to_translate()
{
  Real translate = 0.0;
  if(alignment_i_ != CENTER) {
    // right or left align ... 
    /* If length of longest lyric < 2 * length of shortest lyric,
       - centre longest lyric on notehead
       Otherwise
       - move so shortest lyric just reaches notehead centre
    */
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
Voice_alist_entry::appropriate_alignment(const char *punc)
{
  if(first_in_phrase_b_)
    return LEFT;

  Score_element * lyric;
  bool end_phrase = true;
  /* use a property to determine what constitutes punctuation */

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
Voice_alist_entry::adjust_melisma_align()
{
  if(notehead_l_) {
    // undo what we did before ...
    Real translation = -amount_to_translate();
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
    for(int l = 0; l < lyric_list_.size(); l++) {
      lyric_list_[l]->translate_axis (translation, X_AXIS);
    }
  }
}


bool
Voice_alist_entry::is_empty()
{
  return lyric_list_.size()==0;
}

void
Voice_alist_entry::next_lyric()
{
  first_in_phrase_b_ = (alignment_i_ == RIGHT);
  clear();
}

/* SMOB */



SCM
Voice_alist_entry::mark_smob (SCM)
{
  return SCM_EOL;
}

int
Voice_alist_entry::print_smob (SCM, SCM port, scm_print_state * )
{
  scm_puts ("#<Voice_alist_entry>", port);
  return 1;
}

IMPLEMENT_UNSMOB(Voice_alist_entry, voice_entry);
IMPLEMENT_SIMPLE_SMOBS(Voice_alist_entry);
IMPLEMENT_DEFAULT_EQUAL_P(Voice_alist_entry);

SCM
Voice_alist_entry::make_entry ()
{
  Voice_alist_entry *vi = new Voice_alist_entry;
  return vi->smobbed_self ();
}
