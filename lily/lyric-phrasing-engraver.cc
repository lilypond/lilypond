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

String get_context_id(Translator_group * ancestor, const char * type);
String trim_suffix(String &id);

ADD_THIS_TRANSLATOR (Lyric_phrasing_engraver);


Lyric_phrasing_engraver::Lyric_phrasing_engraver()
{
  voice_alist_ = SCM_EOL;
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
      return unsmob_voice_entry(gh_cdr(s));
    }
  }
  SCM val = Voice_alist_entry::make_entry (); 
  voice_alist_ = scm_acons(key, val, voice_alist_);
  return unsmob_voice_entry (val);
}


void 
Lyric_phrasing_engraver::record_notehead(const String &context_id, Score_element * notehead)
{
  Voice_alist_entry * v = lookup_context_id(context_id);
  v->set_notehead(notehead);
  //  voice_alist_ = 
  //    scm_assoc_set_x(voice_alist_, ly_str02scm(context_id.ch_C()), smobify(v));
}
  
void 
Lyric_phrasing_engraver::record_lyric(const String &context_id, Score_element * lyric)
{
  Voice_alist_entry * v = lookup_context_id(context_id);
  v->add_lyric(lyric);
  //  voice_alist_ = 
  //  scm_assoc_set_x(voice_alist_, ly_str02scm(context_id.ch_C()), smobify(v));
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
  Voice_alist_entry *entry;
  String punc;
  if (punc.empty_b()) {
    SCM sp = get_property("phrasingPunctuation");
    punc = gh_string_p(sp) ? ly_scm2string(sp) : ".,;?!"; 
  }

  for(unsigned v=0; v < gh_length(voice_alist_); v++) {
    entry = unsmob_voice_entry(gh_cdr(gh_list_ref(voice_alist_, gh_int2scm(v))));
    if(! entry->set_lyric_align(punc.ch_C()))
      warning (_ ("lyrics found without matching notehead ... aligning on self"));
  }
}


void
Lyric_phrasing_engraver::do_pre_move_processing ()
{
  Voice_alist_entry * entry;
  for(unsigned v=0; v < gh_length(voice_alist_); v++) {
    entry = unsmob_voice_entry(gh_cdr(gh_list_ref(voice_alist_, gh_int2scm(v))));
    entry->next_lyric();
  }
}



/*=========================================================================================*/

/** Voice_alist_entry is a class to be smobbed and entered as data in the association list
    member of the Lyric_phrasing_engraver class.
*/

Voice_alist_entry::Voice_alist_entry()
{
  first_in_phrase_b_=true;
  clear();
}




void 
Voice_alist_entry::clear()
{
  notehead_l_=0;
  lyric_list_.clear();
  longest_lyric_=-1;
  shortest_lyric_=-1;
}
  
void 
Voice_alist_entry::set_first_in_phrase(bool f) 
{ 
  first_in_phrase_b_ = f; 
}

void 
Voice_alist_entry::set_notehead(Score_element * notehead)
{
  if(!notehead_l_) 
    /* there should only be a single notehead, so silently ignore any extras */
    notehead_l_=notehead;
}

void 
Voice_alist_entry::add_lyric(Score_element * lyric)
{
  int this_lyric = lyric_list_.size();
  lyric_list_.push(lyric);
  /* record longest and shortest lyrics */
  if(longest_lyric_>-1) {
    Real this_length = (lyric->extent(X_AXIS)).length();
    if(this_length > (lyric_list_[longest_lyric_]->extent(X_AXIS)).length())
      longest_lyric_ = this_lyric;
    if(this_length < (lyric_list_[shortest_lyric_]->extent(X_AXIS)).length())
      shortest_lyric_ = this_lyric;
  }
  else
    longest_lyric_ = shortest_lyric_ = this_lyric;
}
  
bool 
Voice_alist_entry::set_lyric_align(const char *punc)
{
  if(lyric_list_.size()<2) {
    /* Only for multi-stanza songs ... if we've only a single lyric (or none at all) we
       do nothing.
    */
    clear();
    return true;
  }

  Score_element * lyric;
  alignment_i_ = appropriate_alignment(punc);

  for(int l = 0; l < lyric_list_.size(); l++) {
    /** set the x alignment of each lyric
     */
    lyric = lyric_list_[l];
    lyric->set_elt_property("self-alignment-X", gh_int2scm(alignment_i_));

    // centre on notehead 

    if(notehead_l_) {
      /* set the parent of each lyric to the notehead,
	 set the offset callback of each lyric to centered_on_parent,
      */
      lyric->set_parent(notehead_l_, X_AXIS);
      lyric->add_offset_callback (Side_position::centered_on_parent, X_AXIS);
      /* reference is on the right of the notehead; move it left half way */
      lyric->translate_axis (-(notehead_l_->extent(X_AXIS)).center(), X_AXIS);
    }
    else {
      /* No matching notehead: just align to the first lyric, and
	  issue a warning about lyric without matching notehead
      */
      if(l) {
	lyric->set_parent(lyric_list_[0], X_AXIS);
	lyric->add_offset_callback (Side_position::centered_on_parent, X_AXIS);
      }
      else
	lyric->add_offset_callback (Side_position::aligned_on_self, X_AXIS);
    }

    if(alignment_i_ != CENTER) {
      // right or left align ... 
      /* If length of longest lyric < 2 * length of shortest lyric,
	   - centre longest lyric on notehead
	 Otherwise
	   - move so shortest lyric just reaches notehead centre
      */
      // FIXME: do we really know the lyric extent here? Some font sizing comes later?
      Real translate;
      if((lyric_list_[longest_lyric_]->extent(X_AXIS)).length() <
	 (lyric_list_[shortest_lyric_]->extent(X_AXIS)).length() * 2 )
	translate = alignment_i_*(lyric_list_[longest_lyric_]->extent(X_AXIS)).length()/2;
      else
	translate = alignment_i_*(lyric_list_[shortest_lyric_]->extent(X_AXIS)).length();
      lyric->translate_axis (translate, X_AXIS);	  
      }
    }

  return (notehead_l_ != 0);
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
    if(lyric_str.length_i()>1) {
      lastchar = lyric_str[lyric_str.length_i()-2];
      /* We look at the second last character, because lily always appends a space. */
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
	  if(lyric_str.length_i()>2) {
	    lastchar = lyric_str[lyric_str.length_i()-3];
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

#include "ly-smobs.icc"

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
