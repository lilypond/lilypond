/*
  lyric-phrasing-engraver.hh -- declare Lyric_phrasing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  2000 Glen Prideaux <glenprideaux@iname.com>
*/


#ifndef LYRIC_PHRASING_ENGRAVER_HH
#define LYRIC_PHRASING_ENGRAVER_HH

#include "lily-proto.hh"
#include "engraver.hh"
#include "item.hh"
#include "smobs.hh"

class Syllable_group;


/**
   Align lyrics with noteheads, left aligning beginning of phrases,
   right aligning end of phrases, centering others under their notes.
 */


/* 
* Build an engraver that catches noteheads and lyrics.

(It needs to be in a context above Staff and Lyrics, eg. in Score
context.)

* Determine which heads belong to which lyrics

(eg. by looking at the names of their originating contexts, or maybe
some \properties)

* Attach the lyrics to the appropriate heads

(by doing lyric->set_parent (head, X_AXIS), that will fix the current
noteheadwidth guessing kludge)

* Check if the lyric syllables end or start a phrase.

(eg. check if the syllable ends with punctuation, and remember that
fact for the next one.)

* Adjust their alignment accordingly. 

(eg. by doing lyric->add_offset_callback(centered_on_parent,X_AXIS)
and setting self-alignment-X)

* Add a property to switch on/off the engraver (for multi stanza
  vs. single stanza music)

Maybe this engraver could also take care of correct lyric alignment
for melismas as well.


 */


class Lyric_phrasing_engraver : public Engraver 
{
protected:
  virtual void acknowledge_element(Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing();
  virtual void do_removal_processing ();  
private:
  void record_notehead(const String &context_id, Score_element * notehead);
  void record_lyric(const String &context_id, Score_element * lyric);
  void record_melisma(const String &context_id);
  void record_extender(const String &context_id, Score_element * extender);
  Syllable_group * lookup_context_id(const String &context_id);

public:
  Lyric_phrasing_engraver ();
  ~Lyric_phrasing_engraver ();
  VIRTUAL_COPY_CONS (Translator);

private:
  /** association list of Syllable_group smobs
  */
  Protected_scm voice_alist_;
  Score_element * any_notehead_l_;
};


class Syllable_group
{
  bool first_in_phrase_b_;
  Score_element * notehead_l_;
  Link_array<Score_element> lyric_list_;
  Score_element * longest_lyric_l_;
  Score_element * shortest_lyric_l_;
  int alignment_i_;
  bool melisma_b_;
  Real group_translation_f_;
public:
  static SCM make_entry();
  void set_first_in_phrase(bool f);
  void set_notehead(Score_element * notehead);
  void add_lyric(Score_element * lyric);
  void add_extender(Score_element * extender);
  void set_melisma() { melisma_b_ = true; }
  bool get_melisma() { return melisma_b_; }
  int lyric_count() { return lyric_list_.size(); }
  void clear();
  bool is_empty();
  bool set_lyric_align(const char *punc, Score_element *default_notehead_l);
  void adjust_melisma_align();
  int appropriate_alignment(const char *punc);
  Real amount_to_translate();
  void next_lyric();
  void copy(Syllable_group *);
private:
  Syllable_group();
  DECLARE_SIMPLE_SMOBS(Syllable_group,);
} ;

Syllable_group * unsmob_voice_entry (SCM);


#endif // LYRIC_PHRASING_ENGRAVER_HH
