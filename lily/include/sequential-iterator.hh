/*   
  sequential-iterator.hh -- declare Sequential_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef SEQUENTIAL_ITERATOR_HH
#define SEQUENTIAL_ITERATOR_HH

#include "music-iterator.hh"
#include "protected-scm.hh"

/*

  This is a lookahead list for grace notes.

   {  ... X \grace Y  Z ... }

   normally, the ending of X is the start of Z. In case of a grace
   note, we take off a little at the end of X. What is stored: START
   (start point of X), LENGTH (length of X), GRACE_START (start_music
   of Y), and the next fixup element.

  This is also done for nested musics, i.e.

  voiceA = \notes { \grace b16 c'2 }
  voiceB = \notes { c'2 \voiceA }

  the iterator for voiceB will contain a fixup entry with (START=0/1,
  LENGTH=2/1, GRACE_START=(0G-1/16) ) 
   
  Graces at the start of a sequential music iterator are handled
  by initting here_mom_ with Music::start_music (); no fixups are needed.

*/
struct Grace_fixup 
{
  Moment start_;
  Moment length_;

  Rational grace_start_;  
  Grace_fixup *next_;
};

/** Sequential_music iteration: walk each element in turn, and
  construct an iterator for every element.
  
 */
class Sequential_iterator :  public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK(constructor, ());
  Sequential_iterator ();
  Sequential_iterator (Sequential_iterator const&);
  virtual void derived_substitute (Context *f, Context *t) ;

  virtual void derived_mark () const;

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit(); 
  virtual bool ok () const;

protected:
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;
  virtual bool run_always () const;
protected:
  Music_iterator * iter_;
  virtual SCM get_music_list ()const;
  virtual void next_element (bool side_effect);


  Grace_fixup *get_grace_fixup () const;
  void next_grace_fixup ();
  
private:
  Moment here_mom_;
  SCM list_;
  SCM cursor_;
  Grace_fixup * grace_fixups_;
  
  virtual void descend_to_child ();
};

#endif /* SEQUENTIAL_ITERATOR_HH */
