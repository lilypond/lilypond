/*
  music-list.hh -- declare Music_sequence,
  Simultaneous_music and Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_sequence_HH
#define Music_sequence_HH

#include "music.hh"
#include "cons.hh"


class Music_list : public Cons_list<Music> 
{
public:
  Musical_pitch do_relative_octave (Musical_pitch, bool); 
  Music_iterator* do_rhythm (Music_iterator*); 
  void add_music (Music*);
  Music_list (Music_list const&);
  Music_list ();
};


/**
  Music can be a list of other "Music" elements
 */
class Music_sequence : public Music
{
public:
  Music_list * music_p_list_p_;

  Music_sequence (Music_sequence const&);
  Music_sequence (Music_list *l_p);
  
  VIRTUAL_COPY_CONS(Music);
  Musical_pitch do_relative_octave (Musical_pitch p, bool b);
  Music_iterator* do_rhythm (Music_iterator*);
  virtual void transpose (Musical_pitch );
  virtual void compress (Moment);
  void add_music (Music *music_p);
  int length_i () const;
  Moment cumulative_length () const;
  Moment maximum_length () const;
  virtual ~Music_sequence ();
  
protected:
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual Music_iterator* to_rhythm (Music_iterator*);
  virtual void do_print() const;
};

/**
  Simultaneous_music is a list of music-elements which happen simultaneously
 */
class Simultaneous_music : public Music_sequence
{
public:
  
  VIRTUAL_COPY_CONS(Music);
  
  Simultaneous_music(Music_list *);
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual Music_iterator* to_rhythm (Music_iterator*);
  virtual Moment length_mom () const;
};


/**
  The request is a collection of Requests. A note that you enter in mudela is 
  one Request_chord, one syllable of lyrics is one Request_chord
 */
class Request_chord : public Simultaneous_music
{
public:
  VIRTUAL_COPY_CONS(Music);
  
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual Music_iterator* to_rhythm (Music_iterator*);
  Request_chord();
};


/**
  Sequential_music is a list of music-elements which are placed behind each other.
 */
class Sequential_music : public Music_sequence
{
public:
  VIRTUAL_COPY_CONS(Music);

  Sequential_music(Music_list*);
  virtual Moment length_mom () const;
};
#endif // Music_sequence_HH
