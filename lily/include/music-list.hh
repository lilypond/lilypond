/*
  music-list.hh -- declare Music_sequence, Simultaneous_music and Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_sequence_HH
#define Music_sequence_HH

#include "music.hh"
#include "plist.hh"


class Music_list : public Pointer_list<Music*>
{
public:
  Musical_pitch do_relative_octave (Musical_pitch, bool); 
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
  int multi_level_i_;
  Music_list * music_p_list_p_;

  Music_sequence (Music_sequence const&);
  Music_sequence (Music_list *l_p);
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Music_sequence,Music);

  virtual void transpose (Musical_pitch );
  virtual void compress (Moment);
  void add_music (Music *music_p);
protected:
  virtual void do_print() const;
};

/**
  Simultaneous_music is a list of music-elements which happen simultaneously
 */
class Simultaneous_music : public Music_sequence
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Simultaneous_music,Music);
  
  Simultaneous_music(Music_list *);
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual Moment duration () const;
};

/**
  The request is a collection of Requests. A note that you enter in mudela is 
  one Request_chord, one syllable of lyrics is one Request_chord
 */
class Request_chord : public Simultaneous_music
{
public:
  VIRTUAL_COPY_CONS(Request_chord, Music);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  Request_chord();
};
/**
  Sequential_music is a list of music-elements which are placed behind each other.
 */
class Sequential_music : public Music_sequence
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Sequential_music, Music);

  virtual Musical_pitch to_relative_octave (Musical_pitch);
  Sequential_music(Music_list*);
  virtual Moment duration () const;
};
#endif // Music_sequence_HH
