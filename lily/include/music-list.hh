/*
  music-list.hh -- declare Music_list, Chord and Voice

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSIC_LIST_HH
#define MUSIC_LIST_HH

#include "music.hh"
#include "plist.hh"


/**
  Music can be a list of other "Music" elements
 */
class Music_list : public Music
{
    
public:
  int multi_level_i_;

  Musical_pitch do_relative_octave (Musical_pitch, bool); 
  Music_list (Music_list const&);
  Music_list();
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Music_list,Music);
  virtual void add (Music*);
  virtual void transpose (Musical_pitch );
    
  Pointer_list<Music*> music_p_list_;
protected:
 
  virtual void do_print() const;
};

/**
  Chord is a list of music-elements which happen simultaneously
 */

class Chord : public Music_list
{
public:
  Chord();
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Chord,Music);
  
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual void translate (Moment dt);
  virtual MInterval time_int() const;
};

/**
  The request is a collection of Requests. A note that you enter in mudela is 
  one Request_chord, one syllable of lyrics is one Request_chord
 */
class Request_chord : public Chord
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  Request_chord();
  VIRTUAL_COPY_CONS(Request_chord, Music);
};
/**
  Voice is a list of music-elements which are placed behind each other.
 */
class Voice : public Music_list
{
    
public:
  Moment offset_mom_;
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  Voice();
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Voice, Music);
  virtual void translate (Moment dt);
  virtual MInterval time_int() const;
};
#endif // MUSIC_LIST_HH
