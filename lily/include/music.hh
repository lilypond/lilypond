/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/



#ifndef MUSIC_HH
#define MUSIC_HH

#include "virtual-methods.hh"
#include "input.hh"
#include "minterval.hh"
#include "lily-proto.hh"
#include "string.hh"

/** 
  Anything which has length or pitch.
  
  In Lily, everything that can be thought to have a length and a pitch
  (which has a duration which can be transposed) is considered "music",

  Music is hierarchical: 

  @see Music_list

  */
class Music:public Input {
public:
  Music_list * parent_music_l_;
    
  /** The kind of iterator needed to walk this music.  This doesn't
    make sense for simple (ie non-list) music, but it does no harm
    here. Yes, it did harm Music_list: you can forget to copy it.
      
    */
  String translator_type_str_;

  /// what name (or look for this name)
  String translator_id_str_;    

  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual MInterval time_int() const;
  virtual ~Music(){}
  void print() const;
  virtual void transpose (Musical_pitch );
  virtual void translate (Moment dt);
  VIRTUAL_COPY_CONS(Music,Music);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Music();
protected:
  virtual void do_print() const;
};

#endif // MUSIC_HH



