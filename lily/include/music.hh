/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/



#ifndef MUSIC_HH
#define MUSIC_HH

#include "virtual-methods.hh"
#include "input.hh"
#include "minterval.hh"
#include "lily-proto.hh"
#include "string.hh"

/** In Lily, everything that has a length and a pitch (which can be
  transposed) is considered "music", 

  Music is hierarchical: 

  @seealso Music_list
  */
class Music:public Input {
public:
    Music_list * parent_music_l_;

    virtual MInterval time_int()const;
    virtual ~Music(){}
    void print() const;
    virtual void transpose(Melodic_req const *);
    virtual void translate(Moment dt);
    VIRTUAL_COPY_CONS(Music,Music)
    NAME_MEMBERS();
    Music();
protected:
    virtual void do_print() const;
  
};

#endif // MUSIC_HH



