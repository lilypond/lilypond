/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/



#ifndef MUSIC_HH
#define MUSIC_HH

#include "plist.hh"
#include "virtual-methods.hh"
#include "input.hh"
#include "minterval.hh"
#include "lily-proto.hh"

class Music:public Input {
public:
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

class Music_list : public Music {
public:
    Music_list(Music_list const&);    
    Music_list();
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Music_list,Music)
    virtual void add(Music*);
    virtual void transpose(Melodic_req const *);
protected:
    Pointer_list<Music*> music_p_list_;
 
    virtual void do_print() const;
};


class Chord : public Music_list {
public:
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Chord,Music)
    virtual void translate(Moment dt);
    virtual MInterval time_int()const;
};


class MVoice : public Music_list {
public:
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(MVoice, Music)
    virtual void translate(Moment dt);
    virtual MInterval time_int()const;
};

#endif // MUSIC_HH



