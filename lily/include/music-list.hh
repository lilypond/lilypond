/*
  music-list.hh -- declare Music_list, Chord and Voice

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSIC_LIST_HH
#define MUSIC_LIST_HH

#include "music.hh"
#include "plist.hh"


/**
  Music can be a list of other "Music" elements
 */
class Music_list : public Music {
    
public:
    int multi_level_i_;
  
    /// what kind of iterator needed to walk this music?
    String type_str_;

    /// what name (or look for this name)
    String id_str_;    
    Music_list(Music_list const&);    
    Music_list();
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Music_list,Music);
    virtual void add(Music*);
    virtual void transpose(Melodic_req const *);
    
    Pointer_list<Music*> music_p_list_;
protected:
 
    virtual void do_print() const;
};

/**
  Chord is a list of music-elements which happen simultaneously
 */

class Chord : public Music_list {
public:
    Chord();
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Chord,Music);
    virtual void translate(Moment dt);
    virtual MInterval time_int()const;
};

class Voice_element : public Chord {
public:
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Voice_element,Music);
};

/**
  Voice is a list of music-elements which are placed behind each other.
 */
class Voice : public Music_list {
    
public:
    Moment offset_mom_;

    Voice();
    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Voice, Music);
    virtual void translate(Moment dt);
    virtual MInterval time_int()const;
};

/** A simple piece of music, which wishes to change the spot of its
  interpretor  */
class Change_reg : public Music {
public:
    
    /// what kind of iterator needed to walk this music?
    String type_str_;

    /// what name (or look for this name)
    String id_str_;

    NAME_MEMBERS();
    VIRTUAL_COPY_CONS(Change_reg, Music);
};
#endif // MUSIC_LIST_HH
