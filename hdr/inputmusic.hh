/*
  inputmusic.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INPUTMUSIC_HH
#define INPUTMUSIC_HH

#include "plist.hh"
#include "proto.hh"

struct Voice_list : public PointerList<Voice*> {
    void translate_time(Real dt);
    /// delete stuff; not in destructor!
    void junk();
};

/// ABC for input structures
struct Input_music {
    virtual Voice_list convert()=0;
    virtual Real length()=0;
    virtual void translate_time(Real dt)=0;
    virtual ~Input_music();
    virtual void print() const =0;
    // virtual void transpose(...) const =0;
};
/**

  Input_music is anything that can simply be regarded as/converted to
  a set of voices "cooperating" or independant. It has some basic
  characteristics that real music has too:

  - it is rhythmic (it has a length, and can be translated horizontally)
  - a pitch (it can be transposed)

  */



/// 
struct Vertical_music : Input_music {
    virtual Vertical_music *clone() const = 0;

    /// check if it is a simple voice
    virtual Vertical_simple *simple() { return 0;}
};
/**
  chord like :

  - different music forms which start at the same time ( stacked "vertically" )

  This class really doesn't do very much, but it enables you to say

  a Music_voice is a List<Vertical_music>
  
  */

///
struct Horizontal_music : Input_music {
    virtual Voice_list convert()=0;
    virtual Horizontal_music *clone() const = 0;
};
/**
  voice like.

  different music forms which start after each other ( concatenated,
  stacked "horizontally )

  This class really doesn't do very much, but it enables you to say

  a Chord is a List<Horizontal_music>
 
 */

/// the most basic element of a chord: a simple voice
struct Vertical_simple : Vertical_music {
    Voice * voice_;		// should be a  real member
    
    /****************/
    Vertical_simple(Vertical_simple const&);
    Vertical_simple();
    ~Vertical_simple();
    void add(Voice_element*);
    virtual Vertical_simple*simple() { return this; }
    virtual Real length();
    virtual Voice_list convert();
    virtual void translate_time(Real dt);
    virtual Vertical_music *clone() const {
	return new Vertical_simple(*this);
    }
    virtual void print() const ;
};

/// the only child of Horizontal_music
struct Music_voice : Horizontal_music {
    IPointerList<Vertical_music*> voice_ ;
    
    /****************/
    Music_voice() {}
    Music_voice(Music_voice const&);
    Real length();
    void add(Vertical_music*);
    void add(Voice_element*);
    virtual Voice_list convert();
    virtual void translate_time(Real dt);
    virtual Horizontal_music *clone() const {
	return new Music_voice(*this);
    }
    void concatenate(Music_voice*);
    virtual void print() const ;
};
///
struct Music_general_chord : Vertical_music {
    IPointerList<Horizontal_music*> chord_;

    /****************/
    Music_general_chord() {}
    Music_general_chord(Music_general_chord const&s);
    void add(Horizontal_music*);
    virtual Real length();
    virtual Voice_list convert();
    virtual void translate_time(Real dt);
    virtual Vertical_music *clone() const {
	return new Music_general_chord(*this);
    }
    void concatenate(Music_general_chord*);
    virtual void print() const ;
};


#endif // INPUTMUSIC_HH
