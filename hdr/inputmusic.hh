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

struct Vertical_music {
    virtual Vertical_simple *simple() { return 0;}
    virtual Voice_list convert()=0;
    virtual Real length()=0;
    virtual void translate_time(Real dt)=0;
    virtual Vertical_music *clone() const = 0;
    virtual ~Vertical_music() {}
    virtual void print() const =0;
};

struct Horizontal_music {
    virtual Voice_list convert()=0;
    virtual Real length()=0;
    virtual void translate_time(Real dt)=0;
    virtual Horizontal_music *clone() const = 0;
    virtual ~Horizontal_music() {}
    virtual void print() const =0;
};

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
