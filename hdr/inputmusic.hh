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
};

struct Horizontal_music {
    virtual Voice_list convert()=0;
    virtual Real length()=0;
    virtual void translate_time(Real dt)=0;
};

struct Horizontal_simple : Horizontal_music {
    Voice * voice_;
    
    /****************/

    Horizontal_simple();
    void set(Voice*);
    virtual Real length();
    virtual Voice_list convert();
    virtual void translate_time(Real dt);

};

struct Vertical_simple : Vertical_music {
    Voice * voice_;
    
    /****************/
    Vertical_simple();
    void add(Voice_element*);
    virtual Vertical_simple*simple() { return this; }
    virtual Real length();
    virtual Voice_list convert();
    virtual void translate_time(Real dt);
};

struct Music_voice : Horizontal_music {
    PointerList<Vertical_music*> voice_ ;
    
    /****************/

    Real length();
    void add(Vertical_music*);
    void add(Voice_element*);
    virtual Voice_list convert();
    virtual void translate_time(Real dt);
};

struct Music_general_chord : Vertical_music {
    PointerList<Horizontal_music*> chord_;

    /****************/
    void add(Horizontal_music*);
    virtual Real length();
    virtual Voice_list convert();
    virtual void translate_time(Real dt);
};


#endif // INPUTMUSIC_HH
