/*
  input-music.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef INPUTMUSIC_HH
#define INPUTMUSIC_HH

#include "plist.hh"
#include "lily-proto.hh"
#include "voice.hh"
#include "moment.hh"

struct Voice_list : public Link_list<Voice*> {
    void translate_time(Moment dt);
};

/**

  A set voices.
  Input_music is anything that can simply be regarded as/converted to
  a set of voices "cooperating" or independant. It has some basic
  characteristics that real music has too:

  - it is rhythmic (it has a length, and can be translated horizontally)
  - a pitch (it can be transposed)

  */
struct Input_music {
    virtual Voice_list convert()const=0;
    void check_plet(Voice_element* velt_l);
    virtual Moment length()const=0;
    virtual void translate_time(Moment dt)=0;
    virtual ~Input_music(){}
    virtual void print() const =0;
    virtual void set_default_group(String)=0;
    virtual bool find_plet_start_b(char c, Moment& moment_r) = 0;
    virtual void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i) = 0;
    virtual void transpose(Melodic_req const&) const =0;
    
    
    virtual Input_music *clone() const = 0;
    virtual Simple_music *simple() { return 0; }
};

/// Simple music consists of one voice
struct Simple_music : Input_music {
    Voice voice_;

    /* *** */
    virtual void transpose(Melodic_req const&) const ;

    virtual Simple_music*simple() { return this; }  
    void add(Voice_element*);
    virtual void set_default_group(String g) { voice_.set_default_group(g); }
    virtual Moment length()const;
    virtual Voice_list convert()const;
    virtual void translate_time(Moment dt);
    virtual void print() const;
    virtual bool find_plet_start_b(char c, Moment& moment_r);
    virtual void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i);
    virtual Input_music *clone() const {
	return new Simple_music(*this);
    }
};

/// Complex_music consists of multiple voices
struct Complex_music : Input_music {
    Pointer_list<Input_music*> elts;
    /* *************** */
    virtual void transpose(Melodic_req const&) const ;
    virtual void set_default_group(String g);
    void add(Input_music*inmusic_p);
    Complex_music();
    Complex_music(Complex_music const &);
    virtual void print() const ;
    void concatenate(Complex_music*);
    virtual bool find_plet_start_b(char c, Moment& moment_r);
    virtual void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i);
};


/**
  A voice like list of music.

  different music forms which start after each other ( concatenated,
  stacked "horizontally )
 
 */

struct Music_voice : Complex_music {
 
    
    /* *************** */
    Moment length()const;
    virtual void translate_time(Moment dt);
    virtual Voice_list convert()const;
    void add_elt(Voice_element*);
    virtual Input_music *clone() const {
	return new Music_voice(*this);
    }
    virtual void print() const ;
};

/**
  Multiple musicstuff stacked on top of each other
  chord like :

  - different music forms which start at the same time ( stacked "vertically" )
  
  */
struct Music_general_chord : Complex_music {


    /* *************** */

    virtual Moment length()const;
    virtual Voice_list convert()const;
    virtual void translate_time(Moment dt);
    void add_elt(Voice_element*);
    virtual Input_music *clone() const {
	return new Music_general_chord(*this);
    }
    
    virtual void print() const ;
};

struct Multi_voice_chord : Music_general_chord {
    void set_default_group(String);
    virtual Input_music *clone() const {
	return new Multi_voice_chord(*this);
    }
};
struct Voice_group_chord : Music_general_chord {

    virtual Input_music *clone() const {
	return new Voice_group_chord(*this);
    }
};
#endif // INPUTMUSIC_HH
