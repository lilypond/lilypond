/*
  duration.hh -- declare Duration Plet

  source file of the LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

*/

// split into 4?

#ifndef DURATION_HH
#define DURATION_HH

#include "fproto.hh"
#include "moment.hh"

// ugh, to get me in lily lib
extern bool no_triplets_bo_g;

/** (plet)
  The type and replacement value of a  plet (triplet, quintuplet.) Conceptually the same as a rational, but 4/6 != 2/3 
 */
struct Plet {
    Plet( int replace_i, int type_i );
    Plet();
    Moment mom()const;
    bool unit_b()const;
    int iso_i_;  // 2/3; 2 is not duration, maar of count!
    int type_i_;
};

/**
  Class to handle "musical" durations. This means: balltype 1,2,4,etc. and dots.
   
  (dur)
  */
struct Duration {
    /**
      Ctor of Duration. type_i should be a power of 2. 
       */
    Duration( int type_i = 1, int dots_i = 0);
    /// is the "plet factor" of this note != 1 ?
    bool plet_b();
    String str()const;
    void set_plet(int,int );
    void set_plet(Duration );
    static bool duration_type_b(int t);
    void set_ticks( int ticks_i );
    Moment length() const ;	// zo naai mij
    static int division_1_i_s;
    int type_i_;
    int dots_i_;
    Plet plet_;
    int ticks_i_;
};
#endif // DURATION_HH

