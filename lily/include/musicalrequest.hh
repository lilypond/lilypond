/*
  musicalrequests.hh -- declare Musical requests

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSICALREQUESTS_HH
#define MUSICALREQUESTS_HH

#include "request.hh"


/**
  A request which is coupled to a #Voice_element# with nonzero duration.
  Base class only
 */
struct Musical_req : virtual Request {
    virtual Skip_req* skip() { return 0; }
    REQUESTMETHODS(Musical_req, musical);
};


struct Skip_req : Musical_req {
    Moment duration_;
    
    virtual Moment duration() const;
    REQUESTMETHODS(Skip_req, skip);
};
/** a request with a duration.
  This request is used only a base class.
 */
struct Rhythmic_req : virtual Musical_req {
    int balltype;
    int dots;
    Moment plet_factor;
    /* *************** */
    static int compare(const Rhythmic_req &, const Rhythmic_req &);
    virtual Moment duration() const;
    Rhythmic_req();
    Rhythmic_req(int,int);
    REQUESTMETHODS(Rhythmic_req, rhythmic);
};

struct Spacing_req :virtual Request {
    Moment next;
    Real distance;
    Real strength;
    /* *************** */
    Spacing_req();
    REQUESTMETHODS(Spacing_req, spacing);
};

struct Blank_req : Spacing_req, Rhythmic_req {
    REQUESTMETHODS(Spacing_req, spacing);
    
};

/// Put a text above or below (?) this staff.
struct Text_req : virtual Musical_req {
    /// preferred position (above/below)
    int dir_i_;
    /// the characteristics of the text
    Text_def *tdef_p_;
    /* *************** */
    Text_req(int d, Text_def*);
    ~Text_req();
    Text_req(Text_req const&);
    static int compare(const Text_req&,const Text_req&);
    REQUESTMETHODS(Text_req,text);
};

/** Put a text in lyric_staff
  @see Lyric_staff
  */
struct Lyric_req : public Rhythmic_req, Text_req {
    Lyric_req(Text_def* t_p);
    REQUESTMETHODS(Lyric_req, lreq_l);
};

/// request which has some kind of pitch
struct Melodic_req :virtual Musical_req
{
    /// 0 is c, 6 is b
    int notename_i_;
    /// 0 is central c
    int octave_i_;

    /// 0 natural, 1 sharp, etc
    int accidental_i_;

    /// force/supress printing of accidental.
    bool forceacc_b_;

    /// return height from central c (in halflines)
    int height()const; 
    /// return pitch from central c (in halfnotes)
    int pitch()const; 
    Melodic_req();
   
    REQUESTMETHODS(Melodic_req,melodic);
};

/// Put a note of specified type, height, and with accidental on the staff.
struct Note_req : Rhythmic_req, virtual Melodic_req {
    

    Rhythmic_req* rhythmic() { return Rhythmic_req::rhythmic(); }
    REQUESTMETHODS(Note_req, note);
 };

/**
Put a rest on the staff. Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/
class Rest_req : public Rhythmic_req {
public:
    REQUESTMETHODS(Rest_req,rest);
};

/**
  attach a stem to the noteball.
  Rhythmic_req parent needed to  determine if it will fit inside a beam.
  */
struct Stem_req : Rhythmic_req {
    /// preferred direction for the stem
    int dir_i_;
    Stem_req(int s, int dots);
    REQUESTMETHODS(Stem_req,stem);
};

/**
  Requests to start or stop something.
 This type of request typically results in the creation of a #Spanner#
*/
struct Span_req : Musical_req {
    /// should the spanner start or stop, or is it unwanted?
    enum {
	NOSPAN, START, STOP
    } spantype ;
    static int compare(const Span_req &r1, const Span_req &r2);
    REQUESTMETHODS(Span_req,span);

    Span_req();
  
};

/// request for backward plet generation
struct Plet_req : Request {
     char type_c_;
     int dur_i_;
     int type_i_;
     Plet_req();
 
     REQUESTMETHODS(Plet_req,plet);
};
/** 
*/

/** Start / stop a beam at this note.  if #nplet# is set, the staff will try to put an
appropriate number over the beam
    */
struct Beam_req : Span_req {
    int nplet;

    /* *************** */
     REQUESTMETHODS(Beam_req,beam);

    Beam_req();
};

/// a slur
struct Slur_req : Span_req {
 REQUESTMETHODS(Slur_req,slur);

};


/**Put a script above or below this ``note''. eg upbow, downbow. Why a
request? These symbols may conflict with slurs and brackets, so this
also a request */
struct Script_req : Musical_req {
    int dir_i_;
    Script_def *scriptdef_p_;

    /* *************** */
    static int compare(const Script_req &, const Script_req &);
    Script_req(int d, Script_def*);
    REQUESTMETHODS(Script_req,script);
    ~Script_req();
    Script_req(Script_req const&);
};




#endif // MUSICALREQUESTS_HH
