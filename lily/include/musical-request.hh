/*
  musical-request.hh -- declare Musical requests

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSICALREQUESTS_HH
#define MUSICALREQUESTS_HH

#include "lily-proto.hh"
#include "request.hh"
#include "duration.hh"


/**
  A request which is coupled to a #Voice_element# with nonzero duration.
  Base class only
 */
class Musical_req  : public virtual Request  {
public:
    
    virtual Skip_req* skip() { return 0; }
    virtual Dynamic_req* dynamic() { return 0; }
    virtual Absolute_dynamic_req * absdynamic() { return 0; }
    virtual Tie_req * tie() { return 0; }
    virtual Subtle_req * subtle() { return 0; }
    virtual Span_dynamic_req * span_dynamic() { return 0; }
    REQUESTMETHODS(Musical_req, musical);
};


class Skip_req  : public Musical_req  {
public:
    Moment duration_;
    
    virtual Moment duration() const;
    REQUESTMETHODS(Skip_req, skip);
};

/** a request with a duration.
  This request is used only a base class.
 */
class Rhythmic_req  : public virtual Musical_req  {
public:
    Duration duration_;
    
    /* *************** */
    void set_duration(Duration);
    static int compare(const Rhythmic_req &, const Rhythmic_req &);
    virtual Moment duration() const;
    Rhythmic_req();
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

class Blank_req  : public Spacing_req, Rhythmic_req  {
public:
    REQUESTMETHODS(Spacing_req, spacing);
    
};

/// Put a text above or below (?) this staff.
class Text_req  : public virtual Musical_req  {
public:
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
class Lyric_req  : public  Rhythmic_req, public Text_req  {
public:
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

    /// return height from central c (in halflines)
    int height()const;

    /// transpose. #delta# is relative to central c.
    void transpose(Melodic_req const &delta);
    /// return pitch from central c (in halfnotes)
    int pitch()const; 
    Melodic_req();
    static int compare(Melodic_req const&, Melodic_req const&);
   
    REQUESTMETHODS(Melodic_req,melodic);
};

/// Put a note of specified type, height, and with accidental on the staff.
class Note_req  : public Rhythmic_req, virtual public Melodic_req  {
public:
    
    /// force/supress printing of accidental.
    bool forceacc_b_;
    Note_req();
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
class Stem_req  : public Rhythmic_req  {
public:
    /// preferred direction for the stem
    int dir_i_;
    Stem_req();
    REQUESTMETHODS(Stem_req,stem);
};

/**
  Requests to start or stop something.
 This type of request typically results in the creation of a #Spanner#
*/
class Span_req  : public virtual Musical_req  {
public:
    /// should the spanner start or stop, or is it unwanted?
    enum {
	NOSPAN, START, STOP
    } spantype ;
    static int compare(const Span_req &r1, const Span_req &r2);
    REQUESTMETHODS(Span_req,span);

    Span_req();
  
};

/** 
 request for backward plet generation.

 ugr. Place in hierarchy?
 */
class Plet_req  : public virtual Request  {
public:
     char type_c_;
     int dur_i_;
     int type_i_;
     Plet_req();
 
     REQUESTMETHODS(Plet_req,plet);
};

/** Start / stop a beam at this note.  if #nplet# is set, the staff
will try to put an appropriate number over the beam */
class Beam_req  : public Span_req  {
public:
    int nplet;

    /* *************** */
     REQUESTMETHODS(Beam_req,beam);

    Beam_req();
};

/**
  Start a tie at this voice element, end it at the next
 */
class Tie_req : public Musical_req {
public:
    REQUESTMETHODS(Tie_req, tie);
};

/// a slur
class Slur_req  : public Span_req  {
public:
 REQUESTMETHODS(Slur_req,slur);

};


/** Put a script above or below this ``note''. eg upbow, downbow. Why
  a request? These symbols may conflict with slurs and brackets, so
  this also a request */
class Script_req  : public Musical_req  {
public:
    int dir_i_;
    Script_def *scriptdef_p_;

    /* *************** */
    static int compare(const Script_req &, const Script_req &);
    Script_req(int d, Script_def*);
    REQUESTMETHODS(Script_req,script);
    ~Script_req();
    Script_req(Script_req const&);
};

/** A helper in the hierarchy. Each dynamic is bound to one note ( a
    crescendo spanning multiple notes is thought to be made of two
    "dynamics": a start and a stop).  Dynamic changes can occur in a
    smaller time than the length of its note, therefore fore each
    Dynamic request carries a time, measured from the start of its
    note.
 */
class Subtle_req  : public virtual Musical_req  {
public:
    /// the time relative to Voice_element start.
    Moment subtime_;
    REQUESTMETHODS(Subtle_req, subtle);
};

class Dynamic_req  : public Subtle_req  {
public:
    /// for absolute dynamics
    enum Loudness {
 	FFF, FF, F, MF, MP, P, PP, PPP
    };
    static String loudness_str(Loudness);
    REQUESTMETHODS(Dynamic_req, dynamic);
};

class Absolute_dynamic_req  : public Dynamic_req  {
public:
    Loudness loudness_;
    Absolute_dynamic_req();
    REQUESTMETHODS(Absolute_dynamic_req, absdynamic);
};

class Span_dynamic_req  : public Dynamic_req, public Span_req  {
public:
    /// Grow or shrink the volume: 1=cresc, -1 = decresc 
    int dynamic_dir_i_;
    Span_dynamic_req();
    REQUESTMETHODS(Span_dynamic_req, span_dynamic);
};

#endif // MUSICALREQUESTS_HH
