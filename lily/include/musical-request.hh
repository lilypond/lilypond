/*
  musical-request.hh -- declare Musical requests

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSICALREQUESTS_HH
#define MUSICALREQUESTS_HH

#include "lily-proto.hh"
#include "request.hh"
#include "duration.hh"


/**
  A request which is coupled to a #Request_chord#
  Base class only
 */
class Musical_req  : public virtual Request  {
public:
    
    virtual Lyric_req* lreq_l() { return 0; }
    virtual Note_req *note() { return 0;}
    virtual Stem_req *stem() { return 0;}
    virtual Melodic_req *melodic() { return 0; }
    virtual Slur_req *slur() { return 0 ; }
    virtual Beam_req *beam() { return 0 ; }
    virtual Rhythmic_req*rhythmic() { return 0; }
    virtual Musical_script_req*musicalscript() { return 0; }
    virtual Text_req*text() { return 0; }
    virtual Rest_req *rest() { return 0; }
    virtual Skip_req* skip() { return 0; }
    virtual Dynamic_req* dynamic() { return 0; }
    virtual Absolute_dynamic_req * absdynamic() { return 0; }
    virtual Tie_req * tie() { return 0; }
    virtual Span_dynamic_req * span_dynamic() { return 0; }
    REQUESTMETHODS(Musical_req, musical);
};



/** a request with a duration.
  This request is used only a base class.
 */
class Rhythmic_req  : public virtual Musical_req  {
public:
    Duration duration_;
    
    /* *************** */
    void set_duration(Duration);
    bool do_equal_b(Request*)const;
    virtual Moment duration() const;
    Rhythmic_req();
    static int compare(Rhythmic_req const&,Rhythmic_req const&);
    REQUESTMETHODS(Rhythmic_req, rhythmic);
};

class Skip_req  : public Rhythmic_req  {
public:
    REQUESTMETHODS(Skip_req, skip);
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
    bool do_equal_b(Request*)const;
    static int compare(    Melodic_req const&,Melodic_req const&);
    REQUESTMETHODS(Melodic_req,melodic);
};

/// Put a note of specified type, height, and with accidental on the staff.
class Note_req  : public Rhythmic_req, virtual public Melodic_req  {
public:
    
    /// force/supress printing of accidental.
    bool forceacc_b_;
    Note_req();
    bool do_equal_b(Request*)const;
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
    bool do_equal_b(Request*)const;
    REQUESTMETHODS(Span_req,span);

    Span_req();
  
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

class Musical_script_req : public Musical_req,  public Script_req {
public:
    REQUESTMETHODS(Musical_script_req, musicalscript);
};


class Dynamic_req  : public virtual Musical_req  {
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
