// LilyPond's second egg of columbus!
#ifndef REQUEST_HH
#define REQUEST_HH

#include "glob.hh"
#include "string.hh"
#include "moment.hh"

/// a voice element wants something printed
struct Request {
    Voice_element*elt_l_;
    
    /****************/

    Request();
    Request(Request const&);
    virtual ~Request(){}

    virtual void print()const ;
    virtual Moment duration() const { return 0.0; }
    virtual Request* clone() const =0;

    /*  accessors for children */
    virtual Barcheck_req *barcheck() { return 0; }
    virtual Note_req *note() {return 0;}
    virtual Script_req *script() {return 0;}
    virtual Stem_req *stem() {return 0;}
    virtual Text_req*text() { return 0; }
    virtual Rest_req *rest() {return 0;}
    virtual Span_req *span() {return 0;}
    virtual Beam_req *beam() { return 0 ; }
    virtual Slur_req *slur() { return 0 ; }
    virtual Rhythmic_req*rhythmic() { return 0; }
    virtual Melodic_req *melodic() { return 0; }
};

/**
see lilygut page
 */
	
struct Barcheck_req : Request {
    virtual Barcheck_req *barcheck() { return this; }
    void print ()const;
    Request*clone() const;
};

/// a request with a duration
struct Rhythmic_req : virtual Request {
    int balltype;
    int dots;
    Moment plet_factor;
    /****************/

    Moment duration() const;
    Rhythmic_req();
    Rhythmic_req*rhythmic() { return this;}
    void print ()const;
    Request*clone() const;
};


struct Melodic_req :virtual  Request
{
    /// 0 is c
    int name;
    int octave;
    int accidental;
    bool forceacc;

    // return height from central c (in halflines)
    int height()const; 
    Melodic_req();
    Melodic_req*melodic() { return this;}
    virtual void print() const;
    Request*clone() const;
};

/// Put a note of specified type, height, and with accidental on the staff.
struct Note_req : Rhythmic_req, virtual Melodic_req {
    Rhythmic_req* rhythmic() { return Rhythmic_req::rhythmic(); }
    
    Note_req*note() { return this;}
    virtual void print() const;
    Request*clone() const;
};
/**
*/


///Put a rest on the staff.
struct Rest_req : Rhythmic_req {

    void print()const;

    Rest_req * rest() { return this;}
    Request*clone() const ;
};
/**
Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/

/// attach a stem to the noteball
struct Stem_req : Request {
    /// 4,8,16, ..
    int stem_number;

    virtual Stem_req *stem() {return this;}
    Stem_req(int s) { stem_number = s; }
    Request*clone() const;
};

/// requests to start or stop something.
struct Span_req : Request {
    /// should the spanner start or stop, or is it unwanted?
    enum {
	NOSPAN, START, STOP
    } spantype ;

    virtual void print() const;
    Span_req*span() { return this; }
    Span_req();
    virtual Request*clone()const;
};
/**
 This type of request typically results in the creation of a #Spanner#
*/


///Start / stop a beam at this note.
struct Beam_req : Span_req {
    int nplet;

    /****************/
    
    Beam_req();
    virtual Beam_req * beam() { return this; }
    virtual Request*clone()const;
};
/**   if #nplet# is set, the staff will try to put an
appropriate number over the beam
    */

/// a slur
struct Slur_req : Span_req {

    virtual Request*clone()const;
    virtual Slur_req*slur() { return this; }
};


///Put a script above or below this ``note''    
struct Script_req : Request {
    int dir;
    Script_def *scriptdef;

    /****************/
    Script_req*script() { return this; }
    virtual void print() const;
    Request *clone()const;
    Script_req(int d, Script_def*);
    ~Script_req();
    Script_req(Script_req const&);
};
/** eg upbow, downbow. Why a request? These symbols may conflict with
slurs and brackets, so this also a request */


///Put a text above or below (?) this staff.
struct Text_req : Request {
    int dir;
    Text_def *spec;
    /****************/
    Text_req*text() { return this; }
    virtual void print() const;
    Request *clone()const;
    Text_req(int d, Text_def*);
    ~Text_req();
    Text_req(Text_req const&);
};


#if 0

///Put a lyric above or below (?) this staff.
struct Lyric_req : Request {
    String text;
};



///Draw a (Guitar) chord above or below this ``note''
struct Chord : Request {
	// don't know how this looks.
};
/**
Why a request?
Because everything else is done in requests.
*/


/// for absolute dynamics
enum Loudness {
    FFF, FF, F, MF, MP, P, PP, PPP
} ;


///Start / stop a slur or a bracket.
struct Bracket_req : Span_req {
    int nplet;			// print a number over the beam.
};

/**
Start/stop a bracket at this note. if #nplet# is set, the staff will
try to put an appropriate number over the bracket
*/

struct Subtle_request {
    Moment subtime;
};

/// helper in the hierarchy
struct Dynamic:Subtle_request {

};
/** Each dynamic is bound to one note ( a crescendo spanning multiple
    notes is thought to be made of two "dynamics": a start and a stop).
    Dynamic changes can occur in a smaller time than the length of its
    note, therefore fore each Dynamic request carries a time, measured
    from the start of its note.

    This subfield would come in handy, if mpp96 was adapted for midi
    support.
    
    Dynamic should have been derived from request, but I don't want to
    fuss with virtual baseclasses.  */

/// do a crescendo
struct Cresc_req : Span_req, Dynamic {
    
};

/// do a decrescendo
struct Decresc_req : Span_req, Dynamic {
    
};

/// do a dynamic like "fff" or "mp"
struct Absdynamic_req : Request, Dynamic {
    Loudness loudness;
};

struct Grace_req : Subtle_request {
    
};

struct Grace_turn_req : Grace_turn {
    
};

struct Grace_note : Melodic_req {
    
};

struct Grace_notes {
    
};
#endif
#endif
