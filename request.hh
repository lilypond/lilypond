// mpp96's second egg of columbus!
#ifndef REQUEST_HH
#define REQUEST_HH

#include "glob.hh"
#include "string.hh"

struct Request {
    Voice_element*elt;
#if 0
    enum {
	UNKNOWN, NOTE, REST, LYRIC, SCRIPT, CHORD, BEAM,
	BRACKET, STEM, SLUR, CRESC, DECRESC, ABSDYNAMIC
    } tag;
#endif
    virtual void print()const ;
    virtual Note_req *note() {return 0;}
    virtual Rest_req *rest() {return 0;}
    virtual  Rhythmic_req*rhythmic() { return 0;}
    Request(Voice_element*);
    Request();
    virtual Real duration() const { return 0.0; }
};

/**
    Any Voice_element can do a number of requests. A request is done
    to the #Staff# which contains the #Voice_element#. The staff decides
    whether to to honor the request, ignore it, or merge it with other
    requests. Merging of requests is preferably done with other
    requests done by members of the same voicegroups (beams, brackets, stems) 

    Please refer to the documentation of the Child classes of
    #Request# for explanation of each request type.

    The result of a request will be an #Item# or a #Spanner#, which
    will be put on a #PStaff#. Note that the #PStaff# and the original
    #Staff# need not have anything in common. For example, the
    ``double'' piano Staff could interpret commands which juggle
    melodies across the left and right hand, and may put the result in
    two five-line PStaffs (maybe with extra PStaffs to carry the dynamic
    signs and any lyric.

    The class #Staff# should be thought as a container for the
    #Voice#s, and an interpreter for #Request#s and #Command#s.
    Different staffs can produce different outputs; a melodious voice
    which is put into a percussion-Staff, will be typeset as the rythm of
    that voice.

    After #Staff# made up her mind (Would #Staff# be a smart
    name? How about #struct Lily {}# :-), the resultant items and
    spanners are put on the PScore, and pointers to these items are
    stored in the #Voice_element#. This construction enables the
    beams/stems to look up the balls it has to connect to.  */
	

struct Rhythmic_req : Request {
    int balltype;
    int dots;
    Real duration() const;
    Rhythmic_req(Voice_element*);
    Rhythmic_req*rhythmic() { return this;} 
};

/// Put a note of specified type, height, and with accidental on the staff.
struct Note_req : Rhythmic_req {
    char name;
    int octave;
    int accidental;
    bool forceacc;
    Note_req(Voice_element*v);
    Note_req*note() { return this;}
};
/**
Staff has to decide if the ball should be hanging left or right. This
influences the horizontal dimensions of a column, and this  is why
request processing should be done before horizontal spacing.

Other voices' frivolities may cause the need for accidentals, so this
is also for the Staff to decide. The Staff can decide on positioning
based on ottava commands and the appropriate clef.
*/


///Put a rest on the staff.
struct Rest_req : Rhythmic_req {

    Rest_req(Voice_element*v) : Rhythmic_req(v) {  }
    Rest_req * rest() { return this;}
};
/**
Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/

#if 0

///Put a lyric above or below (?) this staff.
struct Lyric_req : Request {
    String text;
};


///Put a script above or below this ``note''    
struct Script_req : Request {
    int orientation;
    Symbol sym;
};
/**
eg upbow, downbow. Why a request? These symbols may conflict with slurs and brackets, so this
also a request
*/




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

/// attach a stem to the noteball
struct Stem_req : Request {
    /// 4,8,16, ..
    int stem_number ;
};
/// requests to start or stop something.
struct Span_req : Request {
    /// should the spanner start or stop, or is it unwanted?
    enum {
	NOSPAN, START, STOP
    } spantype ;
};
/**
 This type of request typically results in the creation of a #Spanner#
*/


///Start / stop a beam at this note.
struct Beam_req : Span_req {
    int nplet;
};
/** Staff will have to combine this with the stem_request, since the
    number of flags that a stem wants to carry will determine the
    number of beams.  if #nplet# is set, the staff will try to put an
    appropriate number over the beam

    [what to do  if the nplet fields of start and stop conflict?]
    */

///Start / stop a slur or a bracket.
struct Bracket_req : Span_req {
    int nplet;
};
/**
Start/stop a bracket at this note. if #nplet# is set, the staff will
try to put an appropriate number over the bracket
*/

/// a slur
struct Slur_req : Span_req {
    
};

/// helper in the hierarchy
struct Dynamic {
    Real subtime;
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
#endif
#endif
