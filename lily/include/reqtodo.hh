#if 0



/**Draw a (Guitar) chord above or below this ``note''.
Why a request?
Because everything else is done in requests.
*/
struct Chord : Request {
	// don't know how this looks.
};


/// for absolute dynamics
enum Loudness {
    FFF, FF, F, MF, MP, P, PP, PPP
} ;


/**
Start/stop a bracket at this note. if #nplet# is set, the staff will
try to put an appropriate number over the bracket
*/
struct Bracket_req : Span_req {
    int nplet;			// print a number over the beam.
};

struct Subtle_req {
    Moment subtime;
};


/** helper in the hierarchy. Each dynamic is bound to one note ( a
    crescendo spanning multiple notes is thought to be made of two
    "dynamics": a start and a stop).  Dynamic changes can occur in a
    smaller time than the length of its note, therefore fore each
    Dynamic request carries a time, measured from the start of its
    note.

    This subfield would come in handy, if mpp96 was adapted for midi
    support.
    
    Dynamic should have been derived from request, but I don't want to
    fuss with virtual baseclasses.  */

struct Dynamic:Subtle_req {

};
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

struct Grace_req : Subtle_req {
    
};

struct Grace_turn_req : Grace_turn {
    
};

struct Grace_note : Melodic_req {
    
};

struct Grace_notes {
    
};

struct Glissando_req : Span_req {
    
};
#endif
