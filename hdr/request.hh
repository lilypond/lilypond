// LilyPond's second egg of columbus!
#ifndef REQUEST_HH
#define REQUEST_HH

#include "glob.hh"
#include "string.hh"
#include "moment.hh"

/// Hungarian postfix: req
/**
 a voice element wants something printed.
see lilygut page
 */

struct Request {
    Voice_element*elt_l_;
    char const* defined_ch_c_l_m;
    
    /* *************** */
    Request();
    Request(Request const&);
    virtual ~Request(){}

    virtual const char * name() const { return "Request";}
    virtual Request* clone() const =0;
    void print()const ;
    
    virtual Moment duration() const { return 0; }

    /*  accessors for children
	maybe checkout RTTI
     */
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
    virtual Lyric_req* lreq_l() { return 0; }
    virtual Melodic_req *melodic() { return 0; }
    virtual Mark_req * mark() { return 0; }
    virtual Staff_command_req* command() { return 0;}
    virtual Terminate_voice_req *terminate() {return 0;}
    virtual Group_change_req * groupchange() { return 0;}
    virtual Group_feature_req * groupfeature() { return 0; }
    virtual Spacing_req * spacing() { return 0; }
    virtual Blank_req * blank() { return 0; }
protected:
    virtual void do_print()const ;
};

#define REQUESTMETHODS(T,accessor)	\
virtual T * accessor() { return this;}\
virtual const char* name() const { return #T; }\
virtual Request *clone() const { return  new T(*this); } \
virtual void do_print() const
	
struct Barcheck_req : Request {
    REQUESTMETHODS(Barcheck_req,barcheck);
};

struct Terminate_voice_req : Request {
    REQUESTMETHODS(Terminate_voice_req,terminate);
};

struct Group_feature_req : Request {
    int stemdir_i_;
    Group_feature_req();
    REQUESTMETHODS(Group_feature_req, groupfeature);
};

struct Group_change_req : Request {
    String newgroup_str_;
    REQUESTMETHODS(Group_change_req, groupchange);
};

/// a request with a duration
struct Rhythmic_req : virtual Request {
    int balltype;
    int dots;
    Moment plet_factor;
    /* *************** */
    static int compare(const Rhythmic_req &, const Rhythmic_req &);
    Moment duration() const;
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

///Put a text above or below (?) this staff.
struct Text_req : virtual Request {
    int dir_i_;
    Text_def *tdef_p_;
    /* *************** */
    Text_req(int d, Text_def*);
    ~Text_req();
    Text_req(Text_req const&);
    static int compare(const Text_req&,const Text_req&);
    REQUESTMETHODS(Text_req,text);
};


struct Lyric_req : public Rhythmic_req, Text_req {

    Lyric_req(Text_def* t_p);
    REQUESTMETHODS(Lyric_req, lreq_l);
};

/// request which has some kind of pitch
struct Melodic_req :virtual  Request
{
    /// 0 is c
    int notename;
    int octave;
    int accidental;
    bool forceacc;

    // return height from central c (in halflines)
    int height()const; 
    Melodic_req();
   
    REQUESTMETHODS(Melodic_req,melodic);
};

/// Put a note of specified type, height, and with accidental on the staff.
struct Note_req : Rhythmic_req, virtual Melodic_req {
    

    Rhythmic_req* rhythmic() { return Rhythmic_req::rhythmic(); }
    REQUESTMETHODS(Note_req, note);
 };
/**
*/


///Put a rest on the staff.
/**
Why a request? It might be a good idea to not typeset the rest, if the paper is too crowded.
*/

struct Rest_req : Rhythmic_req {

 REQUESTMETHODS(Rest_req,rest);
};
/// attach a stem to the noteball
/**
  Rhythmic_req parent needed to  determine if it will fit inside a beam.
  */

struct Stem_req : Rhythmic_req {
    int dir_i_;
    Stem_req(int s, int dots);
    REQUESTMETHODS(Stem_req,stem);
};

/// requests to start or stop something.
/**
 This type of request typically results in the creation of a #Spanner#
*/
struct Span_req : Request {
    /// should the spanner start or stop, or is it unwanted?
    enum {
	NOSPAN, START, STOP
    } spantype ;
    static int compare(const Span_req &r1, const Span_req &r2);
    REQUESTMETHODS(Span_req,span);

    Span_req();
  
};


///Start / stop a beam at this note.

/**   if #nplet# is set, the staff will try to put an
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


///Put a script above or below this ``note''    
/** eg upbow, downbow. Why a request? These symbols may conflict with
slurs and brackets, so this also a request */
struct Script_req : Request {
    int dir_i_;
    Script_def *scriptdef_p_;

    /* *************** */
    static int compare(const Script_req &, const Script_req &);
    Script_req(int d, Script_def*);
    REQUESTMETHODS(Script_req,script);
    ~Script_req();
    Script_req(Script_req const&);
};


/// designate this spot with a name.
struct Mark_req : Request {
    String mark_str_;
    /* *************** */
    Mark_req(String);
    REQUESTMETHODS(Mark_req,mark);
};

struct Staff_command_req : Request {
    Input_command * com_p_;
    /* *************** */
    Staff_command_req(Staff_command_req const&);
    ~Staff_command_req();
    Staff_command_req(Input_command*);
    REQUESTMETHODS(Staff_command_req,command);
};

#if 0


///Draw a (Guitar) chord above or below this ``note''
/**
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


///Start / stop a slur or a bracket.
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

/// helper in the hierarchy
/** Each dynamic is bound to one note ( a crescendo spanning multiple
    notes is thought to be made of two "dynamics": a start and a stop).
    Dynamic changes can occur in a smaller time than the length of its
    note, therefore fore each Dynamic request carries a time, measured
    from the start of its note.

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
#endif
