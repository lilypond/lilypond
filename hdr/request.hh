/*
  request.hh -- declare Request baseclasses.

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef REQUEST_HH
#define REQUEST_HH
// LilyPond's second egg of columbus!

#include "glob.hh"
#include "string.hh"
#include "moment.hh"

/**
 a voice element wants something printed.
 Hungarian postfix: req
 @see lilygut manpage
 */
struct Request {
    Voice_element*elt_l_;
    char const* defined_ch_c_l_;
    
    /* *************** */
    Request();
    Request(Request const&);
    virtual ~Request(){}

    virtual const char * name() const { return "Request";}
    virtual Request* clone() const { return new Request(*this); }
    void print()const ;
    
    virtual Moment duration() const { return 0; }

    /*  accessors for children
	maybe checkout RTTI
     */
    virtual Barcheck_req *barcheck() { return 0; }
    virtual Note_req *note() { return 0;}
    virtual Script_req *script() { return 0;}
    virtual Stem_req *stem() { return 0;}
    virtual Text_req*text() { return 0; }
    virtual Rest_req *rest() { return 0; }
    virtual Span_req *span() { return 0; }
    virtual Beam_req *beam() { return 0 ; }
    virtual Plet_req* plet() { return 0; }
    virtual Slur_req *slur() { return 0 ; }
    virtual Rhythmic_req*rhythmic() { return 0; }
    virtual Lyric_req* lreq_l() { return 0; }
    virtual Melodic_req *melodic() { return 0; }
    virtual Terminate_voice_req *terminate() {return 0;}
    virtual Group_change_req * groupchange() { return 0;}
    virtual Group_feature_req * groupfeature() { return 0; }
    virtual Spacing_req * spacing() { return 0; }
    virtual Blank_req * blank() { return 0; }
    virtual Musical_req *musical() { return 0; }
    virtual Nonmusical_req * nonmus() { return 0; }
protected:
    virtual void do_print()const ;
};

#define REQUESTMETHODS(T,accessor)	\
virtual T * accessor() { return this;}\
virtual const char* name() const { return #T; }\
virtual Request *clone() const { return  new T(*this); } \
virtual void do_print() const

#endif
