/*
  engraver.hh -- declare Engraver

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ENGRAVER_HH
#define ENGRAVER_HH

#include "lily-proto.hh"
#include "varray.hh"
#include "request.hh"
#include "score-elem-info.hh"
#include "staff-info.hh"




/**
  a struct which processes requests, and creates the #Score_elem#s.
  It may use derived classes. Hungarian postfix: grav
  
  */

class Engraver {
    
    friend class Engraver_group_engraver;
    /**
      You cannot copy a Engraver
     */
    Engraver(const Engraver&){}

    enum { 
	VIRGIN,
	CREATION_INITED,
	MOVE_INITED,
	ACCEPTED_REQS,
	PROCESSED_REQS,
	ACKED_REQS,
	MOVE_DONE
    } status;

protected:
    

    /// utility
    virtual Paper_def * paper() const;

    
    /// make items/spanners with the requests you got
    virtual void do_process_requests(){}

    /** typeset any items/spanners. Default: do nothing
     */
    virtual void do_pre_move_processing(){}
    /** reset any appropriate data. Default: do nothing
     */
    virtual void do_post_move_processing(){}
   

    virtual void do_creation_processing () {}
    virtual void do_removal_processing() {}

    /**
      Invoke walker method to typeset element. Default: pass on to daddy.
      */
    virtual void typeset_element(Score_elem*elem_p);
    
     /**
      take note of item/spanner
      put item in spanner. Adjust local key; etc.

      Default: ignore the info
      */
    virtual void acknowledge_element(Score_elem_info) {}
    /**
      Announce element. Default: pass on to daddy. Utility
      */
    virtual void announce_element(Score_elem_info);
    /**
      Set Feature of the engraver(s). Default: ignore Feature.
     */
    virtual void set_feature(Feature){}
    /**
      ask daddy for a feature
     */
    virtual Scalar get_feature(String type_str);
    /**
      Does this equal or contain a certain engraver?
     */

    virtual void sync_features() {}
   
    virtual bool contains_b(Engraver*grav_l)const;
    /**
      Get information on the staff. Default: ask daddy.
      */
    virtual Staff_info get_staff_info()const;
    virtual void fill_staff_info(Staff_info&);


    virtual void do_print()const;  
    /*    
	  @see{try_request}
	  Default: always return false
      */
    virtual bool do_try_request(Request *req_l);
public:
    void pre_move_processing();
    void process_requests();
    /**
      try to fit the request in this engraver

      @return
      false: not noted,  not taken.

      true: request swallowed. Don't try to put the request elsewhere.

      */
    bool try_request(Request*);
    bool is_bottom_engraver() const;

    void post_move_processing();
    void removal_processing();
    
    Engraver_group_engraver * daddy_grav_l_;

    Engraver();
    virtual ~Engraver(){}
    DECLARE_MY_RUNTIME_TYPEINFO;
    void print() const;
};

/**
  A macro to automate administration of engravers.
 */
#define ADD_THIS_ENGRAVER(c)				\
struct c ## init {					\
    static Engraver * globalctor (){		\
	return new c;					\
    }							\
    c ## init () {					\
	add_engraver(c::static_name(), globalctor);	\
							\
    }							\
} _ ## c ## init;

typedef Engraver*(*Grav_ctor)(void);
void add_engraver(String s, Grav_ctor f);

#endif // ENGRAVER_HH

