/*
  translator.hh -- declare Translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH

#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "scalar.hh"
#include "dictionary.hh"
#include "parray.hh"
#include "input.hh"

#define TRANSLATOR_CLONE(c)	VIRTUAL_COPY_CONS(c, Translator)

/** Make some kind of #Element#s from Requests. Elements are made by
  hierarchically grouped #Translator#s
  */
class Translator : public Input {
  Dictionary<Scalar> properties_dict_;
public:
  Music_output_def * output_def_l_;
  String  type_str_;

  bool is_alias_b (String) const;
  

  DECLARE_MY_RUNTIME_TYPEINFO;  
  TRANSLATOR_CLONE(Translator);
  Translator (Translator const &);
  Translator ();
  virtual ~Translator ();
  
  Translator_group * daddy_trans_l_ ;
 
  void print () const;
  
  /**
    try to fit the request in this engraver

    @return
    false: not noted,  not taken.

    true: request swallowed. Don't try to put the request elsewhere.

    */
  bool try_request (Request*);
  void pre_move_processing();
  void add_processing ();
  void creation_processing ();
  void process_requests();
  void post_move_processing();
  void removal_processing();
  /**
    ask daddy for a feature
    */
  Scalar get_property (String type_str);
  void set_property (String var_name, Scalar value);
  Music_output_def *output_def_l () const;
  
  virtual Moment now_moment () const;  
  virtual Engraver *access_Engraver () { return 0; }
  virtual Performer *access_Performer() { return 0; }
  virtual Translator_group * access_Translator_group () { return 0; }

protected:
   enum { 
    ORPHAN,
    VIRGIN,
    CREATION_INITED,
    MOVE_INITED,
    ACCEPTED_REQS,
    PROCESSED_REQS,
    ACKED_REQS,
    MOVE_DONE
  } status;

  /*    
	@see{try_request}
	Default: always return false
	*/
  virtual void do_add_processing ();
  virtual bool do_try_request (Request *req_l);
  virtual void do_print () const;
  virtual void do_pre_move_processing(){}
  virtual void do_post_move_processing(){}
  virtual void do_process_requests () {}
  virtual void do_creation_processing() {}
  virtual void do_removal_processing() {}
};


template<class T>
class Translator_adder
{
public:
  static Translator *ctor ()
    {
      Translator *t = new T;
      t->type_str_ = T::static_name ();
      return t;
    }
  Translator_adder () {	
    add_constructor (ctor);
  }				
};

/**
  A macro to automate administration of translators.
 */
#define ADD_THIS_TRANSLATOR(c)				\
  Translator_adder<c> _ ## c ## init;

typedef Translator *(*Translator_ctor) ();

extern Dictionary<Translator*> *global_translator_dict_p;
void add_translator (Translator*trans_p);
void add_constructor (Translator_ctor ctor);

Translator*get_translator_l (String s);

#endif // TRANSLATOR_HH
