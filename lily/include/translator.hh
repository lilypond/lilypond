/*
  translator.hh -- declare Translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH

#include "global-ctor.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "lily-guile.hh"
#include "parray.hh"
#include "input.hh"
#include "smobs.hh"

/** Make some kind of #Element#s from Requests. Elements are made by
  hierarchically grouped #Translator#s
  */
class Translator : public Input {
  void init ();
public:
  Music_output_def * output_def_l_;
  String type_str_;
  
  virtual const char *name() const;
  bool is_alias_b (String) const;
    
  VIRTUAL_COPY_CONS(Translator);
  Translator (Translator const &);
  Translator ();
  
  Translator_group * daddy_trans_l_ ;
 

  
  /**
    try to fit the request in this engraver

    @return
    false: not noted,  not taken.

    true: request swallowed. Don't try to put the request elsewhere.

    */
  bool try_music (Music*);
  void pre_move_processing();
  void add_processing ();
  void creation_processing ();
  void process_music();
  void post_move_processing();
  void removal_processing();
  /**
    ask daddy for a feature
    */
  Music_output_def *output_def_l () const;

  SCM get_property (const char *) const;
  SCM get_property (SCM symbol) const;
  
  virtual Moment now_mom () const;  

  /*
    ugh: bubbled up from Translator_group. 
   */
  SCM simple_trans_list_;
  SCM trans_group_list_;
  SCM definition_;
  
  SCM properties_scm_;
  DECLARE_SMOBS(Translator, dummy);
public:
  /*
    UGH. Clean this up.
   */
   enum { 
    ORPHAN,
    VIRGIN,
    CREATION_INITED,
    MOVE_INITED,
    ACCEPTED_REQS,
    PROCESSED_REQS,
    ACKED_REQS,
    MOVE_DONE
  } status_;			// junkme
protected:

  /*    
	@see{try_request}
	Default: always return false
	*/
  virtual void do_add_processing ();
  virtual bool do_try_music (Music *req_l);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
  virtual void do_process_music () ;
  virtual void do_creation_processing() ;
  virtual void do_removal_processing();
};


/**
  A macro to automate administration of translators.
 */
#define ADD_THIS_TRANSLATOR(T)				\
static void  _ ## T ## _adder () {\
      T *t = new T;\
      t->type_str_ = classname (t);\
      add_translator (t);\
}\
ADD_GLOBAL_CTOR(_ ## T ## _adder);



extern Dictionary<Translator*> *global_translator_dict_p;
void add_translator (Translator*trans_p);

Translator*get_translator_l (String s);
Translator *unsmob_translator (SCM);
#endif // TRANSLATOR_HH
