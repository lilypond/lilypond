/*
  translator-group.hh -- declare Translator_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TRANSLATOR_GROUP_HH
#define TRANSLATOR_GROUP_HH

#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "translator.hh"
#include "plist.hh"
#include "parray.hh"

// egcs
typedef void (Translator::*Method_pointer)(void);
typedef void (Translator::*Const_method_pointer)(void) const; 

/** Make some kind of #Element#s from Requests. Elements are made by
  hierarchically grouped #Translator#s
  */
class Translator_group : public virtual Translator {
  Array<String> consists_str_arr_;
  Array<String> accepts_str_arr_;

public:
  Pointer_list<Translator *> trans_p_list_;
  String id_str_;


  
  
  int iterator_count_;

  TRANSLATOR_CLONE(Translator_group);
  DECLARE_MY_RUNTIME_TYPEINFO;

  void set_acceptor (String accepts, bool add);
  void set_element (String accepts, bool add);  
  
  
  Translator_group(Translator_group const &);
  Translator_group();
  void add_translator (Translator *trans_p);
  
  Link_array<Translator> nongroup_l_arr () const;
  Link_array<Translator_group> group_l_arr () const;
  
  virtual Global_translator *global_l() { return 0; }
  
  /// Score_register = 0, Staff_registers = 1, etc)
  Translator_group* ancestor_l (int l=1);
  int depth_i() const;
  bool is_bottom_translator_b () const;
  bool removable_b() const;
  void terminate_translator (Translator*r_l);
  Translator *remove_translator_p (Translator*trans_l);
  void check_removal ();


  
  Translator *get_simple_translator (char const *type) const;
  Translator_group *find_existing_translator_l (String n, String id);
  Translator_group *find_create_translator_l (String n, String id);
  Link_array<Translator_group> path_to_acceptable_translator (String alias) const;

  Translator_group*get_default_interpreter();
protected:
  virtual ~Translator_group ();
  virtual Translator_group * access_Translator_group () { return this; }
  virtual void do_print () const;
  virtual void do_process_requests ();
  virtual void do_add_processing ();
  virtual bool do_try_request (Request* req_l);       
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();   
  virtual void do_creation_processing();
  virtual void do_removal_processing();
  void each (Method_pointer);
  void each (Const_method_pointer) const;
};

#endif // TRANSLATOR_GROUP_HH
