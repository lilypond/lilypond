/*
  input-translator.hh -- declare Input_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUT_TRANSLATOR_HH
#define INPUT_TRANSLATOR_HH

#include "plist.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"
#include "string.hh"
#error 
#include "varray.hh"

struct Input_translator_list : public Pointer_list<Input_translator*> 
{
  Input_translator_list (Input_translator_list const &);
  Input_translator_list(){}
  ~Input_translator_list(){}
};

/** Define a intereter for music. This is an runtime interface to the
 typesystem */
class Input_translator : public Input { 
public:
  Input_translator_list contains_itrans_p_list_;
  Array<String> consists_str_arr_;
  Array<String> alias_str_arr_;
  String base_str_;
  String type_str_;
  String default_id_str_;

  void add (Input_translator *);
  bool is_name_b (String);
  bool accept_req_b();
  bool accepts_b (String);
  void print() const;
  Translator_group * get_group_translator_p();
  Input_translator * get_default_itrans_l();
  Input_translator * recursive_find (String nm);
  Input_translator * find_itrans_l (String nm);    
};

#endif // Input_translator_HH
