/*
  input-engraver.hh -- declare Input_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUT_ENGRAVER_HH
#define INPUT_ENGRAVER_HH

#include "plist.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"
#include "string.hh"
#include "varray.hh"

struct Input_engraver_list : public Pointer_list<Input_engraver*> 
{
    Input_engraver_list(Input_engraver_list const &);
    Input_engraver_list(){}
};

struct Input_engraver : Input { 
    Input_engraver_list contains_igrav_p_list_;
    Array<String> consists_str_arr_;
    Array<String> alias_str_arr_;
    String type_str_;

    void add(Input_engraver *);
    bool is_name_b(String);
    bool accept_req_b();
    bool accepts_b(String);
    void print() const;
    Engraver_group_engraver * get_group_engraver_p();
    Input_engraver * get_default_igrav_l();
    Input_engraver * recursive_find(String nm);
    Input_engraver * find_igrav_l(String nm);    
};


Engraver*get_engraver_p(String s);

#endif // INPUT_ENGRAVER_HH
