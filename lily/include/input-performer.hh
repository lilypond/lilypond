/*
  input-performer.hh -- declare Input_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/

#ifndef INPUT_PERFORMER_HH
#define INPUT_PERFORMER_HH

#include "plist.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"
#include "string.hh"
#include "varray.hh"

struct Input_performer_list : public Pointer_list<Input_performer*> 
{
    Input_performer_list(Input_performer_list const &);
    Input_performer_list(){}
};

struct Input_performer : Input { 
    Input_performer_list contains_iperf_p_list_;
    Array<String> consists_str_arr_;
    Array<String> alias_str_arr_;
    String type_str_;

    void add(Input_performer *);
    bool is_name_b(String);
    bool accept_req_b();
    bool accepts_b(String);
    void print() const;
    Performer_group_performer * get_group_performer_p();
    Input_performer * get_default_iperf_l();
    Input_performer * recursive_find(String nm);
    Input_performer * find_iperf_l(String nm);    
};


Performer*get_performer_p(String s);

#endif // INPUT_PERFORMER_HH
