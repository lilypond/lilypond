/*
  engravergroup.hh -- declare Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ENGRAVERGROUP_HH
#define ENGRAVERGROUP_HH

#include "lily-proto.hh"
#include "parray.hh"
#include "plist.hh"
#include "score-elem-info.hh"
#include "engraver.hh"
#include "translator.hh"


/**
  Group a number of engravers. Usually delegates everything to its contents.
  Postfix: group
  */
class Engraver_group_engraver : public Engraver, public virtual Translator {
protected:
    Pointer_list<Engraver*> grav_list_;
    Link_array<Engraver_group_engraver> group_l_arr_;
    Link_array<Engraver> nongroup_l_arr_;
    
    Array<Score_elem_info> announce_info_arr_;
    
    virtual void do_print()const;
    virtual bool removable_b()const;
public:
    Engraver*get_simple_engraver (char const*typeinfo)const;
    virtual void print() const ;

    Input_translator * itrans_l_;
    void check_removal();
    Engraver_group_engraver();
    ~Engraver_group_engraver();
    
    bool is_bottom_engraver_b() const;

    
    /**
      Junk #grav_l#.
      Pre:
      #grav_l# is in #grav_list_#
     */
    virtual void terminate_engraver (Engraver * grav_l);
    
    
    DECLARE_MY_RUNTIME_TYPEINFO;
    
    /**
      Remove #grav_l# from the list, and return it.
     */
    virtual Engraver * remove_engraver_p (Engraver*grav_l);
    virtual void set_feature (Feature i);
    virtual void sync_features();

    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
    virtual void do_removal_processing();
    virtual bool do_try_request (Request*);
    virtual bool try_request (Request*);
    virtual void do_process_requests();

    virtual Staff_info get_staff_info()const;
    
    virtual Engraver_group_engraver * find_engraver_l (String name,String id);
    virtual void do_announces();    
    virtual void announce_element (Score_elem_info);
    virtual void add (Engraver* grav_p);
    virtual bool contains_b (Engraver*)const;

    virtual Translator* find_get_translator_l (String name, String id);
    virtual Translator * get_default_interpreter();
    /**
      Go up in the tree. default: choose next parent
     */
    Translator * ancestor_l (int l=1);
    int depth_i() const;
};

#endif // ENGRAVERGROUP_HH


