/*
  complexwalker.hh -- declare Complex_walker

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef COMPLEXWALKER_HH
#define COMPLEXWALKER_HH

#include "proto.hh"
#include "voicegroup.hh"
#include "assoc.hh"
#include "staffwalker.hh"
#include "staffeleminfo.hh"
#include "plist.hh"

/**
  A staff walker which uses registers to decide what to print
 */
class Complex_walker: public Staff_walker {
    bool try_command_request(Nonmusical_req *req_l);
    void do_change_group( Voice * v, String group_id_str);
    void do_announces();
    void try_request(Request*req);    
    int c0_position_i_;

    Array<Item*> prebreak_item_p_arr_;
    Array<Item*> nobreak_item_p_arr_;
    Array<Item*> postbreak_item_p_arr_;
    
public:
    
    IPointerList<Voice_registers *> voice_reg_list_;
    IPointerList<Voice_group_registers*> group_reg_list_;
    Assoc<Voice *, Voice_group_registers *> voice_group_map_;

    Clef_register *clef_reg_p_;
    Local_key_register *local_key_reg_p_;
    Key_register *key_reg_p_;
    Bar_register *bar_reg_p_;
    Meter_register *meter_reg_p_;
    
    Array<Staff_elem_info> announce_info_arr_;
    
    /* *************** */

    Voice_registers *find_voice_reg(Voice*v_l)const;
    Voice_registers *get_voice_reg(Voice*v_l);
    
    /** search for voice_group containing #v_l#

      @return
      pointer if found,  0 if not found.
      */
    Voice_group_registers *find_voice_group(Voice* v_l)const;

    /// search. Create if necessary
    Voice_group_registers *get_voice_group(Voice* v_l);
    /** search for voice_group named #id#

      @return
      pointer if found,  0 if not found.
      */
    Voice_group_registers *find_voice_group(const char* id)const;

    /**
      search for voice_group named #id#, create if necessary
      */
    Voice_group_registers *get_voice_group(const char*);

    Array<Voice_registers *> get_voice_regs(Voice_group_registers *) const;
    
    void regs_process_requests();
    void typeset_breakable_item(Item * pre_p ,
				Item * nobreak_p, Item * post_p);
    void typeset_element(Staff_elem *elem_p);
    void announce_element(Staff_elem_info);
    virtual void process_requests();
    virtual void do_post_move();
    virtual void do_pre_move();
    /**
      @return the position of central c.
     */
    int c0_position_i() const;

    /**
      set the position of central c.
      @param the new position
     */
    void set_c0_position(int j);
    Complex_walker(Complex_staff*);
    ~Complex_walker();

    Complex_staff *staff();
private:
};


#endif // COMPLEXWALKER_HH


