/*
  text.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef TEXT_ITEM_HH
#define TEXT_ITEM_HH

#include "textdef.hh"
#include "item.hh"
  
struct Text_item : Item{
    int pos;
    int staffsize;
    int dir;
    Text_def*specs;
    
    /****************/
    
    void	set_default_pos();
    Molecule*	brew_molecule()const;
    void do_pre_processing();
    
    Text_item(Text_req*,int);
};


#endif // TEXT_HH

