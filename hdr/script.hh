/*
  script.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCRIPT_HH
#define SCRIPT_HH

#include "scriptdef.hh"
#include "item.hh"
  
struct Script : Item {
    int dir;
    int symdir;
    int pos;
    int staffsize;
    Script_def *specs_l_;
    Stem *stem_l_;
    Array<Item *> support;

    /****************/
    const char * name() const;    
    Molecule*	brew_molecule_p()const;
    virtual void do_post_processing();
    virtual void do_pre_processing();
    Script(Script_req*, int staffsize);
    void set_support(Item*);
    void set_stem(Stem*);
    Interval support_height()const;
    virtual Interval width() const;
private:
    void    	set_symdir();
    void	set_default_dir();
    void	set_default_pos();
    Symbol symbol()const;
};


#endif // SCRIPT_HH

