/*
  translator.hh -- declare Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TRANSLATOR_HH
#define TRANSLATOR_HH

#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"

class Translator {
public:
    String id_str_;
    
    int iterator_count_;
    
    virtual Global_translator *global_l() { return 0; }

    /// Score_register = 0, Staff_registers = 1, etc)
    virtual void print()const;
    virtual int depth_i()const=0;
    virtual bool is_bottom_engraver_b() const { return false; }
    virtual bool try_request(Request*);
    virtual Translator *find_get_translator_l(String name, String id)=0;
    virtual Translator *ancestor_l(int l=1)=0;
    virtual ~Translator(){}
    NAME_MEMBERS();
    Translator();
    virtual Translator *get_default_interpreter()=0;
};

#endif // TRANSLATOR_HH
