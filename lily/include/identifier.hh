/*
  identifier.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH

#include "lily-proto.hh"
#include "string.hh"
#include "input.hh"

/* boolean argument to accesor is copy_b..*/
#define IDACCESSOR( Input_staff, staff)\
    virtual Input_staff * staff(bool) { error(#Input_staff); return 0; }


struct Identifier : public Input {
    void *data;
    String name_str_;
    bool init_b_;
    bool accessed_b_;
    int token_code_i_;
    
    Identifier(String n, int code) ;
    virtual ~Identifier() ;

    void print()const;
    virtual char const *classname() const{ return "new Identifier"; }
    void error(String);
    IDACCESSOR(Input_staff, staff)
    IDACCESSOR(Input_music, music)
    IDACCESSOR(Music_voice, mvoice)
    IDACCESSOR(Input_register, iregs)	
	       
    IDACCESSOR(Script_def, script)
    IDACCESSOR(Symtables, symtables)
    IDACCESSOR(Music_general_chord, mchord)
    IDACCESSOR(Lookup,lookup)
    IDACCESSOR(Real,real)
    IDACCESSOR(Request, request)
    IDACCESSOR(Input_score, score)

protected:
    virtual void do_print()const=0;
private:
    Identifier(Identifier const&);
};

#define declare_id_class(Idclass, Class, accessor)	\
struct Idclass : Identifier {\
	virtual char const *classname()const;\
	Idclass(String s, Class*st, int code);\
	virtual Class* accessor(bool copy);\
	~Idclass();\
	virtual void do_print()const; \
}\



declare_id_class(Real_id, Real, real);
declare_id_class(Script_id, Script_def, script);
declare_id_class(Lookup_id, Lookup, lookup);
declare_id_class(Symtables_id, Symtables, symtables);
declare_id_class(Staff_id, Input_staff, staff);
declare_id_class(M_chord_id, Music_general_chord, mchord);
declare_id_class(M_voice_id, Music_voice, mvoice);
declare_id_class(Score_id, Input_score, score);
declare_id_class(Request_id, Request, request);
declare_id_class(Input_regs_id, Input_register, iregs);

#endif // IDENTIFIER_
