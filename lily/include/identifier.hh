/*
  identifier.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH

#include "lily-proto.hh"
#include "string.hh"
#include "input.hh"
#include "virtual-methods.hh"

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
    NAME_MEMBERS();
    void error(String);
    IDACCESSOR(Input_translator, input_translator)
    IDACCESSOR(Music, music)
    IDACCESSOR(General_script_def, script)
    IDACCESSOR(Symtables, symtables)
    IDACCESSOR(Midi_def, mididef)
    IDACCESSOR(Paper_def, paperdef)
    IDACCESSOR(Lookup,lookup)
    IDACCESSOR(Real,real)
    IDACCESSOR(Request, request)
    IDACCESSOR(Score, score)
    IDACCESSOR(int, intid)

protected:
    virtual void do_print()const=0;
private:
    Identifier(Identifier const&);
};

#define declare_id_class(Idclass, Class, accessor)	\
struct Idclass : Identifier {\
        NAME_MEMBERS(); 			    \
	Idclass(String s, Class*st, int code);\
	virtual Class* accessor(bool copy);\
	~Idclass();\
	virtual void do_print()const; \
}\


declare_id_class(Input_translator_id, Input_translator, input_translator);
declare_id_class(Real_id, Real, real);
declare_id_class(Script_id, General_script_def, script);
declare_id_class(Lookup_id, Lookup, lookup);
declare_id_class(Symtables_id, Symtables, symtables);
declare_id_class(Music_id, Music, music);
declare_id_class(Int_id, int, intid);
declare_id_class(Score_id, Score, score);
declare_id_class(Request_id, Request, request);
declare_id_class(Paper_def_id,Paper_def, paperdef);
declare_id_class(Midi_def_id,Midi_def, mididef);
#endif // IDENTIFIER_
