/*
  global-translators.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "proto.hh"
#include "plist.hh"
#include "input-translator.hh"
#include "debug.hh"
#include "performer.hh"

struct Performer_table_entry {
    String name_str_;
    Perf_ctor ctor_l_;
    Performer_table_entry(String s, Perf_ctor f) {
	name_str_ =s;
	ctor_l_ = f;
    }
    Performer_table_entry()
    {
	ctor_l_ =0;
    }
};

static Array<Performer_table_entry> *perf_table=0;

void
add_Performer(String s, Perf_ctor f)
{
    if (!perf_table)
	perf_table = new Array<Performer_table_entry>;
    
    perf_table->push(Performer_table_entry(s, f));
}


Performer*
get_performer_p(String s)
{
    for (int i=0; i < perf_table->size(); i++) {
	if ((*perf_table)[i].name_str_ == s)
	    return (*(*perf_table)[i].ctor_l_)();
    }
    error("Unknown performer `" + s +"\'");
    return 0;
}
