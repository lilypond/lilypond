/*
  global-gravs.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "input-engraver.hh"
#include "debug.hh"
#include "engraver.hh"

struct Request_engraver_table_entry {
    String name_str_;
    Grav_ctor ctor_l_;
    Request_engraver_table_entry(String s, Grav_ctor f) {
	name_str_ =s;
	ctor_l_ = f;
    }
    Request_engraver_table_entry()
    {
	ctor_l_ =0;
    }
};

static Array<Request_engraver_table_entry> *grav_table=0;

void
add_request_engraver(String s, Grav_ctor f)
{
    if (!grav_table)
	grav_table = new Array<Request_engraver_table_entry>;
    
    grav_table->push(Request_engraver_table_entry(s, f));
}


Request_engraver*
get_engraver_p(String s)
{
    for (int i=0; i < grav_table->size(); i++) {
	if ((*grav_table)[i].name_str_ == s)
	    return (*(*grav_table)[i].ctor_l_)();
    }
    error("Unknown engraver `" + s +"\'");
    return 0;
}
