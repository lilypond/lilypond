#include "textdb.hh"

Text_record
Text_db::get_record() 
{
	String s;
	svec<String> fields;
	gobble_leading_white();
	
	while ((s = get_word()) != "")
	    {
	    fields.add(s);	
	    gobble_white();
	    }
	     

	if (get_line() != "")
	    assert(false);
	
	return Text_record(fields, get_name(), line());
}


