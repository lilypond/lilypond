#include <iostream.h>
#include "text-db.hh"

bool
Text_db::eof_b ()
{
  Data_file::gobble_leading_white();
  return  Data_file::eof_b();
}

void
Text_db::gobble_leading_white()
{
  while (1) 
    {
      Data_file::gobble_leading_white();
      if (eof_b ())
	return ;
      char c;
      if  ((c = data_get()) !='\n')
	{
	  data_unget (c);
	  return ;
	}	
    }	
}


Text_record
Text_db::get_record() 
{
  while (1) 
    {
      String s;
      Array<String> fields;
      assert (!eof_b ());
	
      while ((s = get_word()) != "")
	{
	  fields.push (s);	
	  gobble_white();
	}
	     

      if (get_line() != "")
	assert (false);
    
      assert (fields.size());
      return Text_record (fields, get_name(), line ());
    }
}


void
Text_record::message (String s)
{
  cerr << '\n'<< filename << ": "<< line_no << s << "\n";
}	       
