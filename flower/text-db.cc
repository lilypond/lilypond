#include "text-db.hh"
bool
Text_db::eof()
{
  Data_file::gobble_leading_white();
  return  Data_file::eof();
}

void
Text_db::gobble_leading_white()
{
  while (1) 
    {
	Data_file::gobble_leading_white();
	if (eof())
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
	assert (!eof());
	
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


