#include <fstream.h>
#include <ctype.h>

#include "textstr.hh"
Text_stream::Text_stream(String fn)
{	
        if (fn == "") 
	    {
	    name = "<STDIN>";	    
	    f = stdin;
	    }
	
	else 
	    {
	    name = fn;	    
	    f = fopen(fn, "r");
	    }
	
	if (!f) {
	    cerr <<__FUNCTION__<< ": can't open `" << fn << "'\n";
	    exit(1);
	}

	line_no = 1;
    }

void
Text_stream::message(String s)
{
    cerr << "\n"<<get_name() << ": " << line()<<": "<<s<<endl;
}

void 
Data_file::gobble_white()
{
    char c;
    
    while ((c=data_get()) == ' ' ||c == '\t')
	if (eof()) 
	    break;

    data_unget(c);
}

String
Data_file::get_word() 
{// should handle escape seq's
    String s;

    while (1) 
	{
	char 	c  = data_get();
	
	if (isspace(c) || eof()) 
	    {
	    data_unget(c);
	    break;
	    }
	
	
	if (c == '\"')
	    {
	    rawmode= true;

	    while ((c  = data_get()) != '\"')
		if (eof())
		    error("EOF in a string");		
		else
		    s += c;
	    

	    rawmode= false;
	    }	    
	else
	    s += c;		
	}
    
    return s;	      
}

/// get a char.
/**
   Only class member who uses text_file::get
   */
char
Data_file::data_get() {
    char c =  get(); 
    if (!rawmode && c == '#') // gobble comment
	{	
	while ((c = get()) != '\n' && !eof()) 
	    ;
	    return '\n';
	}    

    return c;
}

/// read line, gobble '\n'    
String Data_file::get_line()     
{
    char c; 
    String s;

    while ((c  = data_get()) != '\n' && !eof())
	s += c;
    return s;	
}

/// gobble stuff before first entry on a line.    
void
Data_file::gobble_leading_white() 
{
    // eat blank lines.
    while (!eof()) {
	char c = data_get();		    
	if (!isspace(c)) {
	    data_unget(c);
	    break;
	}
    }
}


