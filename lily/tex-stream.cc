/*
  tex-stream.cc -- implement Tex_stream

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
  
  TODO

  make an abstract interface to output, operations: 

  move(x,y), put(symbol).
*/

#include <fstream.h>
#include <time.h>

#include "tex.hh"
#include "main.hh"
#include "tex-stream.hh"
#include "debug.hh"

const int MAXLINELEN = 200;

Tex_stream::Tex_stream(String filename) 
{
    os = new ofstream(filename);
    if (!*os)
	error("can't open `" + filename+"\'");
    nest_level = 0;
    line_len_i_ = 0;
    outputting_comment=false;
    header();
}
void
Tex_stream::header()
{
    *os << "% Creator: " << get_version_str() << "\n";
    *os << "% Automatically generated, at ";
    time_t t(time(0));
    *os << ctime(&t)<<"\n";
}
Tex_stream::~Tex_stream()
{
    delete os;
    assert(nest_level == 0);
}

// print string. don't forget indent.
Tex_stream &
Tex_stream::operator<<(String s)
{
    
    for (char const *cp = s; *cp; cp++) {
	if (outputting_comment) {
	    *os << *cp;
	    if (*cp == '\n') {
		outputting_comment=false;

	    }
	    continue;
	}
	line_len_i_ ++;
	switch(*cp) 
	    {
	    case '%':
		outputting_comment = true;
		*os << *cp;
		break;
	    case '{':
		nest_level++;
		*os << *cp;		
		break;
	    case '}':
		nest_level--;		
		*os << *cp;
		
		if (nest_level < 0) {
		    delete os;	// we want to see the remains.
		    assert(nest_level>=0);
		}
		/* FALLTHROUGH */
		
	    case '\n':
		break_line();
		break;	      
	    case ' ':
		*os <<  ' ';
		if (line_len_i_ > MAXLINELEN) 
		   break_line(); 
		
		break;
	    default:
		*os << *cp;
		break;
	    }
    }
    return *this;
}

void
Tex_stream::break_line()
{
    *os << "%\n";
    *os << String(' ', nest_level);
    line_len_i_ = 0;
}

/* *************************************************************** */
