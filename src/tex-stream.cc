/*
  tex-stream.cc -- implement Tex_stream

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <fstream.h>
#include <time.h>

#include "tex.hh"
#include "main.hh"
#include "tex-stream.hh"
#include "debug.hh"

Tex_stream::Tex_stream(String filename) 
{
    os = new ofstream(filename);
    if (!*os)
	error("can't open `" + filename+"\'");
    nest_level = 0;
    outputting_comment=false;
    header();
}
void
Tex_stream::header()
{
    *os << "% Creator: " << get_version();
    *os << "% Automatically generated, at ";
    time_t t(time(0));
    *os << ctime(&t);
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
    
    for (const char *cp = s; *cp; cp++) {
	if (outputting_comment) {
	    *os << *cp;
	    if (*cp == '\n') {
		outputting_comment=false;

	    }
	    continue;
	}
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
		*os << "%\n";
		*os << String(' ', nest_level);
		break;	      
	    default:
		*os << *cp;
		break;
	    }
    }
    return *this;
}


/* *************************************************************** */
