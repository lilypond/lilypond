/*
  source.cc -- implement Source

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include <assert.h>

#include "binary-source-file.hh"
#include "string.hh"
#include "proto.hh"
#include "plist.hh"
#include "source-file.hh"
#include "source.hh"

void
Sources::set_path(File_path *f_C)
{
    path_C_ = f_C;
}

/**
  open a file

  @param file_str the file to be opened, name might be changed if it
  is found in a search path.

  @return 0 if no file found
  */
Source_file*
Sources::get_file_l(String &file_str )
{
    if (path_C_){
	file_str = path_C_->find(file_str); 
	if (file_str== "")
	    return 0;
    }
    Source_file * f_p= (!binary_b_) ?
	new Source_file(file_str) : new Binary_source_file(file_str);
    add(f_p);
    return f_p;
}

Sources::Sources()
{
    path_C_= 0;
    binary_b_ = false;
}

void
Sources::add( Source_file* sourcefile_p )
{
    sourcefile_p_iplist_.bottom().add( sourcefile_p );
}

/**
  search the list for file whose map contains pointer #ch_C#

  @return 0 if not found.
  */
Source_file*
Sources::sourcefile_l( char const* ch_C )
{
    PCursor<Source_file*> sourcefile_l_pcur( sourcefile_p_iplist_.top() );
    for ( ; sourcefile_l_pcur.ok(); sourcefile_l_pcur++ )
	if ( sourcefile_l_pcur->in_b( ch_C ) )	
	    return *sourcefile_l_pcur;
    return 0;
}
