/*
  simple-file-storage.cc -- implement Simple_file_storage

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <stdio.h>

#include "file-storage.hh"
#include "varray.hh"
#include "string.hh"
#include "warn.hh"

/**
  Stupid but foolproof way of opening files.

  TODO 
  Should use obstack. Should check IO status

  This is of course a build it yourself version of mmap, so we should
  have been using that... (see Mapped_file_storage) But we noticed
  some problems with this (unexplained lexer crashes)
  
  */

Simple_file_storage::Simple_file_storage(String s)
{
    data_p_ =0;
    FILE * f = fopen ( s.ch_C(), "r");
    if ( !f ) {
	warning("can't open file\n");
	return ;
    }

    int ret = fseek( f, 0, SEEK_END);
    len_i_ = ftell(f);
    rewind(f);
    data_p_ = new char[len_i_+1];
    data_p_[len_i_] = 0;
    ret = fread(data_p_, sizeof(char), len_i_, f);
    assert (ret==len_i_);
    fclose(f);
}

char const*
Simple_file_storage::ch_C() const
{
    return data_p_;
}

int
Simple_file_storage::length_i()const
{
    return len_i_;
}
    

Simple_file_storage::~Simple_file_storage()
{
    delete []data_p_;
}
