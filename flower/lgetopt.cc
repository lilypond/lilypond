/*
   process command line, GNU style.

   this is (Copyleft) 1996, Han-Wen Nienhuys, <hanwen@stack.nl>
 */
#include <stdio.h>
#include <iostream.h>
#include <assert.h>
#include "lgetopt.hh"

long
Getopt_long::argument_to_i()
{
  long l;
  if (!optional_argument_ch_C_ 
	|| sscanf (optional_argument_ch_C_, "%ld", &l) != 1)
	report (E_ILLEGALARG);
  
  return l;
}

const Long_option_init *
Getopt_long::parselong()
{
  char const *optnm = arg_value_ch_a_a_[array_index_i_] + 2 ;
  assert (*optnm);
  
  char const *endopt = strchr (optnm, '=');
  int searchlen  = (endopt) ? endopt - optnm : strlen (optnm);
  
  found_option_l_=0;
  for (int i=0; i< table_len_i_; i++) 
    {
	char const *ln = option_a_[i].longname;

	if (ln && !strncmp (ln, optnm, searchlen)) 
	  {
	    found_option_l_ = option_a_+i;
	    break;
	  }
  }	

  if (!found_option_l_) 
    {
	report (E_UNKNOWNOPTION);
	return 0;
    }
  array_index_i_++;
  argument_index_i_ = 0;

  
  if (found_option_l_->take_arg) 
    {
	if (endopt)
	    optional_argument_ch_C_ = endopt +1; // a '='
	else 
	  {
	    optional_argument_ch_C_ = arg_value_ch_a_a_[array_index_i_];
	    array_index_i_++;
	  }
	if (!optional_argument_ch_C_)
	    report (E_ARGEXPECT);

    }
  else 
    {
	optional_argument_ch_C_ = 0;
	if (endopt)
	    report (E_NOARGEXPECT);
    }
  
  return found_option_l_;
}


void
Long_option_init::printon (ostream &errorout)const
{
  if (shortname)	
	errorout <<"-" << shortname;
  if (shortname && longname)
	errorout << ", ";
  if (longname)	
	errorout << "`--" << longname << "'";
}

// report an error, GNU style.
void
Getopt_long::report (Errorcod c)
{
  error_ = c;
  if (!error_ostream_l_)
	return;

  *error_ostream_l_ << arg_value_ch_a_a_[0] << ": ";
  switch (c) 
    {
  case E_ARGEXPECT:
	*error_ostream_l_<< "option ";
	found_option_l_->printon (*error_ostream_l_);
	*error_ostream_l_ << "requires an argument"<<endl;
	break;
  case  E_NOARGEXPECT:
	*error_ostream_l_ << "option `--" <<
	    found_option_l_->longname << "' does not allow an argument"<<endl;
	break;
	
  case E_UNKNOWNOPTION:
	*error_ostream_l_ << "unrecognized option ";
	if (argument_index_i_)
	    *error_ostream_l_ << "-" << arg_value_ch_a_a_[array_index_i_][argument_index_i_] << endl;
	else
	    *error_ostream_l_ << arg_value_ch_a_a_[array_index_i_] << endl;

	break;
  case E_ILLEGALARG:
	*error_ostream_l_ << "illegal argument `" << optional_argument_ch_C_ << "\'to option ";
	found_option_l_->printon (*error_ostream_l_);
	*error_ostream_l_ << '\n';
  default:
	assert (false);
    }
  exit (2); 
}
  
const Long_option_init *
Getopt_long::parseshort()
{
  char c=arg_value_ch_a_a_[array_index_i_][argument_index_i_];
  found_option_l_=0;
  assert (c);
  
  for (int i=0; i < table_len_i_; i++)
	if (option_a_[i].shortname == c) 
	  {
	    found_option_l_  = option_a_+i;
	    break;
	  }

  if (!found_option_l_)
    {
	report (E_UNKNOWNOPTION);
	return 0;
    }

  argument_index_i_++;
  if (!found_option_l_->take_arg)
    {
	optional_argument_ch_C_ = 0;
	return found_option_l_;
    }
  optional_argument_ch_C_ = arg_value_ch_a_a_[array_index_i_] + argument_index_i_;

  array_index_i_ ++;
  argument_index_i_ = 0;
  
  if (!optional_argument_ch_C_[0]) 
    {
	optional_argument_ch_C_ = arg_value_ch_a_a_[array_index_i_];
	array_index_i_ ++;
    }
  if (!optional_argument_ch_C_) 
    {
	report (E_ARGEXPECT);
    }
  
  return found_option_l_;
}

const Long_option_init *
Getopt_long::operator()() 
{
  if (!ok())
	return 0;
  
  next();
  
  if (argument_index_i_)
 	return parseshort();
  
  const char * argument_C = arg_value_ch_a_a_[array_index_i_];
  
  if (argument_C[0] != '-')
	return 0;

  if (argument_C[1] == '-') {// what to do with "command  --  bla"
	if ( argument_C[2])
	    return parselong();
	else
	    return 0;
    }
  else 
    {
	if (argument_C[ 1 ]) 
	  {
	    argument_index_i_ = 1;
	    return parseshort();
	  }
	else 
	  {
	    return 0;
	  }
    }
}

Getopt_long::Getopt_long (int c, char  **v, Long_option_init *lo)
{
  option_a_ = lo;
  error_ostream_l_ = &cerr;
  arg_value_ch_a_a_ = v;
  argument_count_i_ = c;
  array_index_i_ = 1;
  argument_index_i_ = 0;

  //    reached end of option table?
  table_len_i_ =0;
  for (int i = 0;  option_a_[i].longname ||option_a_[i].shortname; i++)
	table_len_i_ ++;
}

bool
Getopt_long::ok()const
{
  return  array_index_i_ < argument_count_i_;
}

void
Getopt_long::next()
{
  error_ = E_NOERROR;
  while (array_index_i_ < argument_count_i_
	   && !arg_value_ch_a_a_[array_index_i_][argument_index_i_]) 
	     {
	array_index_i_++;
	argument_index_i_ = 0;
    }
}
   
char const *
Getopt_long::current_arg()
{
  if (array_index_i_ >= argument_count_i_)
	return 0;
  char const * a = arg_value_ch_a_a_[array_index_i_];
  return a + argument_index_i_;
}

char const *
Getopt_long::get_next_arg()
{
  char const * a = current_arg();
  if (a) 
    {
	array_index_i_ ++;
	argument_index_i_= 0;
    }
  return a;
}
