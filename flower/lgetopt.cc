/*
   process command line, GNU style.

   this is (Copyleft) 1996, Han-Wen Nienhuys, <hanwen@stack.nl>
 */
#include <stdio.h>
#include <iostream.h>
#include <assert.h>
#include "lgetopt.hh"

long
Getopt_long::intarg()
{
    long l;
    if (sscanf(optarg, "%ld", &l) != 1)
	report(E_ILLEGALARG);
    
    return l;
}

Long_option_init *
Getopt_long::parselong()
{
    char const *optnm = argv[optind] + 2 ;
    assert(*optnm);
    
    char *endopt = strchr(optnm, '=');
    int searchlen  = (endopt) ? endopt - optnm : strlen(optnm);
    
    beet=0;
    for (int i=0; i< table_len; i++) {
	char const *ln = the_opts[i].longname;

	if (ln && !strncmp(ln, optnm, searchlen)) {
	    beet = the_opts+i;
	    break;
	}
    }	

    if (!beet) {
	report(E_UNKNOWNOPTION);
	return 0;
    }
    optind++;
    optindind = 0;

    
    if (beet->take_arg) {
	if (endopt)
	    optarg = endopt +1; // a '='
	else {
	    optarg = argv[optind];
	    optind++;
	}
	if (!optarg)
	    report(E_ARGEXPECT);

    } else {
	optarg = 0;
	if (endopt)
	    report(E_NOARGEXPECT);
    }
    
    return beet;
}


ostream &
Long_option_init::printon(ostream &errorout)
{
    if (shortname)	
	errorout <<"-" << shortname;
    if (shortname && longname)
	errorout << ", ";
    if (longname)	
	errorout << "`--" << longname << "'";
    return errorout;
}

// report an error, GNU style.
void
Getopt_long::report(Errorcod c)
{
    error = c;
    if (!errorout)
	return;

    *errorout << argv[0] << ": ";
    switch (c) {
    case E_ARGEXPECT:
	*errorout<< "option ";
	beet->printon(*errorout);
	*errorout << "requires an argument"<<endl;
	break;
    case  E_NOARGEXPECT:
	*errorout << "option `--" <<
	    beet->longname << "' does not allow an argument"<<endl;
	break;
	
    case E_UNKNOWNOPTION:
	*errorout << "unrecognized option ";
	if (optindind)
	    *errorout << "-" << argv[optind][optindind] << endl;
	else
	    *errorout << argv[optind] << endl;

	break;
    case E_ILLEGALARG:
	*errorout << "illegal argument `" << optarg << "\'to option ";
	beet->printon(*errorout);
	*errorout << '\n';
    default:
	assert(false);
    }
    exit(2); 
}
    
Long_option_init *
Getopt_long::parseshort()
{
    char c=argv[optind][optindind];
    beet=0;
    assert(c);
    
    for (int i=0; i < table_len; i++)
	if (the_opts[i].shortname == c) {
	    beet  = the_opts+i;
	    break;
	}

    if (!beet){
	report(E_UNKNOWNOPTION);
	return 0;
    }

    optindind++;
    if (!beet->take_arg){
	optarg = 0;
	return beet;
    }
    optarg = argv[optind] + optindind;

    optind ++;
    optindind = 0;
    
    if (!optarg[0]) {
	optarg = argv[optind];
	optind ++;
    }
    if (!optarg) {
	report(E_ARGEXPECT);
    }
    
    return beet;
}

Long_option_init *
Getopt_long::operator()() {
    if (!next())
	return 0;
    
    if (optindind)
	return parseshort();
    
    if (argv[optind][0] != '-')
	return 0;

    if (argv[optind][1] == '-') {// what to do with "command  --  bla"
	return parselong();
    } else {
	optindind = 1;
	return parseshort();
    }
}

Getopt_long::Getopt_long(int c, char **v, Long_option_init *lo)
{
    the_opts = lo;
    errorout = &cerr;
    argv = v;
    argc = c;
    optind = 1;
    optindind = 0;

    //    reached end of option table?
    int i;
    for (i = 0;  the_opts[i].longname ||the_opts[i].shortname; i++)
	;
    table_len = i;
}

bool
Getopt_long::next()
{

    error = E_NOERROR;
    while (optind < argc && !argv[optind][optindind]) {
	optind++;
	optindind = 0;
    }
    return (optind < argc);
}
   
char *
Getopt_long::current_arg()
{
    if (optind >= argc)
	return 0;
    char * a = argv[optind];
    return a + optindind;
}

char *
Getopt_long::get_next_arg()
{
    char * a = current_arg();
    if ( a) {
	optind ++;
	optindind = 0;
    }
    return a;
}
