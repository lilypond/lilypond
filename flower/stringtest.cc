// stringtest.cc

#include <iostream.h>
#include "string.hh"

int 
main()
{
    String str( "hai" );
    cout <<  str << endl;
    cout << "left" << endl;
    cout << "	0:" << str.left( 0 ) << endl;
    cout << "	1:" << str.left( 1 ) << endl;
    cout << "	2:" << str.left( 2 ) << endl;
    cout << "	3:" << str.left( 3 ) << endl;
    cout << "	4:" << str.left( 4 ) << endl;
    cout << "right" << endl;
    cout << "	0:" << str.right( 0 ) << endl;
    cout << "	1:" << str.right( 1 ) << endl;
    cout << "	2:" << str.right( 2 ) << endl;
    cout << "	3:" << str.right( 3 ) << endl;
    cout << "	4:" << str.right( 4 ) << endl;

    str += " daar";
    cout << str << endl;

    str = String( "hallo" ) + " daar" + '!';
    cout << str << endl;

    if ( str == String( "" ) )
        cout << str << " is empty" << endl;
    String fn = "";
    if ( fn == "" )
        cout << fn << " is empty" << endl;

    fn = "";
    fn += "";
    delete fn.copy_by_p();

    delete str.copy_by_p();

    cout << StringConversion::bin2hex_str( String( (char)0xff ) ) << endl;
}
