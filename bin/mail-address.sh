#!/bin/sh
# mail-address
if [ "$MAILADDRESS" != "" ]; then
	echo $MAILADDRESS
else
	echo "mail-address:6: warning: \$MAILADDRESS undefined" > /dev/stderr
	echo $USER@`hostname`
fi
