# title	   user changeable settings
# file	   make/User.make

# will be split into CFLAGS/EXTRA_CFLAGS etc, 
# so that defineable generically and per module

#
# -lefence = ElectricFence.
#
# ElectricFence is a memory debugger which uses the 
# VM hardware to trap malloc/free errors.
#

EXTRALIB+= #-lefence

