##
# Simulation platform build rules
#
# When compiled with this platform, the SSM program runs in "simulation" mode,
# stepping through the entire execution without any synchronization with wall
# clock time.
#
# TODO: make this OS/environment-agonistic, to an extent.
##

CC = gcc
AR = ar

CPPFLAGS += -I $(PLATFORMDIR)/include
