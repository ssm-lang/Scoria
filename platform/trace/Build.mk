##
# Trace platform build rules
#
# This platform is meant to be used in conjunction with this project's
# Quickcheck property-based tests. When compiled with this platform, the
# resulting executable will run on the host machine, and print a trace to stdout
# that can be parsed and compared against the execution trace of the SSM
# interpreter.
##

CC = gcc
AR = ar

CPPFLAGS += -I $(PLATFORMDIR)/include -DDEBUG
vpath %.c $(PLATFORMDIR)/src

LDLIBS += -lplatform
LIBS += libplatform.a

PLATFORMSRC = ssm-trace.c

SRCS += $(PLATFORMSRC)

libplatform.a : libplatform.a($(PLATFORMSRC:%.c=%.o))
