##
# Test platform build rules
##

CC = gcc
AR = ar

CPPFLAGS += -I $(PLATFORMDIR)/include -DDEBUG
vpath %.c $(PLATFORMDIR)/src

LDLIBS += -lplatform
LIBS += libplatform.a

PLATFORMSRC = ssm-debug.c

SRCS += $(PLATFORMSRC)

libplatform.a : libplatform.a($(PLATFORMSRC:%.c=%.o))

# ssm-debug.o: ssm-debug.c
