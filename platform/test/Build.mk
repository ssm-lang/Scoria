##
# Test platform build rules
##

CC = gcc
AR = ar

CPPFLAGS += -I $(PLATFORMDIR)/include
vpath %.c $(PLATFORMDIR)/src
LDFLAGS += -lplatform

PLATFORMSRC = ssm-debug.c

SRCS += $(PLATFORMSRC)

libplatform.a : libplatform.a($(PLATFORMSRC:%.c=%.o))

# ssm-debug.o: ssm-debug.c
