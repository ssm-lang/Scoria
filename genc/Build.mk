##
# Makefile for generated C code
##

vpath %.c $(GENCDIR)

GEN_SRCS := $(notdir $(wildcard $(GENCDIR)/*.c))

SRCS += $(GEN_SRCS)

# The generated code uses the pattern x << y - z without unnecessary
# parentheses, which GCC goes out of its way to warn about without this flag.
CFLAGS += -Wno-parentheses

# Declare an executable target for each generated C file found in GENCDIR.
#
# Note that due to the way Make's implicit rules are defined, this will cause
# the linker command to contain both libLIB.a (from the pre-requisite) and -lLIB
# (from the LDLIBS) flag.
$(GEN_SRCS:%.c=%): %: %.o $(LIBS)

$(GEN_SRCS:%.c=%.o): $(LIBS)
