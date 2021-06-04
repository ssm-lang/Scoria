##
# Compilation rules common to all parts of the build system. This should be
# included last by the main Makefile.
#
# This file defines compiler flags which causes the C compiler to generate a .d
# file which lists the dependency information on other header files. On
# subsequent builds, these .d files may be included by Make to attach additional
# dependencies to .o files, which will force them to be rebuilt if header files
# they depend on have been updated.
#
# The list of dependency files is determined using the SRCS variable, which
# should be populated by each of the other build directories (which is why this
# should be included last).
#
# Auto-dependency generation based on technique described by Paul D. Smith:
# http://make.mad-scientist.net/papers/advanced-auto-dependency-generation/
##

# Leave behind build artifacts
.PRECIOUS: %.c %.o %.a

# For compiling tools that should run on the host
HOSTCC = cc

# Produce .d files to capture dependency information
CPPFLAGS += -MT $@ -MMD -MP -MF $(*F).d

# Have .o targets depend on .d files, in case .d files are ever deleted
%.o : %.c %.d

# .d dependency files, based on .c files SRCS
DEPFILES := $(SRCS:%.c=%.d)

# Prevent dependency files from being deleted as intermediate files by declaring
# them as explicit targets
$(DEPFILES):

# Gather header dependency information from dependency files
include $(wildcard $(DEPFILES))

CFLAGS += -std=c99 -Wall -O2 -g -pedantic
ARFLAGS = -crU

# .elf and hex file generation rules
%.elf :
	$(CC) $(LDFLAGS) -o $@ $^
%.hex : %.elf
	$(OBJCOPY) -O ihex $^ $@

# # Running the Peng compiler

# %.c : %.pen
# 	$(PENG) --module-name=$(MODULE) --generate-c $< > $@

# %.h : %.pen
# 	$(PENG) --module-name=$(MODULE) --generate-h $< > $*.h
