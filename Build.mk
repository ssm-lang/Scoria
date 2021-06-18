##
# Top-level Build.mk. Designed to be included by a Makefile that defines the
# following variables:
#
# - PLATFORM: the platform for which targets should be built.
# - SSMDIR: the root of the SSM source tree (i.e., where this file is).
#
# ---
#
# Build directory setup based on Paul D. Smith's multi-platform recommendation:
# http://make.mad-scientist.net/papers/multi-architecture-builds
#
# If make isn't invoked from inside a build directory, this will set it up
# according to the specific platform, and re-invoke itself from within.
# The build directory is located at build/$(PLATFORM). This Makefile also sets
# up clean targets to remove the build directory.
#
# It will set up the following variables to help other Makefiles orient
# themselves:
#
#   SSMDIR := absolute path of the directory where this Makefile lives
#   PLATFORMDIR := platform-specific directory
#   RUNTIMEDIR := directory where platform-generic runtime is located
#
# ---
#
# Auto-dependency generation based on technique described by Paul D. Smith:
# http://make.mad-scientist.net/papers/advanced-auto-dependency-generation/
#
# Header dependencies are generated for any .c source files added to the SRCS
# variable. It does so by specifying compiler flags which cause the
# C preprocessor to generate .d files containing dependency information.
# On subsequent builds, these .d files may be included by Make to attach
# additional dependencies to .o targets, forcing them to be rebuilt if header
# files they depend on have been updated.
#
# The list of dependency files is determined using the SRCS variable, which
# should be populated by each of the other build directories. The LIBS variable
# is similarly used to determine libraries that the main target should depend
# on.
##

ifndef PLATFORM
$(error PLATFORM is not defined.)
endif

ifndef SSMDIR
$(error SSMDIR is not defined.)
endif

# If make isn't invoked from inside a build directory, set it up and go there
ifeq (,$(filter build, $(notdir $(abspath $(CURDIR)/..))))

export PLATFORM
export SSMDIR

# No built-in rules for this Makefile.
.SUFFIXES:

BUILDDIR := $(CURDIR)/build/$(PLATFORM)

# Set up build directory and recursively invoke Makefile from within, with the
# same goals.
.PHONY: $(BUILDDIR)
$(BUILDDIR):
	@+[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	@+$(MAKE) -C $(BUILDDIR) -f $(CURDIR)/Makefile $(MAKECMDGOALS)

# Do nothing for these targets, overriding the match-all rule defined below to
# prevent recursive evaluation.
Makefile : ;
%.mk :: ;

# Match-all rule that jumps to BUILDDIR rule above, and does nothing else.
% :: $(BUILDDIR) ; @:

# Create build directory, and echo the path.
.PHONY: make_builddir
make_builddir:
	@+[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	@+echo $(BUILDDIR)

# Clean up by removing platform build directory. Note that this leaves other
# platforms' build directories alone.
.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

# Clean build directories for all platforms.
.PHONY: distclean
distclean:
	$(RM) -r build

else # make was invoked from inside a build directory

RUNTIMEDIR := $(SSMDIR)/runtime
PLATFORMDIR := $(SSMDIR)/platform/$(PLATFORM)

include $(RUNTIMEDIR)/Build.mk
include $(PLATFORMDIR)/Build.mk

# Generated C files are placed in the build directory.
GEN_SRCS := $(notdir $(wildcard *.c))
SRCS += $(GEN_SRCS)

# The generated code uses the pattern x << y - z without unnecessary
# parentheses, which GCC goes out of its way to warn about without this flag.
CFLAGS += -Wno-parentheses

# Declare an executable target for each generated C file.
#
# Note that due to the way Make's implicit rules are defined, this will cause
# the linker command to contain both libLIB.a (from the pre-requisite) and -lLIB
# (from the LDLIBS) flag.
$(GEN_SRCS:%.c=%): %: %.o $(LIBS)
$(GEN_SRCS:%.c=%.o): $(LIBS)

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

CFLAGS += -std=c99 -Wall -O2 -g -Wpedantic
ARFLAGS = -crU

# We need to tell CC to look in the build directory (i.e., current directory)
# for libraries.
LDFLAGS += -L.

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

endif
