################################################################################
# Top-level Build.mk. Designed to be included by a Makefile that defines the
# following variables:
#
# - PLATFORM: the platform for which targets should be built.
# - SSMDIR: the root of the SSM source tree (i.e., where this file is).
#
# The including Makefile may also define the SSMTARGET to to indicate the
# default build target. The Haskell file that defines the SSM source program
# should be in $(SSMTARGET).hs in the same directory as the including Makefile.
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
################################################################################

ifndef PLATFORM
$(error PLATFORM is not defined.)
endif

ifndef SSMDIR
$(error SSMDIR is not defined.)
else
SSMDIR := $(abspath $(SSMDIR))
endif

# This file is planted in the build directory to help us figure out whether we
# are in there.
BUILDDIRTOKEN := .ssm-build-dir

################################################################################
ifeq (,$(wildcard $(CURDIR)/$(BUILDDIRTOKEN))) # not inside build directory
################################################################################

# No built-in Make rules until we are inside the build directory.
.SUFFIXES:

########
# For any target, set up build directory and re-run make from inside.
##

# These variables need to available when we re-invoke make.
export PLATFORM
export SSMDIR
export MAKECMDDIR := $(CURDIR)
ifdef SSMTARGET
export SSMTARGET
endif

BUILDDIR := $(CURDIR)/build/$(PLATFORM)

# Set up build directory and recursively invoke Makefile from within, with the
# same goals.
.PHONY: $(BUILDDIR)
$(BUILDDIR):
	@+[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	@+touch $(BUILDDIR)/$(BUILDDIRTOKEN)
	@+$(MAKE) -C $(BUILDDIR) -f $(SSMDIR)/Build.mk $(MAKECMDGOALS)

# Do nothing for these targets, overriding the match-all rule defined below to
# prevent recursive evaluation.
Makefile : ;
%.mk :: ;

# Match-all rule that jumps to BUILDDIR rule above, and does nothing else.
% :: $(BUILDDIR) ; @:


########
# Convenice phony targets for managing build directory.
##

# Create build directory, and echo the path.
.PHONY: make_builddir
make_builddir:
	@+[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	@+touch $(BUILDDIR)/$(BUILDDIRTOKEN)
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


################################################################################
else # make was invoked from inside build directory
################################################################################

########
# Compiler frontend (.hs => .c)
##

ifdef SSMTARGET
# Default target.
$(SSMTARGET):
endif

# SSM EDSL source files can be found where make was originally invoked.
vpath %.hs $(MAKECMDDIR)

SSM_SRCS := $(foreach hs,$(wildcard $(MAKECMDDIR)/*.hs),$(notdir $(hs)))

# Build rule for .c files from SSM EDSL files.
$(SSM_SRCS:%.hs=%.c): %.c: %.hs
	stack --stack-yaml $(SSMDIR)/stack.yaml runghc $< -- -o $@

########
# Runtime library compilation (.c => .a)
##

# These variables are used by runtime and platform Build.mk to orient themselves
RUNTIMEDIR := $(SSMDIR)/runtime
PLATFORMDIR := $(SSMDIR)/platform/$(PLATFORM)

include $(PLATFORMDIR)/Build.mk

####
# Build platform-generic runtime library
##

CPPFLAGS += -I $(RUNTIMEDIR)/include
LDLIBS += -lssm

ifdef SSM_ACT_QUEUE_SIZE
CPPFLAGS += -DSSM_ACT_QUEUE_SIZE=$(SSM_ACT_QUEUE_SIZE)
endif

ifdef SSM_EVENT_QUEUE_SIZE
CPPFLAGS += -DSSM_EVENT_QUEUE_SIZE=$(SSM_EVENT_QUEUE_SIZE)
endif

vpath %.c $(RUNTIMEDIR)/src

RUNTIMESRC := $(notdir $(wildcard $(RUNTIMEDIR)/src/*.c))

libssm.a : libssm.a($(RUNTIMESRC:%.c=%.o))

SRCS += $(RUNTIMESRC)
LIBS += libssm.a


########
# Compiler backend/C compilation (.c => .o)
##

# The generated code uses the pattern x << y - z without unnecessary
# parentheses, which GCC goes out of its way to warn about without this flag.
CFLAGS += -Wno-parentheses

# Generated C files are placed in the build directory.
# TODO: remove this, or at least make this specific to the trace platform.
# This is no longer needed for generic builds.
GEN_SRCS := $(notdir $(wildcard *.c))

SRCS += $(SSM_SRCS:%.hs=%.c)
SRCS += $(GEN_SRCS)

$(GEN_SRCS:%.c=%.o):

# Leave behind build artifacts.
.PRECIOUS: %.c %.o %.a

# For compiling tools that should run on the host.
HOSTCC = cc

# Produce .d files to capture dependency information.
CPPFLAGS += -MT $@ -MMD -MP -MF $(*F).d

# Have .o targets depend on .d files, in case .d files are ever deleted.
%.o : %.c %.d

# .d dependency files, based on .c files SRCS.
DEPFILES := $(SRCS:%.c=%.d)

# Prevent dependency files from being deleted as intermediate files by declaring
# them as explicit targets.
$(DEPFILES):

# Gather header dependency information from dependency files.
include $(wildcard $(DEPFILES))

CFLAGS += -Wall -O2 -g -Wpedantic


########
# Compiler backend/archiving (.o => .a)
##

ARFLAGS = -crU


########
# Compiler backend/linkage (.o/.a => executable/hex)
##

# We need to tell CC to look in the build directory (i.e., current directory)
# for libraries.
LDFLAGS += -L.

# Declare an executable target for each generated C file.
#
# Note that due to the way Make's implicit rules are defined, this will cause
# the linker command to contain both libLIB.a (from the pre-requisite) and -lLIB
# (from the LDLIBS) flag.
$(GEN_SRCS:%.c=%): %: %.o $(LIBS)
$(SSM_SRCS:%.hs=%): %: %.o $(LIBS)

# $(CC) $(LDFLAGS) $< $(LDLIBS) -o $@
# $(CC) $(LDFLAGS) $< $(LDLIBS) -o $@

# .elf and hex file generation rules.
%.elf :
	$(CC) $(LDFLAGS) -o $@ $^
%.hex : %.elf
	$(OBJCOPY) -O ihex $^ $@

endif
