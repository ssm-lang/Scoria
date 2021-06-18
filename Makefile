##
# Master Makefile. Sets up build directory for multi-platform build environment.
# Based on Paul D. Smith's multi-architecture build recommendation:
# http://make.mad-scientist.net/papers/multi-architecture-builds
#
# If this Makefile isn't invoked from inside the build directory, this will
# determine the platform being built for, and set up the build environment
# accordingly. Finally, it invoke itself again, from within the build directory.
#
# It will set up the following variables to help other Makefiles orient
# themselves:
#
#   ROOTDIR := absolute path of the directory where this Makefile lives
#   PLATFORM := name of the platform being targeted
#   PLATFORMDIR := platform-specific directory
#   RUNTIMEDIR := directory where platform-generic runtime is located
#
# The build directory is located at build/$(PLATFORM). This Makefile also sets
# up a clean target to remove the build directory.
#
# Header dependencies are generated for any .c source files added to the SRCS
# variable. See ./Build.mk for details.
##

ifeq (,$(filter build, $(notdir $(abspath $(CURDIR)/..))))
# If make isn't invoked from inside a build directory, set it up and go there

# No built-in rules for this Makefile.
.SUFFIXES:

ifndef PLATFORM
PLATFORM := simulation
export PLATFORM
endif

ROOTDIR := $(CURDIR)
export ROOTDIR

BUILDDIR := build/$(PLATFORM)

# Set up build directory and recursively invoke Makefile from within, with the
# same goals.
.PHONY: $(BUILDDIR)
$(BUILDDIR):
	@+[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	@+$(MAKE) -C $(BUILDDIR) -f $(CURDIR)/Makefile $(MAKECMDGOALS)

# Create build directory, and echo the path.
.PHONY: make_builddir
make_builddir:
	@+[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	@+echo $(BUILDDIR)

# Do nothing for these targets, overriding the match-all rule defined below to
# prevent recursive evaluation.
Makefile : ;
%.mk :: ;

# Match-all rule that jumps to BUILDDIR rule above, and does nothing else.
% :: $(BUILDDIR) ; @:

# Clean up by removing platform build directory. Note that this leaves other
# platforms' build directories alone.
.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

# Clean build artifacts for all platforms.
.PHONY: distclean
distclean:
	$(RM) -r build

else # make was invoked from inside a build directory

RUNTIMEDIR := $(ROOTDIR)/runtime
PLATFORMDIR := $(ROOTDIR)/platform/$(PLATFORM)

include $(RUNTIMEDIR)/Build.mk

include $(PLATFORMDIR)/Build.mk

include $(ROOTDIR)/Build.mk

endif
