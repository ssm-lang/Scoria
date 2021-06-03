##
# Master Makefile. Sets up build directory for multi-platform build environment.
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
#   RUNTIMEDIR := directory where platform-generic runtime is located
#   PLATFORMDIR := platform-specific directory
#   GENCDIR := directory where generated C code can be found
#
# The build directory is located at build/$(PLATFORM).
#
# This file also sets up a clean targets.
#
# Based on Paul D. Smith's multi-architecture build recommendation:
# http://make.mad-scientist.net/papers/multi-architecture-builds
##

ifeq (,$(filter build, $(notdir $(abspath $(CURDIR)/..))))
# If make isn't invoked from inside a build directory, set it up and go there

# No built-in rules for this Makefile.
.SUFFIXES:

ifndef PLATFORM
PLATFORM := linux
export PLATFORM
endif

ROOTDIR := $(CURDIR)
export ROOTDIR

BUILDDIR := build/$(PLATFORM)

# Set up build directory and recursively invoke Makefile from within, with the
# same goals.
.PHONY: $(BUILDDIR)
$(BUILDDIR):
	@+[ -d $@ ] || mkdir -p $@         # Create build directory
	@+$(MAKE) -C $@ -f $(CURDIR)/Makefile $(MAKECMDGOALS)

# Do nothing for these targets, overriding the match-all rule defined below to
# prevent recursive evaluation.
Makefile : ;
%.mk :: ;

# Match-all rule that jumps to BUILDDIR rule above, and does nothing else.
% :: $(BUILDDIR) ; @:

# Clean by simply removing build directory. Note that this only removes the
# build directory for the specified PLATFORM. It also leaves generated C code
# under genc intact.
.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

# Clean all build artifacts, including generated C code.
.PHONY: distclean
distclean:
	$(RM) -r build
	$(RM) -r genc/*.c genc/*.h

else # make was invoked from inside a build directory

RUNTIMEDIR := $(ROOTDIR)/runtime
PLATFORMDIR := $(ROOTDIR)/platform/$(PLATFORM)
GENCDIR := $(ROOTDIR)/genc

include $(ROOTDIR)/Build.mk

include $(RUNTIMEDIR)/Build.mk

include $(PLATFORMDIR)/Build.mk

include $(GENCDIR)/Build.mk

endif
