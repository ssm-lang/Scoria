##
# SSM project Makefile, which provides entry point to top-level Build.mk.
##

ifndef PLATFORM
PLATFORM := simulation
endif

ifndef SSMDIR
SSMDIR := $(CURDIR)
endif

include $(SSMDIR)/Build.mk
