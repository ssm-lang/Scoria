##
# Compilation rules common to all parts of the build system.
#
# Each platform is expected to define their own CC and AR paths (to allow
# cross-compilation), and add necessary flags to CPPFLAGS, CFLAGS, and LDFLAGS.
##

# For compiling tools that should run on the host
HOSTCC := cc

# Leave behind build artifacts and dependency information, useful for debugging
.PRECIOUS: %.c %.o %.a
CPPFLAGS += -MT $@ -MMD -MP -MF $(*F).d

CFLAGS += -std=c99 -Wall -O2 -g -pedantic

ARFLAGS = -crU

# .elf and hex file generation rules, for
%.elf :
	$(CC) $(LDFLAGS) -o $@ $^
%.hex : %.elf
	$(OBJCOPY) -O ihex $^ $@

# # Running the Peng compiler

# %.c : %.pen
# 	$(PENG) --module-name=$(MODULE) --generate-c $< > $@

# %.h : %.pen
# 	$(PENG) --module-name=$(MODULE) --generate-h $< > $*.h
