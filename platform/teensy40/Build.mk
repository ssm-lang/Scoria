##
# Teensy 4.0 platform make rules
##

CC = arm-none-eabi-gcc
AR = arm-none-eabi-ar
OBJCOPY = arm-none-eabi-objcopy

CPPFLAGS += -I $(PLATFORMDIR)/include

# The Teensy 4.0's IMXRT1062 has an ARM Cortex M7 processor with hardware
# floating-point in the form of the VFPv5 architecture (Double-precision)
# and support for the Thumb ISA
CPUOPTIONS = -mcpu=cortex-m7 -mfloat-abi=hard -mfpu=fpv5-d16 -mthumb

# -ffunction-sections
# -fdata-sections
#     Puts everything in its own section in the elf file.
#     Not clear it's useful
CFLAGS += -Wall -O2 $(CPUOPTIONS) -ffunction-sections -fdata-sections

vpath %.c $(PLATFORMDIR)/src

SRCS += startup.c teensy_loader_cli.c

LDFLAGS += -Os -Wl,--gc-sections,--relax $(CPUOPTIONS)
LDFLAGS += -T$(PLATFORMDIR)/imxrt1062.ld

LDLIBS += -lplatform
LIBS += libplatform.a

libplatform.a: libplatform.a(startup.o)

$(MODULE).hex : $(MODULE).elf

##
# Loader program (which runs on host)
##

LOADER = teensy_loader_cli
LOADERFLAGS = --mcu=imxrt1062

$(LOADER) : $(LOADER).c
	$(HOSTCC) -O2 -Wall -s -DUSE_LIBUSB -o $@ $< -lusb

.PHONY : program
program : $(MODULE).hex $(LOADER)
	@echo "Press the key on the Teensy"
	./$(LOADER) $(LOADERFLAGS) -w $(MODULE).hex

.PHONY : reboot
reboot : $(LOADER)
	@echo "Press the key on the Teensy"
	./$(LOADER) $(LOADERFLAGS) -w -b
