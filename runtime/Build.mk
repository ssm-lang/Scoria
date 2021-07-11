##
# Build rules for the runtime system. Meant to be included from the master
# Makefile.
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
vpath %.c $(RUNTIMEDIR)/test

#RUNTIMESRC := ssm-queue.c ssm-sched.c ssm-types.c
RUNTIMESRC := ssm-bool.c ssm-event.c ssm-i16.c ssm-i32.c ssm-i64.c \
	ssm-i8.c ssm-scheduler.c ssm-top-act.c ssm-u16.c ssm-u32.c ssm-u64.c ssm-u8.c

#TESTSRC := test-ssm-queue.c

SRCS += $(RUNTIMESRC) $(TESTSRC)
LIBS += libssm.a

libssm.a : libssm.a($(RUNTIMESRC:%.c=%.o))

test-ssm-queue:
