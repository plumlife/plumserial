PROJECT = serial

C_SRC_OUTPUT = priv/$(PROJECT)

include erlang.mk

# c_src compile flags
CC      =`which arm-linux-gnueabi-gcc`
CFLAGS  = -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
LDFLAGS = -lpthread

# Compile flags
ERLC_COMPILE_OPTS= +debug_info

# Use the same settings for compiling releases as well as for testing
ERLC_OPTS= $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS= $(ERLC_COMPILE_OPTS)
