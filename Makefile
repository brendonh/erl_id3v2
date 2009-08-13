PKG_NAME = erl_id3v2

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

ERL_EBINS = -pa $(EBIN_DIR)

ERLC = erlc
ERLC_OPTS = -o $(EBIN_DIR) -Wall -v +debug_info

ERL_CMD=erl \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	$(ERL_EBINS)

all: $(TARGETS)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -s erl_id3v2_test test

clean: cleanlog
	rm -f $(TARGETS)
	rm -f $(EBIN_DIR)/*.beam

cleanlog:
	rm -f auth.log report.log sasl_err.log
	rm -f *.access

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<
