#
# standard bencherl makefile
#
ERL   ?= erl
ERLC  ?= erlc
EBIN  ?= ebin-${OTP}

ERLC_OPTS ?= -Werror

ERLF  = $(wildcard src/*.erl)
BEAMF = $(patsubst src/%.erl,$(EBIN)/%.beam,$(ERLF))

.PHONY: clean suite

suite: $(BEAMF)

$(EBIN)/%.beam: src/%.erl
	$(ERLC) $(ERLC_OPTS) -o$(EBIN) $<

$(BEAMF): | $(EBIN)

$(EBIN):
	mkdir -p $(EBIN)

clean:
	$(RM) -rf $(EBIN)/*
