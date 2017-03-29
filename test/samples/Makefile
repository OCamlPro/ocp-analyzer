%.cmt: %.ml
	ocamlc -nostdlib -nopervasives -c -bin-annot $<

ANALYZER=../../_obuild/ocp-analyzer/ocp-analyzer.asm
PERVASIVES=camlinternalFormatBasics.cmt pervasives.cmt

# Hard-coded dependencies
camlinternalFormatBasics.cmi: camlinternalFormatBasics.mli
	ocamlc -nostdlib -nopervasives -c -bin-annot $<
camlinternalFormatBasics.cmt: camlinternalFormatBasics.cmi
pervasives.cmi: pervasives.mli camlinternalFormatBasics.cmi
	ocamlc -nostdlib -nopervasives -c -bin-annot $<
pervasives.cmt: pervasives.cmi
pervasives.cmt: camlinternalFormatBasics.cmt
a.cmt: pervasives.cmt
booleans.cmt: pervasives.cmt
cycle.cmt: pervasives.cmt
good.cmt: pervasives.cmt
ifs.cmt: pervasives.cmt
integers.cmt: pervasives.cmt
mccarthy_nonrec.cmt: pervasives.cmt
option.cmt: pervasives.cmt
recursives.cmt: pervasives.cmt
semantics.cmt: pervasives.cmt


test_a: $(PERVASIVES) a.cmt
	$(ANALYZER) $^

test_applys: applys.cmt
	$(ANALYZER) $^

test_bad: bad.cmt
	$(ANALYZER) $^

test_blocks: blocks.cmt
	$(ANALYZER) $^

test_booleans: $(PERVASIVES) booleans.cmt
	$(ANALYZER) $^

test_bool_of_string: bool_of_string.cmt
	$(ANALYZER) $^

test_conditionnal: conditionnal.cmt
	$(ANALYZER) $^

test_cycle: $(PERVASIVES) cycle.cmt
	$(ANALYZER) $^

test_functions: functions.cmt
	$(ANALYZER) $^

test_good: $(PERVASIVES) good.cmt
	$(ANALYZER) $^

test_ifs: $(PERVASIVES) ifs.cmt
	$(ANALYZER) $^

test_integers: $(PERVASIVES) integers.cmt
	$(ANALYZER) $^

test_loop_no_rec: loop_no_rec.cmt
	$(ANALYZER) $^

test_mccarthy: mccarthy.cmt
	$(ANALYZER) $^

test_mccarthy_nonrec: $(PERVASIVES) mccarthy_nonrec.cmt
	$(ANALYZER) $^

test_minicycle: minicycle.cmt
	$(ANALYZER) $^

test_option: $(PERVASIVES) option.cmt
	$(ANALYZER) $^

test_recursives: $(PERVASIVES) recursives.cmt
	$(ANALYZER) $^

test_semantics: $(PERVASIVES) semantics.cmt
	$(ANALYZER) $^

test_simpler: simpler.cmt
	$(ANALYZER) $^

test_trywith_conststring: trywith_conststring.cmt
	$(ANALYZER) $^

test_trywith_exn: trywith_exn.cmt
	$(ANALYZER) $^

test_trywith_noexn: trywith_noexn.cmt
	$(ANALYZER) $^

ALL_TESTS=test_a test_applys test_bad test_blocks test_booleans \
          test_bool_of_string test_conditionnal test_cycle test_functions \
          test_good test_ifs test_integers test_loop_no_rec test_mccarthy \
          test_mccarthy_nonrec test_minicycle test_option test_recursives \
          test_semantics test_simpler test_trywith_conststring \
          test_trywith_exn test_trywith_noexn

all: $(ALL_TESTS)

clean:
	-rm *.cmt *.cmo *.cmi analyzer_output*

.PHONY: all clean $(ALL_TESTS)
