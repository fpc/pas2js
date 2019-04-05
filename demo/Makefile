TARGETS=democomponents demobrowseconsole demoajax demoxhr restbridgeclient \
  fpreportdemo chartjs_demoprogressbar chartjs_demoscatter demodate demomixed \
  chartjs_demobar chartjs_demoarea chartjs_demobubble chartjs_demopie \
  chartjs_demodatalabelling chartjs_demodoughnut chartjs_demointeractions \
  chartjs_demoline chartjs_demotime chartjs_demoscriptablebubble chartjs_demoradar \
  chartjs_democustompoints chartjs_demopolararea promiseall promisestory1 \
  promisestory2 promisestory demodb demoload demorest fpcunitbrowsertest \
  sampleda webgl1 pdfbasic hotreload
ifneq ($(SKIPWEBCOMPILER),1)
TARGETS:=$(TARGETS) demowebcompiler
BASEDIR=$(CURDIR)/../compiler/packages
COMPILERDIR=$(CURDIR)/../compiler/utils/pas2js
COMPILERUNITPATH=$(BASEDIR)/compat;$(BASEDIR)/fcl-json/src;$(BASEDIR)/fcl-passrc/src;$(BASEDIR)/pastojs/src;$(BASEDIR)/fcl-js/src
endif
  
.PHONY: $(TARGETS)
P2JS=pas2js
P2JSOPT=-Fu../packages/\*
all: $(TARGETS)

info:
	@echo Available targets: $(TARGETS)

democomponents: ./rtl/democomponents.lpr
	$(P2JS) $(P2JSOPT) $<
	
demobrowseconsole: ./rtl/demobrowserconsole.lpr
	$(P2JS) $(P2JSOPT) $<
demoajax: ./rtl/demoajax.lpr
	$(P2JS) $(P2JSOPT) $<
demoxhr: ./rtl/demoxhr.lpr
	$(P2JS) $(P2JSOPT) $<
restbridgeclient: ./restbridge/simple/restbridgeclient.lpr
	$(P2JS) $(P2JSOPT) $<
demowebcompiler: ./webcompiler/demowebcompiler.lpr
	$(P2JS) -Sc -TBrowser "-Fu$(COMPILERUNITPATH)" "-Fu$(COMPILERDIR)" $(P2JSOPT) $<
fpreportdemo: ./fpreport/reportdemo.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demoprogressbar: ./chartjs/demoprogressbar.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demodate: ./chartjs/demodate.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demoscatter: ./chartjs/demoscatter.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demomixed: ./chartjs/demomixed.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demobar: ./chartjs/demobar.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demoarea: ./chartjs/demoarea.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demobubble: ./chartjs/demobubble.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demopie: ./chartjs/demopie.lpr
	$(P2JS) -Sc $(P2JSOPT) $<
chartjs_demodatalabelling: ./chartjs/demodatalabelling.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demodoughnut: ./chartjs/demodoughnut.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demointeractions: ./chartjs/demointeractions.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demoline: ./chartjs/demoline.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demotime: ./chartjs/demotime.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demoscriptablebubble: ./chartjs/demoscriptablebubble.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demoradar: ./chartjs/demoradar.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_democustompoints: ./chartjs/democustompoints.lpr
	$(P2JS) $(P2JSOPT) $<
chartjs_demopolararea: ./chartjs/demopolararea.lpr
	$(P2JS) $(P2JSOPT) $<
promiseall: ./promise/demoall.lpr
	$(P2JS) $(P2JSOPT) $<
promisestory1: ./promise/story2.lpr
	$(P2JS) $(P2JSOPT) $<
promisestory2: ./promise/story3.lpr
	$(P2JS) $(P2JSOPT) $<
promisestory: ./promise/story.lpr
	$(P2JS) $(P2JSOPT) $<
demodb: ./fcldb/demodb.lpr
	$(P2JS) $(P2JSOPT) $<
demoload: ./fcldb/demoload.lpr
	$(P2JS) $(P2JSOPT) $<
demorest: ./fcldb/demorest.lpr
	$(P2JS) $(P2JSOPT) $<
fpcunitbrowsertest: ./fpcunit/browsertest.lpr
	$(P2JS) $(P2JSOPT) $<
sampleda: ./dataabstract/sampleda.lpr
	$(P2JS) $(P2JSOPT) $<
webgl1: ./webgl/project1.lpr
	$(P2JS) $(P2JSOPT) $<
pdfbasic: ./jspdf/basic.lpr
	$(P2JS) $(P2JSOPT) $<
hotreload: ./hotreload/hotreload.lpr
	$(P2JS) $(P2JSOPT) $<
