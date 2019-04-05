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
P2JS=pas2js -Fu../packages/*

all: $(TARGETS)

info:
	@echo Available targets: $(TARGETS)

democomponents: ./rtl/democomponents.lpr
	$(P2JS) $<
	
demobrowseconsole: ./rtl/demobrowserconsole.lpr
	$(P2JS) $<
demoajax: ./rtl/demoajax.lpr
	$(P2JS) $<
demoxhr: ./rtl/demoxhr.lpr
	$(P2JS) $<
restbridgeclient: ./restbridge/simple/restbridgeclient.lpr
	$(P2JS) $<
demowebcompiler: ./webcompiler/demowebcompiler.lpr
	$(P2JS) -Sc -TBrowser "-Fu$(COMPILERUNITPATH)" "-Fu$(COMPILERDIR)" $<
fpreportdemo: ./fpreport/reportdemo.lpr
	$(P2JS) $<
chartjs_demoprogressbar: ./chartjs/demoprogressbar.lpr
	$(P2JS) $<
chartjs_demodate: ./chartjs/demodate.lpr
	$(P2JS) $<
chartjs_demoscatter: ./chartjs/demoscatter.lpr
	$(P2JS) $<
chartjs_demomixed: ./chartjs/demomixed.lpr
	$(P2JS) $<
chartjs_demobar: ./chartjs/demobar.lpr
	$(P2JS) $<
chartjs_demoarea: ./chartjs/demoarea.lpr
	$(P2JS) $<
chartjs_demobubble: ./chartjs/demobubble.lpr
	$(P2JS) $<
chartjs_demopie: ./chartjs/demopie.lpr
	$(P2JS) -Sc $<
chartjs_demodatalabelling: ./chartjs/demodatalabelling.lpr
	$(P2JS) $<
chartjs_demodoughnut: ./chartjs/demodoughnut.lpr
	$(P2JS) $<
chartjs_demointeractions: ./chartjs/demointeractions.lpr
	$(P2JS) $<
chartjs_demoline: ./chartjs/demoline.lpr
	$(P2JS) $<
chartjs_demotime: ./chartjs/demotime.lpr
	$(P2JS) $<
chartjs_demoscriptablebubble: ./chartjs/demoscriptablebubble.lpr
	$(P2JS) $<
chartjs_demoradar: ./chartjs/demoradar.lpr
	$(P2JS) $<
chartjs_democustompoints: ./chartjs/democustompoints.lpr
	$(P2JS) $<
chartjs_demopolararea: ./chartjs/demopolararea.lpr
	$(P2JS) $<
promiseall: ./promise/demoall.lpr
	$(P2JS) $<
promisestory1: ./promise/story2.lpr
	$(P2JS) $<
promisestory2: ./promise/story3.lpr
	$(P2JS) $<
promisestory: ./promise/story.lpr
	$(P2JS) $<
demodb: ./fcldb/demodb.lpr
	$(P2JS) $<
demoload: ./fcldb/demoload.lpr
	$(P2JS) $<
demorest: ./fcldb/demorest.lpr
	$(P2JS) $<
fpcunitbrowsertest: ./fpcunit/browsertest.lpr
	$(P2JS) $<
sampleda: ./dataabstract/sampleda.lpr
	$(P2JS) $<
webgl1: ./webgl/project1.lpr
	$(P2JS) $<
pdfbasic: ./jspdf/basic.lpr
	$(P2JS) $<
hotreload: ./hotreload/hotreload.lpr
	$(P2JS) $<
