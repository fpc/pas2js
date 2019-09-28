TARGETS=democomponents demobrowseconsole demoajax demoxhr restbridgeclient \
  fpreportdemo chartjs_demoprogressbar chartjs_demoscatter demodate demomixed \
  chartjs_demobar chartjs_demoarea chartjs_demobubble chartjs_demopie \
  chartjs_demodatalabelling chartjs_demodoughnut chartjs_demointeractions \
  chartjs_demoline chartjs_demotime chartjs_demoscriptablebubble chartjs_demoradar \
  chartjs_democustompoints chartjs_demopolararea promiseall promisestory1 \
  promisestory2 promisestory demodb demoload demorest fpcunitbrowsertest \
  sampleda webgl1 pdfbasic hotreload dynload bootstraptable
ifneq ($(SKIPWEBCOMPILER),1)
TARGETS:=$(TARGETS) demowebcompiler
BASEDIR=$(CURDIR)/../compiler/packages
COMPILERDIR=$(CURDIR)/../compiler/utils/pas2js
COMPILERUNITPATH=$(BASEDIR)/compat;$(BASEDIR)/fcl-json/src;$(BASEDIR)/fcl-passrc/src;$(BASEDIR)/pastojs/src;$(BASEDIR)/fcl-js/src
endif
  
.PHONY: $(TARGETS)
P2JSOPT=-Fu../packages/\*
P2JS=pas2js 
BROWSERP2JS=$(P2JS) $(P2JSOPT) -Jirtl.js -Jc -Tbrowser
CHARTJSDIR=chartjs/js
CHARTOUTPUT=-FE$(CHARTJSDIR)
CHARTCMD=$(BROWSERP2JS) $(CHARTOUTPUT)

all: $(TARGETS)

info:
	@echo Available targets: $(TARGETS)
$(CHARTJSDIR):
	mkdir $(CHARTJSDIR)

democomponents: ./rtl/democomponents.lpr
	$(BROWSERP2JS) $<
demobrowseconsole: ./rtl/demobrowserconsole.lpr
	$(BROWSERP2JS) $<
demoajax: ./rtl/demoajax.lpr
	$(BROWSERP2JS) $<
demoxhr: ./rtl/demoxhr.lpr
	$(BROWSERP2JS) $<
restbridgeclient: ./restbridge/simple/restbridgeclient.lpr
	$(BROWSERP2JS) $<
demowebcompiler: ./webcompiler/demowebcompiler.lpr
	$(BROWSERP2JS) -Sc -TBrowser "-Fu$(COMPILERUNITPATH)" "-Fu$(COMPILERDIR)" $<
fpreportdemo: ./fpreport/reportdemo.lpr
	$(BROWSERP2JS) $<
chartjs_demoprogressbar: ./chartjs/demoprogressbar.lpr
	$(CHARTCMD)  $<
chartjs_demodate: ./chartjs/demodate.lpr
	$(CHARTCMD) $<
chartjs_demoscatter: ./chartjs/demoscatter.lpr
	$(CHARTCMD) $<
chartjs_demomixed: ./chartjs/demomixed.lpr
	$(CHARTCMD) $<
chartjs_demobar: ./chartjs/demobar.lpr
	$(CHARTCMD) $<
chartjs_demoarea: ./chartjs/demoarea.lpr
	$(CHARTCMD) $<
chartjs_demobubble: ./chartjs/demobubble.lpr
	$(CHARTCMD)  $<
chartjs_demopie: ./chartjs/demopie.lpr
	$(CHARTCMD) -Sc $<
chartjs_demodatalabelling: ./chartjs/demodatalabelling.lpr
	$(CHARTCMD) $<
chartjs_demodoughnut: ./chartjs/demodoughnut.lpr
	$(CHARTCMD) $<
chartjs_demointeractions: ./chartjs/demointeractions.lpr
	$(CHARTCMD) $<
chartjs_demoline: ./chartjs/demoline.lpr
	$(CHARTCMD) $<
chartjs_demotime: ./chartjs/demotime.lpr
	$(CHARTCMD) $<
chartjs_demoscriptablebubble: ./chartjs/demoscriptablebubble.lpr
	$(CHARTCMD) $<
chartjs_demoradar: ./chartjs/demoradar.lpr
	$(CHARTCMD) $<
chartjs_democustompoints: ./chartjs/democustompoints.lpr
	$(CHARTCMD) $<
chartjs_demopolararea: ./chartjs/demopolararea.lpr
	$(CHARTCMD) $<
promiseall: ./promise/demoall.lpr
	$(BROWSERP2JS) $<
promisestory1: ./promise/story2.lpr
	$(BROWSERP2JS) $<
promisestory2: ./promise/story3.lpr
	$(BROWSERP2JS) $<
promisestory: ./promise/story.lpr
	$(BROWSERP2JS) $<
demodb: ./fcldb/demodb.lpr
	$(BROWSERP2JS) $<
demoload: ./fcldb/demoload.lpr
	$(BROWSERP2JS) $<
demorest: ./fcldb/demorest.lpr
	$(BROWSERP2JS) $<
fpcunitbrowsertest: ./fpcunit/browsertest.lpr
	$(BROWSERP2JS) $<
sampleda: ./dataabstract/sampleda.lpr
	$(BROWSERP2JS) $<
webgl1: ./webgl/project1.lpr
	$(BROWSERP2JS) $<
pdfbasic: ./jspdf/basic.lpr
	$(BROWSERP2JS) $<
hotreload: ./hotreload/hotreload.lpr
	$(BROWSERP2JS) $<
dynload: ./dynload/testloader.lpr ./dynload/myform.pp
	$(BROWSERP2JS) -OoRemoveNotUsedDeclarations- ./dynload/testloader.lpr
	$(P2JS) ./dynload/myform.pp
bootstraptable: ./bootstrap/demobootstraptable.lpr
	$(BROWSERP2JS) $<
	