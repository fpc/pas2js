{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2022 by Michael Van Canneyt

    Interface to PDF.js

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit pdfjs;

{$MODE ObjFPC}
{$H+}
{$modeswitch externalclass}

interface

uses SysUtils, JS,web;

{$INTERFACES CORBA}
Type
  // Forward class definitions
  TGlobalWorkerOptions = Class;
  TPDFPromise = Class;
  TPDFTreeNode = Class;
  TPDFInfo = Class;
  TPDFMetadata = Class;
  TPDFWorkerParameters = Class;
  TCMapReaderFactory = Class;
  TCMapReader = Class;
  TPDFSource = Class;
  TPDFProgressData = Class;
  TPDFDocumentProxy = Class;
  TPDFRef = Class;
  TPDFPageViewportOptions = Class;
  TPDFPageViewport = Class;
  TViewportParameters = Class;
  TPDFAnnotationData = Class;
  TPDFAnnotations = Class;
  TPDFRenderTextLayer = Class;
  TPDFRenderImageLayer = Class;
  TPDFRenderParams = Class;
  TPDFViewerParams = Class;
  TPDFRenderTask = Class;
  TPDFPageProxy = Class;
  TTextContentItem = Class;
  TTextContent = Class;
  TPDFObjects = Class;
  TPDFJSUtilStatic = Class;
  TPDFJSStatic = Class;
  TPDFLoadingTask = Class;
  // Forward class definitions
  TPDFDataRangeTransport = Class;
  TPDFWorker = Class;
  
  TVerbosityLevel = (ERRORS, WARNINGS, INFOS);
  TCMapCompressionType = (NONE, BINARY, STREAM);
  TgetDocument_passwordCallback_fn = Procedure (password : string);
  TgetDocument_passwordCallback = Function (fn : TgetDocument_passwordCallback_fn; reason : string): string;
  TgetDocument_progressCallback = Procedure (progressData : TPDFProgressData);
  TPDFDataRangeTransportListener = Procedure (loaded : Double; total : Double);
  
  TGlobalWorkerOptions = class external name 'Object' (TJSObject)
    workerSrc: string;
  end;
  
  TPDFDataRangeTransport = class external name 'PDFDataRangeTransport' (TJSObject)
  Public
    Constructor New(&length : Double; initialData : jsvalue; progressiveDone : boolean); overload;
    Constructor New(&length : Double; initialData : jsvalue); overload;
    Procedure abort;
    Procedure addProgressiveDoneListener(listener : TPDFDataRangeTransportListener);
    Procedure addProgressiveReadListener(listener : TPDFDataRangeTransportListener);
    Procedure addProgressListener(listener : TPDFDataRangeTransportListener);
    Procedure addRangeListener(listener : TPDFDataRangeTransportListener);
    Procedure onDataProgress(loaded : Double; total : Double);
    Procedure onDataProgressiveDone;
    Procedure onDataProgressiveRead(chunk : JSValue);
    Procedure onDataRange(&begin : Double; chunk : JSValue);
    Procedure requestDataRange(&begin : Double; &end : Double);
    Procedure transportReady;
  end;
  
  TPDFWorker = class external name 'PDFWorker' (TJSObject)
  Private
    Fpromise : TJSPromise; external name 'promise'; 
    Fport : jsvalue; external name 'port'; 
    FmessageHandler : jsvalue; external name 'messageHandler'; 
  Public
    Constructor New(params : TPDFWorkerParameters); overload;
    Constructor New; overload;
    Procedure destroy;
    Function fromPort(params : TPDFWorkerParameters): TPDFWorker; overload;
    Function fromPort: TPDFWorker; overload;
    Function getWorkerSrc: string;
    Property promise : TJSPromise Read Fpromise; 
    Property port : jsvalue Read Fport; 
    Property messageHandler : jsvalue Read FmessageHandler; 
  end;
  
  TPDFPromise_then_onResolve = reference to Function (promise : JSValue): JSValue;
  TPDFPromise_then_onReject = reference to Procedure (reason : string);
  TPDFPromise = class external name 'Object' (TJSObject)
      Function &then(onResolve : TPDFPromise_then_onResolve; onReject : TPDFPromise_then_onReject): TPDFPromise; overload;
      Function &then(onResolve : TPDFPromise_then_onResolve): TPDFPromise; overload;
      Function isRejected: boolean;
      Function isResolved: boolean;
      Procedure reject(reason : string);
      Procedure resolve(value : JSValue);
  end;
  
  TPDFTreeNode_color = array of Double;
  TPDFTreeNode_items = array of TPDFTreeNode;
  TPDFTreeNode = class external name 'Object' (TJSObject)
      title : string;
      bold : boolean;
      italic : boolean;
      color : TPDFTreeNode_color;
      dest : JSValue;
      items : TPDFTreeNode_items;
  end;
  
  TPDFInfo = class external name 'Object' (TJSObject)
      PDFFormatVersion : string;
      IsAcroFormPresent : boolean;
      IsXFAPresent : boolean;
  end;
  
  TPDFMetadata = class external name 'Object' (TJSObject)
      Function get(name : string): string;
      Function has(name : string): boolean;
      Procedure parse;
  end;
  
  TPDFWorkerParameters = class external name 'Object' (TJSObject)
      name : string;
      port : JSValue;
      verbosity : TVerbosityLevel;
  end;
  
  TCMapReaderFactory_new_params = class external name 'Object' (TJSObject)
  Public
    baseUrl : string;
    isCompressed : boolean;
  end;
  
  TCMapReaderFactory = class external name 'Object' (TJSObject)
      Function &new(params : TCMapReaderFactory_new_params): TCMapReader;
  end;
  
  TCMapReader_fetch_params = class external name 'Object' (TJSObject)
  Public
    name : string;
  end;
  
  TCMapReader = class external name 'Object' (TJSObject)
      Function fetch(params : TCMapReader_fetch_params): TJSPromise;
  end;
  
  TPDFSource_httpHeaders = class external name 'Object' (TJSObject)
  Public
  end;
  
  TPDFSource = class external name 'Object' (TJSObject)
      url : string;
      data : jsvalue;
      httpHeaders : TPDFSource_httpHeaders;
      password : string;
      withCredentials : boolean;
      initialData : jsvalue;
      &length : Double;external name 'length';
      range : TPDFDataRangeTransport;
      rangeChunkSize : Double;
      worker : TPDFWorker;
      verbosity : Double;
      docBaseUrl : string;
      nativeImageDecoderSupport : jsvalue;
      cMapUrl : string;
      cMapPacked : boolean;
      CMapReaderFactory : JSValue;
      stopAtErrors : boolean;
      maxImageSize : Double;
      isEvalSupported : boolean;
      disableFontFace : boolean;
      disableRange : boolean;
      disableStream : boolean;
      disableAutoFetch : boolean;
      disableCreateObjectURL : boolean;
      pdfBug : boolean;
  end;
  
  TPDFProgressData = class external name 'Object' (TJSObject)
      loaded : Double;
      total : Double;
  end;
  
  TPDFDocumentProxy = class external name 'Object' (TJSObject)
      Function dataLoaded: TPDFPromise;
      Procedure destroy;
      Function embeddedFontsUsed: boolean;
      Function getData: TPDFPromise;
      Function getDestinations: TPDFPromise;
      Function getJavaScript: TPDFPromise;
      Function getMetadata: TPDFPromise;
      Function getOutline: TPDFPromise;
      Function getPage(number : Double): TPDFPromise;
      Function getPermissions: TPDFPromise;
      Function isEncrypted: TPDFPromise;
      numPages : Integer;
      fingerprint : string;
  end;
  
  TPDFRef = class external name 'Object' (TJSObject)
      num : Double;
      gen : JSValue;
  end;
  
  TPDFPageViewportOptions = class external name 'Object' (TJSObject)
      viewBox : JSValue;
      scale : Double;
      rotation : Double;
      offsetX : Double;
      offsetY : Double;
      dontFlip : boolean;
  end;
  
  TPDFPageViewport_transforms = array of Double;
  TPDFPageViewport_convertToViewportPoint_Result = array of Double;
  TPDFPageViewport_convertToViewportRectangle_Result = array of Double;
  TPDFPageViewport_convertToPdfPoint_Result = array of Double;
  TPDFPageViewport = class external name 'Object' (TJSObject)
      Function clone(options : TPDFPageViewportOptions): TPDFPageViewport;
      Function convertToPdfPoint(x : Double; y : Double): TPDFPageViewport_convertToPdfPoint_Result;
      Function convertToViewportPoint(x : Double; y : Double): TPDFPageViewport_convertToViewportPoint_Result;
      Function convertToViewportRectangle(rect : array of Double): TPDFPageViewport_convertToViewportRectangle_Result;
      width : integer;
      height : Integer;
      scale : Double;
      transforms : TPDFPageViewport_transforms;
  end;
  
  TViewportParameters = class external name 'Object' (TJSObject)
      scale : Double;
      rotation : Double;
      offsetX : Double;
      offsetY : Double;
      dontFlip : boolean;
  end;
  
  TPDFAnnotationData_rect = array of Double;
  TPDFAnnotationData_color = array of Double;
  TPDFAnnotationData = class external name 'Object' (TJSObject)
      subtype : string;
      rect : TPDFAnnotationData_rect;
      annotationFlags : JSValue;
      color : TPDFAnnotationData_color;
      borderWidth : Double;
      hasAppearance : boolean;
  end;
  
  TPDFAnnotations = class external name 'Object' (TJSObject)
      Function getData: TPDFAnnotationData;
      Function getEmptyContainer(tagName : string; rect : array of Double): TJSHTMLElement;
      Function getHtmlElement(commonOjbs : JSValue): TJSHTMLElement;
      Function getOperatorList(evaluator : JSValue): TPDFPromise;
      Function hasHtml: boolean;
      Function isViewable: boolean;
      Function loadResources(keys : JSValue): TPDFPromise;
  end;
  
  TPDFRenderTextLayer = class external name 'Object' (TJSObject)
      Procedure appendText;
      Procedure beginLayout;
      Procedure endLayout;
  end;
  
  TPDFRenderImageLayer = class external name 'Object' (TJSObject)
      Procedure appendImage;
      Procedure beginLayout;
      Procedure endLayout;
  end;
  
  TPDFRenderParams_continueCallback__continue = Procedure;
  TPDFRenderParams_continueCallback = Procedure (_continue : TPDFRenderParams_continueCallback__continue);
  TPDFRenderParams = class external name 'Object' (TJSObject)
      canvasContext : TJSCanvasRenderingContext2D;
      viewport : TPDFPageViewport;
      textLayer : TPDFRenderTextLayer;
      imageLayer : TPDFRenderImageLayer;
      renderInteractiveForms : boolean;
      continueCallback : TPDFRenderParams_continueCallback;
  end;
  
  TPDFViewerParams = class external name 'Object' (TJSObject)
      container : TJSHTMLElement;
      viewer : TJSHTMLElement;
  end;

  TPDFLoadingTask = class external name 'Object' (TJSObject)
      promise : TPDFPromise;
  end;

  TPDFRenderTask = class external name 'Object' (TPDFLoadingTask)
      Procedure cancel;
  end;
  
  TPDFPageProxy_view = array of Double;
  TPDFPageProxy = class external name 'Object' (TJSObject)
      Procedure destroy;
      Function getAnnotations: TPDFPromise;
      Function getTextContent: TPDFPromise;
      Function getViewport(params : TViewportParameters): TPDFPageViewport;
      Function render(params : TPDFRenderParams): TPDFRenderTask;
      pageIndex : Double;
      pageNumber : Double;
      rotate : Double;
      ref : TPDFRef;
      view : TPDFPageProxy_view;
  end;
  
  TTextContentItem_transform = array of Double;
  TTextContentItem = class external name 'Object' (TJSObject)
      str : string;
      transform : TTextContentItem_transform;
      width : Double;
      height : Double;
      dir : string;
      fontName : string;
  end;
  
  TTextContent_items = array of TTextContentItem;
  TTextContent = class external name 'Object' (TJSObject)
      items : TTextContent_items;
      styles : JSValue;
  end;
  
  TPDFObjects = class external name 'Object' (TJSObject)
      Procedure clear;
      Function get(objId : Double; callback : JSValue): JSValue; overload;
      Function get(objId : Double): JSValue; overload;
      Function getData(objId : Double): JSValue;
      Function hasData(objId : Double): boolean;
      Function isResolved(objId : Double): boolean;
      Function resolve(objId : Double; data : JSValue): JSValue;
  end;
  
  TPDFJSUtilStatic_normalizeRect_Result = array of Double;
  TPDFJSUtilStatic = class external name 'Object' (TJSObject)
      Function normalizeRect(rect : array of Double): TPDFJSUtilStatic_normalizeRect_Result;
  end;
  
  TPDFJSStatic = class external name 'Object' (TJSObject)
      Procedure PDFSinglePageViewer(params : TPDFViewerParams);
      Procedure PDFViewer(params : TPDFViewerParams);
      maxImageSize : Double;
      cMapUrl : string;
      cMapPacked : boolean;
      disableFontFace : boolean;
      imageResourcesPath : string;
      disableWorker : boolean;
      workerSrc : string;
      disableRange : boolean;
      disableStream : boolean;
      disableAutoFetch : boolean;
      pdfBug : boolean;
      postMessageTransfers : boolean;
      disableCreateObjectURL : boolean;
      disableWebGL : boolean;
      disableFullscreen : boolean;
      disableTextLayer : boolean;
      useOnlyCssZoom : boolean;
      verbosity : Double;
      maxCanvasPixels : Double;
      openExternalLinksInNewWindow : boolean;
      isEvalSupported : boolean;
      GlobalWorkerOptions :  TGlobalWorkerOptions;
      Function getDocument(url : string; pdfDataRangeTransport : TPDFDataRangeTransport; passwordCallback : TgetDocument_passwordCallback; progressCallback : TgetDocument_progressCallback): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(url : string): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(url : string; pdfDataRangeTransport : TPDFDataRangeTransport): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(url : string; pdfDataRangeTransport : TPDFDataRangeTransport; passwordCallback : TgetDocument_passwordCallback): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(data : jsvalue; pdfDataRangeTransport : TPDFDataRangeTransport; passwordCallback : TgetDocument_passwordCallback; progressCallback : TgetDocument_progressCallback): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(data : jsvalue): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(data : jsvalue; pdfDataRangeTransport : TPDFDataRangeTransport): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(data : jsvalue; pdfDataRangeTransport : TPDFDataRangeTransport; passwordCallback : TgetDocument_passwordCallback): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(source : TPDFSource; pdfDataRangeTransport : TPDFDataRangeTransport; passwordCallback : TgetDocument_passwordCallback; progressCallback : TgetDocument_progressCallback): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(source : TPDFSource): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(source : TPDFSource; pdfDataRangeTransport : TPDFDataRangeTransport): TPDFLoadingTask; external name 'getDocument'; overload;
      Function getDocument(source : TPDFSource; pdfDataRangeTransport : TPDFDataRangeTransport; passwordCallback : TgetDocument_passwordCallback): TPDFLoadingTask; external name 'getDocument'; overload;
  end;
  


implementation

end.
