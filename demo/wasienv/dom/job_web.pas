unit JOB_Web;

{$MODE ObjFPC}
{$H+}

interface

uses SysUtils, JOB_JS;

Type
  // Forward class definitions
  IJSEventTarget = interface;
  TJSEventTarget = class;
  IJSNode = interface;
  TJSNode = class;
  IJSEvent = interface;
  TJSEvent = class;
  IJSGlobalEventHandlers = interface;
  TJSGlobalEventHandlers = class;
  IJSWindowEventHandlers = interface;
  TJSWindowEventHandlers = class;
  IJSDocumentAndElementEventHandlers = interface;
  TJSDocumentAndElementEventHandlers = class;
  IJSOnErrorEventHandlerForNodes = interface;
  TJSOnErrorEventHandlerForNodes = class;
  IJSOnErrorEventHandlerForWindow = interface;
  TJSOnErrorEventHandlerForWindow = class;
  IJSContentSecurityPolicy = interface;
  TJSContentSecurityPolicy = class;
  IJSPrincipal = interface;
  TJSPrincipal = class;
  IJSWindowProxy = interface;
  TJSWindowProxy = class;
  IJSnsISupports = interface;
  TJSnsISupports = class;
  IJSURI = interface;
  TJSURI = class;
  IJSnsIDocShell = interface;
  TJSnsIDocShell = class;
  IJSnsILoadGroup = interface;
  TJSnsILoadGroup = class;
  IJSnsIReferrerInfo = interface;
  TJSnsIReferrerInfo = class;
  IJSnsICookieJarSettings = interface;
  TJSnsICookieJarSettings = class;
  IJSnsIPermissionDelegateHandler = interface;
  TJSnsIPermissionDelegateHandler = class;
  IJSXULCommandDispatcher = interface;
  TJSXULCommandDispatcher = class;
  IJSDocument = interface;
  TJSDocument = class;
  IJSXPathExpression = interface;
  TJSXPathExpression = class;
  IJSParentNode = interface;
  TJSParentNode = class;
  IJSFontFaceSource = interface;
  TJSFontFaceSource = class;
  IJSDocumentOrShadowRoot = interface;
  TJSDocumentOrShadowRoot = class;
  IJSDOMImplementation = interface;
  TJSDOMImplementation = class;
  IJSDocumentType = interface;
  TJSDocumentType = class;
  IJSChildNode = interface;
  TJSChildNode = class;
  IJSNonDocumentTypeChildNode = interface;
  TJSNonDocumentTypeChildNode = class;
  IJSnsIScreen = interface;
  TJSnsIScreen = class;
  IJSElement = interface;
  TJSElement = class;
  IJSHTMLOrForeignElement = interface;
  TJSHTMLOrForeignElement = class;
  IJSElementCSSInlineStyle = interface;
  TJSElementCSSInlineStyle = class;
  IJSHTMLCollection = interface;
  TJSHTMLCollection = class;
  IJSDocumentFragment = interface;
  TJSDocumentFragment = class;
  IJSText = interface;
  TJSText = class;
  IJSComment = interface;
  TJSComment = class;
  IJSCDATASection = interface;
  TJSCDATASection = class;
  IJSAttr = interface;
  TJSAttr = class;
  IJSLocation = interface;
  TJSLocation = class;
  IJSNodeList = interface;
  TJSNodeList = class;
  IJSDOMStringList = interface;
  TJSDOMStringList = class;
  IJSCaretPosition = interface;
  TJSCaretPosition = class;
  IJSDOMTokenList = interface;
  TJSDOMTokenList = class;
  IJSNamedNodeMap = interface;
  TJSNamedNodeMap = class;
  IJSDOMStringMap = interface;
  TJSDOMStringMap = class;
  IJSCSSStyleDeclaration = interface;
  TJSCSSStyleDeclaration = class;
  IJSCharacterData = interface;
  TJSCharacterData = class;
  IJSnsIBrowserDOMWindow = interface;
  TJSnsIBrowserDOMWindow = class;
  IJSXULControllers = interface;
  TJSXULControllers = class;
  IJSnsIDOMWindowUtils = interface;
  TJSnsIDOMWindowUtils = class;
  IJSnsIPrintSettings = interface;
  TJSnsIPrintSettings = class;
  IJSWindow = interface;
  TJSWindow = class;
  IJSWindowSessionStorage = interface;
  TJSWindowSessionStorage = class;
  IJSWindowLocalStorage = interface;
  TJSWindowLocalStorage = class;
  IJSTouch = interface;
  TJSTouch = class;
  IJSTouchList = interface;
  TJSTouchList = class;
  IJSOfflineResourceList = interface;
  TJSOfflineResourceList = class;
  IJSHistory = interface;
  TJSHistory = class;
  IJSCustomElementRegistry = interface;
  TJSCustomElementRegistry = class;
  IJSBarProp = interface;
  TJSBarProp = class;
  IJSNavigator = interface;
  TJSNavigator = class;
  IJSNavigatorID = interface;
  TJSNavigatorID = class;
  IJSNavigatorLanguage = interface;
  TJSNavigatorLanguage = class;
  IJSNavigatorOnLine = interface;
  TJSNavigatorOnLine = class;
  IJSNavigatorContentUtils = interface;
  TJSNavigatorContentUtils = class;
  IJSNavigatorStorage = interface;
  TJSNavigatorStorage = class;
  IJSNavigatorStorageUtils = interface;
  TJSNavigatorStorageUtils = class;
  IJSNavigatorGeolocation = interface;
  TJSNavigatorGeolocation = class;
  IJSNavigatorConcurrentHardware = interface;
  TJSNavigatorConcurrentHardware = class;
  IJSNavigatorAutomationInformation = interface;
  TJSNavigatorAutomationInformation = class;
  IJSNavigatorLocks = interface;
  TJSNavigatorLocks = class;
  IJSStorage = interface;
  TJSStorage = class;
  IJSSelection = interface;
  TJSSelection = class;
  IJSnsISelectionListener = interface;
  TJSnsISelectionListener = class;
  IJSScreen = interface;
  TJSScreen = class;
  IJSScreenLuminance = interface;
  TJSScreenLuminance = class;
  IJSClipboard = interface;
  TJSClipboard = class;
  IJSClipboardItem = interface;
  TJSClipboardItem = class;
  IJSRange = interface;
  TJSRange = class;
  IJSAbstractRange = interface;
  TJSAbstractRange = class;
  IJSScreenOrientation = interface;
  TJSScreenOrientation = class;
  IJSDOMRectList = interface;
  TJSDOMRectList = class;
  IJSDOMRect = interface;
  TJSDOMRect = class;
  IJSDOMRectReadOnly = interface;
  TJSDOMRectReadOnly = class;
  IJSHTMLElement = interface;
  TJSHTMLElement = class;
  IJSTouchEventHandlers = interface;
  TJSTouchEventHandlers = class;
  IJSHTMLUnknownElement = interface;
  TJSHTMLUnknownElement = class;
  IJSHTMLHeadElement = interface;
  TJSHTMLHeadElement = class;
  IJSHTMLAllCollection = interface;
  TJSHTMLAllCollection = class;
  IJSMenuBuilder = interface;
  TJSMenuBuilder = class;
  IJSHTMLMenuElement = interface;
  TJSHTMLMenuElement = class;
  IJSElementInternals = interface;
  TJSElementInternals = class;
  IJSShadowRoot = interface;
  TJSShadowRoot = class;
  IJSHTMLFormElement = interface;
  TJSHTMLFormElement = class;
  IJSValidityState = interface;
  TJSValidityState = class;
  IJSSVGViewSpec = interface;
  TJSSVGViewSpec = class;
  IJSSVGSVGElement = interface;
  TJSSVGSVGElement = class;
  IJSSVGFitToViewBox = interface;
  TJSSVGFitToViewBox = class;
  IJSSVGZoomAndPan = interface;
  TJSSVGZoomAndPan = class;
  IJSCSSRule = interface;
  TJSCSSRule = class;
  IJSSVGAnimatedLength = interface;
  TJSSVGAnimatedLength = class;
  IJSSVGPoint = interface;
  TJSSVGPoint = class;
  IJSSVGNumber = interface;
  TJSSVGNumber = class;
  IJSSVGLength = interface;
  TJSSVGLength = class;
  IJSSVGAngle = interface;
  TJSSVGAngle = class;
  IJSSVGMatrix = interface;
  TJSSVGMatrix = class;
  IJSSVGRect = interface;
  TJSSVGRect = class;
  IJSSVGTransform = interface;
  TJSSVGTransform = class;
  IJSDOMMatrixReadOnly = interface;
  TJSDOMMatrixReadOnly = class;
  IJSDOMMatrix = interface;
  TJSDOMMatrix = class;
  IJSSVGAnimatedRect = interface;
  TJSSVGAnimatedRect = class;
  IJSSVGAnimatedPreserveAspectRatio = interface;
  TJSSVGAnimatedPreserveAspectRatio = class;
  IJSCSSStyleSheet = interface;
  TJSCSSStyleSheet = class;
  IJSDOMPointReadOnly = interface;
  TJSDOMPointReadOnly = class;
  IJSDOMPoint = interface;
  TJSDOMPoint = class;
  IJSSVGPreserveAspectRatio = interface;
  TJSSVGPreserveAspectRatio = class;
  IJSCSSRuleList = interface;
  TJSCSSRuleList = class;
  IJSSVGGraphicsElement = interface;
  TJSSVGGraphicsElement = class;
  IJSSVGTests = interface;
  TJSSVGTests = class;
  IJSSVGAnimatedTransformList = interface;
  TJSSVGAnimatedTransformList = class;
  IJSSVGElement = interface;
  TJSSVGElement = class;
  IJSSVGStringList = interface;
  TJSSVGStringList = class;
  IJSSVGTransformList = interface;
  TJSSVGTransformList = class;
  IJSSVGAnimatedString = interface;
  TJSSVGAnimatedString = class;
  IJSStyleSheet = interface;
  TJSStyleSheet = class;
  IJSHTMLAnchorElement = interface;
  TJSHTMLAnchorElement = class;
  IJSHTMLButtonElement = interface;
  TJSHTMLButtonElement = class;
  IJSHTMLCanvasElement = interface;
  TJSHTMLCanvasElement = class;
  IJSMozCanvasPrintState = interface;
  TJSMozCanvasPrintState = class;
  IJSHTMLDivElement = interface;
  TJSHTMLDivElement = class;
  IJSHTMLEmbedElement = interface;
  TJSHTMLEmbedElement = class;
  IJSHTMLIFrameElement = interface;
  TJSHTMLIFrameElement = class;
  IJSimgINotificationObserver = interface;
  TJSimgINotificationObserver = class;
  IJSimgIRequest = interface;
  TJSimgIRequest = class;
  IJSnsIStreamListener = interface;
  TJSnsIStreamListener = class;
  IJSHTMLImageElement = interface;
  TJSHTMLImageElement = class;
  IJSMozImageLoadingContent = interface;
  TJSMozImageLoadingContent = class;
  IJSHTMLInputElement = interface;
  TJSHTMLInputElement = class;
  IJSMozEditableElement = interface;
  TJSMozEditableElement = class;
  IJSHTMLLabelElement = interface;
  TJSHTMLLabelElement = class;
  IJSHTMLLinkElement = interface;
  TJSHTMLLinkElement = class;
  IJSHTMLOptionElement = interface;
  TJSHTMLOptionElement = class;
  IJSHTMLProgressElement = interface;
  TJSHTMLProgressElement = class;
  IJSnsIEditor = interface;
  TJSnsIEditor = class;
  IJSHTMLTextAreaElement = interface;
  TJSHTMLTextAreaElement = class;
  IJSHTMLHyperlinkElementUtils = interface;
  TJSHTMLHyperlinkElementUtils = class;
  IJSLinkStyle = interface;
  TJSLinkStyle = class;
  IJSOffscreenCanvas = interface;
  TJSOffscreenCanvas = class;
  IJSBlob = interface;
  TJSBlob = class;
  IJSFileList = interface;
  TJSFileList = class;
  IJSnsIFile = interface;
  TJSnsIFile = class;
  IJSFile = interface;
  TJSFile = class;
  IJSFileSystem = interface;
  TJSFileSystem = class;
  IJSFileSystemEntry = interface;
  TJSFileSystemEntry = class;
  IJSImageBitmap = interface;
  TJSImageBitmap = class;
  IJSFileSystemDirectoryEntry = interface;
  TJSFileSystemDirectoryEntry = class;
  IJSFileSystemDirectoryReader = interface;
  TJSFileSystemDirectoryReader = class;
  IJSFormData = interface;
  TJSFormData = class;
  TJSEventListenerOptions = TJOB_Dictionary;
  TJSAddEventListenerOptions = TJOB_Dictionary;
  TJSGetRootNodeOptions = TJOB_Dictionary;
  TJSElementCreationOptions = TJOB_Dictionary;
  TJSBlockParsingOptions = TJOB_Dictionary;
  TJSWireframeTaggedRect = TJOB_Dictionary;
  TJSWireframe = TJOB_Dictionary;
  TJSFocusOptions = TJOB_Dictionary;
  TJSScrollIntoViewOptions = TJOB_Dictionary;
  TJSShadowRootInit = TJOB_Dictionary;
  TJSSetHTMLOptions = TJOB_Dictionary;
  TJSScrollOptions = TJOB_Dictionary;
  TJSScrollToOptions = TJOB_Dictionary;
  TJSIdleRequestOptions = TJOB_Dictionary;
  TJSWindowPostMessageOptions = TJOB_Dictionary;
  TJSTouchInit = TJOB_Dictionary;
  TJSElementDefinitionOptions = TJOB_Dictionary;
  TJSLifecycleCallbacks = TJOB_Dictionary;
  TJSShareData = TJOB_Dictionary;
  TJSClipboardItemOptions = TJOB_Dictionary;
  TJSClientRectsAndTexts = TJOB_Dictionary;
  TJSDOMRectInit = TJOB_Dictionary;
  TJSValidityStateFlags = TJOB_Dictionary;
  TJSDOMMatrix2DInit = TJOB_Dictionary;
  TJSDOMMatrixInit = TJOB_Dictionary;
  TJSCSSStyleSheetInit = TJOB_Dictionary;
  TJSDOMPointInit = TJOB_Dictionary;
  TJSSVGBoundingBoxOptions = TJOB_Dictionary;
  TJSDateTimeValue = TJOB_Dictionary;
  TJSImageEncodeOptions = TJOB_Dictionary;
  TJSBlobPropertyBag = TJOB_Dictionary;
  TJSFilePropertyBag = TJOB_Dictionary;
  TJSChromeFilePropertyBag = TJOB_Dictionary;
  TJSAutocompleteInfo = TJOB_Dictionary;
  TJSFileSystemFlags = TJOB_Dictionary;
  TJSChannelPixelLayout = TJOB_Dictionary;
  TJSImageBitmapOptions = TJOB_Dictionary;
  TVisibilityState = UnicodeString;
  TDocumentAutoplayPolicy = UnicodeString;
  TFlashClassification = UnicodeString;
  TWireframeRectType = UnicodeString;
  TScrollLogicalPosition = UnicodeString;
  TReferrerPolicy = UnicodeString;
  TScrollBehavior = UnicodeString;
  TScrollRestoration = UnicodeString;
  TScreenColorGamut = UnicodeString;
  TPresentationStyle = UnicodeString;
  TOrientationType = UnicodeString;
  TOrientationLockType = UnicodeString;
  TShadowRootMode = UnicodeString;
  TSlotAssignmentMode = UnicodeString;
  TCSSStyleSheetParsingMode = UnicodeString;
  TSelectionMode = UnicodeString;
  TOffscreenRenderingContextId = UnicodeString;
  TEndingType = UnicodeString;
  TImageBitmapFormat = UnicodeString;
  TChannelPixelLayoutDataType = UnicodeString;
  TImageOrientation = UnicodeString;
  TPremultiplyAlpha = UnicodeString;
  TColorSpaceConversion = UnicodeString;
  TEventListener = function (event: IJSEvent): Boolean of object;
  TEventHandlerNonNull = function (event: IJSEvent): TJOB_JSValue of object;
  TEventHandler = TEventHandlerNonNull;
  TOnBeforeUnloadEventHandlerNonNull = function (event: IJSEvent): UnicodeString of object;
  TOnBeforeUnloadEventHandler = TOnBeforeUnloadEventHandlerNonNull;
  TOnErrorEventHandlerNonNull = function (event: TJOB_JSValue; const source: UnicodeString; lineno: LongWord; column: LongWord; error: TJOB_JSValue): TJOB_JSValue of object;
  TOnErrorEventHandler = TOnErrorEventHandlerNonNull;
  TApplicationCache = TJSOfflineResourceList;
  TPromiseDocumentFlushedCallback = function : TJOB_JSValue of object;
  TCustomElementCreationCallback = procedure (const name: UnicodeString) of object;
  TLifecycleConnectedCallback = procedure  of object;
  TLifecycleDisconnectedCallback = procedure  of object;
  TLifecycleAdoptedCallback = procedure (oldDocument: IJSDocument; newDocment: IJSDocument) of object;
  TLifecycleAttributeChangedCallback = procedure (const attrName: UnicodeString; const oldValue: UnicodeString; const newValue: UnicodeString; const namespaceURI: UnicodeString) of object;
  TLifecycleFormAssociatedCallback = procedure (form: IJSHTMLFormElement) of object;
  TLifecycleFormResetCallback = procedure  of object;
  TLifecycleFormDisabledCallback = procedure (disabled: Boolean) of object;
  TClipboardItems = IJSArray; // array of TJSClipboardItem
  // Union of DOMString, Blob
  TClipboardItemDataType = TJOB_JSValue;
  // Union of HTMLCanvasElement, OffscreenCanvas
  TCanvasSource = TJOB_JSValue;
  TPrintCallback = procedure (ctx: IJSMozCanvasPrintState) of object;
  TBlobCallback = procedure (blob: IJSBlob) of object;
  // Union of OffscreenCanvasRenderingContext2D, ImageBitmapRenderingContext, WebGLRenderingContext, WebGL2RenderingContext, GPUCanvasContext
  TOffscreenRenderingContext = TJOB_JSValue;
  // Union of BufferSource, Blob, USVString
  TBlobPart = TJOB_JSValue;
  TFileSystemEntryCallback = procedure (entry: IJSFileSystemEntry) of object;
  // Union of CanvasImageSource, Blob, CanvasRenderingContext2D, ImageData
  TImageBitmapSource = TJOB_JSValue;
  TImagePixelLayout = IJSArray; // array of TJSChannelPixelLayout
  // Union of Blob, Directory, USVString
  TFormDataEntryValue = TJOB_JSValue;

  { --------------------------------------------------------------------
    TJSEventListenerOptions
    --------------------------------------------------------------------}

  TJSEventListenerOptionsRec = record
    capture: Boolean;
    mozSystemGroup: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSAddEventListenerOptions
    --------------------------------------------------------------------}

  TJSAddEventListenerOptionsRec = record
    passive: Boolean;
    once: Boolean;
    wantUntrusted: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSGetRootNodeOptions
    --------------------------------------------------------------------}

  TJSGetRootNodeOptionsRec = record
    composed: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSElementCreationOptions
    --------------------------------------------------------------------}

  TJSElementCreationOptionsRec = record
    is_: UnicodeString;
    pseudo: UnicodeString;
  end;

  { --------------------------------------------------------------------
    TJSBlockParsingOptions
    --------------------------------------------------------------------}

  TJSBlockParsingOptionsRec = record
    blockScriptCreated: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSWireframeTaggedRect
    --------------------------------------------------------------------}

  TJSWireframeTaggedRectRec = record
    x: Double;
    y: Double;
    width: Double;
    height: Double;
    color: LongWord;
    type_: TWireframeRectType;
    node: TJSNode;
  end;

  { --------------------------------------------------------------------
    TJSWireframe
    --------------------------------------------------------------------}

  TJSWireframeTaggedRectDynArray = IJSArray; // array of TJSWireframeTaggedRect
  TJSWireframeRec = record
    canvasBackground: LongWord;
    rects: TJSWireframeTaggedRectDynArray;
    version: LongWord;
  end;

  { --------------------------------------------------------------------
    TJSFocusOptions
    --------------------------------------------------------------------}

  TJSFocusOptionsRec = record
    preventScroll: Boolean;
    preventFocusRing: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSScrollIntoViewOptions
    --------------------------------------------------------------------}

  TJSScrollIntoViewOptionsRec = record
    block: TScrollLogicalPosition;
    inline_: TScrollLogicalPosition;
  end;

  { --------------------------------------------------------------------
    TJSShadowRootInit
    --------------------------------------------------------------------}

  TJSShadowRootInitRec = record
    delegatesFocus: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSSetHTMLOptions
    --------------------------------------------------------------------}

  TJSSetHTMLOptionsRec = record
  end;

  { --------------------------------------------------------------------
    TJSScrollOptions
    --------------------------------------------------------------------}

  TJSScrollOptionsRec = record
    behavior: TScrollBehavior;
  end;

  { --------------------------------------------------------------------
    TJSScrollToOptions
    --------------------------------------------------------------------}

  TJSScrollToOptionsRec = record
    left: Double;
    top: Double;
  end;

  { --------------------------------------------------------------------
    TJSIdleRequestOptions
    --------------------------------------------------------------------}

  TJSIdleRequestOptionsRec = record
    timeout: LongWord;
  end;

  { --------------------------------------------------------------------
    TJSWindowPostMessageOptions
    --------------------------------------------------------------------}

  TJSWindowPostMessageOptionsRec = record
    targetOrigin: UnicodeString;
  end;

  { --------------------------------------------------------------------
    TJSTouchInit
    --------------------------------------------------------------------}

  TJSTouchInitRec = record
    identifier: Integer;
    target: TJSEventTarget;
    clientX: Integer;
    clientY: Integer;
    screenX: Integer;
    screenY: Integer;
    pageX: Integer;
    pageY: Integer;
    radiusX: Single;
    radiusY: Single;
    rotationAngle: Single;
    force: Single;
  end;

  { --------------------------------------------------------------------
    TJSElementDefinitionOptions
    --------------------------------------------------------------------}

  TJSElementDefinitionOptionsRec = record
    extends: UnicodeString;
  end;

  { --------------------------------------------------------------------
    TJSLifecycleCallbacks
    --------------------------------------------------------------------}

  TJSLifecycleCallbacksRec = record
    connectedCallback: TLifecycleConnectedCallback;
    disconnectedCallback: TLifecycleDisconnectedCallback;
    adoptedCallback: TLifecycleAdoptedCallback;
    attributeChangedCallback: TLifecycleAttributeChangedCallback;
    formResetCallback: TLifecycleFormResetCallback;
    formDisabledCallback: TLifecycleFormDisabledCallback;
  end;

  { --------------------------------------------------------------------
    TJSShareData
    --------------------------------------------------------------------}

  TJSShareDataRec = record
    title: UnicodeString;
    text: UnicodeString;
    url: UnicodeString;
  end;

  { --------------------------------------------------------------------
    TJSClipboardItemOptions
    --------------------------------------------------------------------}

  TJSClipboardItemOptionsRec = record
    presentationStyle: TPresentationStyle;
  end;

  { --------------------------------------------------------------------
    TJSClientRectsAndTexts
    --------------------------------------------------------------------}

  TUnicodeStringDynArray = IJSArray; // array of UnicodeString
  TJSClientRectsAndTextsRec = record
    rectList: TJSDOMRectList;
    textList: TUnicodeStringDynArray;
  end;

  { --------------------------------------------------------------------
    TJSDOMRectInit
    --------------------------------------------------------------------}

  TJSDOMRectInitRec = record
    x: Double;
    y: Double;
    width: Double;
    height: Double;
  end;

  { --------------------------------------------------------------------
    TJSValidityStateFlags
    --------------------------------------------------------------------}

  TJSValidityStateFlagsRec = record
    valueMissing: Boolean;
    typeMismatch: Boolean;
    patternMismatch: Boolean;
    tooLong: Boolean;
    tooShort: Boolean;
    rangeUnderflow: Boolean;
    rangeOverflow: Boolean;
    stepMismatch: Boolean;
    badInput: Boolean;
    customError: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSDOMMatrix2DInit
    --------------------------------------------------------------------}

  TJSDOMMatrix2DInitRec = record
    a: Double;
    b: Double;
    c: Double;
    d: Double;
    e: Double;
    f: Double;
    m11: Double;
    m12: Double;
    m21: Double;
    m22: Double;
    m41: Double;
    m42: Double;
  end;

  { --------------------------------------------------------------------
    TJSDOMMatrixInit
    --------------------------------------------------------------------}

  TJSDOMMatrixInitRec = record
    m13: Double;
    m14: Double;
    m23: Double;
    m24: Double;
    m31: Double;
    m32: Double;
    m33: Double;
    m34: Double;
    m43: Double;
    m44: Double;
    is2D: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSCSSStyleSheetInit
    --------------------------------------------------------------------}

  TJSCSSStyleSheetInitRec = record
    media: TJOB_JSValue;
    disabled: Boolean;
    baseURL: UTF8String;
  end;

  { --------------------------------------------------------------------
    TJSDOMPointInit
    --------------------------------------------------------------------}

  TJSDOMPointInitRec = record
    x: Double;
    y: Double;
    z: Double;
    w: Double;
  end;

  { --------------------------------------------------------------------
    TJSSVGBoundingBoxOptions
    --------------------------------------------------------------------}

  TJSSVGBoundingBoxOptionsRec = record
    fill: Boolean;
    stroke: Boolean;
    markers: Boolean;
    clipped: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSDateTimeValue
    --------------------------------------------------------------------}

  TJSDateTimeValueRec = record
    hour: Integer;
    minute: Integer;
    year: Integer;
    month: Integer;
    day: Integer;
  end;

  { --------------------------------------------------------------------
    TJSImageEncodeOptions
    --------------------------------------------------------------------}

  TJSImageEncodeOptionsRec = record
    type_: UnicodeString;
    quality: Double;
  end;

  { --------------------------------------------------------------------
    TJSBlobPropertyBag
    --------------------------------------------------------------------}

  TJSBlobPropertyBagRec = record
    type_: UnicodeString;
    endings: TEndingType;
  end;

  { --------------------------------------------------------------------
    TJSFilePropertyBag
    --------------------------------------------------------------------}

  TJSFilePropertyBagRec = record
    lastModified: Int64;
  end;

  { --------------------------------------------------------------------
    TJSChromeFilePropertyBag
    --------------------------------------------------------------------}

  TJSChromeFilePropertyBagRec = record
    name: UnicodeString;
    existenceCheck: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSAutocompleteInfo
    --------------------------------------------------------------------}

  TJSAutocompleteInfoRec = record
    section: UnicodeString;
    addressType: UnicodeString;
    contactType: UnicodeString;
    fieldName: UnicodeString;
    canAutomaticallyPersist: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSFileSystemFlags
    --------------------------------------------------------------------}

  TJSFileSystemFlagsRec = record
    create: Boolean;
    exclusive: Boolean;
  end;

  { --------------------------------------------------------------------
    TJSChannelPixelLayout
    --------------------------------------------------------------------}

  TJSChannelPixelLayoutRec = record
    offset: LongWord;
    width: LongWord;
    height: LongWord;
    dataType: TChannelPixelLayoutDataType;
    stride: LongWord;
    skip: LongWord;
  end;

  { --------------------------------------------------------------------
    TJSImageBitmapOptions
    --------------------------------------------------------------------}

  TJSImageBitmapOptionsRec = record
    imageOrientation: TImageOrientation;
    premultiplyAlpha: TPremultiplyAlpha;
    colorSpaceConversion: TColorSpaceConversion;
    resizeWidth: LongWord;
    resizeHeight: LongWord;
  end;

  { --------------------------------------------------------------------
    TJSEventTarget
    --------------------------------------------------------------------}

  IJSEventTarget = interface(IJSObject)
    ['{213070F7-E432-3F58-9D50-83348F15F73F}']
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSAddEventListenerOptions; aWantsUntrusted: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean; aWantsUntrusted: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSAddEventListenerOptions); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSEventListenerOptions); overload;
    function dispatchEvent(aEvent: IJSEvent): Boolean;
    procedure setEventHandler(const aType_: UnicodeString; const aHandler: TEventHandler);
  end;

  TJSEventTarget = class(TJSObject,IJSEventTarget)
  Private
  Public
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSAddEventListenerOptions; aWantsUntrusted: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean; aWantsUntrusted: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSAddEventListenerOptions); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSEventListenerOptions); overload;
    function dispatchEvent(aEvent: IJSEvent): Boolean;
    procedure setEventHandler(const aType_: UnicodeString; const aHandler: TEventHandler);
    class function Cast(Intf: IJSObject): IJSEventTarget;
  end;

  { --------------------------------------------------------------------
    TJSEvent
    --------------------------------------------------------------------}

  TJSEventTargetDynArray = IJSArray; // array of TJSEventTarget

  IJSEvent = interface(IJSObject)
    ['{6E09FB36-4F97-35C4-8985-9F112D1FBA79}']
    function _Gettype_: UnicodeString;
    function _Gettarget: IJSEventTarget;
    function _GetsrcElement: IJSEventTarget;
    function _GetcurrentTarget: IJSEventTarget;
    function _GeteventPhase: Word;
    function _GetcancelBubble: Boolean;
    function _Getbubbles: Boolean;
    function _Getcancelable: Boolean;
    function _GetreturnValue: Boolean;
    function _GetdefaultPrevented: Boolean;
    function _Getcomposed: Boolean;
    procedure _SetcancelBubble(const aValue: Boolean);
    procedure _SetreturnValue(const aValue: Boolean);
    function composedPath: TJSEventTargetDynArray;
    procedure stopPropagation;
    procedure stopImmediatePropagation;
    procedure preventDefault;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean; aCancelable: Boolean); overload;
    procedure initEvent(const aType_: UnicodeString); overload;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean); overload;
    property type_: UnicodeString read _Gettype_;
    property target: IJSEventTarget read _Gettarget;
    property srcElement: IJSEventTarget read _GetsrcElement;
    property currentTarget: IJSEventTarget read _GetcurrentTarget;
    property eventPhase: Word read _GeteventPhase;
    property cancelBubble: Boolean read _GetcancelBubble write _SetcancelBubble;
    property bubbles: Boolean read _Getbubbles;
    property cancelable: Boolean read _Getcancelable;
    property returnValue: Boolean read _GetreturnValue write _SetreturnValue;
    property defaultPrevented: Boolean read _GetdefaultPrevented;
    property composed: Boolean read _Getcomposed;
  end;

  TJSEvent = class(TJSObject,IJSEvent)
  Private
    function _Gettype_: UnicodeString;
    function _Gettarget: IJSEventTarget;
    function _GetsrcElement: IJSEventTarget;
    function _GetcurrentTarget: IJSEventTarget;
    function _GeteventPhase: Word;
    function _GetcancelBubble: Boolean;
    function _Getbubbles: Boolean;
    function _Getcancelable: Boolean;
    function _GetreturnValue: Boolean;
    function _GetdefaultPrevented: Boolean;
    function _Getcomposed: Boolean;
    procedure _SetcancelBubble(const aValue: Boolean);
    procedure _SetreturnValue(const aValue: Boolean);
  Public
    Const
      NONE = 0;
      CAPTURING_PHASE = 1;
      AT_TARGET = 2;
      BUBBLING_PHASE = 3;
  Public
    function composedPath: TJSEventTargetDynArray;
    procedure stopPropagation;
    procedure stopImmediatePropagation;
    procedure preventDefault;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean; aCancelable: Boolean); overload;
    procedure initEvent(const aType_: UnicodeString); overload;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean); overload;
    class function Cast(Intf: IJSObject): IJSEvent;
    property type_: UnicodeString read _Gettype_;
    property target: IJSEventTarget read _Gettarget;
    property srcElement: IJSEventTarget read _GetsrcElement;
    property currentTarget: IJSEventTarget read _GetcurrentTarget;
    property eventPhase: Word read _GeteventPhase;
    property cancelBubble: Boolean read _GetcancelBubble write _SetcancelBubble;
    property bubbles: Boolean read _Getbubbles;
    property cancelable: Boolean read _Getcancelable;
    property returnValue: Boolean read _GetreturnValue write _SetreturnValue;
    property defaultPrevented: Boolean read _GetdefaultPrevented;
    property composed: Boolean read _Getcomposed;
  end;

  { --------------------------------------------------------------------
    TJSGlobalEventHandlers
    --------------------------------------------------------------------}

  IJSGlobalEventHandlers = interface(IJSObject)
    ['{9093C593-24B5-3F97-AE30-A16B1665E64C}']
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
  end;

  TJSGlobalEventHandlers = class(TJSObject,IJSGlobalEventHandlers)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSGlobalEventHandlers;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
  end;

  { --------------------------------------------------------------------
    TJSWindowEventHandlers
    --------------------------------------------------------------------}

  IJSWindowEventHandlers = interface(IJSObject)
    ['{C5F51AA0-4D09-32C0-8D4C-DF91FD075298}']
    // property onafterprint: TEventHandler read _Getonafterprint write _Setonafterprint;
    // property onbeforeprint: TEventHandler read _Getonbeforeprint write _Setonbeforeprint;
    // property onbeforeunload: TOnBeforeUnloadEventHandler read _Getonbeforeunload write _Setonbeforeunload;
    // property onhashchange: TEventHandler read _Getonhashchange write _Setonhashchange;
    // property onlanguagechange: TEventHandler read _Getonlanguagechange write _Setonlanguagechange;
    // property onmessage: TEventHandler read _Getonmessage write _Setonmessage;
    // property onmessageerror: TEventHandler read _Getonmessageerror write _Setonmessageerror;
    // property onoffline: TEventHandler read _Getonoffline write _Setonoffline;
    // property ononline: TEventHandler read _Getononline write _Setononline;
    // property onpagehide: TEventHandler read _Getonpagehide write _Setonpagehide;
    // property onpageshow: TEventHandler read _Getonpageshow write _Setonpageshow;
    // property onpopstate: TEventHandler read _Getonpopstate write _Setonpopstate;
    // property onrejectionhandled: TEventHandler read _Getonrejectionhandled write _Setonrejectionhandled;
    // property onstorage: TEventHandler read _Getonstorage write _Setonstorage;
    // property onunhandledrejection: TEventHandler read _Getonunhandledrejection write _Setonunhandledrejection;
    // property onunload: TEventHandler read _Getonunload write _Setonunload;
    // property ongamepadconnected: TEventHandler read _Getongamepadconnected write _Setongamepadconnected;
    // property ongamepaddisconnected: TEventHandler read _Getongamepaddisconnected write _Setongamepaddisconnected;
  end;

  TJSWindowEventHandlers = class(TJSObject,IJSWindowEventHandlers)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSWindowEventHandlers;
    // property onafterprint: TEventHandler read _Getonafterprint write _Setonafterprint;
    // property onbeforeprint: TEventHandler read _Getonbeforeprint write _Setonbeforeprint;
    // property onbeforeunload: TOnBeforeUnloadEventHandler read _Getonbeforeunload write _Setonbeforeunload;
    // property onhashchange: TEventHandler read _Getonhashchange write _Setonhashchange;
    // property onlanguagechange: TEventHandler read _Getonlanguagechange write _Setonlanguagechange;
    // property onmessage: TEventHandler read _Getonmessage write _Setonmessage;
    // property onmessageerror: TEventHandler read _Getonmessageerror write _Setonmessageerror;
    // property onoffline: TEventHandler read _Getonoffline write _Setonoffline;
    // property ononline: TEventHandler read _Getononline write _Setononline;
    // property onpagehide: TEventHandler read _Getonpagehide write _Setonpagehide;
    // property onpageshow: TEventHandler read _Getonpageshow write _Setonpageshow;
    // property onpopstate: TEventHandler read _Getonpopstate write _Setonpopstate;
    // property onrejectionhandled: TEventHandler read _Getonrejectionhandled write _Setonrejectionhandled;
    // property onstorage: TEventHandler read _Getonstorage write _Setonstorage;
    // property onunhandledrejection: TEventHandler read _Getonunhandledrejection write _Setonunhandledrejection;
    // property onunload: TEventHandler read _Getonunload write _Setonunload;
    // property ongamepadconnected: TEventHandler read _Getongamepadconnected write _Setongamepadconnected;
    // property ongamepaddisconnected: TEventHandler read _Getongamepaddisconnected write _Setongamepaddisconnected;
  end;

  { --------------------------------------------------------------------
    TJSDocumentAndElementEventHandlers
    --------------------------------------------------------------------}

  IJSDocumentAndElementEventHandlers = interface(IJSObject)
    ['{48324969-C686-38A8-8C7E-97A6E1EAA4AE}']
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
  end;

  TJSDocumentAndElementEventHandlers = class(TJSObject,IJSDocumentAndElementEventHandlers)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSDocumentAndElementEventHandlers;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
  end;

  { --------------------------------------------------------------------
    TJSOnErrorEventHandlerForNodes
    --------------------------------------------------------------------}

  IJSOnErrorEventHandlerForNodes = interface(IJSObject)
    ['{13D166E9-F70F-347B-AD4A-D12FD6CD69D0}']
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;

  TJSOnErrorEventHandlerForNodes = class(TJSObject,IJSOnErrorEventHandlerForNodes)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSOnErrorEventHandlerForNodes;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;

  { --------------------------------------------------------------------
    TJSOnErrorEventHandlerForWindow
    --------------------------------------------------------------------}

  IJSOnErrorEventHandlerForWindow = interface(IJSObject)
    ['{8A74D988-13C5-35AE-A088-38C9F644AB11}']
    // property onerror: TOnErrorEventHandler read _Getonerror write _Setonerror;
  end;

  TJSOnErrorEventHandlerForWindow = class(TJSObject,IJSOnErrorEventHandlerForWindow)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSOnErrorEventHandlerForWindow;
    // property onerror: TOnErrorEventHandler read _Getonerror write _Setonerror;
  end;

  { --------------------------------------------------------------------
    TJSContentSecurityPolicy
    --------------------------------------------------------------------}

  IJSContentSecurityPolicy = interface(IJSObject)
    ['{E0F14358-2D99-30AE-B53A-19677BFCD90F}']
  end;

  TJSContentSecurityPolicy = class(TJSObject,IJSContentSecurityPolicy)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSContentSecurityPolicy;
  end;

  { --------------------------------------------------------------------
    TJSPrincipal
    --------------------------------------------------------------------}

  IJSPrincipal = interface(IJSObject)
    ['{33824E56-58B9-38D0-B869-8DD15390C552}']
  end;

  TJSPrincipal = class(TJSObject,IJSPrincipal)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSPrincipal;
  end;

  { --------------------------------------------------------------------
    TJSWindowProxy
    --------------------------------------------------------------------}

  IJSWindowProxy = interface(IJSObject)
    ['{EEE0FA86-59DA-3E5C-A83D-1A2372542131}']
  end;

  TJSWindowProxy = class(TJSObject,IJSWindowProxy)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSWindowProxy;
  end;

  { --------------------------------------------------------------------
    TJSnsISupports
    --------------------------------------------------------------------}

  IJSnsISupports = interface(IJSObject)
    ['{EEE16FF0-3EAA-398A-903D-1A2372542131}']
  end;

  TJSnsISupports = class(TJSObject,IJSnsISupports)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsISupports;
  end;

  { --------------------------------------------------------------------
    TJSURI
    --------------------------------------------------------------------}

  IJSURI = interface(IJSObject)
    ['{677FA928-3D1A-3372-9421-3194B5554269}']
  end;

  TJSURI = class(TJSObject,IJSURI)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSURI;
  end;

  { --------------------------------------------------------------------
    TJSnsIDocShell
    --------------------------------------------------------------------}

  IJSnsIDocShell = interface(IJSObject)
    ['{EEE16FEE-5839-3BB6-B43D-1A2372542131}']
  end;

  TJSnsIDocShell = class(TJSObject,IJSnsIDocShell)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIDocShell;
  end;

  { --------------------------------------------------------------------
    TJSnsILoadGroup
    --------------------------------------------------------------------}

  IJSnsILoadGroup = interface(IJSObject)
    ['{E6D0CC5C-1971-3F00-A220-F0D81AA2A621}']
  end;

  TJSnsILoadGroup = class(TJSObject,IJSnsILoadGroup)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsILoadGroup;
  end;

  { --------------------------------------------------------------------
    TJSnsIReferrerInfo
    --------------------------------------------------------------------}

  IJSnsIReferrerInfo = interface(IJSObject)
    ['{AC6D32F7-0AAD-30F2-A5E1-4B03E0B13444}']
  end;

  TJSnsIReferrerInfo = class(TJSObject,IJSnsIReferrerInfo)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIReferrerInfo;
  end;

  { --------------------------------------------------------------------
    TJSnsICookieJarSettings
    --------------------------------------------------------------------}

  IJSnsICookieJarSettings = interface(IJSObject)
    ['{3346BB52-F01F-3C27-81BD-9658712A80F0}']
  end;

  TJSnsICookieJarSettings = class(TJSObject,IJSnsICookieJarSettings)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsICookieJarSettings;
  end;

  { --------------------------------------------------------------------
    TJSnsIPermissionDelegateHandler
    --------------------------------------------------------------------}

  IJSnsIPermissionDelegateHandler = interface(IJSObject)
    ['{E64C3B0E-5583-330B-976E-C5C335C10FA8}']
  end;

  TJSnsIPermissionDelegateHandler = class(TJSObject,IJSnsIPermissionDelegateHandler)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIPermissionDelegateHandler;
  end;

  { --------------------------------------------------------------------
    TJSXULCommandDispatcher
    --------------------------------------------------------------------}

  IJSXULCommandDispatcher = interface(IJSObject)
    ['{33454C5E-F00F-3CC4-8CBE-E8980E2260F0}']
  end;

  TJSXULCommandDispatcher = class(TJSObject,IJSXULCommandDispatcher)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSXULCommandDispatcher;
  end;

  { --------------------------------------------------------------------
    TJSXPathExpression
    --------------------------------------------------------------------}

  IJSXPathExpression = interface(IJSObject)
    ['{AC6DC1DB-4DAB-3CF2-AE1C-93C3E0B13444}']
  end;

  TJSXPathExpression = class(TJSObject,IJSXPathExpression)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSXPathExpression;
  end;

  { --------------------------------------------------------------------
    TJSParentNode
    --------------------------------------------------------------------}

  IJSParentNode = interface(IJSObject)
    ['{A5F2D389-B6DF-30CC-9F4A-FAE334A57A22}']
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
  end;

  TJSParentNode = class(TJSObject,IJSParentNode)
  Private
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
  Public
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    class function Cast(Intf: IJSObject): IJSParentNode;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
  end;

  { --------------------------------------------------------------------
    TJSFontFaceSource
    --------------------------------------------------------------------}

  IJSFontFaceSource = interface(IJSObject)
    ['{A28DC0B0-71AB-3181-8B03-307C3446E5A9}']
  end;

  TJSFontFaceSource = class(TJSObject,IJSFontFaceSource)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSFontFaceSource;
  end;

  { --------------------------------------------------------------------
    TJSDocumentOrShadowRoot
    --------------------------------------------------------------------}

  TJSElementDynArray = IJSArray; // array of TJSElement
  TJSNodeDynArray = IJSArray; // array of TJSNode

  IJSDocumentOrShadowRoot = interface(IJSObject)
    ['{4EC8ACF6-4EAC-3911-912E-C978DD981EDD}']
    function _GetactiveElement: IJSElement;
    function _GetpointerLockElement: IJSElement;
    function _GetfullscreenElement: IJSElement;
    function _GetmozFullScreenElement: IJSElement;
    function elementFromPoint(aX: Single; aY: Single): IJSElement;
    function elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
    function nodeFromPoint(aX: Single; aY: Single): IJSNode;
    function nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
    property activeElement: IJSElement read _GetactiveElement;
    property pointerLockElement: IJSElement read _GetpointerLockElement;
    property fullscreenElement: IJSElement read _GetfullscreenElement;
    property mozFullScreenElement: IJSElement read _GetmozFullScreenElement;
  end;

  TJSDocumentOrShadowRoot = class(TJSObject,IJSDocumentOrShadowRoot)
  Private
    function _GetactiveElement: IJSElement;
    function _GetpointerLockElement: IJSElement;
    function _GetfullscreenElement: IJSElement;
    function _GetmozFullScreenElement: IJSElement;
  Public
    function elementFromPoint(aX: Single; aY: Single): IJSElement;
    function elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
    function nodeFromPoint(aX: Single; aY: Single): IJSNode;
    function nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
    class function Cast(Intf: IJSObject): IJSDocumentOrShadowRoot;
    property activeElement: IJSElement read _GetactiveElement;
    property pointerLockElement: IJSElement read _GetpointerLockElement;
    property fullscreenElement: IJSElement read _GetfullscreenElement;
    property mozFullScreenElement: IJSElement read _GetmozFullScreenElement;
  end;

  { --------------------------------------------------------------------
    TJSDOMImplementation
    --------------------------------------------------------------------}

  IJSDOMImplementation = interface(IJSObject)
    ['{3D38DA1F-BC10-3889-894E-706A3B0086C7}']
    function hasFeature: Boolean;
    function createDocumentType(const aQualifiedName: UnicodeString; const aPublicId: UnicodeString; const aSystemId: UnicodeString): IJSDocumentType;
    function createDocument(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; aDoctype: IJSDocumentType): IJSDocument; overload;
    function createDocument(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString): IJSDocument; overload;
    function createHTMLDocument(const aTitle: UnicodeString): IJSDocument; overload;
    function createHTMLDocument: IJSDocument; overload;
  end;

  TJSDOMImplementation = class(TJSObject,IJSDOMImplementation)
  Private
  Public
    function hasFeature: Boolean;
    function createDocumentType(const aQualifiedName: UnicodeString; const aPublicId: UnicodeString; const aSystemId: UnicodeString): IJSDocumentType;
    function createDocument(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; aDoctype: IJSDocumentType): IJSDocument; overload;
    function createDocument(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString): IJSDocument; overload;
    function createHTMLDocument(const aTitle: UnicodeString): IJSDocument; overload;
    function createHTMLDocument: IJSDocument; overload;
    class function Cast(Intf: IJSObject): IJSDOMImplementation;
  end;

  { --------------------------------------------------------------------
    TJSChildNode
    --------------------------------------------------------------------}

  IJSChildNode = interface(IJSObject)
    ['{BACC9ED2-6A16-35B6-BEDC-EB2379E2F83C}']
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
  end;

  TJSChildNode = class(TJSObject,IJSChildNode)
  Private
  Public
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    class function Cast(Intf: IJSObject): IJSChildNode;
  end;

  { --------------------------------------------------------------------
    TJSNonDocumentTypeChildNode
    --------------------------------------------------------------------}

  IJSNonDocumentTypeChildNode = interface(IJSObject)
    ['{85DEDD4B-08D5-3A9F-93A6-CC9EB13C1ECF}']
    function _GetpreviousElementSibling: IJSElement;
    function _GetnextElementSibling: IJSElement;
    property previousElementSibling: IJSElement read _GetpreviousElementSibling;
    property nextElementSibling: IJSElement read _GetnextElementSibling;
  end;

  TJSNonDocumentTypeChildNode = class(TJSObject,IJSNonDocumentTypeChildNode)
  Private
    function _GetpreviousElementSibling: IJSElement;
    function _GetnextElementSibling: IJSElement;
  Public
    class function Cast(Intf: IJSObject): IJSNonDocumentTypeChildNode;
    property previousElementSibling: IJSElement read _GetpreviousElementSibling;
    property nextElementSibling: IJSElement read _GetnextElementSibling;
  end;

  { --------------------------------------------------------------------
    TJSnsIScreen
    --------------------------------------------------------------------}

  IJSnsIScreen = interface(IJSObject)
    ['{33832E53-F8FD-30F0-B869-8DD15390C552}']
  end;

  TJSnsIScreen = class(TJSObject,IJSnsIScreen)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIScreen;
  end;

  { --------------------------------------------------------------------
    TJSHTMLOrForeignElement
    --------------------------------------------------------------------}

  IJSHTMLOrForeignElement = interface(IJSObject)
    ['{0F23A427-F29B-30D0-879C-44F48A62D4F7}']
    function _Getdataset: IJSDOMStringMap;
    function _GettabIndex: Integer;
    procedure _SettabIndex(const aValue: Integer);
    procedure focus(const aOptions: TJSFocusOptions); overload;
    procedure focus; overload;
    procedure blur;
    property dataset: IJSDOMStringMap read _Getdataset;
    property tabIndex: Integer read _GettabIndex write _SettabIndex;
  end;

  TJSHTMLOrForeignElement = class(TJSObject,IJSHTMLOrForeignElement)
  Private
    function _Getdataset: IJSDOMStringMap;
    function _GettabIndex: Integer;
    procedure _SettabIndex(const aValue: Integer);
  Public
    procedure focus(const aOptions: TJSFocusOptions); overload;
    procedure focus; overload;
    procedure blur;
    class function Cast(Intf: IJSObject): IJSHTMLOrForeignElement;
    property dataset: IJSDOMStringMap read _Getdataset;
    property tabIndex: Integer read _GettabIndex write _SettabIndex;
  end;

  { --------------------------------------------------------------------
    TJSElementCSSInlineStyle
    --------------------------------------------------------------------}

  IJSElementCSSInlineStyle = interface(IJSObject)
    ['{A68CF718-98F3-3A7F-BD8B-3F0C52623AE6}']
    function _Getstyle: IJSCSSStyleDeclaration;
    property style: IJSCSSStyleDeclaration read _Getstyle;
  end;

  TJSElementCSSInlineStyle = class(TJSObject,IJSElementCSSInlineStyle)
  Private
    function _Getstyle: IJSCSSStyleDeclaration;
  Public
    class function Cast(Intf: IJSObject): IJSElementCSSInlineStyle;
    property style: IJSCSSStyleDeclaration read _Getstyle;
  end;

  { --------------------------------------------------------------------
    TJSHTMLCollection
    --------------------------------------------------------------------}

  IJSHTMLCollection = interface(IJSObject)
    ['{167FC231-E447-3789-AA0A-B64B7A3229FE}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): IJSElement;
    function namedItem(const aName: UnicodeString): IJSElement;
    property length_: LongWord read _Getlength_;
  end;

  TJSHTMLCollection = class(TJSObject,IJSHTMLCollection)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): IJSElement;
    function namedItem(const aName: UnicodeString): IJSElement;
    class function Cast(Intf: IJSObject): IJSHTMLCollection;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSLocation
    --------------------------------------------------------------------}

  IJSLocation = interface(IJSObject)
    ['{FC64DCEE-D861-35B1-94D7-84CF91BA520D}']
    function _Gethref: UnicodeString;
    function _Getorigin: UnicodeString;
    function _Getprotocol: UnicodeString;
    function _Gethost: UnicodeString;
    function _Gethostname: UnicodeString;
    function _Getport: UnicodeString;
    function _Getpathname: UnicodeString;
    function _Getsearch: UnicodeString;
    function _Gethash: UnicodeString;
    procedure _Sethref(const aValue: UnicodeString);
    procedure _Setprotocol(const aValue: UnicodeString);
    procedure _Sethost(const aValue: UnicodeString);
    procedure _Sethostname(const aValue: UnicodeString);
    procedure _Setport(const aValue: UnicodeString);
    procedure _Setpathname(const aValue: UnicodeString);
    procedure _Setsearch(const aValue: UnicodeString);
    procedure _Sethash(const aValue: UnicodeString);
    procedure assign(const aUrl: UnicodeString);
    procedure replace(const aUrl: UnicodeString);
    procedure reload(aForceget: Boolean); overload;
    procedure reload; overload;
    property href: UnicodeString read _Gethref write _Sethref;
    property origin: UnicodeString read _Getorigin;
    property protocol: UnicodeString read _Getprotocol write _Setprotocol;
    property host: UnicodeString read _Gethost write _Sethost;
    property hostname: UnicodeString read _Gethostname write _Sethostname;
    property port: UnicodeString read _Getport write _Setport;
    property pathname: UnicodeString read _Getpathname write _Setpathname;
    property search: UnicodeString read _Getsearch write _Setsearch;
    property hash: UnicodeString read _Gethash write _Sethash;
  end;

  TJSLocation = class(TJSObject,IJSLocation)
  Private
    function _Gethref: UnicodeString;
    function _Getorigin: UnicodeString;
    function _Getprotocol: UnicodeString;
    function _Gethost: UnicodeString;
    function _Gethostname: UnicodeString;
    function _Getport: UnicodeString;
    function _Getpathname: UnicodeString;
    function _Getsearch: UnicodeString;
    function _Gethash: UnicodeString;
    procedure _Sethref(const aValue: UnicodeString);
    procedure _Setprotocol(const aValue: UnicodeString);
    procedure _Sethost(const aValue: UnicodeString);
    procedure _Sethostname(const aValue: UnicodeString);
    procedure _Setport(const aValue: UnicodeString);
    procedure _Setpathname(const aValue: UnicodeString);
    procedure _Setsearch(const aValue: UnicodeString);
    procedure _Sethash(const aValue: UnicodeString);
  Public
    procedure assign(const aUrl: UnicodeString);
    procedure replace(const aUrl: UnicodeString);
    procedure reload(aForceget: Boolean); overload;
    procedure reload; overload;
    class function Cast(Intf: IJSObject): IJSLocation;
    property href: UnicodeString read _Gethref write _Sethref;
    property origin: UnicodeString read _Getorigin;
    property protocol: UnicodeString read _Getprotocol write _Setprotocol;
    property host: UnicodeString read _Gethost write _Sethost;
    property hostname: UnicodeString read _Gethostname write _Sethostname;
    property port: UnicodeString read _Getport write _Setport;
    property pathname: UnicodeString read _Getpathname write _Setpathname;
    property search: UnicodeString read _Getsearch write _Setsearch;
    property hash: UnicodeString read _Gethash write _Sethash;
  end;

  { --------------------------------------------------------------------
    TJSNodeList
    --------------------------------------------------------------------}

  IJSNodeList = interface(IJSObject)
    ['{20966F1F-96FF-3E7A-9A09-8A3EED5244FC}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): IJSNode;
    property length_: LongWord read _Getlength_;
  end;

  TJSNodeList = class(TJSObject,IJSNodeList)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): IJSNode;
    class function Cast(Intf: IJSObject): IJSNodeList;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSDOMStringList
    --------------------------------------------------------------------}

  IJSDOMStringList = interface(IJSObject)
    ['{D9C88094-BA78-36DA-88C1-DD30586AB521}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): UnicodeString;
    function contains(const aString_: UnicodeString): Boolean;
    property length_: LongWord read _Getlength_;
  end;

  TJSDOMStringList = class(TJSObject,IJSDOMStringList)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): UnicodeString;
    function contains(const aString_: UnicodeString): Boolean;
    class function Cast(Intf: IJSObject): IJSDOMStringList;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSCaretPosition
    --------------------------------------------------------------------}

  IJSCaretPosition = interface(IJSObject)
    ['{2E28B25D-DB44-3336-873C-C97DF3AB0C7C}']
    function _GetoffsetNode: IJSNode;
    function _Getoffset: LongWord;
    function getClientRect: IJSDOMRect;
    property offsetNode: IJSNode read _GetoffsetNode;
    property offset: LongWord read _Getoffset;
  end;

  TJSCaretPosition = class(TJSObject,IJSCaretPosition)
  Private
    function _GetoffsetNode: IJSNode;
    function _Getoffset: LongWord;
  Public
    function getClientRect: IJSDOMRect;
    class function Cast(Intf: IJSObject): IJSCaretPosition;
    property offsetNode: IJSNode read _GetoffsetNode;
    property offset: LongWord read _Getoffset;
  end;

  { --------------------------------------------------------------------
    TJSDOMTokenList
    --------------------------------------------------------------------}

  IJSDOMTokenList = interface(IJSObject)
    ['{5C22C42B-4A6D-340F-BC7C-6D117C60D6F4}']
    function _Getlength_: LongWord;
    function _Getvalue: UnicodeString;
    procedure _Setvalue(const aValue: UnicodeString);
    function item(aIndex: LongWord): UnicodeString;
    function contains(const aToken: UnicodeString): Boolean;
    procedure add(const aTokens: UnicodeString){; ToDo:varargs};
    procedure remove(const aTokens: UnicodeString){; ToDo:varargs};
    function replace(const aToken: UnicodeString; const aNewToken: UnicodeString): Boolean;
    function toggle(const aToken: UnicodeString; aForce: Boolean): Boolean; overload;
    function toggle(const aToken: UnicodeString): Boolean; overload;
    function supports(const aToken: UnicodeString): Boolean;
    property length_: LongWord read _Getlength_;
    property value: UnicodeString read _Getvalue write _Setvalue;
  end;

  TJSDOMTokenList = class(TJSObject,IJSDOMTokenList)
  Private
    function _Getlength_: LongWord;
    function _Getvalue: UnicodeString;
    procedure _Setvalue(const aValue: UnicodeString);
  Public
    function item(aIndex: LongWord): UnicodeString;
    function contains(const aToken: UnicodeString): Boolean;
    procedure add(const aTokens: UnicodeString){; ToDo:varargs};
    procedure remove(const aTokens: UnicodeString){; ToDo:varargs};
    function replace(const aToken: UnicodeString; const aNewToken: UnicodeString): Boolean;
    function toggle(const aToken: UnicodeString; aForce: Boolean): Boolean; overload;
    function toggle(const aToken: UnicodeString): Boolean; overload;
    function supports(const aToken: UnicodeString): Boolean;
    class function Cast(Intf: IJSObject): IJSDOMTokenList;
    property length_: LongWord read _Getlength_;
    property value: UnicodeString read _Getvalue write _Setvalue;
  end;

  { --------------------------------------------------------------------
    TJSNamedNodeMap
    --------------------------------------------------------------------}

  IJSNamedNodeMap = interface(IJSObject)
    ['{B73A6CC2-1C47-3EA1-B002-E888A1E09A83}']
    function _Getlength_: LongWord;
    function getNamedItem(const aName: UnicodeString): IJSAttr;
    function setNamedItem(arg: IJSAttr): IJSAttr;
    function removeNamedItem(const aName: UnicodeString): IJSAttr;
    function item(aIndex: LongWord): IJSAttr;
    function getNamedItemNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
    function setNamedItemNS(arg: IJSAttr): IJSAttr;
    function removeNamedItemNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
    property length_: LongWord read _Getlength_;
  end;

  TJSNamedNodeMap = class(TJSObject,IJSNamedNodeMap)
  Private
    function _Getlength_: LongWord;
  Public
    function getNamedItem(const aName: UnicodeString): IJSAttr;
    function setNamedItem(arg: IJSAttr): IJSAttr;
    function removeNamedItem(const aName: UnicodeString): IJSAttr;
    function item(aIndex: LongWord): IJSAttr;
    function getNamedItemNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
    function setNamedItemNS(arg: IJSAttr): IJSAttr;
    function removeNamedItemNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
    class function Cast(Intf: IJSObject): IJSNamedNodeMap;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSDOMStringMap
    --------------------------------------------------------------------}

  IJSDOMStringMap = interface(IJSObject)
    ['{E6CE1A6D-FFFA-3C3F-9220-F0D81AA2A621}']
  end;

  TJSDOMStringMap = class(TJSObject,IJSDOMStringMap)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSDOMStringMap;
  end;

  { --------------------------------------------------------------------
    TJSCSSStyleDeclaration
    --------------------------------------------------------------------}

  TUTF8StringDynArray = IJSArray; // array of UTF8String

  IJSCSSStyleDeclaration = interface(IJSObject)
    ['{995CF03B-00F3-357A-BBF6-E8DA3D672884}']
    function _GetcssText: UTF8String;
    function _Getlength_: LongWord;
    function _GetparentRule: IJSCSSRule;
    procedure _SetcssText(const aValue: UTF8String);
    function item(aIndex: LongWord): UTF8String;
    function getCSSImageURLs(const aProperty_: UTF8String): TUTF8StringDynArray;
    function getPropertyValue(const aProperty_: UTF8String): UTF8String;
    function getPropertyPriority(const aProperty_: UTF8String): UTF8String;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String; const aPriority: UTF8String); overload;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String); overload;
    function removeProperty(const aProperty_: UTF8String): UTF8String;
    property cssText: UTF8String read _GetcssText write _SetcssText;
    property length_: LongWord read _Getlength_;
    property parentRule: IJSCSSRule read _GetparentRule;
  end;

  TJSCSSStyleDeclaration = class(TJSObject,IJSCSSStyleDeclaration)
  Private
    function _GetcssText: UTF8String;
    function _Getlength_: LongWord;
    function _GetparentRule: IJSCSSRule;
    procedure _SetcssText(const aValue: UTF8String);
  Public
    function item(aIndex: LongWord): UTF8String;
    function getCSSImageURLs(const aProperty_: UTF8String): TUTF8StringDynArray;
    function getPropertyValue(const aProperty_: UTF8String): UTF8String;
    function getPropertyPriority(const aProperty_: UTF8String): UTF8String;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String; const aPriority: UTF8String); overload;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String); overload;
    function removeProperty(const aProperty_: UTF8String): UTF8String;
    class function Cast(Intf: IJSObject): IJSCSSStyleDeclaration;
    property cssText: UTF8String read _GetcssText write _SetcssText;
    property length_: LongWord read _Getlength_;
    property parentRule: IJSCSSRule read _GetparentRule;
  end;

  { --------------------------------------------------------------------
    TJSnsIBrowserDOMWindow
    --------------------------------------------------------------------}

  IJSnsIBrowserDOMWindow = interface(IJSObject)
    ['{7D9E6ECD-E32A-31DE-9B97-DF76A7603D1A}']
  end;

  TJSnsIBrowserDOMWindow = class(TJSObject,IJSnsIBrowserDOMWindow)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIBrowserDOMWindow;
  end;

  { --------------------------------------------------------------------
    TJSXULControllers
    --------------------------------------------------------------------}

  IJSXULControllers = interface(IJSObject)
    ['{A28ED322-7B18-3C41-A572-A07C3446E5A9}']
  end;

  TJSXULControllers = class(TJSObject,IJSXULControllers)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSXULControllers;
  end;

  { --------------------------------------------------------------------
    TJSnsIDOMWindowUtils
    --------------------------------------------------------------------}

  IJSnsIDOMWindowUtils = interface(IJSObject)
    ['{2B7FAA71-2156-3F28-AF6A-0EDF40F8698D}']
  end;

  TJSnsIDOMWindowUtils = class(TJSObject,IJSnsIDOMWindowUtils)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIDOMWindowUtils;
  end;

  { --------------------------------------------------------------------
    TJSnsIPrintSettings
    --------------------------------------------------------------------}

  IJSnsIPrintSettings = interface(IJSObject)
    ['{84BC3276-CC8E-3E16-A193-21A81E0D8EB9}']
  end;

  TJSnsIPrintSettings = class(TJSObject,IJSnsIPrintSettings)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIPrintSettings;
  end;

  { --------------------------------------------------------------------
    TJSWindowSessionStorage
    --------------------------------------------------------------------}

  IJSWindowSessionStorage = interface(IJSObject)
    ['{978F59A5-BA9C-30AF-85E6-7DA480FC721D}']
    function _GetsessionStorage: IJSStorage;
    property sessionStorage: IJSStorage read _GetsessionStorage;
  end;

  TJSWindowSessionStorage = class(TJSObject,IJSWindowSessionStorage)
  Private
    function _GetsessionStorage: IJSStorage;
  Public
    class function Cast(Intf: IJSObject): IJSWindowSessionStorage;
    property sessionStorage: IJSStorage read _GetsessionStorage;
  end;

  { --------------------------------------------------------------------
    TJSWindowLocalStorage
    --------------------------------------------------------------------}

  IJSWindowLocalStorage = interface(IJSObject)
    ['{327EE918-DAC7-33FC-B85D-E16E5FF6F080}']
    function _GetlocalStorage: IJSStorage;
    property localStorage: IJSStorage read _GetlocalStorage;
  end;

  TJSWindowLocalStorage = class(TJSObject,IJSWindowLocalStorage)
  Private
    function _GetlocalStorage: IJSStorage;
  Public
    class function Cast(Intf: IJSObject): IJSWindowLocalStorage;
    property localStorage: IJSStorage read _GetlocalStorage;
  end;

  { --------------------------------------------------------------------
    TJSTouch
    --------------------------------------------------------------------}

  IJSTouch = interface(IJSObject)
    ['{D28EA4F4-D2EB-3DE9-84F8-60CDCF55579F}']
    function _Getidentifier: Integer;
    function _Gettarget: IJSEventTarget;
    function _GetscreenX: Integer;
    function _GetscreenY: Integer;
    function _GetclientX: Integer;
    function _GetclientY: Integer;
    function _GetpageX: Integer;
    function _GetpageY: Integer;
    function _GetradiusX: Integer;
    function _GetradiusY: Integer;
    function _GetrotationAngle: Single;
    function _Getforce: Single;
    property identifier: Integer read _Getidentifier;
    property target: IJSEventTarget read _Gettarget;
    property screenX: Integer read _GetscreenX;
    property screenY: Integer read _GetscreenY;
    property clientX: Integer read _GetclientX;
    property clientY: Integer read _GetclientY;
    property pageX: Integer read _GetpageX;
    property pageY: Integer read _GetpageY;
    property radiusX: Integer read _GetradiusX;
    property radiusY: Integer read _GetradiusY;
    property rotationAngle: Single read _GetrotationAngle;
    property force: Single read _Getforce;
  end;

  TJSTouch = class(TJSObject,IJSTouch)
  Private
    function _Getidentifier: Integer;
    function _Gettarget: IJSEventTarget;
    function _GetscreenX: Integer;
    function _GetscreenY: Integer;
    function _GetclientX: Integer;
    function _GetclientY: Integer;
    function _GetpageX: Integer;
    function _GetpageY: Integer;
    function _GetradiusX: Integer;
    function _GetradiusY: Integer;
    function _GetrotationAngle: Single;
    function _Getforce: Single;
  Public
    class function Cast(Intf: IJSObject): IJSTouch;
    property identifier: Integer read _Getidentifier;
    property target: IJSEventTarget read _Gettarget;
    property screenX: Integer read _GetscreenX;
    property screenY: Integer read _GetscreenY;
    property clientX: Integer read _GetclientX;
    property clientY: Integer read _GetclientY;
    property pageX: Integer read _GetpageX;
    property pageY: Integer read _GetpageY;
    property radiusX: Integer read _GetradiusX;
    property radiusY: Integer read _GetradiusY;
    property rotationAngle: Single read _GetrotationAngle;
    property force: Single read _Getforce;
  end;

  { --------------------------------------------------------------------
    TJSTouchList
    --------------------------------------------------------------------}

  IJSTouchList = interface(IJSObject)
    ['{476CDE25-FDCE-3303-9DDC-6C58FE78A837}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): IJSTouch;
    property length_: LongWord read _Getlength_;
  end;

  TJSTouchList = class(TJSObject,IJSTouchList)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): IJSTouch;
    class function Cast(Intf: IJSObject): IJSTouchList;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSHistory
    --------------------------------------------------------------------}

  IJSHistory = interface(IJSObject)
    ['{D1662DAB-C089-3BB4-B798-0ADFC0728031}']
    function _Getlength_: LongWord;
    function _GetscrollRestoration: TScrollRestoration;
    function _Getstate: TJOB_JSValue;
    procedure _SetscrollRestoration(const aValue: TScrollRestoration);
    procedure go(aDelta: Integer); overload;
    procedure go; overload;
    procedure back;
    procedure forward;
    procedure pushState(aData: TJOB_JSValue; const aTitle: UnicodeString; const aUrl: UnicodeString); overload;
    procedure pushState(aData: TJOB_JSValue; const aTitle: UnicodeString); overload;
    procedure replaceState(aData: TJOB_JSValue; const aTitle: UnicodeString; const aUrl: UnicodeString); overload;
    procedure replaceState(aData: TJOB_JSValue; const aTitle: UnicodeString); overload;
    property length_: LongWord read _Getlength_;
    property scrollRestoration: TScrollRestoration read _GetscrollRestoration write _SetscrollRestoration;
    property state: TJOB_JSValue read _Getstate;
  end;

  TJSHistory = class(TJSObject,IJSHistory)
  Private
    function _Getlength_: LongWord;
    function _GetscrollRestoration: TScrollRestoration;
    function _Getstate: TJOB_JSValue;
    procedure _SetscrollRestoration(const aValue: TScrollRestoration);
  Public
    procedure go(aDelta: Integer); overload;
    procedure go; overload;
    procedure back;
    procedure forward;
    procedure pushState(aData: TJOB_JSValue; const aTitle: UnicodeString; const aUrl: UnicodeString); overload;
    procedure pushState(aData: TJOB_JSValue; const aTitle: UnicodeString); overload;
    procedure replaceState(aData: TJOB_JSValue; const aTitle: UnicodeString; const aUrl: UnicodeString); overload;
    procedure replaceState(aData: TJOB_JSValue; const aTitle: UnicodeString); overload;
    class function Cast(Intf: IJSObject): IJSHistory;
    property length_: LongWord read _Getlength_;
    property scrollRestoration: TScrollRestoration read _GetscrollRestoration write _SetscrollRestoration;
    property state: TJOB_JSValue read _Getstate;
  end;

  { --------------------------------------------------------------------
    TJSCustomElementRegistry
    --------------------------------------------------------------------}

  IJSCustomElementRegistry = interface(IJSObject)
    ['{50551E84-59A5-3976-9ECE-78DF21CD10F1}']
    procedure setElementCreationCallback(const aName: UnicodeString; const aCallback: TCustomElementCreationCallback);
    function get(const aName: UnicodeString): TJOB_JSValue;
    procedure upgrade(aRoot: IJSNode);
  end;

  TJSCustomElementRegistry = class(TJSObject,IJSCustomElementRegistry)
  Private
  Public
    procedure setElementCreationCallback(const aName: UnicodeString; const aCallback: TCustomElementCreationCallback);
    function get(const aName: UnicodeString): TJOB_JSValue;
    procedure upgrade(aRoot: IJSNode);
    class function Cast(Intf: IJSObject): IJSCustomElementRegistry;
  end;

  { --------------------------------------------------------------------
    TJSBarProp
    --------------------------------------------------------------------}

  IJSBarProp = interface(IJSObject)
    ['{BE7FF026-C5F3-3839-B3AD-FCA98AFE6CE5}']
    function _Getvisible: Boolean;
    procedure _Setvisible(const aValue: Boolean);
    property visible: Boolean read _Getvisible write _Setvisible;
  end;

  TJSBarProp = class(TJSObject,IJSBarProp)
  Private
    function _Getvisible: Boolean;
    procedure _Setvisible(const aValue: Boolean);
  Public
    class function Cast(Intf: IJSObject): IJSBarProp;
    property visible: Boolean read _Getvisible write _Setvisible;
  end;

  { --------------------------------------------------------------------
    TJSNavigator
    --------------------------------------------------------------------}

  IJSNavigator = interface(IJSObject)
    ['{EC31AA97-2FBB-3349-8E7A-B2E745FFA21D}']
    function _GetpdfViewerEnabled: Boolean;
    function _GetdoNotTrack: UnicodeString;
    function _GetglobalPrivacyControl: Boolean;
    function _GetmaxTouchPoints: Integer;
    function _Getoscpu: UnicodeString;
    function _Getvendor: UnicodeString;
    function _GetvendorSub: UnicodeString;
    function _GetproductSub: UnicodeString;
    function _GetcookieEnabled: Boolean;
    function _GetbuildID: UnicodeString;
    function _GetisWebVRContentDetected: Boolean;
    function _GetisWebVRContentPresenting: Boolean;
    function _Getclipboard: IJSClipboard;
    function _GettestTrialGatedAttribute: Boolean;
    function _GetappCodeName: UnicodeString;
    function _GetappName: UnicodeString;
    function _GetappVersion: UnicodeString;
    function _Getplatform: UnicodeString;
    function _GetuserAgent: UnicodeString;
    function _Getproduct: UnicodeString;
    function _Getlanguage: UnicodeString;
    function _Getlanguages: TUnicodeStringDynArray;
    function _GetonLine: Boolean;
    function _GethardwareConcurrency: QWord;
    function _Getwebdriver: Boolean;
    function vibrate(aDuration: LongWord): Boolean;
    procedure setVibrationPermission(aPermitted: Boolean; aPersistent: Boolean); overload;
    procedure setVibrationPermission(aPermitted: Boolean); overload;
    function javaEnabled: Boolean;
    function canShare(const aData: TJSShareData): Boolean; overload;
    function canShare: Boolean; overload;
    function taintEnabled: Boolean;
    procedure checkProtocolHandlerAllowed(const aScheme: UnicodeString; aHandlerURI: IJSURI; aDocumentURI: IJSURI);
    procedure registerProtocolHandler(const aScheme: UnicodeString; const aUrl: UnicodeString);
    property pdfViewerEnabled: Boolean read _GetpdfViewerEnabled;
    property doNotTrack: UnicodeString read _GetdoNotTrack;
    property globalPrivacyControl: Boolean read _GetglobalPrivacyControl;
    property maxTouchPoints: Integer read _GetmaxTouchPoints;
    property oscpu: UnicodeString read _Getoscpu;
    property vendor: UnicodeString read _Getvendor;
    property vendorSub: UnicodeString read _GetvendorSub;
    property productSub: UnicodeString read _GetproductSub;
    property cookieEnabled: Boolean read _GetcookieEnabled;
    property buildID: UnicodeString read _GetbuildID;
    property isWebVRContentDetected: Boolean read _GetisWebVRContentDetected;
    property isWebVRContentPresenting: Boolean read _GetisWebVRContentPresenting;
    property clipboard: IJSClipboard read _Getclipboard;
    property testTrialGatedAttribute: Boolean read _GettestTrialGatedAttribute;
    property appCodeName: UnicodeString read _GetappCodeName;
    property appName: UnicodeString read _GetappName;
    property appVersion: UnicodeString read _GetappVersion;
    property platform: UnicodeString read _Getplatform;
    property userAgent: UnicodeString read _GetuserAgent;
    property product: UnicodeString read _Getproduct;
    property language: UnicodeString read _Getlanguage;
    property languages: TUnicodeStringDynArray read _Getlanguages;
    property onLine: Boolean read _GetonLine;
    property hardwareConcurrency: QWord read _GethardwareConcurrency;
    property webdriver: Boolean read _Getwebdriver;
  end;

  TJSNavigator = class(TJSObject,IJSNavigator)
  Private
    function _GetpdfViewerEnabled: Boolean;
    function _GetdoNotTrack: UnicodeString;
    function _GetglobalPrivacyControl: Boolean;
    function _GetmaxTouchPoints: Integer;
    function _Getoscpu: UnicodeString;
    function _Getvendor: UnicodeString;
    function _GetvendorSub: UnicodeString;
    function _GetproductSub: UnicodeString;
    function _GetcookieEnabled: Boolean;
    function _GetbuildID: UnicodeString;
    function _GetisWebVRContentDetected: Boolean;
    function _GetisWebVRContentPresenting: Boolean;
    function _Getclipboard: IJSClipboard;
    function _GettestTrialGatedAttribute: Boolean;
    function _GetappCodeName: UnicodeString;
    function _GetappName: UnicodeString;
    function _GetappVersion: UnicodeString;
    function _Getplatform: UnicodeString;
    function _GetuserAgent: UnicodeString;
    function _Getproduct: UnicodeString;
    function _Getlanguage: UnicodeString;
    function _Getlanguages: TUnicodeStringDynArray;
    function _GetonLine: Boolean;
    function _GethardwareConcurrency: QWord;
    function _Getwebdriver: Boolean;
  Public
    function vibrate(aDuration: LongWord): Boolean;
    procedure setVibrationPermission(aPermitted: Boolean; aPersistent: Boolean); overload;
    procedure setVibrationPermission(aPermitted: Boolean); overload;
    function javaEnabled: Boolean;
    function canShare(const aData: TJSShareData): Boolean; overload;
    function canShare: Boolean; overload;
    function taintEnabled: Boolean;
    procedure checkProtocolHandlerAllowed(const aScheme: UnicodeString; aHandlerURI: IJSURI; aDocumentURI: IJSURI);
    procedure registerProtocolHandler(const aScheme: UnicodeString; const aUrl: UnicodeString);
    class function Cast(Intf: IJSObject): IJSNavigator;
    property pdfViewerEnabled: Boolean read _GetpdfViewerEnabled;
    property doNotTrack: UnicodeString read _GetdoNotTrack;
    property globalPrivacyControl: Boolean read _GetglobalPrivacyControl;
    property maxTouchPoints: Integer read _GetmaxTouchPoints;
    property oscpu: UnicodeString read _Getoscpu;
    property vendor: UnicodeString read _Getvendor;
    property vendorSub: UnicodeString read _GetvendorSub;
    property productSub: UnicodeString read _GetproductSub;
    property cookieEnabled: Boolean read _GetcookieEnabled;
    property buildID: UnicodeString read _GetbuildID;
    property isWebVRContentDetected: Boolean read _GetisWebVRContentDetected;
    property isWebVRContentPresenting: Boolean read _GetisWebVRContentPresenting;
    property clipboard: IJSClipboard read _Getclipboard;
    property testTrialGatedAttribute: Boolean read _GettestTrialGatedAttribute;
    property appCodeName: UnicodeString read _GetappCodeName;
    property appName: UnicodeString read _GetappName;
    property appVersion: UnicodeString read _GetappVersion;
    property platform: UnicodeString read _Getplatform;
    property userAgent: UnicodeString read _GetuserAgent;
    property product: UnicodeString read _Getproduct;
    property language: UnicodeString read _Getlanguage;
    property languages: TUnicodeStringDynArray read _Getlanguages;
    property onLine: Boolean read _GetonLine;
    property hardwareConcurrency: QWord read _GethardwareConcurrency;
    property webdriver: Boolean read _Getwebdriver;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorID
    --------------------------------------------------------------------}

  IJSNavigatorID = interface(IJSObject)
    ['{A467BFDD-3501-3C2A-8713-CFBA29723AC1}']
    function _GetappCodeName: UnicodeString;
    function _GetappName: UnicodeString;
    function _GetappVersion: UnicodeString;
    function _Getplatform: UnicodeString;
    function _GetuserAgent: UnicodeString;
    function _Getproduct: UnicodeString;
    function taintEnabled: Boolean;
    property appCodeName: UnicodeString read _GetappCodeName;
    property appName: UnicodeString read _GetappName;
    property appVersion: UnicodeString read _GetappVersion;
    property platform: UnicodeString read _Getplatform;
    property userAgent: UnicodeString read _GetuserAgent;
    property product: UnicodeString read _Getproduct;
  end;

  TJSNavigatorID = class(TJSObject,IJSNavigatorID)
  Private
    function _GetappCodeName: UnicodeString;
    function _GetappName: UnicodeString;
    function _GetappVersion: UnicodeString;
    function _Getplatform: UnicodeString;
    function _GetuserAgent: UnicodeString;
    function _Getproduct: UnicodeString;
  Public
    function taintEnabled: Boolean;
    class function Cast(Intf: IJSObject): IJSNavigatorID;
    property appCodeName: UnicodeString read _GetappCodeName;
    property appName: UnicodeString read _GetappName;
    property appVersion: UnicodeString read _GetappVersion;
    property platform: UnicodeString read _Getplatform;
    property userAgent: UnicodeString read _GetuserAgent;
    property product: UnicodeString read _Getproduct;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorLanguage
    --------------------------------------------------------------------}

  IJSNavigatorLanguage = interface(IJSObject)
    ['{40C53898-A00F-3AF7-BC3E-FC7CF090621F}']
    function _Getlanguage: UnicodeString;
    function _Getlanguages: TUnicodeStringDynArray;
    property language: UnicodeString read _Getlanguage;
    property languages: TUnicodeStringDynArray read _Getlanguages;
  end;

  TJSNavigatorLanguage = class(TJSObject,IJSNavigatorLanguage)
  Private
    function _Getlanguage: UnicodeString;
    function _Getlanguages: TUnicodeStringDynArray;
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorLanguage;
    property language: UnicodeString read _Getlanguage;
    property languages: TUnicodeStringDynArray read _Getlanguages;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorOnLine
    --------------------------------------------------------------------}

  IJSNavigatorOnLine = interface(IJSObject)
    ['{D3A72A1E-EB8A-3AE7-953C-E8AA1D3F6A4F}']
    function _GetonLine: Boolean;
    property onLine: Boolean read _GetonLine;
  end;

  TJSNavigatorOnLine = class(TJSObject,IJSNavigatorOnLine)
  Private
    function _GetonLine: Boolean;
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorOnLine;
    property onLine: Boolean read _GetonLine;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorContentUtils
    --------------------------------------------------------------------}

  IJSNavigatorContentUtils = interface(IJSObject)
    ['{362EC3BB-3CE7-318B-B2C8-528F06651BFC}']
    procedure checkProtocolHandlerAllowed(const aScheme: UnicodeString; aHandlerURI: IJSURI; aDocumentURI: IJSURI);
    procedure registerProtocolHandler(const aScheme: UnicodeString; const aUrl: UnicodeString);
  end;

  TJSNavigatorContentUtils = class(TJSObject,IJSNavigatorContentUtils)
  Private
  Public
    procedure checkProtocolHandlerAllowed(const aScheme: UnicodeString; aHandlerURI: IJSURI; aDocumentURI: IJSURI);
    procedure registerProtocolHandler(const aScheme: UnicodeString; const aUrl: UnicodeString);
    class function Cast(Intf: IJSObject): IJSNavigatorContentUtils;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorStorage
    --------------------------------------------------------------------}

  IJSNavigatorStorage = interface(IJSObject)
    ['{84BA292D-E150-36FD-A0FC-B98C1E0D8EB9}']
  end;

  TJSNavigatorStorage = class(TJSObject,IJSNavigatorStorage)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorStorage;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorStorageUtils
    --------------------------------------------------------------------}

  IJSNavigatorStorageUtils = interface(IJSObject)
    ['{E0F1ED77-CF32-3976-B0E2-7B11DFFDF30F}']
  end;

  TJSNavigatorStorageUtils = class(TJSObject,IJSNavigatorStorageUtils)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorStorageUtils;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorGeolocation
    --------------------------------------------------------------------}

  IJSNavigatorGeolocation = interface(IJSObject)
    ['{3344B20B-A7B2-39F5-9077-E0F31DC9E0F0}']
  end;

  TJSNavigatorGeolocation = class(TJSObject,IJSNavigatorGeolocation)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorGeolocation;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorConcurrentHardware
    --------------------------------------------------------------------}

  IJSNavigatorConcurrentHardware = interface(IJSObject)
    ['{EC4D90C7-F0E9-3C79-BDA2-E1F4EB5283F2}']
    function _GethardwareConcurrency: QWord;
    property hardwareConcurrency: QWord read _GethardwareConcurrency;
  end;

  TJSNavigatorConcurrentHardware = class(TJSObject,IJSNavigatorConcurrentHardware)
  Private
    function _GethardwareConcurrency: QWord;
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorConcurrentHardware;
    property hardwareConcurrency: QWord read _GethardwareConcurrency;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorAutomationInformation
    --------------------------------------------------------------------}

  IJSNavigatorAutomationInformation = interface(IJSObject)
    ['{D644165F-F322-372F-90C1-895B3CEB56D6}']
    function _Getwebdriver: Boolean;
    property webdriver: Boolean read _Getwebdriver;
  end;

  TJSNavigatorAutomationInformation = class(TJSObject,IJSNavigatorAutomationInformation)
  Private
    function _Getwebdriver: Boolean;
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorAutomationInformation;
    property webdriver: Boolean read _Getwebdriver;
  end;

  { --------------------------------------------------------------------
    TJSNavigatorLocks
    --------------------------------------------------------------------}

  IJSNavigatorLocks = interface(IJSObject)
    ['{A28C39CF-32B0-3761-B12B-A07C3446E5A9}']
  end;

  TJSNavigatorLocks = class(TJSObject,IJSNavigatorLocks)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSNavigatorLocks;
  end;

  { --------------------------------------------------------------------
    TJSStorage
    --------------------------------------------------------------------}

  IJSStorage = interface(IJSObject)
    ['{E34581BA-4F5C-3DF4-AE45-D6181F402189}']
    function _Getlength_: LongWord;
    function _GetisSessionOnly: Boolean;
    function _GethasSnapshot: Boolean;
    function _GetsnapshotUsage: Int64;
    function key(aIndex: LongWord): UnicodeString;
    function getItem(const aKey: UnicodeString): UnicodeString;
    procedure setItem(const aKey: UnicodeString; const aValue: UnicodeString);
    procedure removeItem(const aKey: UnicodeString);
    procedure clear;
    procedure open;
    procedure close;
    procedure beginExplicitSnapshot;
    procedure checkpointExplicitSnapshot;
    procedure endExplicitSnapshot;
    property length_: LongWord read _Getlength_;
    property isSessionOnly: Boolean read _GetisSessionOnly;
    property hasSnapshot: Boolean read _GethasSnapshot;
    property snapshotUsage: Int64 read _GetsnapshotUsage;
  end;

  TJSStorage = class(TJSObject,IJSStorage)
  Private
    function _Getlength_: LongWord;
    function _GetisSessionOnly: Boolean;
    function _GethasSnapshot: Boolean;
    function _GetsnapshotUsage: Int64;
  Public
    function key(aIndex: LongWord): UnicodeString;
    function getItem(const aKey: UnicodeString): UnicodeString;
    procedure setItem(const aKey: UnicodeString; const aValue: UnicodeString);
    procedure removeItem(const aKey: UnicodeString);
    procedure clear;
    procedure open;
    procedure close;
    procedure beginExplicitSnapshot;
    procedure checkpointExplicitSnapshot;
    procedure endExplicitSnapshot;
    class function Cast(Intf: IJSObject): IJSStorage;
    property length_: LongWord read _Getlength_;
    property isSessionOnly: Boolean read _GetisSessionOnly;
    property hasSnapshot: Boolean read _GethasSnapshot;
    property snapshotUsage: Int64 read _GetsnapshotUsage;
  end;

  { --------------------------------------------------------------------
    TJSSelection
    --------------------------------------------------------------------}

  TJSRangeDynArray = IJSArray; // array of TJSRange

  IJSSelection = interface(IJSObject)
    ['{522D4098-FC2A-35AF-A806-57B88516481A}']
    function _GetanchorNode: IJSNode;
    function _GetanchorOffset: LongWord;
    function _GetfocusNode: IJSNode;
    function _GetfocusOffset: LongWord;
    function _GetisCollapsed: Boolean;
    function _GetrangeCount: LongWord;
    function _Gettype_: UnicodeString;
    function _GetinterlinePosition: Boolean;
    function _GetcaretBidiLevel: SmallInt;
    function _GetselectionType: SmallInt;
    procedure _SetinterlinePosition(const aValue: Boolean);
    procedure _SetcaretBidiLevel(const aValue: SmallInt);
    function getRangeAt(aIndex: LongWord): IJSRange;
    procedure addRange(aRange: IJSRange);
    procedure removeRange(aRange: IJSRange);
    procedure removeAllRanges;
    procedure empty;
    procedure collapse(aNode: IJSNode; aOffset: LongWord); overload;
    procedure collapse(aNode: IJSNode); overload;
    procedure setPosition(aNode: IJSNode; aOffset: LongWord); overload;
    procedure setPosition(aNode: IJSNode); overload;
    procedure collapseToStart;
    procedure collapseToEnd;
    procedure extend(aNode: IJSNode; aOffset: LongWord); overload;
    procedure extend(aNode: IJSNode); overload;
    procedure setBaseAndExtent(_anchorNode: IJSNode; _anchorOffset: LongWord; aFocusNode: IJSNode; aFocusOffset: LongWord);
    procedure selectAllChildren(aNode: IJSNode);
    procedure deleteFromDocument;
    function containsNode(aNode: IJSNode; allowPartialContainment: Boolean): Boolean; overload;
    function containsNode(aNode: IJSNode): Boolean; overload;
    procedure modify(const alter: UnicodeString; const aDirection: UnicodeString; const aGranularity: UnicodeString);
    function toStringWithFormat(const aFormatType: UnicodeString; aFlags: LongWord; aWrapColumn: Integer): UnicodeString;
    procedure addSelectionListener(aNewListener: IJSnsISelectionListener);
    procedure removeSelectionListener(aListenerToRemove: IJSnsISelectionListener);
    function GetRangesForInterval(aBeginNode: IJSNode; aBeginOffset: Integer; aEndNode: IJSNode; aEndOffset: Integer; allowAdjacent: Boolean): TJSRangeDynArray;
    procedure scrollIntoView(aRegion: SmallInt; aIsSynchronous: Boolean; aVPercent: SmallInt; aHPercent: SmallInt);
    procedure setColors(const aForegroundColor: UnicodeString; const aBackgroundColor: UnicodeString; const aAltForegroundColor: UnicodeString; const aAltBackgroundColor: UnicodeString);
    procedure resetColors;
    property anchorNode: IJSNode read _GetanchorNode;
    property anchorOffset: LongWord read _GetanchorOffset;
    property focusNode: IJSNode read _GetfocusNode;
    property focusOffset: LongWord read _GetfocusOffset;
    property isCollapsed: Boolean read _GetisCollapsed;
    property rangeCount: LongWord read _GetrangeCount;
    property type_: UnicodeString read _Gettype_;
    property interlinePosition: Boolean read _GetinterlinePosition write _SetinterlinePosition;
    property caretBidiLevel: SmallInt read _GetcaretBidiLevel write _SetcaretBidiLevel;
    property selectionType: SmallInt read _GetselectionType;
  end;

  TJSSelection = class(TJSObject,IJSSelection)
  Private
    function _GetanchorNode: IJSNode;
    function _GetanchorOffset: LongWord;
    function _GetfocusNode: IJSNode;
    function _GetfocusOffset: LongWord;
    function _GetisCollapsed: Boolean;
    function _GetrangeCount: LongWord;
    function _Gettype_: UnicodeString;
    function _GetinterlinePosition: Boolean;
    function _GetcaretBidiLevel: SmallInt;
    function _GetselectionType: SmallInt;
    procedure _SetinterlinePosition(const aValue: Boolean);
    procedure _SetcaretBidiLevel(const aValue: SmallInt);
  Public
    function getRangeAt(aIndex: LongWord): IJSRange;
    procedure addRange(aRange: IJSRange);
    procedure removeRange(aRange: IJSRange);
    procedure removeAllRanges;
    procedure empty;
    procedure collapse(aNode: IJSNode; aOffset: LongWord); overload;
    procedure collapse(aNode: IJSNode); overload;
    procedure setPosition(aNode: IJSNode; aOffset: LongWord); overload;
    procedure setPosition(aNode: IJSNode); overload;
    procedure collapseToStart;
    procedure collapseToEnd;
    procedure extend(aNode: IJSNode; aOffset: LongWord); overload;
    procedure extend(aNode: IJSNode); overload;
    procedure setBaseAndExtent(_anchorNode: IJSNode; _anchorOffset: LongWord; aFocusNode: IJSNode; aFocusOffset: LongWord);
    procedure selectAllChildren(aNode: IJSNode);
    procedure deleteFromDocument;
    function containsNode(aNode: IJSNode; allowPartialContainment: Boolean): Boolean; overload;
    function containsNode(aNode: IJSNode): Boolean; overload;
    procedure modify(const alter: UnicodeString; const aDirection: UnicodeString; const aGranularity: UnicodeString);
    function toStringWithFormat(const aFormatType: UnicodeString; aFlags: LongWord; aWrapColumn: Integer): UnicodeString;
    procedure addSelectionListener(aNewListener: IJSnsISelectionListener);
    procedure removeSelectionListener(aListenerToRemove: IJSnsISelectionListener);
    function GetRangesForInterval(aBeginNode: IJSNode; aBeginOffset: Integer; aEndNode: IJSNode; aEndOffset: Integer; allowAdjacent: Boolean): TJSRangeDynArray;
    procedure scrollIntoView(aRegion: SmallInt; aIsSynchronous: Boolean; aVPercent: SmallInt; aHPercent: SmallInt);
    procedure setColors(const aForegroundColor: UnicodeString; const aBackgroundColor: UnicodeString; const aAltForegroundColor: UnicodeString; const aAltBackgroundColor: UnicodeString);
    procedure resetColors;
    class function Cast(Intf: IJSObject): IJSSelection;
    property anchorNode: IJSNode read _GetanchorNode;
    property anchorOffset: LongWord read _GetanchorOffset;
    property focusNode: IJSNode read _GetfocusNode;
    property focusOffset: LongWord read _GetfocusOffset;
    property isCollapsed: Boolean read _GetisCollapsed;
    property rangeCount: LongWord read _GetrangeCount;
    property type_: UnicodeString read _Gettype_;
    property interlinePosition: Boolean read _GetinterlinePosition write _SetinterlinePosition;
    property caretBidiLevel: SmallInt read _GetcaretBidiLevel write _SetcaretBidiLevel;
    property selectionType: SmallInt read _GetselectionType;
  end;

  { --------------------------------------------------------------------
    TJSnsISelectionListener
    --------------------------------------------------------------------}

  IJSnsISelectionListener = interface(IJSObject)
    ['{3346BB54-E605-3126-B836-B418312260F0}']
  end;

  TJSnsISelectionListener = class(TJSObject,IJSnsISelectionListener)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsISelectionListener;
  end;

  { --------------------------------------------------------------------
    TJSScreenLuminance
    --------------------------------------------------------------------}

  IJSScreenLuminance = interface(IJSObject)
    ['{69ACF4AD-9D71-36E9-B9C6-114013B8E811}']
    function _Getmin: Double;
    function _Getmax: Double;
    function _GetmaxAverage: Double;
    property min: Double read _Getmin;
    property max: Double read _Getmax;
    property maxAverage: Double read _GetmaxAverage;
  end;

  TJSScreenLuminance = class(TJSObject,IJSScreenLuminance)
  Private
    function _Getmin: Double;
    function _Getmax: Double;
    function _GetmaxAverage: Double;
  Public
    class function Cast(Intf: IJSObject): IJSScreenLuminance;
    property min: Double read _Getmin;
    property max: Double read _Getmax;
    property maxAverage: Double read _GetmaxAverage;
  end;

  { --------------------------------------------------------------------
    TJSClipboardItem
    --------------------------------------------------------------------}

  IJSClipboardItem = interface(IJSObject)
    ['{30F7800A-CF52-3002-A893-0A382C27AF19}']
    function _GetpresentationStyle: TPresentationStyle;
    function _Gettypes: TUnicodeStringDynArray;
    property presentationStyle: TPresentationStyle read _GetpresentationStyle;
    property types: TUnicodeStringDynArray read _Gettypes;
  end;

  TJSClipboardItem = class(TJSObject,IJSClipboardItem)
  Private
    function _GetpresentationStyle: TPresentationStyle;
    function _Gettypes: TUnicodeStringDynArray;
  Public
    class function Cast(Intf: IJSObject): IJSClipboardItem;
    property presentationStyle: TPresentationStyle read _GetpresentationStyle;
    property types: TUnicodeStringDynArray read _Gettypes;
  end;

  { --------------------------------------------------------------------
    TJSAbstractRange
    --------------------------------------------------------------------}

  IJSAbstractRange = interface(IJSObject)
    ['{D943C641-A456-3F77-9673-65A069D13E3A}']
    function _GetstartContainer: IJSNode;
    function _GetstartOffset: LongWord;
    function _GetendContainer: IJSNode;
    function _GetendOffset: LongWord;
    function _Getcollapsed: Boolean;
    property startContainer: IJSNode read _GetstartContainer;
    property startOffset: LongWord read _GetstartOffset;
    property endContainer: IJSNode read _GetendContainer;
    property endOffset: LongWord read _GetendOffset;
    property collapsed: Boolean read _Getcollapsed;
  end;

  TJSAbstractRange = class(TJSObject,IJSAbstractRange)
  Private
    function _GetstartContainer: IJSNode;
    function _GetstartOffset: LongWord;
    function _GetendContainer: IJSNode;
    function _GetendOffset: LongWord;
    function _Getcollapsed: Boolean;
  Public
    class function Cast(Intf: IJSObject): IJSAbstractRange;
    property startContainer: IJSNode read _GetstartContainer;
    property startOffset: LongWord read _GetstartOffset;
    property endContainer: IJSNode read _GetendContainer;
    property endOffset: LongWord read _GetendOffset;
    property collapsed: Boolean read _Getcollapsed;
  end;

  { --------------------------------------------------------------------
    TJSDOMRectList
    --------------------------------------------------------------------}

  IJSDOMRectList = interface(IJSObject)
    ['{E624F494-5642-33FE-BDF6-3619147BDBAA}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): IJSDOMRect;
    property length_: LongWord read _Getlength_;
  end;

  TJSDOMRectList = class(TJSObject,IJSDOMRectList)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): IJSDOMRect;
    class function Cast(Intf: IJSObject): IJSDOMRectList;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSDOMRectReadOnly
    --------------------------------------------------------------------}

  IJSDOMRectReadOnly = interface(IJSObject)
    ['{EEEF5917-0433-31A9-8F18-FDAA73D86752}']
    function _Getx: Double;
    function _Gety: Double;
    function _Getwidth: Double;
    function _Getheight: Double;
    function _Gettop: Double;
    function _Getright: Double;
    function _Getbottom: Double;
    function _Getleft: Double;
    function fromRect(const aOther: TJSDOMRectInit): IJSDOMRectReadOnly; overload;
    function fromRect: IJSDOMRectReadOnly; overload;
    function toJSON: IJSObject;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property width: Double read _Getwidth;
    property height: Double read _Getheight;
    property top: Double read _Gettop;
    property right: Double read _Getright;
    property bottom: Double read _Getbottom;
    property left: Double read _Getleft;
  end;

  TJSDOMRectReadOnly = class(TJSObject,IJSDOMRectReadOnly)
  Private
    function _Getx: Double;
    function _Gety: Double;
    function _Getwidth: Double;
    function _Getheight: Double;
    function _Gettop: Double;
    function _Getright: Double;
    function _Getbottom: Double;
    function _Getleft: Double;
  Public
    function fromRect(const aOther: TJSDOMRectInit): IJSDOMRectReadOnly; overload;
    function fromRect: IJSDOMRectReadOnly; overload;
    function toJSON: IJSObject;
    class function Cast(Intf: IJSObject): IJSDOMRectReadOnly;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property width: Double read _Getwidth;
    property height: Double read _Getheight;
    property top: Double read _Gettop;
    property right: Double read _Getright;
    property bottom: Double read _Getbottom;
    property left: Double read _Getleft;
  end;

  { --------------------------------------------------------------------
    TJSTouchEventHandlers
    --------------------------------------------------------------------}

  IJSTouchEventHandlers = interface(IJSObject)
    ['{E08E9CBE-A140-3326-8B74-575D87A7903F}']
    // property ontouchstart: TEventHandler read _Getontouchstart write _Setontouchstart;
    // property ontouchend: TEventHandler read _Getontouchend write _Setontouchend;
    // property ontouchmove: TEventHandler read _Getontouchmove write _Setontouchmove;
    // property ontouchcancel: TEventHandler read _Getontouchcancel write _Setontouchcancel;
  end;

  TJSTouchEventHandlers = class(TJSObject,IJSTouchEventHandlers)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSTouchEventHandlers;
    // property ontouchstart: TEventHandler read _Getontouchstart write _Setontouchstart;
    // property ontouchend: TEventHandler read _Getontouchend write _Setontouchend;
    // property ontouchmove: TEventHandler read _Getontouchmove write _Setontouchmove;
    // property ontouchcancel: TEventHandler read _Getontouchcancel write _Setontouchcancel;
  end;

  { --------------------------------------------------------------------
    TJSHTMLAllCollection
    --------------------------------------------------------------------}

  IJSHTMLAllCollection = interface(IJSObject)
    ['{B22722E5-F868-3D8D-8C1A-551B6C97F66B}']
    function _Getlength_: LongWord;
    function namedItem(const aName: UnicodeString): TJOB_JSValue;
    function item(const aNameOrIndex: UnicodeString): TJOB_JSValue; overload;
    function item: TJOB_JSValue; overload;
    property length_: LongWord read _Getlength_;
  end;

  TJSHTMLAllCollection = class(TJSObject,IJSHTMLAllCollection)
  Private
    function _Getlength_: LongWord;
  Public
    function namedItem(const aName: UnicodeString): TJOB_JSValue;
    function item(const aNameOrIndex: UnicodeString): TJOB_JSValue; overload;
    function item: TJOB_JSValue; overload;
    class function Cast(Intf: IJSObject): IJSHTMLAllCollection;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSMenuBuilder
    --------------------------------------------------------------------}

  IJSMenuBuilder = interface(IJSObject)
    ['{EEDF5888-4CCF-32A2-8C3D-1A2372542131}']
  end;

  TJSMenuBuilder = class(TJSObject,IJSMenuBuilder)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSMenuBuilder;
  end;

  { --------------------------------------------------------------------
    TJSElementInternals
    --------------------------------------------------------------------}

  IJSElementInternals = interface(IJSObject)
    ['{CEA65518-EF01-3D51-8D7A-96820855BC21}']
    function _GetshadowRoot: IJSShadowRoot;
    function _Getform: IJSHTMLFormElement;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    function _GetvalidationAnchor: IJSHTMLElement;
    procedure setFormValue(aValue: IJSFile; aState: IJSFile); overload;
    procedure setFormValue(aValue: IJSFormData; aState: IJSFile); overload;
    procedure setFormValue(const aValue: UnicodeString; aState: IJSFile); overload;
    procedure setFormValue(const aValue: UnicodeString); overload;
    procedure setFormValue(aValue: IJSFormData); overload;
    procedure setFormValue(aValue: IJSFile); overload;
    procedure setFormValue(const aValue: UnicodeString; aState: IJSFormData); overload;
    procedure setFormValue(aValue: IJSFormData; aState: IJSFormData); overload;
    procedure setFormValue(aValue: IJSFile; aState: IJSFormData); overload;
    procedure setFormValue(const aValue: UnicodeString; const aState: UnicodeString); overload;
    procedure setFormValue(aValue: IJSFormData; const aState: UnicodeString); overload;
    procedure setFormValue(aValue: IJSFile; const aState: UnicodeString); overload;
    procedure setValidity(const aFlags: TJSValidityStateFlags; const aMessage: UnicodeString; anchor: IJSHTMLElement); overload;
    procedure setValidity; overload;
    procedure setValidity(const aFlags: TJSValidityStateFlags); overload;
    procedure setValidity(const aFlags: TJSValidityStateFlags; const aMessage: UnicodeString); overload;
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    property shadowRoot: IJSShadowRoot read _GetshadowRoot;
    property form: IJSHTMLFormElement read _Getform;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
    property validationAnchor: IJSHTMLElement read _GetvalidationAnchor;
  end;

  TJSElementInternals = class(TJSObject,IJSElementInternals)
  Private
    function _GetshadowRoot: IJSShadowRoot;
    function _Getform: IJSHTMLFormElement;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    function _GetvalidationAnchor: IJSHTMLElement;
  Public
    procedure setFormValue(aValue: IJSFile; aState: IJSFile); overload;
    procedure setFormValue(aValue: IJSFormData; aState: IJSFile); overload;
    procedure setFormValue(const aValue: UnicodeString; aState: IJSFile); overload;
    procedure setFormValue(const aValue: UnicodeString); overload;
    procedure setFormValue(aValue: IJSFormData); overload;
    procedure setFormValue(aValue: IJSFile); overload;
    procedure setFormValue(const aValue: UnicodeString; aState: IJSFormData); overload;
    procedure setFormValue(aValue: IJSFormData; aState: IJSFormData); overload;
    procedure setFormValue(aValue: IJSFile; aState: IJSFormData); overload;
    procedure setFormValue(const aValue: UnicodeString; const aState: UnicodeString); overload;
    procedure setFormValue(aValue: IJSFormData; const aState: UnicodeString); overload;
    procedure setFormValue(aValue: IJSFile; const aState: UnicodeString); overload;
    procedure setValidity(const aFlags: TJSValidityStateFlags; const aMessage: UnicodeString; anchor: IJSHTMLElement); overload;
    procedure setValidity; overload;
    procedure setValidity(const aFlags: TJSValidityStateFlags); overload;
    procedure setValidity(const aFlags: TJSValidityStateFlags; const aMessage: UnicodeString); overload;
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    class function Cast(Intf: IJSObject): IJSElementInternals;
    property shadowRoot: IJSShadowRoot read _GetshadowRoot;
    property form: IJSHTMLFormElement read _Getform;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
    property validationAnchor: IJSHTMLElement read _GetvalidationAnchor;
  end;

  { --------------------------------------------------------------------
    TJSValidityState
    --------------------------------------------------------------------}

  IJSValidityState = interface(IJSObject)
    ['{C907A0BC-CCB4-3507-A275-90428B183C13}']
    function _GetvalueMissing: Boolean;
    function _GettypeMismatch: Boolean;
    function _GetpatternMismatch: Boolean;
    function _GettooLong: Boolean;
    function _GettooShort: Boolean;
    function _GetrangeUnderflow: Boolean;
    function _GetrangeOverflow: Boolean;
    function _GetstepMismatch: Boolean;
    function _GetbadInput: Boolean;
    function _GetcustomError: Boolean;
    function _Getvalid: Boolean;
    property valueMissing: Boolean read _GetvalueMissing;
    property typeMismatch: Boolean read _GettypeMismatch;
    property patternMismatch: Boolean read _GetpatternMismatch;
    property tooLong: Boolean read _GettooLong;
    property tooShort: Boolean read _GettooShort;
    property rangeUnderflow: Boolean read _GetrangeUnderflow;
    property rangeOverflow: Boolean read _GetrangeOverflow;
    property stepMismatch: Boolean read _GetstepMismatch;
    property badInput: Boolean read _GetbadInput;
    property customError: Boolean read _GetcustomError;
    property valid: Boolean read _Getvalid;
  end;

  TJSValidityState = class(TJSObject,IJSValidityState)
  Private
    function _GetvalueMissing: Boolean;
    function _GettypeMismatch: Boolean;
    function _GetpatternMismatch: Boolean;
    function _GettooLong: Boolean;
    function _GettooShort: Boolean;
    function _GetrangeUnderflow: Boolean;
    function _GetrangeOverflow: Boolean;
    function _GetstepMismatch: Boolean;
    function _GetbadInput: Boolean;
    function _GetcustomError: Boolean;
    function _Getvalid: Boolean;
  Public
    class function Cast(Intf: IJSObject): IJSValidityState;
    property valueMissing: Boolean read _GetvalueMissing;
    property typeMismatch: Boolean read _GettypeMismatch;
    property patternMismatch: Boolean read _GetpatternMismatch;
    property tooLong: Boolean read _GettooLong;
    property tooShort: Boolean read _GettooShort;
    property rangeUnderflow: Boolean read _GetrangeUnderflow;
    property rangeOverflow: Boolean read _GetrangeOverflow;
    property stepMismatch: Boolean read _GetstepMismatch;
    property badInput: Boolean read _GetbadInput;
    property customError: Boolean read _GetcustomError;
    property valid: Boolean read _Getvalid;
  end;

  { --------------------------------------------------------------------
    TJSSVGViewSpec
    --------------------------------------------------------------------}

  IJSSVGViewSpec = interface(IJSObject)
    ['{EEE0B168-9252-3162-903D-1A2372542131}']
  end;

  TJSSVGViewSpec = class(TJSObject,IJSSVGViewSpec)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSSVGViewSpec;
  end;

  { --------------------------------------------------------------------
    TJSSVGFitToViewBox
    --------------------------------------------------------------------}

  IJSSVGFitToViewBox = interface(IJSObject)
    ['{9B43C1A7-4B41-3529-93D2-A7DDD24F9AAA}']
    function _GetviewBox: IJSSVGAnimatedRect;
    function _GetpreserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio;
    property viewBox: IJSSVGAnimatedRect read _GetviewBox;
    property preserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio read _GetpreserveAspectRatio;
  end;

  TJSSVGFitToViewBox = class(TJSObject,IJSSVGFitToViewBox)
  Private
    function _GetviewBox: IJSSVGAnimatedRect;
    function _GetpreserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio;
  Public
    class function Cast(Intf: IJSObject): IJSSVGFitToViewBox;
    property viewBox: IJSSVGAnimatedRect read _GetviewBox;
    property preserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio read _GetpreserveAspectRatio;
  end;

  { --------------------------------------------------------------------
    TJSSVGZoomAndPan
    --------------------------------------------------------------------}

  IJSSVGZoomAndPan = interface(IJSObject)
    ['{8036E4E3-9C38-39BE-9A2A-F0330C1DCED0}']
    function _GetzoomAndPan: Word;
    procedure _SetzoomAndPan(const aValue: Word);
    property zoomAndPan: Word read _GetzoomAndPan write _SetzoomAndPan;
  end;

  TJSSVGZoomAndPan = class(TJSObject,IJSSVGZoomAndPan)
  Private
    function _GetzoomAndPan: Word;
    procedure _SetzoomAndPan(const aValue: Word);
  Public
    Const
      SVG_ZOOMANDPAN_UNKNOWN = 0;
      SVG_ZOOMANDPAN_DISABLE = 1;
      SVG_ZOOMANDPAN_MAGNIFY = 2;
  Public
    class function Cast(Intf: IJSObject): IJSSVGZoomAndPan;
    property zoomAndPan: Word read _GetzoomAndPan write _SetzoomAndPan;
  end;

  { --------------------------------------------------------------------
    TJSCSSRule
    --------------------------------------------------------------------}

  IJSCSSRule = interface(IJSObject)
    ['{152E7B15-2E19-3E3A-AE43-96A8ACA8333E}']
    function _Gettype_: Word;
    function _GetcssText: UTF8String;
    function _GetparentRule: IJSCSSRule;
    function _GetparentStyleSheet: IJSCSSStyleSheet;
    procedure _SetcssText(const aValue: UTF8String);
    property type_: Word read _Gettype_;
    property cssText: UTF8String read _GetcssText write _SetcssText;
    property parentRule: IJSCSSRule read _GetparentRule;
    property parentStyleSheet: IJSCSSStyleSheet read _GetparentStyleSheet;
  end;

  TJSCSSRule = class(TJSObject,IJSCSSRule)
  Private
    function _Gettype_: Word;
    function _GetcssText: UTF8String;
    function _GetparentRule: IJSCSSRule;
    function _GetparentStyleSheet: IJSCSSStyleSheet;
    procedure _SetcssText(const aValue: UTF8String);
  Public
    Const
      STYLE_RULE = 1;
      CHARSET_RULE = 2;
      IMPORT_RULE = 3;
      MEDIA_RULE = 4;
      FONT_FACE_RULE = 5;
      PAGE_RULE = 6;
      NAMESPACE_RULE = 10;
      KEYFRAMES_RULE = 7;
      KEYFRAME_RULE = 8;
      COUNTER_STYLE_RULE = 11;
      SUPPORTS_RULE = 12;
      DOCUMENT_RULE = 13;
      FONT_FEATURE_VALUES_RULE = 14;
  Public
    class function Cast(Intf: IJSObject): IJSCSSRule;
    property type_: Word read _Gettype_;
    property cssText: UTF8String read _GetcssText write _SetcssText;
    property parentRule: IJSCSSRule read _GetparentRule;
    property parentStyleSheet: IJSCSSStyleSheet read _GetparentStyleSheet;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedLength
    --------------------------------------------------------------------}

  IJSSVGAnimatedLength = interface(IJSObject)
    ['{DFDFF7BA-9CCE-35DB-89E4-28A24390149E}']
    function _GetbaseVal: IJSSVGLength;
    function _GetanimVal: IJSSVGLength;
    property baseVal: IJSSVGLength read _GetbaseVal;
    property animVal: IJSSVGLength read _GetanimVal;
  end;

  TJSSVGAnimatedLength = class(TJSObject,IJSSVGAnimatedLength)
  Private
    function _GetbaseVal: IJSSVGLength;
    function _GetanimVal: IJSSVGLength;
  Public
    class function Cast(Intf: IJSObject): IJSSVGAnimatedLength;
    property baseVal: IJSSVGLength read _GetbaseVal;
    property animVal: IJSSVGLength read _GetanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGPoint
    --------------------------------------------------------------------}

  IJSSVGPoint = interface(IJSObject)
    ['{813ED8B2-B864-3BC3-A349-5AFBD6DE5CB7}']
    function _Getx: Single;
    function _Gety: Single;
    procedure _Setx(const aValue: Single);
    procedure _Sety(const aValue: Single);
    function matrixTransform(const aMatrix: TJSDOMMatrix2DInit): IJSSVGPoint; overload;
    function matrixTransform: IJSSVGPoint; overload;
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
  end;

  TJSSVGPoint = class(TJSObject,IJSSVGPoint)
  Private
    function _Getx: Single;
    function _Gety: Single;
    procedure _Setx(const aValue: Single);
    procedure _Sety(const aValue: Single);
  Public
    function matrixTransform(const aMatrix: TJSDOMMatrix2DInit): IJSSVGPoint; overload;
    function matrixTransform: IJSSVGPoint; overload;
    class function Cast(Intf: IJSObject): IJSSVGPoint;
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
  end;

  { --------------------------------------------------------------------
    TJSSVGNumber
    --------------------------------------------------------------------}

  IJSSVGNumber = interface(IJSObject)
    ['{338270CA-6AD5-3715-842A-473598DCED52}']
    function _Getvalue: Single;
    procedure _Setvalue(const aValue: Single);
    property value: Single read _Getvalue write _Setvalue;
  end;

  TJSSVGNumber = class(TJSObject,IJSSVGNumber)
  Private
    function _Getvalue: Single;
    procedure _Setvalue(const aValue: Single);
  Public
    class function Cast(Intf: IJSObject): IJSSVGNumber;
    property value: Single read _Getvalue write _Setvalue;
  end;

  { --------------------------------------------------------------------
    TJSSVGLength
    --------------------------------------------------------------------}

  IJSSVGLength = interface(IJSObject)
    ['{77EF3352-3671-331C-9210-8ED31593609B}']
    function _GetunitType: Word;
    function _Getvalue: Single;
    function _GetvalueInSpecifiedUnits: Single;
    function _GetvalueAsString: UnicodeString;
    procedure _Setvalue(const aValue: Single);
    procedure _SetvalueInSpecifiedUnits(const aValue: Single);
    procedure _SetvalueAsString(const aValue: UnicodeString);
    procedure newValueSpecifiedUnits(aUnitType: Word; aValueInSpecifiedUnits: Single);
    procedure convertToSpecifiedUnits(aUnitType: Word);
    property unitType: Word read _GetunitType;
    property value: Single read _Getvalue write _Setvalue;
    property valueInSpecifiedUnits: Single read _GetvalueInSpecifiedUnits write _SetvalueInSpecifiedUnits;
    property valueAsString: UnicodeString read _GetvalueAsString write _SetvalueAsString;
  end;

  TJSSVGLength = class(TJSObject,IJSSVGLength)
  Private
    function _GetunitType: Word;
    function _Getvalue: Single;
    function _GetvalueInSpecifiedUnits: Single;
    function _GetvalueAsString: UnicodeString;
    procedure _Setvalue(const aValue: Single);
    procedure _SetvalueInSpecifiedUnits(const aValue: Single);
    procedure _SetvalueAsString(const aValue: UnicodeString);
  Public
    Const
      SVG_LENGTHTYPE_UNKNOWN = 0;
      SVG_LENGTHTYPE_NUMBER = 1;
      SVG_LENGTHTYPE_PERCENTAGE = 2;
      SVG_LENGTHTYPE_EMS = 3;
      SVG_LENGTHTYPE_EXS = 4;
      SVG_LENGTHTYPE_PX = 5;
      SVG_LENGTHTYPE_CM = 6;
      SVG_LENGTHTYPE_MM = 7;
      SVG_LENGTHTYPE_IN = 8;
      SVG_LENGTHTYPE_PT = 9;
      SVG_LENGTHTYPE_PC = 10;
  Public
    procedure newValueSpecifiedUnits(aUnitType: Word; aValueInSpecifiedUnits: Single);
    procedure convertToSpecifiedUnits(aUnitType: Word);
    class function Cast(Intf: IJSObject): IJSSVGLength;
    property unitType: Word read _GetunitType;
    property value: Single read _Getvalue write _Setvalue;
    property valueInSpecifiedUnits: Single read _GetvalueInSpecifiedUnits write _SetvalueInSpecifiedUnits;
    property valueAsString: UnicodeString read _GetvalueAsString write _SetvalueAsString;
  end;

  { --------------------------------------------------------------------
    TJSSVGAngle
    --------------------------------------------------------------------}

  IJSSVGAngle = interface(IJSObject)
    ['{D41D6A48-63A8-365C-A924-5145029B1174}']
    function _GetunitType: Word;
    function _Getvalue: Single;
    function _GetvalueInSpecifiedUnits: Single;
    function _GetvalueAsString: UnicodeString;
    procedure _Setvalue(const aValue: Single);
    procedure _SetvalueInSpecifiedUnits(const aValue: Single);
    procedure _SetvalueAsString(const aValue: UnicodeString);
    procedure newValueSpecifiedUnits(aUnitType: Word; aValueInSpecifiedUnits: Single);
    procedure convertToSpecifiedUnits(aUnitType: Word);
    property unitType: Word read _GetunitType;
    property value: Single read _Getvalue write _Setvalue;
    property valueInSpecifiedUnits: Single read _GetvalueInSpecifiedUnits write _SetvalueInSpecifiedUnits;
    property valueAsString: UnicodeString read _GetvalueAsString write _SetvalueAsString;
  end;

  TJSSVGAngle = class(TJSObject,IJSSVGAngle)
  Private
    function _GetunitType: Word;
    function _Getvalue: Single;
    function _GetvalueInSpecifiedUnits: Single;
    function _GetvalueAsString: UnicodeString;
    procedure _Setvalue(const aValue: Single);
    procedure _SetvalueInSpecifiedUnits(const aValue: Single);
    procedure _SetvalueAsString(const aValue: UnicodeString);
  Public
    Const
      SVG_ANGLETYPE_UNKNOWN = 0;
      SVG_ANGLETYPE_UNSPECIFIED = 1;
      SVG_ANGLETYPE_DEG = 2;
      SVG_ANGLETYPE_RAD = 3;
      SVG_ANGLETYPE_GRAD = 4;
  Public
    procedure newValueSpecifiedUnits(aUnitType: Word; aValueInSpecifiedUnits: Single);
    procedure convertToSpecifiedUnits(aUnitType: Word);
    class function Cast(Intf: IJSObject): IJSSVGAngle;
    property unitType: Word read _GetunitType;
    property value: Single read _Getvalue write _Setvalue;
    property valueInSpecifiedUnits: Single read _GetvalueInSpecifiedUnits write _SetvalueInSpecifiedUnits;
    property valueAsString: UnicodeString read _GetvalueAsString write _SetvalueAsString;
  end;

  { --------------------------------------------------------------------
    TJSSVGMatrix
    --------------------------------------------------------------------}

  IJSSVGMatrix = interface(IJSObject)
    ['{BB1C59B1-8B25-3CF8-B601-CBF4C7802746}']
    function _Geta: Single;
    function _Getb: Single;
    function _Getc: Single;
    function _Getd: Single;
    function _Gete: Single;
    function _Getf: Single;
    procedure _Seta(const aValue: Single);
    procedure _Setb(const aValue: Single);
    procedure _Setc(const aValue: Single);
    procedure _Setd(const aValue: Single);
    procedure _Sete(const aValue: Single);
    procedure _Setf(const aValue: Single);
    function multiply(aSecondMatrix: IJSSVGMatrix): IJSSVGMatrix;
    function inverse: IJSSVGMatrix;
    function translate(aX: Single; aY: Single): IJSSVGMatrix;
    function scale(aScaleFactor: Single): IJSSVGMatrix;
    function scaleNonUniform(aScaleFactorX: Single; aScaleFactorY: Single): IJSSVGMatrix;
    function rotate(angle: Single): IJSSVGMatrix;
    function rotateFromVector(aX: Single; aY: Single): IJSSVGMatrix;
    function flipX: IJSSVGMatrix;
    function flipY: IJSSVGMatrix;
    function skewX(angle: Single): IJSSVGMatrix;
    function skewY(angle: Single): IJSSVGMatrix;
    property a: Single read _Geta write _Seta;
    property b: Single read _Getb write _Setb;
    property c: Single read _Getc write _Setc;
    property d: Single read _Getd write _Setd;
    property e: Single read _Gete write _Sete;
    property f: Single read _Getf write _Setf;
  end;

  TJSSVGMatrix = class(TJSObject,IJSSVGMatrix)
  Private
    function _Geta: Single;
    function _Getb: Single;
    function _Getc: Single;
    function _Getd: Single;
    function _Gete: Single;
    function _Getf: Single;
    procedure _Seta(const aValue: Single);
    procedure _Setb(const aValue: Single);
    procedure _Setc(const aValue: Single);
    procedure _Setd(const aValue: Single);
    procedure _Sete(const aValue: Single);
    procedure _Setf(const aValue: Single);
  Public
    function multiply(aSecondMatrix: IJSSVGMatrix): IJSSVGMatrix;
    function inverse: IJSSVGMatrix;
    function translate(aX: Single; aY: Single): IJSSVGMatrix;
    function scale(aScaleFactor: Single): IJSSVGMatrix;
    function scaleNonUniform(aScaleFactorX: Single; aScaleFactorY: Single): IJSSVGMatrix;
    function rotate(angle: Single): IJSSVGMatrix;
    function rotateFromVector(aX: Single; aY: Single): IJSSVGMatrix;
    function flipX: IJSSVGMatrix;
    function flipY: IJSSVGMatrix;
    function skewX(angle: Single): IJSSVGMatrix;
    function skewY(angle: Single): IJSSVGMatrix;
    class function Cast(Intf: IJSObject): IJSSVGMatrix;
    property a: Single read _Geta write _Seta;
    property b: Single read _Getb write _Setb;
    property c: Single read _Getc write _Setc;
    property d: Single read _Getd write _Setd;
    property e: Single read _Gete write _Sete;
    property f: Single read _Getf write _Setf;
  end;

  { --------------------------------------------------------------------
    TJSSVGRect
    --------------------------------------------------------------------}

  IJSSVGRect = interface(IJSObject)
    ['{74C88E5E-4D86-354B-AC04-16B3E3E36010}']
    function _Getx: Single;
    function _Gety: Single;
    function _Getwidth: Single;
    function _Getheight: Single;
    procedure _Setx(const aValue: Single);
    procedure _Sety(const aValue: Single);
    procedure _Setwidth(const aValue: Single);
    procedure _Setheight(const aValue: Single);
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
    property width: Single read _Getwidth write _Setwidth;
    property height: Single read _Getheight write _Setheight;
  end;

  TJSSVGRect = class(TJSObject,IJSSVGRect)
  Private
    function _Getx: Single;
    function _Gety: Single;
    function _Getwidth: Single;
    function _Getheight: Single;
    procedure _Setx(const aValue: Single);
    procedure _Sety(const aValue: Single);
    procedure _Setwidth(const aValue: Single);
    procedure _Setheight(const aValue: Single);
  Public
    class function Cast(Intf: IJSObject): IJSSVGRect;
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
    property width: Single read _Getwidth write _Setwidth;
    property height: Single read _Getheight write _Setheight;
  end;

  { --------------------------------------------------------------------
    TJSSVGTransform
    --------------------------------------------------------------------}

  IJSSVGTransform = interface(IJSObject)
    ['{461D9C03-A57A-35F8-B59D-C7C8041DF44C}']
    function _Gettype_: Word;
    function _Getmatrix: IJSSVGMatrix;
    function _Getangle: Single;
    procedure setMatrix(const aMatrix: TJSDOMMatrix2DInit); overload;
    procedure setMatrix; overload;
    procedure setTranslate(aTx: Single; aTy: Single);
    procedure setScale(aSx: Single; aSy: Single);
    procedure setRotate(_angle: Single; aCx: Single; aCy: Single);
    procedure setSkewX(_angle: Single);
    procedure setSkewY(_angle: Single);
    property type_: Word read _Gettype_;
    property matrix: IJSSVGMatrix read _Getmatrix;
    property angle: Single read _Getangle;
  end;

  TJSSVGTransform = class(TJSObject,IJSSVGTransform)
  Private
    function _Gettype_: Word;
    function _Getmatrix: IJSSVGMatrix;
    function _Getangle: Single;
  Public
    Const
      SVG_TRANSFORM_UNKNOWN = 0;
      SVG_TRANSFORM_MATRIX = 1;
      SVG_TRANSFORM_TRANSLATE = 2;
      SVG_TRANSFORM_SCALE = 3;
      SVG_TRANSFORM_ROTATE = 4;
      SVG_TRANSFORM_SKEWX = 5;
      SVG_TRANSFORM_SKEWY = 6;
  Public
    procedure setMatrix(const aMatrix: TJSDOMMatrix2DInit); overload;
    procedure setMatrix; overload;
    procedure setTranslate(aTx: Single; aTy: Single);
    procedure setScale(aSx: Single; aSy: Single);
    procedure setRotate(_angle: Single; aCx: Single; aCy: Single);
    procedure setSkewX(_angle: Single);
    procedure setSkewY(_angle: Single);
    class function Cast(Intf: IJSObject): IJSSVGTransform;
    property type_: Word read _Gettype_;
    property matrix: IJSSVGMatrix read _Getmatrix;
    property angle: Single read _Getangle;
  end;

  { --------------------------------------------------------------------
    TJSDOMMatrixReadOnly
    --------------------------------------------------------------------}

  TDoubleDynArray = IJSArray; // array of Double

  IJSDOMMatrixReadOnly = interface(IJSObject)
    ['{D78C1B1E-BDAF-3216-AF7C-21900C8FCBFC}']
    function _Geta: Double;
    function _Getb: Double;
    function _Getc: Double;
    function _Getd: Double;
    function _Gete: Double;
    function _Getf: Double;
    function _Getm11: Double;
    function _Getm12: Double;
    function _Getm13: Double;
    function _Getm14: Double;
    function _Getm21: Double;
    function _Getm22: Double;
    function _Getm23: Double;
    function _Getm24: Double;
    function _Getm31: Double;
    function _Getm32: Double;
    function _Getm33: Double;
    function _Getm34: Double;
    function _Getm41: Double;
    function _Getm42: Double;
    function _Getm43: Double;
    function _Getm44: Double;
    function _Getis2D: Boolean;
    function _GetisIdentity: Boolean;
    function fromMatrix(const aOther: TJSDOMMatrixInit): IJSDOMMatrixReadOnly; overload;
    function fromMatrix: IJSDOMMatrixReadOnly; overload;
    function translate(aTx: Double; aTy: Double; aTz: Double): IJSDOMMatrix; overload;
    function translate: IJSDOMMatrix; overload;
    function translate(aTx: Double): IJSDOMMatrix; overload;
    function translate(aTx: Double; aTy: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scale: IJSDOMMatrix; overload;
    function scale(aScaleX: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function scaleNonUniform(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
    function scaleNonUniform: IJSDOMMatrix; overload;
    function scaleNonUniform(aScaleX: Double): IJSDOMMatrix; overload;
    function scale3d(aScale: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scale3d: IJSDOMMatrix; overload;
    function scale3d(aScale: Double): IJSDOMMatrix; overload;
    function scale3d(aScale: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scale3d(aScale: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function rotate(aRotX: Double; aRotY: Double; aRotZ: Double): IJSDOMMatrix; overload;
    function rotate: IJSDOMMatrix; overload;
    function rotate(aRotX: Double): IJSDOMMatrix; overload;
    function rotate(aRotX: Double; aRotY: Double): IJSDOMMatrix; overload;
    function rotateFromVector(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateFromVector: IJSDOMMatrix; overload;
    function rotateFromVector(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double; aY: Double; aZ: Double; angle: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle: IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double; aY: Double; aZ: Double): IJSDOMMatrix; overload;
    function skewX(aSx: Double): IJSDOMMatrix; overload;
    function skewX: IJSDOMMatrix; overload;
    function skewY(aSy: Double): IJSDOMMatrix; overload;
    function skewY: IJSDOMMatrix; overload;
    function multiply(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function multiply: IJSDOMMatrix; overload;
    function flipX: IJSDOMMatrix;
    function flipY: IJSDOMMatrix;
    function inverse: IJSDOMMatrix;
    function transformPoint(const aPoint: TJSDOMPointInit): IJSDOMPoint; overload;
    function transformPoint: IJSDOMPoint; overload;
    function toJSON: IJSObject;
    property a: Double read _Geta;
    property b: Double read _Getb;
    property c: Double read _Getc;
    property d: Double read _Getd;
    property e: Double read _Gete;
    property f: Double read _Getf;
    property m11: Double read _Getm11;
    property m12: Double read _Getm12;
    property m13: Double read _Getm13;
    property m14: Double read _Getm14;
    property m21: Double read _Getm21;
    property m22: Double read _Getm22;
    property m23: Double read _Getm23;
    property m24: Double read _Getm24;
    property m31: Double read _Getm31;
    property m32: Double read _Getm32;
    property m33: Double read _Getm33;
    property m34: Double read _Getm34;
    property m41: Double read _Getm41;
    property m42: Double read _Getm42;
    property m43: Double read _Getm43;
    property m44: Double read _Getm44;
    property is2D: Boolean read _Getis2D;
    property isIdentity: Boolean read _GetisIdentity;
  end;

  TJSDOMMatrixReadOnly = class(TJSObject,IJSDOMMatrixReadOnly)
  Private
    function _Geta: Double;
    function _Getb: Double;
    function _Getc: Double;
    function _Getd: Double;
    function _Gete: Double;
    function _Getf: Double;
    function _Getm11: Double;
    function _Getm12: Double;
    function _Getm13: Double;
    function _Getm14: Double;
    function _Getm21: Double;
    function _Getm22: Double;
    function _Getm23: Double;
    function _Getm24: Double;
    function _Getm31: Double;
    function _Getm32: Double;
    function _Getm33: Double;
    function _Getm34: Double;
    function _Getm41: Double;
    function _Getm42: Double;
    function _Getm43: Double;
    function _Getm44: Double;
    function _Getis2D: Boolean;
    function _GetisIdentity: Boolean;
  Public
    function fromMatrix(const aOther: TJSDOMMatrixInit): IJSDOMMatrixReadOnly; overload;
    function fromMatrix: IJSDOMMatrixReadOnly; overload;
    function translate(aTx: Double; aTy: Double; aTz: Double): IJSDOMMatrix; overload;
    function translate: IJSDOMMatrix; overload;
    function translate(aTx: Double): IJSDOMMatrix; overload;
    function translate(aTx: Double; aTy: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scale: IJSDOMMatrix; overload;
    function scale(aScaleX: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function scaleNonUniform(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
    function scaleNonUniform: IJSDOMMatrix; overload;
    function scaleNonUniform(aScaleX: Double): IJSDOMMatrix; overload;
    function scale3d(aScale: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scale3d: IJSDOMMatrix; overload;
    function scale3d(aScale: Double): IJSDOMMatrix; overload;
    function scale3d(aScale: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scale3d(aScale: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function rotate(aRotX: Double; aRotY: Double; aRotZ: Double): IJSDOMMatrix; overload;
    function rotate: IJSDOMMatrix; overload;
    function rotate(aRotX: Double): IJSDOMMatrix; overload;
    function rotate(aRotX: Double; aRotY: Double): IJSDOMMatrix; overload;
    function rotateFromVector(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateFromVector: IJSDOMMatrix; overload;
    function rotateFromVector(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double; aY: Double; aZ: Double; angle: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle: IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateAxisAngle(aX: Double; aY: Double; aZ: Double): IJSDOMMatrix; overload;
    function skewX(aSx: Double): IJSDOMMatrix; overload;
    function skewX: IJSDOMMatrix; overload;
    function skewY(aSy: Double): IJSDOMMatrix; overload;
    function skewY: IJSDOMMatrix; overload;
    function multiply(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function multiply: IJSDOMMatrix; overload;
    function flipX: IJSDOMMatrix;
    function flipY: IJSDOMMatrix;
    function inverse: IJSDOMMatrix;
    function transformPoint(const aPoint: TJSDOMPointInit): IJSDOMPoint; overload;
    function transformPoint: IJSDOMPoint; overload;
    function toJSON: IJSObject;
    class function Cast(Intf: IJSObject): IJSDOMMatrixReadOnly;
    property a: Double read _Geta;
    property b: Double read _Getb;
    property c: Double read _Getc;
    property d: Double read _Getd;
    property e: Double read _Gete;
    property f: Double read _Getf;
    property m11: Double read _Getm11;
    property m12: Double read _Getm12;
    property m13: Double read _Getm13;
    property m14: Double read _Getm14;
    property m21: Double read _Getm21;
    property m22: Double read _Getm22;
    property m23: Double read _Getm23;
    property m24: Double read _Getm24;
    property m31: Double read _Getm31;
    property m32: Double read _Getm32;
    property m33: Double read _Getm33;
    property m34: Double read _Getm34;
    property m41: Double read _Getm41;
    property m42: Double read _Getm42;
    property m43: Double read _Getm43;
    property m44: Double read _Getm44;
    property is2D: Boolean read _Getis2D;
    property isIdentity: Boolean read _GetisIdentity;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedRect
    --------------------------------------------------------------------}

  IJSSVGAnimatedRect = interface(IJSObject)
    ['{B2962ABA-4331-3AB3-A0EA-D97D5C54C856}']
    function _GetbaseVal: IJSSVGRect;
    function _GetanimVal: IJSSVGRect;
    property baseVal: IJSSVGRect read _GetbaseVal;
    property animVal: IJSSVGRect read _GetanimVal;
  end;

  TJSSVGAnimatedRect = class(TJSObject,IJSSVGAnimatedRect)
  Private
    function _GetbaseVal: IJSSVGRect;
    function _GetanimVal: IJSSVGRect;
  Public
    class function Cast(Intf: IJSObject): IJSSVGAnimatedRect;
    property baseVal: IJSSVGRect read _GetbaseVal;
    property animVal: IJSSVGRect read _GetanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedPreserveAspectRatio
    --------------------------------------------------------------------}

  IJSSVGAnimatedPreserveAspectRatio = interface(IJSObject)
    ['{44962271-B15F-365C-B6DA-BD378285DAB8}']
    function _GetbaseVal: IJSSVGPreserveAspectRatio;
    function _GetanimVal: IJSSVGPreserveAspectRatio;
    property baseVal: IJSSVGPreserveAspectRatio read _GetbaseVal;
    property animVal: IJSSVGPreserveAspectRatio read _GetanimVal;
  end;

  TJSSVGAnimatedPreserveAspectRatio = class(TJSObject,IJSSVGAnimatedPreserveAspectRatio)
  Private
    function _GetbaseVal: IJSSVGPreserveAspectRatio;
    function _GetanimVal: IJSSVGPreserveAspectRatio;
  Public
    class function Cast(Intf: IJSObject): IJSSVGAnimatedPreserveAspectRatio;
    property baseVal: IJSSVGPreserveAspectRatio read _GetbaseVal;
    property animVal: IJSSVGPreserveAspectRatio read _GetanimVal;
  end;

  { --------------------------------------------------------------------
    TJSDOMPointReadOnly
    --------------------------------------------------------------------}

  IJSDOMPointReadOnly = interface(IJSObject)
    ['{49D1CC78-8B87-3F26-88A0-472391BCC685}']
    function _Getx: Double;
    function _Gety: Double;
    function _Getz: Double;
    function _Getw: Double;
    function fromPoint(const aOther: TJSDOMPointInit): IJSDOMPointReadOnly; overload;
    function fromPoint: IJSDOMPointReadOnly; overload;
    function matrixTransform(const aMatrix: TJSDOMMatrixInit): IJSDOMPoint; overload;
    function matrixTransform: IJSDOMPoint; overload;
    function toJSON: IJSObject;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property z: Double read _Getz;
    property w: Double read _Getw;
  end;

  TJSDOMPointReadOnly = class(TJSObject,IJSDOMPointReadOnly)
  Private
    function _Getx: Double;
    function _Gety: Double;
    function _Getz: Double;
    function _Getw: Double;
  Public
    function fromPoint(const aOther: TJSDOMPointInit): IJSDOMPointReadOnly; overload;
    function fromPoint: IJSDOMPointReadOnly; overload;
    function matrixTransform(const aMatrix: TJSDOMMatrixInit): IJSDOMPoint; overload;
    function matrixTransform: IJSDOMPoint; overload;
    function toJSON: IJSObject;
    class function Cast(Intf: IJSObject): IJSDOMPointReadOnly;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property z: Double read _Getz;
    property w: Double read _Getw;
  end;

  { --------------------------------------------------------------------
    TJSSVGPreserveAspectRatio
    --------------------------------------------------------------------}

  IJSSVGPreserveAspectRatio = interface(IJSObject)
    ['{41805C95-BA57-3EAC-BAD5-9BEDE774631D}']
    function _Getalign: Word;
    function _GetmeetOrSlice: Word;
    procedure _Setalign(const aValue: Word);
    procedure _SetmeetOrSlice(const aValue: Word);
    property align: Word read _Getalign write _Setalign;
    property meetOrSlice: Word read _GetmeetOrSlice write _SetmeetOrSlice;
  end;

  TJSSVGPreserveAspectRatio = class(TJSObject,IJSSVGPreserveAspectRatio)
  Private
    function _Getalign: Word;
    function _GetmeetOrSlice: Word;
    procedure _Setalign(const aValue: Word);
    procedure _SetmeetOrSlice(const aValue: Word);
  Public
    Const
      SVG_PRESERVEASPECTRATIO_UNKNOWN = 0;
      SVG_PRESERVEASPECTRATIO_NONE = 1;
      SVG_PRESERVEASPECTRATIO_XMINYMIN = 2;
      SVG_PRESERVEASPECTRATIO_XMIDYMIN = 3;
      SVG_PRESERVEASPECTRATIO_XMAXYMIN = 4;
      SVG_PRESERVEASPECTRATIO_XMINYMID = 5;
      SVG_PRESERVEASPECTRATIO_XMIDYMID = 6;
      SVG_PRESERVEASPECTRATIO_XMAXYMID = 7;
      SVG_PRESERVEASPECTRATIO_XMINYMAX = 8;
      SVG_PRESERVEASPECTRATIO_XMIDYMAX = 9;
      SVG_PRESERVEASPECTRATIO_XMAXYMAX = 10;
      SVG_MEETORSLICE_UNKNOWN = 0;
      SVG_MEETORSLICE_MEET = 1;
      SVG_MEETORSLICE_SLICE = 2;
  Public
    class function Cast(Intf: IJSObject): IJSSVGPreserveAspectRatio;
    property align: Word read _Getalign write _Setalign;
    property meetOrSlice: Word read _GetmeetOrSlice write _SetmeetOrSlice;
  end;

  { --------------------------------------------------------------------
    TJSCSSRuleList
    --------------------------------------------------------------------}

  IJSCSSRuleList = interface(IJSObject)
    ['{E624E6AC-6687-33FE-BDF6-3619147BDBAA}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): IJSCSSRule;
    property length_: LongWord read _Getlength_;
  end;

  TJSCSSRuleList = class(TJSObject,IJSCSSRuleList)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): IJSCSSRule;
    class function Cast(Intf: IJSObject): IJSCSSRuleList;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSSVGTests
    --------------------------------------------------------------------}

  IJSSVGTests = interface(IJSObject)
    ['{69002D3F-3EEF-3016-B1C9-D9DA0E0677C1}']
    function _GetrequiredExtensions: IJSSVGStringList;
    function _GetsystemLanguage: IJSSVGStringList;
    property requiredExtensions: IJSSVGStringList read _GetrequiredExtensions;
    property systemLanguage: IJSSVGStringList read _GetsystemLanguage;
  end;

  TJSSVGTests = class(TJSObject,IJSSVGTests)
  Private
    function _GetrequiredExtensions: IJSSVGStringList;
    function _GetsystemLanguage: IJSSVGStringList;
  Public
    class function Cast(Intf: IJSObject): IJSSVGTests;
    property requiredExtensions: IJSSVGStringList read _GetrequiredExtensions;
    property systemLanguage: IJSSVGStringList read _GetsystemLanguage;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedTransformList
    --------------------------------------------------------------------}

  IJSSVGAnimatedTransformList = interface(IJSObject)
    ['{3B9EC5A6-DEE9-35F0-8F16-C6C5355A8D49}']
    function _GetbaseVal: IJSSVGTransformList;
    function _GetanimVal: IJSSVGTransformList;
    property baseVal: IJSSVGTransformList read _GetbaseVal;
    property animVal: IJSSVGTransformList read _GetanimVal;
  end;

  TJSSVGAnimatedTransformList = class(TJSObject,IJSSVGAnimatedTransformList)
  Private
    function _GetbaseVal: IJSSVGTransformList;
    function _GetanimVal: IJSSVGTransformList;
  Public
    class function Cast(Intf: IJSObject): IJSSVGAnimatedTransformList;
    property baseVal: IJSSVGTransformList read _GetbaseVal;
    property animVal: IJSSVGTransformList read _GetanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGStringList
    --------------------------------------------------------------------}

  IJSSVGStringList = interface(IJSObject)
    ['{46F7C8E7-CDFA-372C-BF64-C56531AD54E8}']
    function _Getlength_: LongWord;
    function _GetnumberOfItems: LongWord;
    procedure clear;
    function initialize(const aNewItem: UnicodeString): UnicodeString;
    function getItem(aIndex: LongWord): UnicodeString;
    function insertItemBefore(const aNewItem: UnicodeString; aIndex: LongWord): UnicodeString;
    function replaceItem(const aNewItem: UnicodeString; aIndex: LongWord): UnicodeString;
    function removeItem(aIndex: LongWord): UnicodeString;
    function appendItem(const aNewItem: UnicodeString): UnicodeString;
    property length_: LongWord read _Getlength_;
    property numberOfItems: LongWord read _GetnumberOfItems;
  end;

  TJSSVGStringList = class(TJSObject,IJSSVGStringList)
  Private
    function _Getlength_: LongWord;
    function _GetnumberOfItems: LongWord;
  Public
    procedure clear;
    function initialize(const aNewItem: UnicodeString): UnicodeString;
    function getItem(aIndex: LongWord): UnicodeString;
    function insertItemBefore(const aNewItem: UnicodeString; aIndex: LongWord): UnicodeString;
    function replaceItem(const aNewItem: UnicodeString; aIndex: LongWord): UnicodeString;
    function removeItem(aIndex: LongWord): UnicodeString;
    function appendItem(const aNewItem: UnicodeString): UnicodeString;
    class function Cast(Intf: IJSObject): IJSSVGStringList;
    property length_: LongWord read _Getlength_;
    property numberOfItems: LongWord read _GetnumberOfItems;
  end;

  { --------------------------------------------------------------------
    TJSSVGTransformList
    --------------------------------------------------------------------}

  IJSSVGTransformList = interface(IJSObject)
    ['{3DA3974F-E49D-353A-9673-7EB697C6B3D5}']
    function _GetnumberOfItems: LongWord;
    function _Getlength_: LongWord;
    procedure clear;
    function initialize(aNewItem: IJSSVGTransform): IJSSVGTransform;
    function getItem(aIndex: LongWord): IJSSVGTransform;
    function insertItemBefore(aNewItem: IJSSVGTransform; aIndex: LongWord): IJSSVGTransform;
    function replaceItem(aNewItem: IJSSVGTransform; aIndex: LongWord): IJSSVGTransform;
    function removeItem(aIndex: LongWord): IJSSVGTransform;
    function appendItem(aNewItem: IJSSVGTransform): IJSSVGTransform;
    function createSVGTransformFromMatrix(const aMatrix: TJSDOMMatrix2DInit): IJSSVGTransform; overload;
    function createSVGTransformFromMatrix: IJSSVGTransform; overload;
    function consolidate: IJSSVGTransform;
    property numberOfItems: LongWord read _GetnumberOfItems;
    property length_: LongWord read _Getlength_;
  end;

  TJSSVGTransformList = class(TJSObject,IJSSVGTransformList)
  Private
    function _GetnumberOfItems: LongWord;
    function _Getlength_: LongWord;
  Public
    procedure clear;
    function initialize(aNewItem: IJSSVGTransform): IJSSVGTransform;
    function getItem(aIndex: LongWord): IJSSVGTransform;
    function insertItemBefore(aNewItem: IJSSVGTransform; aIndex: LongWord): IJSSVGTransform;
    function replaceItem(aNewItem: IJSSVGTransform; aIndex: LongWord): IJSSVGTransform;
    function removeItem(aIndex: LongWord): IJSSVGTransform;
    function appendItem(aNewItem: IJSSVGTransform): IJSSVGTransform;
    function createSVGTransformFromMatrix(const aMatrix: TJSDOMMatrix2DInit): IJSSVGTransform; overload;
    function createSVGTransformFromMatrix: IJSSVGTransform; overload;
    function consolidate: IJSSVGTransform;
    class function Cast(Intf: IJSObject): IJSSVGTransformList;
    property numberOfItems: LongWord read _GetnumberOfItems;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedString
    --------------------------------------------------------------------}

  IJSSVGAnimatedString = interface(IJSObject)
    ['{B9AF4D0A-E767-3530-8828-4F500844615C}']
    function _GetbaseVal: UnicodeString;
    function _GetanimVal: UnicodeString;
    procedure _SetbaseVal(const aValue: UnicodeString);
    property baseVal: UnicodeString read _GetbaseVal write _SetbaseVal;
    property animVal: UnicodeString read _GetanimVal;
  end;

  TJSSVGAnimatedString = class(TJSObject,IJSSVGAnimatedString)
  Private
    function _GetbaseVal: UnicodeString;
    function _GetanimVal: UnicodeString;
    procedure _SetbaseVal(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSSVGAnimatedString;
    property baseVal: UnicodeString read _GetbaseVal write _SetbaseVal;
    property animVal: UnicodeString read _GetanimVal;
  end;

  { --------------------------------------------------------------------
    TJSStyleSheet
    --------------------------------------------------------------------}

  IJSStyleSheet = interface(IJSObject)
    ['{E7A9BF11-C980-3DDE-817D-DE4231838EF9}']
    function _Gettype_: UnicodeString;
    function _Gethref: UnicodeString;
    function _GetownerNode: IJSNode;
    function _GetparentStyleSheet: IJSStyleSheet;
    function _Gettitle: UnicodeString;
    function _Getdisabled: Boolean;
    function _GetsourceMapURL: UnicodeString;
    function _GetsourceURL: UnicodeString;
    function _GetassociatedDocument: IJSDocument;
    function _Getconstructed: Boolean;
    procedure _Setdisabled(const aValue: Boolean);
    property type_: UnicodeString read _Gettype_;
    property href: UnicodeString read _Gethref;
    property ownerNode: IJSNode read _GetownerNode;
    property parentStyleSheet: IJSStyleSheet read _GetparentStyleSheet;
    property title: UnicodeString read _Gettitle;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property sourceMapURL: UnicodeString read _GetsourceMapURL;
    property sourceURL: UnicodeString read _GetsourceURL;
    property associatedDocument: IJSDocument read _GetassociatedDocument;
    property constructed: Boolean read _Getconstructed;
  end;

  TJSStyleSheet = class(TJSObject,IJSStyleSheet)
  Private
    function _Gettype_: UnicodeString;
    function _Gethref: UnicodeString;
    function _GetownerNode: IJSNode;
    function _GetparentStyleSheet: IJSStyleSheet;
    function _Gettitle: UnicodeString;
    function _Getdisabled: Boolean;
    function _GetsourceMapURL: UnicodeString;
    function _GetsourceURL: UnicodeString;
    function _GetassociatedDocument: IJSDocument;
    function _Getconstructed: Boolean;
    procedure _Setdisabled(const aValue: Boolean);
  Public
    class function Cast(Intf: IJSObject): IJSStyleSheet;
    property type_: UnicodeString read _Gettype_;
    property href: UnicodeString read _Gethref;
    property ownerNode: IJSNode read _GetownerNode;
    property parentStyleSheet: IJSStyleSheet read _GetparentStyleSheet;
    property title: UnicodeString read _Gettitle;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property sourceMapURL: UnicodeString read _GetsourceMapURL;
    property sourceURL: UnicodeString read _GetsourceURL;
    property associatedDocument: IJSDocument read _GetassociatedDocument;
    property constructed: Boolean read _Getconstructed;
  end;

  { --------------------------------------------------------------------
    TJSMozCanvasPrintState
    --------------------------------------------------------------------}

  IJSMozCanvasPrintState = interface(IJSObject)
    ['{0973F0F6-9576-39BE-B3A3-3A3F6F051088}']
    function _Getcontext: IJSnsISupports;
    procedure done;
    property context: IJSnsISupports read _Getcontext;
  end;

  TJSMozCanvasPrintState = class(TJSObject,IJSMozCanvasPrintState)
  Private
    function _Getcontext: IJSnsISupports;
  Public
    procedure done;
    class function Cast(Intf: IJSObject): IJSMozCanvasPrintState;
    property context: IJSnsISupports read _Getcontext;
  end;

  { --------------------------------------------------------------------
    TJSimgINotificationObserver
    --------------------------------------------------------------------}

  IJSimgINotificationObserver = interface(IJSObject)
    ['{804BD3B3-27C0-39AC-A0EE-8EC1D6DBBD61}']
  end;

  TJSimgINotificationObserver = class(TJSObject,IJSimgINotificationObserver)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSimgINotificationObserver;
  end;

  { --------------------------------------------------------------------
    TJSimgIRequest
    --------------------------------------------------------------------}

  IJSimgIRequest = interface(IJSObject)
    ['{EEE11C67-DB51-35B9-943D-1A2372542131}']
  end;

  TJSimgIRequest = class(TJSObject,IJSimgIRequest)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSimgIRequest;
  end;

  { --------------------------------------------------------------------
    TJSnsIStreamListener
    --------------------------------------------------------------------}

  IJSnsIStreamListener = interface(IJSObject)
    ['{2B7FAA73-2682-300B-9709-981130F8698D}']
  end;

  TJSnsIStreamListener = class(TJSObject,IJSnsIStreamListener)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIStreamListener;
  end;

  { --------------------------------------------------------------------
    TJSMozImageLoadingContent
    --------------------------------------------------------------------}

  IJSMozImageLoadingContent = interface(IJSObject)
    ['{F6888F9D-886B-3DB9-8E2F-7BE3FFB95EA3}']
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _SetloadingEnabled(const aValue: Boolean);
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  TJSMozImageLoadingContent = class(TJSObject,IJSMozImageLoadingContent)
  Private
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _SetloadingEnabled(const aValue: Boolean);
  Public
    Const
      UNKNOWN_REQUEST = -1;
      CURRENT_REQUEST = 0;
      PENDING_REQUEST = 1;
  Public
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    class function Cast(Intf: IJSObject): IJSMozImageLoadingContent;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  { --------------------------------------------------------------------
    TJSMozEditableElement
    --------------------------------------------------------------------}

  IJSMozEditableElement = interface(IJSObject)
    ['{50EC1C1F-BFC6-365C-B0FF-6728A0716E61}']
    function _Geteditor: IJSnsIEditor;
    function _GethasEditor: Boolean;
    function _GetisInputEventTarget: Boolean;
    procedure setUserInput(const aInput: UnicodeString);
    property editor: IJSnsIEditor read _Geteditor;
    property hasEditor: Boolean read _GethasEditor;
    property isInputEventTarget: Boolean read _GetisInputEventTarget;
  end;

  TJSMozEditableElement = class(TJSObject,IJSMozEditableElement)
  Private
    function _Geteditor: IJSnsIEditor;
    function _GethasEditor: Boolean;
    function _GetisInputEventTarget: Boolean;
  Public
    procedure setUserInput(const aInput: UnicodeString);
    class function Cast(Intf: IJSObject): IJSMozEditableElement;
    property editor: IJSnsIEditor read _Geteditor;
    property hasEditor: Boolean read _GethasEditor;
    property isInputEventTarget: Boolean read _GetisInputEventTarget;
  end;

  { --------------------------------------------------------------------
    TJSnsIEditor
    --------------------------------------------------------------------}

  IJSnsIEditor = interface(IJSObject)
    ['{33832E51-39BA-3530-B869-8DD15390C552}']
  end;

  TJSnsIEditor = class(TJSObject,IJSnsIEditor)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIEditor;
  end;

  { --------------------------------------------------------------------
    TJSHTMLHyperlinkElementUtils
    --------------------------------------------------------------------}

  IJSHTMLHyperlinkElementUtils = interface(IJSObject)
    ['{2BB8EFD6-9D34-31D0-96D7-879D3888DBD6}']
    function _Gethref: UnicodeString;
    function _Getorigin: UnicodeString;
    function _Getprotocol: UnicodeString;
    function _Getusername: UnicodeString;
    function _Getpassword: UnicodeString;
    function _Gethost: UnicodeString;
    function _Gethostname: UnicodeString;
    function _Getport: UnicodeString;
    function _Getpathname: UnicodeString;
    function _Getsearch: UnicodeString;
    function _Gethash: UnicodeString;
    procedure _Sethref(const aValue: UnicodeString);
    procedure _Setprotocol(const aValue: UnicodeString);
    procedure _Setusername(const aValue: UnicodeString);
    procedure _Setpassword(const aValue: UnicodeString);
    procedure _Sethost(const aValue: UnicodeString);
    procedure _Sethostname(const aValue: UnicodeString);
    procedure _Setport(const aValue: UnicodeString);
    procedure _Setpathname(const aValue: UnicodeString);
    procedure _Setsearch(const aValue: UnicodeString);
    procedure _Sethash(const aValue: UnicodeString);
    property href: UnicodeString read _Gethref write _Sethref;
    property origin: UnicodeString read _Getorigin;
    property protocol: UnicodeString read _Getprotocol write _Setprotocol;
    property username: UnicodeString read _Getusername write _Setusername;
    property password: UnicodeString read _Getpassword write _Setpassword;
    property host: UnicodeString read _Gethost write _Sethost;
    property hostname: UnicodeString read _Gethostname write _Sethostname;
    property port: UnicodeString read _Getport write _Setport;
    property pathname: UnicodeString read _Getpathname write _Setpathname;
    property search: UnicodeString read _Getsearch write _Setsearch;
    property hash: UnicodeString read _Gethash write _Sethash;
  end;

  TJSHTMLHyperlinkElementUtils = class(TJSObject,IJSHTMLHyperlinkElementUtils)
  Private
    function _Gethref: UnicodeString;
    function _Getorigin: UnicodeString;
    function _Getprotocol: UnicodeString;
    function _Getusername: UnicodeString;
    function _Getpassword: UnicodeString;
    function _Gethost: UnicodeString;
    function _Gethostname: UnicodeString;
    function _Getport: UnicodeString;
    function _Getpathname: UnicodeString;
    function _Getsearch: UnicodeString;
    function _Gethash: UnicodeString;
    procedure _Sethref(const aValue: UnicodeString);
    procedure _Setprotocol(const aValue: UnicodeString);
    procedure _Setusername(const aValue: UnicodeString);
    procedure _Setpassword(const aValue: UnicodeString);
    procedure _Sethost(const aValue: UnicodeString);
    procedure _Sethostname(const aValue: UnicodeString);
    procedure _Setport(const aValue: UnicodeString);
    procedure _Setpathname(const aValue: UnicodeString);
    procedure _Setsearch(const aValue: UnicodeString);
    procedure _Sethash(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLHyperlinkElementUtils;
    property href: UnicodeString read _Gethref write _Sethref;
    property origin: UnicodeString read _Getorigin;
    property protocol: UnicodeString read _Getprotocol write _Setprotocol;
    property username: UnicodeString read _Getusername write _Setusername;
    property password: UnicodeString read _Getpassword write _Setpassword;
    property host: UnicodeString read _Gethost write _Sethost;
    property hostname: UnicodeString read _Gethostname write _Sethostname;
    property port: UnicodeString read _Getport write _Setport;
    property pathname: UnicodeString read _Getpathname write _Setpathname;
    property search: UnicodeString read _Getsearch write _Setsearch;
    property hash: UnicodeString read _Gethash write _Sethash;
  end;

  { --------------------------------------------------------------------
    TJSLinkStyle
    --------------------------------------------------------------------}

  IJSLinkStyle = interface(IJSObject)
    ['{97221DEA-E812-3439-9DAE-0736A964A821}']
    function _Getsheet: IJSStyleSheet;
    property sheet: IJSStyleSheet read _Getsheet;
  end;

  TJSLinkStyle = class(TJSObject,IJSLinkStyle)
  Private
    function _Getsheet: IJSStyleSheet;
  Public
    class function Cast(Intf: IJSObject): IJSLinkStyle;
    property sheet: IJSStyleSheet read _Getsheet;
  end;

  { --------------------------------------------------------------------
    TJSBlob
    --------------------------------------------------------------------}

  TBlobPartDynArray = IJSArray; // array of TBlobPart

  IJSBlob = interface(IJSObject)
    ['{3DBE6824-1548-3683-832A-6E8BE1C6CFAC}']
    function _Getsize: QWord;
    function _Gettype_: UnicodeString;
    function _GetblobImplType: UnicodeString;
    function slice(aStart: Int64; aEnd_: Int64; const aContentType: UnicodeString): IJSBlob; overload;
    function slice: IJSBlob; overload;
    function slice(aStart: Int64): IJSBlob; overload;
    function slice(aStart: Int64; aEnd_: Int64): IJSBlob; overload;
    property size: QWord read _Getsize;
    property type_: UnicodeString read _Gettype_;
    property blobImplType: UnicodeString read _GetblobImplType;
  end;

  TJSBlob = class(TJSObject,IJSBlob)
  Private
    function _Getsize: QWord;
    function _Gettype_: UnicodeString;
    function _GetblobImplType: UnicodeString;
  Public
    function slice(aStart: Int64; aEnd_: Int64; const aContentType: UnicodeString): IJSBlob; overload;
    function slice: IJSBlob; overload;
    function slice(aStart: Int64): IJSBlob; overload;
    function slice(aStart: Int64; aEnd_: Int64): IJSBlob; overload;
    class function Cast(Intf: IJSObject): IJSBlob;
    property size: QWord read _Getsize;
    property type_: UnicodeString read _Gettype_;
    property blobImplType: UnicodeString read _GetblobImplType;
  end;

  { --------------------------------------------------------------------
    TJSFileList
    --------------------------------------------------------------------}

  IJSFileList = interface(IJSObject)
    ['{2095EC3F-96FF-3E7A-9A09-8A3EED5244FC}']
    function _Getlength_: LongWord;
    function item(aIndex: LongWord): IJSFile;
    property length_: LongWord read _Getlength_;
  end;

  TJSFileList = class(TJSObject,IJSFileList)
  Private
    function _Getlength_: LongWord;
  Public
    function item(aIndex: LongWord): IJSFile;
    class function Cast(Intf: IJSObject): IJSFileList;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSnsIFile
    --------------------------------------------------------------------}

  IJSnsIFile = interface(IJSObject)
    ['{BE81BA80-7C79-33E0-B134-445241194965}']
  end;

  TJSnsIFile = class(TJSObject,IJSnsIFile)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSnsIFile;
  end;

  { --------------------------------------------------------------------
    TJSFileSystem
    --------------------------------------------------------------------}

  IJSFileSystem = interface(IJSObject)
    ['{BEA93F0E-4CFF-38D5-9199-0FD0CD2CDA94}']
    function _Getname: UnicodeString;
    function _Getroot: IJSFileSystemDirectoryEntry;
    property name: UnicodeString read _Getname;
    property root: IJSFileSystemDirectoryEntry read _Getroot;
  end;

  TJSFileSystem = class(TJSObject,IJSFileSystem)
  Private
    function _Getname: UnicodeString;
    function _Getroot: IJSFileSystemDirectoryEntry;
  Public
    class function Cast(Intf: IJSObject): IJSFileSystem;
    property name: UnicodeString read _Getname;
    property root: IJSFileSystemDirectoryEntry read _Getroot;
  end;

  { --------------------------------------------------------------------
    TJSFileSystemEntry
    --------------------------------------------------------------------}

  IJSFileSystemEntry = interface(IJSObject)
    ['{9755D0ED-37BB-3D15-B104-4C1E1A522F9B}']
    function _GetisFile: Boolean;
    function _GetisDirectory: Boolean;
    function _Getname: UnicodeString;
    function _GetfullPath: UnicodeString;
    function _Getfilesystem: IJSFileSystem;
    property isFile: Boolean read _GetisFile;
    property isDirectory: Boolean read _GetisDirectory;
    property name: UnicodeString read _Getname;
    property fullPath: UnicodeString read _GetfullPath;
    property filesystem: IJSFileSystem read _Getfilesystem;
  end;

  TJSFileSystemEntry = class(TJSObject,IJSFileSystemEntry)
  Private
    function _GetisFile: Boolean;
    function _GetisDirectory: Boolean;
    function _Getname: UnicodeString;
    function _GetfullPath: UnicodeString;
    function _Getfilesystem: IJSFileSystem;
  Public
    class function Cast(Intf: IJSObject): IJSFileSystemEntry;
    property isFile: Boolean read _GetisFile;
    property isDirectory: Boolean read _GetisDirectory;
    property name: UnicodeString read _Getname;
    property fullPath: UnicodeString read _GetfullPath;
    property filesystem: IJSFileSystem read _Getfilesystem;
  end;

  { --------------------------------------------------------------------
    TJSImageBitmap
    --------------------------------------------------------------------}

  IJSImageBitmap = interface(IJSObject)
    ['{B4A84226-588D-3BB9-A131-BECD55F1BB1D}']
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    procedure close;
    property width: LongWord read _Getwidth;
    property height: LongWord read _Getheight;
  end;

  TJSImageBitmap = class(TJSObject,IJSImageBitmap)
  Private
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
  Public
    procedure close;
    class function Cast(Intf: IJSObject): IJSImageBitmap;
    property width: LongWord read _Getwidth;
    property height: LongWord read _Getheight;
  end;

  { --------------------------------------------------------------------
    TJSFileSystemDirectoryReader
    --------------------------------------------------------------------}

  IJSFileSystemDirectoryReader = interface(IJSObject)
    ['{EB5A8CA5-80B5-3116-A2E4-19053F93DA75}']
  end;

  TJSFileSystemDirectoryReader = class(TJSObject,IJSFileSystemDirectoryReader)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSFileSystemDirectoryReader;
  end;

  { --------------------------------------------------------------------
    TJSFormData
    --------------------------------------------------------------------}

  TFormDataEntryValueDynArray = IJSArray; // array of TFormDataEntryValue

  IJSFormData = interface(IJSObject)
    ['{2E99E2FA-2016-3D80-95F1-0B04E8B02321}']
    procedure append(const aName: UnicodeString; aValue: IJSBlob; const aFilename: UnicodeString); overload;
    procedure append(const aName: UnicodeString; aValue: IJSBlob); overload;
    procedure append(const aName: UnicodeString; const aValue: UnicodeString);
    procedure delete(const aName: UnicodeString);
    function get(const aName: UnicodeString): TFormDataEntryValue;
    function getAll(const aName: UnicodeString): TFormDataEntryValueDynArray;
    function has(const aName: UnicodeString): Boolean;
    procedure set_(const aName: UnicodeString; aValue: IJSBlob; const aFilename: UnicodeString); overload;
    procedure set_(const aName: UnicodeString; aValue: IJSBlob); overload;
    procedure set_(const aName: UnicodeString; const aValue: UnicodeString);
  end;

  TJSFormData = class(TJSObject,IJSFormData)
  Private
  Public
    procedure append(const aName: UnicodeString; aValue: IJSBlob; const aFilename: UnicodeString); overload;
    procedure append(const aName: UnicodeString; aValue: IJSBlob); overload;
    procedure append(const aName: UnicodeString; const aValue: UnicodeString);
    procedure delete(const aName: UnicodeString);
    function get(const aName: UnicodeString): TFormDataEntryValue;
    function getAll(const aName: UnicodeString): TFormDataEntryValueDynArray;
    function has(const aName: UnicodeString): Boolean;
    procedure set_(const aName: UnicodeString; aValue: IJSBlob; const aFilename: UnicodeString); overload;
    procedure set_(const aName: UnicodeString; aValue: IJSBlob); overload;
    procedure set_(const aName: UnicodeString; const aValue: UnicodeString);
    class function Cast(Intf: IJSObject): IJSFormData;
  end;

  { --------------------------------------------------------------------
    TJSNode
    --------------------------------------------------------------------}

  IJSNode = interface(IJSEventTarget)
    ['{14C2BA88-3B6E-38A7-B7BB-309263ADD6A5}']
    function _GetnodeType: Word;
    function _GetnodeName: UnicodeString;
    function _GetbaseURI: UnicodeString;
    function _GetisConnected: Boolean;
    function _GetparentNode: IJSNode;
    function _GetfirstChild: IJSNode;
    function _GetlastChild: IJSNode;
    function _GetpreviousSibling: IJSNode;
    function _GetnextSibling: IJSNode;
    function _GetnodeValue: UnicodeString;
    function _GettextContent: UnicodeString;
    procedure _SetnodeValue(const aValue: UnicodeString);
    procedure _SettextContent(const aValue: UnicodeString);
    function getRootNode(const aOptions: TJSGetRootNodeOptions): IJSNode; overload;
    function getRootNode: IJSNode; overload;
    function hasChildNodes: Boolean;
    procedure normalize;
    function cloneNode(aDeep: Boolean): IJSNode; overload;
    function cloneNode: IJSNode; overload;
    function isEqualNode(aOtherNode: IJSNode): Boolean;
    function isSameNode(aOtherNode: IJSNode): Boolean;
    function compareDocumentPosition(aOther: IJSNode): Word;
    function contains(aOther: IJSNode): Boolean;
    function lookupPrefix(const aNamespace: UnicodeString): UnicodeString;
    function lookupNamespaceURI(const aPrefix: UnicodeString): UnicodeString;
    function isDefaultNamespace(const aNamespace: UnicodeString): Boolean;
    function insertBefore(aNode: IJSNode; aChild: IJSNode): IJSNode;
    function appendChild(aNode: IJSNode): IJSNode;
    function replaceChild(aNode: IJSNode; aChild: IJSNode): IJSNode;
    function removeChild(aChild: IJSNode): IJSNode;
    property nodeType: Word read _GetnodeType;
    property nodeName: UnicodeString read _GetnodeName;
    property baseURI: UnicodeString read _GetbaseURI;
    property isConnected: Boolean read _GetisConnected;
    property parentNode: IJSNode read _GetparentNode;
    property firstChild: IJSNode read _GetfirstChild;
    property lastChild: IJSNode read _GetlastChild;
    property previousSibling: IJSNode read _GetpreviousSibling;
    property nextSibling: IJSNode read _GetnextSibling;
    property nodeValue: UnicodeString read _GetnodeValue write _SetnodeValue;
    property textContent: UnicodeString read _GettextContent write _SettextContent;
  end;

  TJSNode = class(TJSEventTarget,IJSNode)
  Private
    function _GetnodeType: Word;
    function _GetnodeName: UnicodeString;
    function _GetbaseURI: UnicodeString;
    function _GetisConnected: Boolean;
    function _GetparentNode: IJSNode;
    function _GetfirstChild: IJSNode;
    function _GetlastChild: IJSNode;
    function _GetpreviousSibling: IJSNode;
    function _GetnextSibling: IJSNode;
    function _GetnodeValue: UnicodeString;
    function _GettextContent: UnicodeString;
    procedure _SetnodeValue(const aValue: UnicodeString);
    procedure _SettextContent(const aValue: UnicodeString);
  Public
    Const
      ELEMENT_NODE = 1;
      ATTRIBUTE_NODE = 2;
      TEXT_NODE = 3;
      CDATA_SECTION_NODE = 4;
      ENTITY_REFERENCE_NODE = 5;
      ENTITY_NODE = 6;
      PROCESSING_INSTRUCTION_NODE = 7;
      COMMENT_NODE = 8;
      DOCUMENT_NODE = 9;
      DOCUMENT_TYPE_NODE = 10;
      DOCUMENT_FRAGMENT_NODE = 11;
      NOTATION_NODE = 12;
      DOCUMENT_POSITION_DISCONNECTED = $01;
      DOCUMENT_POSITION_PRECEDING = $02;
      DOCUMENT_POSITION_FOLLOWING = $04;
      DOCUMENT_POSITION_CONTAINS = $08;
      DOCUMENT_POSITION_CONTAINED_BY = $10;
      DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = $20;
  Public
    function getRootNode(const aOptions: TJSGetRootNodeOptions): IJSNode; overload;
    function getRootNode: IJSNode; overload;
    function hasChildNodes: Boolean;
    procedure normalize;
    function cloneNode(aDeep: Boolean): IJSNode; overload;
    function cloneNode: IJSNode; overload;
    function isEqualNode(aOtherNode: IJSNode): Boolean;
    function isSameNode(aOtherNode: IJSNode): Boolean;
    function compareDocumentPosition(aOther: IJSNode): Word;
    function contains(aOther: IJSNode): Boolean;
    function lookupPrefix(const aNamespace: UnicodeString): UnicodeString;
    function lookupNamespaceURI(const aPrefix: UnicodeString): UnicodeString;
    function isDefaultNamespace(const aNamespace: UnicodeString): Boolean;
    function insertBefore(aNode: IJSNode; aChild: IJSNode): IJSNode;
    function appendChild(aNode: IJSNode): IJSNode;
    function replaceChild(aNode: IJSNode; aChild: IJSNode): IJSNode;
    function removeChild(aChild: IJSNode): IJSNode;
    class function Cast(Intf: IJSObject): IJSNode;
    property nodeType: Word read _GetnodeType;
    property nodeName: UnicodeString read _GetnodeName;
    property baseURI: UnicodeString read _GetbaseURI;
    property isConnected: Boolean read _GetisConnected;
    property parentNode: IJSNode read _GetparentNode;
    property firstChild: IJSNode read _GetfirstChild;
    property lastChild: IJSNode read _GetlastChild;
    property previousSibling: IJSNode read _GetpreviousSibling;
    property nextSibling: IJSNode read _GetnextSibling;
    property nodeValue: UnicodeString read _GetnodeValue write _SetnodeValue;
    property textContent: UnicodeString read _GettextContent write _SettextContent;
  end;

  { --------------------------------------------------------------------
    TJSWindow
    --------------------------------------------------------------------}

  IJSWindow = interface(IJSEventTarget)
    ['{E4999E14-601D-3CE8-8B92-1F5E5AA3BFA6}']
    function _Getwindow: IJSWindowProxy;
    function _Getself_: IJSWindowProxy;
    function _Getdocument: IJSDocument;
    function _Getname: UnicodeString;
    function _Getlocation: IJSLocation;
    function _Gethistory: IJSHistory;
    function _GetcustomElements: IJSCustomElementRegistry;
    function _Getlocationbar: IJSBarProp;
    function _Getmenubar: IJSBarProp;
    function _Getpersonalbar: IJSBarProp;
    function _Getscrollbars: IJSBarProp;
    function _Getstatusbar: IJSBarProp;
    function _Gettoolbar: IJSBarProp;
    function _Getstatus: UnicodeString;
    function _Getclosed: Boolean;
    function _Getevent: TJOB_JSValue;
    function _Getframes: IJSWindowProxy;
    function _Getlength_: LongWord;
    function _Gettop: IJSWindowProxy;
    function _Getopener: TJOB_JSValue;
    function _Getparent: IJSWindowProxy;
    function _GetframeElement: IJSElement;
    function _Getnavigator: IJSNavigator;
    function _GetclientInformation: IJSNavigator;
    function _GetapplicationCache: IJSOfflineResourceList;
    function _Getscreen: IJSScreen;
    function _GetinnerWidth: TJOB_JSValue;
    function _GetinnerHeight: TJOB_JSValue;
    function _GetscrollX: Double;
    function _GetpageXOffset: Double;
    function _GetscrollY: Double;
    function _GetpageYOffset: Double;
    function _GetscreenLeft: Double;
    function _GetscreenTop: Double;
    function _GetscreenX: TJOB_JSValue;
    function _GetscreenY: TJOB_JSValue;
    function _GetouterWidth: TJOB_JSValue;
    function _GetouterHeight: TJOB_JSValue;
    function _Getcontrollers: IJSXULControllers;
    function _GetrealFrameElement: IJSElement;
    function _GetdocShell: IJSnsIDocShell;
    function _GetmozInnerScreenX: Single;
    function _GetmozInnerScreenY: Single;
    function _GetdevicePixelRatio: Double;
    function _GetdesktopToDeviceScale: Double;
    function _GetscrollMinX: Integer;
    function _GetscrollMinY: Integer;
    function _GetscrollMaxX: Integer;
    function _GetscrollMaxY: Integer;
    function _GetfullScreen: Boolean;
    function _Getcontent: IJSObject;
    function _GetwindowUtils: IJSnsIDOMWindowUtils;
    function _GetclientPrincipal: IJSPrincipal;
    function _Getsidebar: TJOB_JSValue;
    function _GetwindowState: Word;
    function _GetisFullyOccluded: Boolean;
    function _GetbrowserDOMWindow: IJSnsIBrowserDOMWindow;
    function _GetisChromeWindow: Boolean;
    function _GetsessionStorage: IJSStorage;
    function _GetlocalStorage: IJSStorage;
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setstatus(const aValue: UnicodeString);
    procedure _Setopener(const aValue: TJOB_JSValue);
    procedure _SetinnerWidth(const aValue: TJOB_JSValue);
    procedure _SetinnerHeight(const aValue: TJOB_JSValue);
    procedure _SetscreenX(const aValue: TJOB_JSValue);
    procedure _SetscreenY(const aValue: TJOB_JSValue);
    procedure _SetouterWidth(const aValue: TJOB_JSValue);
    procedure _SetouterHeight(const aValue: TJOB_JSValue);
    procedure _SetfullScreen(const aValue: Boolean);
    procedure _SetbrowserDOMWindow(const aValue: IJSnsIBrowserDOMWindow);
    procedure close;
    procedure stop;
    procedure focus;
    procedure blur;
    function open(const aUrl: UnicodeString; const aTarget: UnicodeString; const aFeatures: UnicodeString): IJSWindowProxy; overload;
    function open: IJSWindowProxy; overload;
    function open(const aUrl: UnicodeString): IJSWindowProxy; overload;
    function open(const aUrl: UnicodeString; const aTarget: UnicodeString): IJSWindowProxy; overload;
    procedure alert;
    procedure alert(const aMessage: UnicodeString);
    function confirm(const aMessage: UnicodeString): Boolean; overload;
    function confirm: Boolean; overload;
    function prompt(const aMessage: UnicodeString; const aDefault: UnicodeString): UnicodeString; overload;
    function prompt: UnicodeString; overload;
    function prompt(const aMessage: UnicodeString): UnicodeString; overload;
    procedure print;
    procedure postMessage(aMessage: TJOB_JSValue; const aOptions: TJSWindowPostMessageOptions); overload;
    procedure postMessage(aMessage: TJOB_JSValue); overload;
    procedure captureEvents;
    procedure releaseEvents;
    function getSelection: IJSSelection;
    function getComputedStyle(aElt: IJSElement; const aPseudoElt: UnicodeString): IJSCSSStyleDeclaration; overload;
    function getComputedStyle(aElt: IJSElement): IJSCSSStyleDeclaration; overload;
    procedure moveTo(aX: Integer; aY: Integer);
    procedure moveBy(aX: Integer; aY: Integer);
    procedure resizeTo(aX: Integer; aY: Integer);
    procedure resizeBy(aX: Integer; aY: Integer);
    procedure scroll(aX: Double; aY: Double);
    procedure scroll(const aOptions: TJSScrollToOptions); overload;
    procedure scroll; overload;
    procedure scrollTo(aX: Double; aY: Double);
    procedure scrollTo(const aOptions: TJSScrollToOptions); overload;
    procedure scrollTo; overload;
    procedure scrollBy(aX: Double; aY: Double);
    procedure scrollBy(const aOptions: TJSScrollToOptions); overload;
    procedure scrollBy; overload;
    procedure mozScrollSnap;
    function getDefaultComputedStyle(aElt: IJSElement; const aPseudoElt: UnicodeString): IJSCSSStyleDeclaration; overload;
    function getDefaultComputedStyle(aElt: IJSElement): IJSCSSStyleDeclaration; overload;
    procedure scrollByLines(aNumLines: Integer; const aOptions: TJSScrollOptions); overload;
    procedure scrollByLines(aNumLines: Integer); overload;
    procedure scrollByPages(aNumPages: Integer; const aOptions: TJSScrollOptions); overload;
    procedure scrollByPages(aNumPages: Integer); overload;
    procedure sizeToContent;
    procedure updateCommands(const action: UnicodeString; aSel: IJSSelection; aReason: SmallInt); overload;
    procedure updateCommands(const action: UnicodeString); overload;
    procedure updateCommands(const action: UnicodeString; aSel: IJSSelection); overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean; aSearchInFrames: Boolean; aShowDialog: Boolean): Boolean; overload;
    function find: Boolean; overload;
    function find(const aStr: UnicodeString): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean; aSearchInFrames: Boolean): Boolean; overload;
    procedure dump(const aStr: UnicodeString);
    procedure setResizable(aResizable: Boolean);
    function openDialog(const aUrl: UnicodeString; const aName: UnicodeString; const aOptions: UnicodeString; aExtraArguments: TJOB_JSValue): IJSWindowProxy{; ToDo:varargs}; overload;
    function openDialog: IJSWindowProxy{; ToDo:varargs}; overload;
    function openDialog(const aUrl: UnicodeString): IJSWindowProxy{; ToDo:varargs}; overload;
    function openDialog(const aUrl: UnicodeString; const aName: UnicodeString): IJSWindowProxy{; ToDo:varargs}; overload;
    function getInterface(aIid: TJOB_JSValue): TJOB_JSValue;
    function shouldReportForServiceWorkerScope(const aScope: UnicodeString): Boolean;
    procedure getAttention;
    procedure getAttentionWithCycleCount(aCycleCount: Integer);
    procedure setCursor(const aCursor: UTF8String);
    procedure maximize;
    procedure minimize;
    procedure restore;
    function getWorkspaceID: UnicodeString;
    procedure moveToWorkspace(const aWorkspaceID: UnicodeString);
    procedure notifyDefaultButtonLoaded(aDefaultButton: IJSElement);
    procedure cancelIdleCallback(aHandle: LongWord);
    function getRegionalPrefsLocales: TUnicodeStringDynArray;
    function getWebExposedLocales: TUnicodeStringDynArray;
    property window: IJSWindowProxy read _Getwindow;
    property self_: IJSWindowProxy read _Getself_;
    property document: IJSDocument read _Getdocument;
    property name: UnicodeString read _Getname write _Setname;
    property location: IJSLocation read _Getlocation;
    property history: IJSHistory read _Gethistory;
    property customElements: IJSCustomElementRegistry read _GetcustomElements;
    property locationbar: IJSBarProp read _Getlocationbar;
    property menubar: IJSBarProp read _Getmenubar;
    property personalbar: IJSBarProp read _Getpersonalbar;
    property scrollbars: IJSBarProp read _Getscrollbars;
    property statusbar: IJSBarProp read _Getstatusbar;
    property toolbar: IJSBarProp read _Gettoolbar;
    property status: UnicodeString read _Getstatus write _Setstatus;
    property closed: Boolean read _Getclosed;
    property event: TJOB_JSValue read _Getevent;
    property frames: IJSWindowProxy read _Getframes;
    property length_: LongWord read _Getlength_;
    property top: IJSWindowProxy read _Gettop;
    property opener: TJOB_JSValue read _Getopener write _Setopener;
    property parent: IJSWindowProxy read _Getparent;
    property frameElement: IJSElement read _GetframeElement;
    property navigator: IJSNavigator read _Getnavigator;
    property clientInformation: IJSNavigator read _GetclientInformation;
    property applicationCache: IJSOfflineResourceList read _GetapplicationCache;
    property screen: IJSScreen read _Getscreen;
    property innerWidth: TJOB_JSValue read _GetinnerWidth write _SetinnerWidth;
    property innerHeight: TJOB_JSValue read _GetinnerHeight write _SetinnerHeight;
    property scrollX: Double read _GetscrollX;
    property pageXOffset: Double read _GetpageXOffset;
    property scrollY: Double read _GetscrollY;
    property pageYOffset: Double read _GetpageYOffset;
    property screenLeft: Double read _GetscreenLeft;
    property screenTop: Double read _GetscreenTop;
    property screenX: TJOB_JSValue read _GetscreenX write _SetscreenX;
    property screenY: TJOB_JSValue read _GetscreenY write _SetscreenY;
    property outerWidth: TJOB_JSValue read _GetouterWidth write _SetouterWidth;
    property outerHeight: TJOB_JSValue read _GetouterHeight write _SetouterHeight;
    property controllers: IJSXULControllers read _Getcontrollers;
    property realFrameElement: IJSElement read _GetrealFrameElement;
    property docShell: IJSnsIDocShell read _GetdocShell;
    property mozInnerScreenX: Single read _GetmozInnerScreenX;
    property mozInnerScreenY: Single read _GetmozInnerScreenY;
    property devicePixelRatio: Double read _GetdevicePixelRatio;
    property desktopToDeviceScale: Double read _GetdesktopToDeviceScale;
    property scrollMinX: Integer read _GetscrollMinX;
    property scrollMinY: Integer read _GetscrollMinY;
    property scrollMaxX: Integer read _GetscrollMaxX;
    property scrollMaxY: Integer read _GetscrollMaxY;
    property fullScreen: Boolean read _GetfullScreen write _SetfullScreen;
    // property ondevicemotion: TEventHandler read _Getondevicemotion write _Setondevicemotion;
    // property ondeviceorientation: TEventHandler read _Getondeviceorientation write _Setondeviceorientation;
    // property onabsolutedeviceorientation: TEventHandler read _Getonabsolutedeviceorientation write _Setonabsolutedeviceorientation;
    // property onuserproximity: TEventHandler read _Getonuserproximity write _Setonuserproximity;
    // property ondevicelight: TEventHandler read _Getondevicelight write _Setondevicelight;
    property content: IJSObject read _Getcontent;
    property windowUtils: IJSnsIDOMWindowUtils read _GetwindowUtils;
    property clientPrincipal: IJSPrincipal read _GetclientPrincipal;
    property sidebar: TJOB_JSValue read _Getsidebar;
    property windowState: Word read _GetwindowState;
    property isFullyOccluded: Boolean read _GetisFullyOccluded;
    property browserDOMWindow: IJSnsIBrowserDOMWindow read _GetbrowserDOMWindow write _SetbrowserDOMWindow;
    property isChromeWindow: Boolean read _GetisChromeWindow;
    // property onvrdisplayconnect: TEventHandler read _Getonvrdisplayconnect write _Setonvrdisplayconnect;
    // property onvrdisplaydisconnect: TEventHandler read _Getonvrdisplaydisconnect write _Setonvrdisplaydisconnect;
    // property onvrdisplayactivate: TEventHandler read _Getonvrdisplayactivate write _Setonvrdisplayactivate;
    // property onvrdisplaydeactivate: TEventHandler read _Getonvrdisplaydeactivate write _Setonvrdisplaydeactivate;
    // property onvrdisplaypresentchange: TEventHandler read _Getonvrdisplaypresentchange write _Setonvrdisplaypresentchange;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    // property onafterprint: TEventHandler read _Getonafterprint write _Setonafterprint;
    // property onbeforeprint: TEventHandler read _Getonbeforeprint write _Setonbeforeprint;
    // property onbeforeunload: TOnBeforeUnloadEventHandler read _Getonbeforeunload write _Setonbeforeunload;
    // property onhashchange: TEventHandler read _Getonhashchange write _Setonhashchange;
    // property onlanguagechange: TEventHandler read _Getonlanguagechange write _Setonlanguagechange;
    // property onmessage: TEventHandler read _Getonmessage write _Setonmessage;
    // property onmessageerror: TEventHandler read _Getonmessageerror write _Setonmessageerror;
    // property onoffline: TEventHandler read _Getonoffline write _Setonoffline;
    // property ononline: TEventHandler read _Getononline write _Setononline;
    // property onpagehide: TEventHandler read _Getonpagehide write _Setonpagehide;
    // property onpageshow: TEventHandler read _Getonpageshow write _Setonpageshow;
    // property onpopstate: TEventHandler read _Getonpopstate write _Setonpopstate;
    // property onrejectionhandled: TEventHandler read _Getonrejectionhandled write _Setonrejectionhandled;
    // property onstorage: TEventHandler read _Getonstorage write _Setonstorage;
    // property onunhandledrejection: TEventHandler read _Getonunhandledrejection write _Setonunhandledrejection;
    // property onunload: TEventHandler read _Getonunload write _Setonunload;
    // property ongamepadconnected: TEventHandler read _Getongamepadconnected write _Setongamepadconnected;
    // property ongamepaddisconnected: TEventHandler read _Getongamepaddisconnected write _Setongamepaddisconnected;
    property sessionStorage: IJSStorage read _GetsessionStorage;
    property localStorage: IJSStorage read _GetlocalStorage;
  end;

  TJSWindow = class(TJSEventTarget,IJSWindow)
  Private
    function _Getwindow: IJSWindowProxy;
    function _Getself_: IJSWindowProxy;
    function _Getdocument: IJSDocument;
    function _Getname: UnicodeString;
    function _Getlocation: IJSLocation;
    function _Gethistory: IJSHistory;
    function _GetcustomElements: IJSCustomElementRegistry;
    function _Getlocationbar: IJSBarProp;
    function _Getmenubar: IJSBarProp;
    function _Getpersonalbar: IJSBarProp;
    function _Getscrollbars: IJSBarProp;
    function _Getstatusbar: IJSBarProp;
    function _Gettoolbar: IJSBarProp;
    function _Getstatus: UnicodeString;
    function _Getclosed: Boolean;
    function _Getevent: TJOB_JSValue;
    function _Getframes: IJSWindowProxy;
    function _Getlength_: LongWord;
    function _Gettop: IJSWindowProxy;
    function _Getopener: TJOB_JSValue;
    function _Getparent: IJSWindowProxy;
    function _GetframeElement: IJSElement;
    function _Getnavigator: IJSNavigator;
    function _GetclientInformation: IJSNavigator;
    function _GetapplicationCache: IJSOfflineResourceList;
    function _Getscreen: IJSScreen;
    function _GetinnerWidth: TJOB_JSValue;
    function _GetinnerHeight: TJOB_JSValue;
    function _GetscrollX: Double;
    function _GetpageXOffset: Double;
    function _GetscrollY: Double;
    function _GetpageYOffset: Double;
    function _GetscreenLeft: Double;
    function _GetscreenTop: Double;
    function _GetscreenX: TJOB_JSValue;
    function _GetscreenY: TJOB_JSValue;
    function _GetouterWidth: TJOB_JSValue;
    function _GetouterHeight: TJOB_JSValue;
    function _Getcontrollers: IJSXULControllers;
    function _GetrealFrameElement: IJSElement;
    function _GetdocShell: IJSnsIDocShell;
    function _GetmozInnerScreenX: Single;
    function _GetmozInnerScreenY: Single;
    function _GetdevicePixelRatio: Double;
    function _GetdesktopToDeviceScale: Double;
    function _GetscrollMinX: Integer;
    function _GetscrollMinY: Integer;
    function _GetscrollMaxX: Integer;
    function _GetscrollMaxY: Integer;
    function _GetfullScreen: Boolean;
    function _Getcontent: IJSObject;
    function _GetwindowUtils: IJSnsIDOMWindowUtils;
    function _GetclientPrincipal: IJSPrincipal;
    function _Getsidebar: TJOB_JSValue;
    function _GetwindowState: Word;
    function _GetisFullyOccluded: Boolean;
    function _GetbrowserDOMWindow: IJSnsIBrowserDOMWindow;
    function _GetisChromeWindow: Boolean;
    function _GetsessionStorage: IJSStorage;
    function _GetlocalStorage: IJSStorage;
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setstatus(const aValue: UnicodeString);
    procedure _Setopener(const aValue: TJOB_JSValue);
    procedure _SetinnerWidth(const aValue: TJOB_JSValue);
    procedure _SetinnerHeight(const aValue: TJOB_JSValue);
    procedure _SetscreenX(const aValue: TJOB_JSValue);
    procedure _SetscreenY(const aValue: TJOB_JSValue);
    procedure _SetouterWidth(const aValue: TJOB_JSValue);
    procedure _SetouterHeight(const aValue: TJOB_JSValue);
    procedure _SetfullScreen(const aValue: Boolean);
    procedure _SetbrowserDOMWindow(const aValue: IJSnsIBrowserDOMWindow);
  Public
    Const
      STATE_MAXIMIZED = 1;
      STATE_MINIMIZED = 2;
      STATE_NORMAL = 3;
      STATE_FULLSCREEN = 4;
  Public
    procedure close;
    procedure stop;
    procedure focus;
    procedure blur;
    function open(const aUrl: UnicodeString; const aTarget: UnicodeString; const aFeatures: UnicodeString): IJSWindowProxy; overload;
    function open: IJSWindowProxy; overload;
    function open(const aUrl: UnicodeString): IJSWindowProxy; overload;
    function open(const aUrl: UnicodeString; const aTarget: UnicodeString): IJSWindowProxy; overload;
    procedure alert;
    procedure alert(const aMessage: UnicodeString);
    function confirm(const aMessage: UnicodeString): Boolean; overload;
    function confirm: Boolean; overload;
    function prompt(const aMessage: UnicodeString; const aDefault: UnicodeString): UnicodeString; overload;
    function prompt: UnicodeString; overload;
    function prompt(const aMessage: UnicodeString): UnicodeString; overload;
    procedure print;
    procedure postMessage(aMessage: TJOB_JSValue; const aOptions: TJSWindowPostMessageOptions); overload;
    procedure postMessage(aMessage: TJOB_JSValue); overload;
    procedure captureEvents;
    procedure releaseEvents;
    function getSelection: IJSSelection;
    function getComputedStyle(aElt: IJSElement; const aPseudoElt: UnicodeString): IJSCSSStyleDeclaration; overload;
    function getComputedStyle(aElt: IJSElement): IJSCSSStyleDeclaration; overload;
    procedure moveTo(aX: Integer; aY: Integer);
    procedure moveBy(aX: Integer; aY: Integer);
    procedure resizeTo(aX: Integer; aY: Integer);
    procedure resizeBy(aX: Integer; aY: Integer);
    procedure scroll(aX: Double; aY: Double);
    procedure scroll(const aOptions: TJSScrollToOptions); overload;
    procedure scroll; overload;
    procedure scrollTo(aX: Double; aY: Double);
    procedure scrollTo(const aOptions: TJSScrollToOptions); overload;
    procedure scrollTo; overload;
    procedure scrollBy(aX: Double; aY: Double);
    procedure scrollBy(const aOptions: TJSScrollToOptions); overload;
    procedure scrollBy; overload;
    procedure mozScrollSnap;
    function getDefaultComputedStyle(aElt: IJSElement; const aPseudoElt: UnicodeString): IJSCSSStyleDeclaration; overload;
    function getDefaultComputedStyle(aElt: IJSElement): IJSCSSStyleDeclaration; overload;
    procedure scrollByLines(aNumLines: Integer; const aOptions: TJSScrollOptions); overload;
    procedure scrollByLines(aNumLines: Integer); overload;
    procedure scrollByPages(aNumPages: Integer; const aOptions: TJSScrollOptions); overload;
    procedure scrollByPages(aNumPages: Integer); overload;
    procedure sizeToContent;
    procedure updateCommands(const action: UnicodeString; aSel: IJSSelection; aReason: SmallInt); overload;
    procedure updateCommands(const action: UnicodeString); overload;
    procedure updateCommands(const action: UnicodeString; aSel: IJSSelection); overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean; aSearchInFrames: Boolean; aShowDialog: Boolean): Boolean; overload;
    function find: Boolean; overload;
    function find(const aStr: UnicodeString): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean): Boolean; overload;
    function find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean; aSearchInFrames: Boolean): Boolean; overload;
    procedure dump(const aStr: UnicodeString);
    procedure setResizable(aResizable: Boolean);
    function openDialog(const aUrl: UnicodeString; const aName: UnicodeString; const aOptions: UnicodeString; aExtraArguments: TJOB_JSValue): IJSWindowProxy{; ToDo:varargs}; overload;
    function openDialog: IJSWindowProxy{; ToDo:varargs}; overload;
    function openDialog(const aUrl: UnicodeString): IJSWindowProxy{; ToDo:varargs}; overload;
    function openDialog(const aUrl: UnicodeString; const aName: UnicodeString): IJSWindowProxy{; ToDo:varargs}; overload;
    function getInterface(aIid: TJOB_JSValue): TJOB_JSValue;
    function shouldReportForServiceWorkerScope(const aScope: UnicodeString): Boolean;
    procedure getAttention;
    procedure getAttentionWithCycleCount(aCycleCount: Integer);
    procedure setCursor(const aCursor: UTF8String);
    procedure maximize;
    procedure minimize;
    procedure restore;
    function getWorkspaceID: UnicodeString;
    procedure moveToWorkspace(const aWorkspaceID: UnicodeString);
    procedure notifyDefaultButtonLoaded(aDefaultButton: IJSElement);
    procedure cancelIdleCallback(aHandle: LongWord);
    function getRegionalPrefsLocales: TUnicodeStringDynArray;
    function getWebExposedLocales: TUnicodeStringDynArray;
    class function Cast(Intf: IJSObject): IJSWindow;
    property window: IJSWindowProxy read _Getwindow;
    property self_: IJSWindowProxy read _Getself_;
    property document: IJSDocument read _Getdocument;
    property name: UnicodeString read _Getname write _Setname;
    property location: IJSLocation read _Getlocation;
    property history: IJSHistory read _Gethistory;
    property customElements: IJSCustomElementRegistry read _GetcustomElements;
    property locationbar: IJSBarProp read _Getlocationbar;
    property menubar: IJSBarProp read _Getmenubar;
    property personalbar: IJSBarProp read _Getpersonalbar;
    property scrollbars: IJSBarProp read _Getscrollbars;
    property statusbar: IJSBarProp read _Getstatusbar;
    property toolbar: IJSBarProp read _Gettoolbar;
    property status: UnicodeString read _Getstatus write _Setstatus;
    property closed: Boolean read _Getclosed;
    property event: TJOB_JSValue read _Getevent;
    property frames: IJSWindowProxy read _Getframes;
    property length_: LongWord read _Getlength_;
    property top: IJSWindowProxy read _Gettop;
    property opener: TJOB_JSValue read _Getopener write _Setopener;
    property parent: IJSWindowProxy read _Getparent;
    property frameElement: IJSElement read _GetframeElement;
    property navigator: IJSNavigator read _Getnavigator;
    property clientInformation: IJSNavigator read _GetclientInformation;
    property applicationCache: IJSOfflineResourceList read _GetapplicationCache;
    property screen: IJSScreen read _Getscreen;
    property innerWidth: TJOB_JSValue read _GetinnerWidth write _SetinnerWidth;
    property innerHeight: TJOB_JSValue read _GetinnerHeight write _SetinnerHeight;
    property scrollX: Double read _GetscrollX;
    property pageXOffset: Double read _GetpageXOffset;
    property scrollY: Double read _GetscrollY;
    property pageYOffset: Double read _GetpageYOffset;
    property screenLeft: Double read _GetscreenLeft;
    property screenTop: Double read _GetscreenTop;
    property screenX: TJOB_JSValue read _GetscreenX write _SetscreenX;
    property screenY: TJOB_JSValue read _GetscreenY write _SetscreenY;
    property outerWidth: TJOB_JSValue read _GetouterWidth write _SetouterWidth;
    property outerHeight: TJOB_JSValue read _GetouterHeight write _SetouterHeight;
    property controllers: IJSXULControllers read _Getcontrollers;
    property realFrameElement: IJSElement read _GetrealFrameElement;
    property docShell: IJSnsIDocShell read _GetdocShell;
    property mozInnerScreenX: Single read _GetmozInnerScreenX;
    property mozInnerScreenY: Single read _GetmozInnerScreenY;
    property devicePixelRatio: Double read _GetdevicePixelRatio;
    property desktopToDeviceScale: Double read _GetdesktopToDeviceScale;
    property scrollMinX: Integer read _GetscrollMinX;
    property scrollMinY: Integer read _GetscrollMinY;
    property scrollMaxX: Integer read _GetscrollMaxX;
    property scrollMaxY: Integer read _GetscrollMaxY;
    property fullScreen: Boolean read _GetfullScreen write _SetfullScreen;
    // property ondevicemotion: TEventHandler read _Getondevicemotion write _Setondevicemotion;
    // property ondeviceorientation: TEventHandler read _Getondeviceorientation write _Setondeviceorientation;
    // property onabsolutedeviceorientation: TEventHandler read _Getonabsolutedeviceorientation write _Setonabsolutedeviceorientation;
    // property onuserproximity: TEventHandler read _Getonuserproximity write _Setonuserproximity;
    // property ondevicelight: TEventHandler read _Getondevicelight write _Setondevicelight;
    property content: IJSObject read _Getcontent;
    property windowUtils: IJSnsIDOMWindowUtils read _GetwindowUtils;
    property clientPrincipal: IJSPrincipal read _GetclientPrincipal;
    property sidebar: TJOB_JSValue read _Getsidebar;
    property windowState: Word read _GetwindowState;
    property isFullyOccluded: Boolean read _GetisFullyOccluded;
    property browserDOMWindow: IJSnsIBrowserDOMWindow read _GetbrowserDOMWindow write _SetbrowserDOMWindow;
    property isChromeWindow: Boolean read _GetisChromeWindow;
    // property onvrdisplayconnect: TEventHandler read _Getonvrdisplayconnect write _Setonvrdisplayconnect;
    // property onvrdisplaydisconnect: TEventHandler read _Getonvrdisplaydisconnect write _Setonvrdisplaydisconnect;
    // property onvrdisplayactivate: TEventHandler read _Getonvrdisplayactivate write _Setonvrdisplayactivate;
    // property onvrdisplaydeactivate: TEventHandler read _Getonvrdisplaydeactivate write _Setonvrdisplaydeactivate;
    // property onvrdisplaypresentchange: TEventHandler read _Getonvrdisplaypresentchange write _Setonvrdisplaypresentchange;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    // property onafterprint: TEventHandler read _Getonafterprint write _Setonafterprint;
    // property onbeforeprint: TEventHandler read _Getonbeforeprint write _Setonbeforeprint;
    // property onbeforeunload: TOnBeforeUnloadEventHandler read _Getonbeforeunload write _Setonbeforeunload;
    // property onhashchange: TEventHandler read _Getonhashchange write _Setonhashchange;
    // property onlanguagechange: TEventHandler read _Getonlanguagechange write _Setonlanguagechange;
    // property onmessage: TEventHandler read _Getonmessage write _Setonmessage;
    // property onmessageerror: TEventHandler read _Getonmessageerror write _Setonmessageerror;
    // property onoffline: TEventHandler read _Getonoffline write _Setonoffline;
    // property ononline: TEventHandler read _Getononline write _Setononline;
    // property onpagehide: TEventHandler read _Getonpagehide write _Setonpagehide;
    // property onpageshow: TEventHandler read _Getonpageshow write _Setonpageshow;
    // property onpopstate: TEventHandler read _Getonpopstate write _Setonpopstate;
    // property onrejectionhandled: TEventHandler read _Getonrejectionhandled write _Setonrejectionhandled;
    // property onstorage: TEventHandler read _Getonstorage write _Setonstorage;
    // property onunhandledrejection: TEventHandler read _Getonunhandledrejection write _Setonunhandledrejection;
    // property onunload: TEventHandler read _Getonunload write _Setonunload;
    // property ongamepadconnected: TEventHandler read _Getongamepadconnected write _Setongamepadconnected;
    // property ongamepaddisconnected: TEventHandler read _Getongamepaddisconnected write _Setongamepaddisconnected;
    property sessionStorage: IJSStorage read _GetsessionStorage;
    property localStorage: IJSStorage read _GetlocalStorage;
  end;

  { --------------------------------------------------------------------
    TJSOfflineResourceList
    --------------------------------------------------------------------}

  IJSOfflineResourceList = interface(IJSEventTarget)
    ['{0F5006FC-6AC1-32E9-B592-D161ADF981EC}']
    function _Getstatus: Word;
    function _GetmozItems: IJSDOMStringList;
    function _GetmozLength: LongWord;
    function _Getlength_: LongWord;
    procedure update;
    procedure swapCache;
    function mozHasItem(const aUri: UnicodeString): Boolean;
    function mozItem(aIndex: LongWord): UnicodeString;
    procedure mozAdd(const aUri: UnicodeString);
    procedure mozRemove(const aUri: UnicodeString);
    property status: Word read _Getstatus;
    // property onchecking: TEventHandler read _Getonchecking write _Setonchecking;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
    // property onnoupdate: TEventHandler read _Getonnoupdate write _Setonnoupdate;
    // property ondownloading: TEventHandler read _Getondownloading write _Setondownloading;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onupdateready: TEventHandler read _Getonupdateready write _Setonupdateready;
    // property oncached: TEventHandler read _Getoncached write _Setoncached;
    // property onobsolete: TEventHandler read _Getonobsolete write _Setonobsolete;
    property mozItems: IJSDOMStringList read _GetmozItems;
    property mozLength: LongWord read _GetmozLength;
    property length_: LongWord read _Getlength_;
  end;

  TJSOfflineResourceList = class(TJSEventTarget,IJSOfflineResourceList)
  Private
    function _Getstatus: Word;
    function _GetmozItems: IJSDOMStringList;
    function _GetmozLength: LongWord;
    function _Getlength_: LongWord;
  Public
    Const
      UNCACHED = 0;
      IDLE = 1;
      CHECKING = 2;
      DOWNLOADING = 3;
      UPDATEREADY = 4;
      OBSOLETE = 5;
  Public
    procedure update;
    procedure swapCache;
    function mozHasItem(const aUri: UnicodeString): Boolean;
    function mozItem(aIndex: LongWord): UnicodeString;
    procedure mozAdd(const aUri: UnicodeString);
    procedure mozRemove(const aUri: UnicodeString);
    class function Cast(Intf: IJSObject): IJSOfflineResourceList;
    property status: Word read _Getstatus;
    // property onchecking: TEventHandler read _Getonchecking write _Setonchecking;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
    // property onnoupdate: TEventHandler read _Getonnoupdate write _Setonnoupdate;
    // property ondownloading: TEventHandler read _Getondownloading write _Setondownloading;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onupdateready: TEventHandler read _Getonupdateready write _Setonupdateready;
    // property oncached: TEventHandler read _Getoncached write _Setoncached;
    // property onobsolete: TEventHandler read _Getonobsolete write _Setonobsolete;
    property mozItems: IJSDOMStringList read _GetmozItems;
    property mozLength: LongWord read _GetmozLength;
    property length_: LongWord read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSScreen
    --------------------------------------------------------------------}

  IJSScreen = interface(IJSEventTarget)
    ['{2D98D0FF-EEF0-376F-84EA-27D25A956C09}']
    function _GetavailWidth: Integer;
    function _GetavailHeight: Integer;
    function _Getwidth: Integer;
    function _Getheight: Integer;
    function _GetcolorDepth: Integer;
    function _GetpixelDepth: Integer;
    function _Gettop: Integer;
    function _Getleft: Integer;
    function _GetavailTop: Integer;
    function _GetavailLeft: Integer;
    function _GetmozOrientation: UnicodeString;
    function _Getorientation: IJSScreenOrientation;
    function _GetcolorGamut: TScreenColorGamut;
    function _Getluminance: IJSScreenLuminance;
    function mozLockOrientation(const aOrientation: UnicodeString): Boolean;
    procedure mozUnlockOrientation;
    property availWidth: Integer read _GetavailWidth;
    property availHeight: Integer read _GetavailHeight;
    property width: Integer read _Getwidth;
    property height: Integer read _Getheight;
    property colorDepth: Integer read _GetcolorDepth;
    property pixelDepth: Integer read _GetpixelDepth;
    property top: Integer read _Gettop;
    property left: Integer read _Getleft;
    property availTop: Integer read _GetavailTop;
    property availLeft: Integer read _GetavailLeft;
    property mozOrientation: UnicodeString read _GetmozOrientation;
    // property onmozorientationchange: TEventHandler read _Getonmozorientationchange write _Setonmozorientationchange;
    property orientation: IJSScreenOrientation read _Getorientation;
    property colorGamut: TScreenColorGamut read _GetcolorGamut;
    property luminance: IJSScreenLuminance read _Getluminance;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
  end;

  TJSScreen = class(TJSEventTarget,IJSScreen)
  Private
    function _GetavailWidth: Integer;
    function _GetavailHeight: Integer;
    function _Getwidth: Integer;
    function _Getheight: Integer;
    function _GetcolorDepth: Integer;
    function _GetpixelDepth: Integer;
    function _Gettop: Integer;
    function _Getleft: Integer;
    function _GetavailTop: Integer;
    function _GetavailLeft: Integer;
    function _GetmozOrientation: UnicodeString;
    function _Getorientation: IJSScreenOrientation;
    function _GetcolorGamut: TScreenColorGamut;
    function _Getluminance: IJSScreenLuminance;
  Public
    function mozLockOrientation(const aOrientation: UnicodeString): Boolean;
    procedure mozUnlockOrientation;
    class function Cast(Intf: IJSObject): IJSScreen;
    property availWidth: Integer read _GetavailWidth;
    property availHeight: Integer read _GetavailHeight;
    property width: Integer read _Getwidth;
    property height: Integer read _Getheight;
    property colorDepth: Integer read _GetcolorDepth;
    property pixelDepth: Integer read _GetpixelDepth;
    property top: Integer read _Gettop;
    property left: Integer read _Getleft;
    property availTop: Integer read _GetavailTop;
    property availLeft: Integer read _GetavailLeft;
    property mozOrientation: UnicodeString read _GetmozOrientation;
    // property onmozorientationchange: TEventHandler read _Getonmozorientationchange write _Setonmozorientationchange;
    property orientation: IJSScreenOrientation read _Getorientation;
    property colorGamut: TScreenColorGamut read _GetcolorGamut;
    property luminance: IJSScreenLuminance read _Getluminance;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
  end;

  { --------------------------------------------------------------------
    TJSClipboard
    --------------------------------------------------------------------}

  IJSClipboard = interface(IJSEventTarget)
    ['{9D1D9F4C-E0C3-3B78-AFBB-9ABDACA26E5A}']
    procedure onUserReactedToPasteMenuPopup(allowed: Boolean);
  end;

  TJSClipboard = class(TJSEventTarget,IJSClipboard)
  Private
  Public
    procedure onUserReactedToPasteMenuPopup(allowed: Boolean);
    class function Cast(Intf: IJSObject): IJSClipboard;
  end;

  { --------------------------------------------------------------------
    TJSRange
    --------------------------------------------------------------------}

  IJSRange = interface(IJSAbstractRange)
    ['{3B56532E-B37D-3224-9516-68F5A8DDE94E}']
    function _GetcommonAncestorContainer: IJSNode;
    procedure setStart(aRefNode: IJSNode; aOffset: LongWord);
    procedure setEnd(aRefNode: IJSNode; aOffset: LongWord);
    procedure setStartBefore(aRefNode: IJSNode);
    procedure setStartAfter(aRefNode: IJSNode);
    procedure setEndBefore(aRefNode: IJSNode);
    procedure setEndAfter(aRefNode: IJSNode);
    procedure collapse(aToStart: Boolean); overload;
    procedure collapse; overload;
    procedure selectNode(aRefNode: IJSNode);
    procedure selectNodeContents(aRefNode: IJSNode);
    function compareBoundaryPoints(aHow: Word; aSourceRange: IJSRange): SmallInt;
    procedure deleteContents;
    function extractContents: IJSDocumentFragment;
    function cloneContents: IJSDocumentFragment;
    procedure insertNode(aNode: IJSNode);
    procedure surroundContents(aNewParent: IJSNode);
    function cloneRange: IJSRange;
    procedure detach;
    function isPointInRange(aNode: IJSNode; aOffset: LongWord): Boolean;
    function comparePoint(aNode: IJSNode; aOffset: LongWord): SmallInt;
    function intersectsNode(aNode: IJSNode): Boolean;
    function createContextualFragment(const aFragment: UnicodeString): IJSDocumentFragment;
    function getClientRects: IJSDOMRectList;
    function getBoundingClientRect: IJSDOMRect;
    property commonAncestorContainer: IJSNode read _GetcommonAncestorContainer;
  end;

  TJSRange = class(TJSAbstractRange,IJSRange)
  Private
    function _GetcommonAncestorContainer: IJSNode;
  Public
    Const
      START_TO_START = 0;
      START_TO_END = 1;
      END_TO_END = 2;
      END_TO_START = 3;
  Public
    procedure setStart(aRefNode: IJSNode; aOffset: LongWord);
    procedure setEnd(aRefNode: IJSNode; aOffset: LongWord);
    procedure setStartBefore(aRefNode: IJSNode);
    procedure setStartAfter(aRefNode: IJSNode);
    procedure setEndBefore(aRefNode: IJSNode);
    procedure setEndAfter(aRefNode: IJSNode);
    procedure collapse(aToStart: Boolean); overload;
    procedure collapse; overload;
    procedure selectNode(aRefNode: IJSNode);
    procedure selectNodeContents(aRefNode: IJSNode);
    function compareBoundaryPoints(aHow: Word; aSourceRange: IJSRange): SmallInt;
    procedure deleteContents;
    function extractContents: IJSDocumentFragment;
    function cloneContents: IJSDocumentFragment;
    procedure insertNode(aNode: IJSNode);
    procedure surroundContents(aNewParent: IJSNode);
    function cloneRange: IJSRange;
    procedure detach;
    function isPointInRange(aNode: IJSNode; aOffset: LongWord): Boolean;
    function comparePoint(aNode: IJSNode; aOffset: LongWord): SmallInt;
    function intersectsNode(aNode: IJSNode): Boolean;
    function createContextualFragment(const aFragment: UnicodeString): IJSDocumentFragment;
    function getClientRects: IJSDOMRectList;
    function getBoundingClientRect: IJSDOMRect;
    class function Cast(Intf: IJSObject): IJSRange;
    property commonAncestorContainer: IJSNode read _GetcommonAncestorContainer;
  end;

  { --------------------------------------------------------------------
    TJSScreenOrientation
    --------------------------------------------------------------------}

  IJSScreenOrientation = interface(IJSEventTarget)
    ['{60123529-C46F-3B2E-910C-471EFD441E19}']
    function _Gettype_: TOrientationType;
    function _Getangle: Word;
    procedure unlock;
    property type_: TOrientationType read _Gettype_;
    property angle: Word read _Getangle;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
  end;

  TJSScreenOrientation = class(TJSEventTarget,IJSScreenOrientation)
  Private
    function _Gettype_: TOrientationType;
    function _Getangle: Word;
  Public
    procedure unlock;
    class function Cast(Intf: IJSObject): IJSScreenOrientation;
    property type_: TOrientationType read _Gettype_;
    property angle: Word read _Getangle;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
  end;

  { --------------------------------------------------------------------
    TJSDOMRect
    --------------------------------------------------------------------}

  IJSDOMRect = interface(IJSDOMRectReadOnly)
    ['{1B4170E0-4137-3EEC-B409-DC2AA3D2D28F}']
    function _Getx: Double;
    function _Gety: Double;
    function _Getwidth: Double;
    function _Getheight: Double;
    procedure _Setx(const aValue: Double);
    procedure _Sety(const aValue: Double);
    procedure _Setwidth(const aValue: Double);
    procedure _Setheight(const aValue: Double);
    function fromRect(const aOther: TJSDOMRectInit): IJSDOMRect; overload;
    function fromRect: IJSDOMRect; overload;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property width: Double read _Getwidth write _Setwidth;
    property height: Double read _Getheight write _Setheight;
  end;

  TJSDOMRect = class(TJSDOMRectReadOnly,IJSDOMRect)
  Private
    function _Getx: Double;
    function _Gety: Double;
    function _Getwidth: Double;
    function _Getheight: Double;
    procedure _Setx(const aValue: Double);
    procedure _Sety(const aValue: Double);
    procedure _Setwidth(const aValue: Double);
    procedure _Setheight(const aValue: Double);
  Public
    function fromRect(const aOther: TJSDOMRectInit): IJSDOMRect; overload;
    function fromRect: IJSDOMRect; overload;
    class function Cast(Intf: IJSObject): IJSDOMRect;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property width: Double read _Getwidth write _Setwidth;
    property height: Double read _Getheight write _Setheight;
  end;

  { --------------------------------------------------------------------
    TJSDOMMatrix
    --------------------------------------------------------------------}

  IJSDOMMatrix = interface(IJSDOMMatrixReadOnly)
    ['{AB9F8CF5-39C1-3F7B-89D9-5949A6EB4458}']
    function _Geta: Double;
    function _Getb: Double;
    function _Getc: Double;
    function _Getd: Double;
    function _Gete: Double;
    function _Getf: Double;
    function _Getm11: Double;
    function _Getm12: Double;
    function _Getm13: Double;
    function _Getm14: Double;
    function _Getm21: Double;
    function _Getm22: Double;
    function _Getm23: Double;
    function _Getm24: Double;
    function _Getm31: Double;
    function _Getm32: Double;
    function _Getm33: Double;
    function _Getm34: Double;
    function _Getm41: Double;
    function _Getm42: Double;
    function _Getm43: Double;
    function _Getm44: Double;
    procedure _Seta(const aValue: Double);
    procedure _Setb(const aValue: Double);
    procedure _Setc(const aValue: Double);
    procedure _Setd(const aValue: Double);
    procedure _Sete(const aValue: Double);
    procedure _Setf(const aValue: Double);
    procedure _Setm11(const aValue: Double);
    procedure _Setm12(const aValue: Double);
    procedure _Setm13(const aValue: Double);
    procedure _Setm14(const aValue: Double);
    procedure _Setm21(const aValue: Double);
    procedure _Setm22(const aValue: Double);
    procedure _Setm23(const aValue: Double);
    procedure _Setm24(const aValue: Double);
    procedure _Setm31(const aValue: Double);
    procedure _Setm32(const aValue: Double);
    procedure _Setm33(const aValue: Double);
    procedure _Setm34(const aValue: Double);
    procedure _Setm41(const aValue: Double);
    procedure _Setm42(const aValue: Double);
    procedure _Setm43(const aValue: Double);
    procedure _Setm44(const aValue: Double);
    function fromMatrix(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function fromMatrix: IJSDOMMatrix; overload;
    function fromFloat32Array(array32: IJSFloat32Array): IJSDOMMatrix;
    function fromFloat64Array(array64: IJSFloat64Array): IJSDOMMatrix;
    function multiplySelf(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function multiplySelf: IJSDOMMatrix; overload;
    function preMultiplySelf(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function preMultiplySelf: IJSDOMMatrix; overload;
    function translateSelf(aTx: Double; aTy: Double; aTz: Double): IJSDOMMatrix; overload;
    function translateSelf: IJSDOMMatrix; overload;
    function translateSelf(aTx: Double): IJSDOMMatrix; overload;
    function translateSelf(aTx: Double; aTy: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scaleSelf: IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scale3dSelf: IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double): IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function rotateSelf(aRotX: Double; aRotY: Double; aRotZ: Double): IJSDOMMatrix; overload;
    function rotateSelf: IJSDOMMatrix; overload;
    function rotateSelf(aRotX: Double): IJSDOMMatrix; overload;
    function rotateSelf(aRotX: Double; aRotY: Double): IJSDOMMatrix; overload;
    function rotateFromVectorSelf(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateFromVectorSelf: IJSDOMMatrix; overload;
    function rotateFromVectorSelf(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double; aY: Double; aZ: Double; angle: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf: IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double; aY: Double; aZ: Double): IJSDOMMatrix; overload;
    function skewXSelf(aSx: Double): IJSDOMMatrix; overload;
    function skewXSelf: IJSDOMMatrix; overload;
    function skewYSelf(aSy: Double): IJSDOMMatrix; overload;
    function skewYSelf: IJSDOMMatrix; overload;
    function invertSelf: IJSDOMMatrix;
    function setMatrixValue(const aTransformList: UTF8String): IJSDOMMatrix;
    property a: Double read _Geta write _Seta;
    property b: Double read _Getb write _Setb;
    property c: Double read _Getc write _Setc;
    property d: Double read _Getd write _Setd;
    property e: Double read _Gete write _Sete;
    property f: Double read _Getf write _Setf;
    property m11: Double read _Getm11 write _Setm11;
    property m12: Double read _Getm12 write _Setm12;
    property m13: Double read _Getm13 write _Setm13;
    property m14: Double read _Getm14 write _Setm14;
    property m21: Double read _Getm21 write _Setm21;
    property m22: Double read _Getm22 write _Setm22;
    property m23: Double read _Getm23 write _Setm23;
    property m24: Double read _Getm24 write _Setm24;
    property m31: Double read _Getm31 write _Setm31;
    property m32: Double read _Getm32 write _Setm32;
    property m33: Double read _Getm33 write _Setm33;
    property m34: Double read _Getm34 write _Setm34;
    property m41: Double read _Getm41 write _Setm41;
    property m42: Double read _Getm42 write _Setm42;
    property m43: Double read _Getm43 write _Setm43;
    property m44: Double read _Getm44 write _Setm44;
  end;

  TJSDOMMatrix = class(TJSDOMMatrixReadOnly,IJSDOMMatrix)
  Private
    function _Geta: Double;
    function _Getb: Double;
    function _Getc: Double;
    function _Getd: Double;
    function _Gete: Double;
    function _Getf: Double;
    function _Getm11: Double;
    function _Getm12: Double;
    function _Getm13: Double;
    function _Getm14: Double;
    function _Getm21: Double;
    function _Getm22: Double;
    function _Getm23: Double;
    function _Getm24: Double;
    function _Getm31: Double;
    function _Getm32: Double;
    function _Getm33: Double;
    function _Getm34: Double;
    function _Getm41: Double;
    function _Getm42: Double;
    function _Getm43: Double;
    function _Getm44: Double;
    procedure _Seta(const aValue: Double);
    procedure _Setb(const aValue: Double);
    procedure _Setc(const aValue: Double);
    procedure _Setd(const aValue: Double);
    procedure _Sete(const aValue: Double);
    procedure _Setf(const aValue: Double);
    procedure _Setm11(const aValue: Double);
    procedure _Setm12(const aValue: Double);
    procedure _Setm13(const aValue: Double);
    procedure _Setm14(const aValue: Double);
    procedure _Setm21(const aValue: Double);
    procedure _Setm22(const aValue: Double);
    procedure _Setm23(const aValue: Double);
    procedure _Setm24(const aValue: Double);
    procedure _Setm31(const aValue: Double);
    procedure _Setm32(const aValue: Double);
    procedure _Setm33(const aValue: Double);
    procedure _Setm34(const aValue: Double);
    procedure _Setm41(const aValue: Double);
    procedure _Setm42(const aValue: Double);
    procedure _Setm43(const aValue: Double);
    procedure _Setm44(const aValue: Double);
  Public
    function fromMatrix(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function fromMatrix: IJSDOMMatrix; overload;
    function fromFloat32Array(array32: IJSFloat32Array): IJSDOMMatrix;
    function fromFloat64Array(array64: IJSFloat64Array): IJSDOMMatrix;
    function multiplySelf(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function multiplySelf: IJSDOMMatrix; overload;
    function preMultiplySelf(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
    function preMultiplySelf: IJSDOMMatrix; overload;
    function translateSelf(aTx: Double; aTy: Double; aTz: Double): IJSDOMMatrix; overload;
    function translateSelf: IJSDOMMatrix; overload;
    function translateSelf(aTx: Double): IJSDOMMatrix; overload;
    function translateSelf(aTx: Double; aTy: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scaleSelf: IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
    function scale3dSelf: IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double): IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double; aOriginX: Double): IJSDOMMatrix; overload;
    function scale3dSelf(aScale: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
    function rotateSelf(aRotX: Double; aRotY: Double; aRotZ: Double): IJSDOMMatrix; overload;
    function rotateSelf: IJSDOMMatrix; overload;
    function rotateSelf(aRotX: Double): IJSDOMMatrix; overload;
    function rotateSelf(aRotX: Double; aRotY: Double): IJSDOMMatrix; overload;
    function rotateFromVectorSelf(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateFromVectorSelf: IJSDOMMatrix; overload;
    function rotateFromVectorSelf(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double; aY: Double; aZ: Double; angle: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf: IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double; aY: Double): IJSDOMMatrix; overload;
    function rotateAxisAngleSelf(aX: Double; aY: Double; aZ: Double): IJSDOMMatrix; overload;
    function skewXSelf(aSx: Double): IJSDOMMatrix; overload;
    function skewXSelf: IJSDOMMatrix; overload;
    function skewYSelf(aSy: Double): IJSDOMMatrix; overload;
    function skewYSelf: IJSDOMMatrix; overload;
    function invertSelf: IJSDOMMatrix;
    function setMatrixValue(const aTransformList: UTF8String): IJSDOMMatrix;
    class function Cast(Intf: IJSObject): IJSDOMMatrix;
    property a: Double read _Geta write _Seta;
    property b: Double read _Getb write _Setb;
    property c: Double read _Getc write _Setc;
    property d: Double read _Getd write _Setd;
    property e: Double read _Gete write _Sete;
    property f: Double read _Getf write _Setf;
    property m11: Double read _Getm11 write _Setm11;
    property m12: Double read _Getm12 write _Setm12;
    property m13: Double read _Getm13 write _Setm13;
    property m14: Double read _Getm14 write _Setm14;
    property m21: Double read _Getm21 write _Setm21;
    property m22: Double read _Getm22 write _Setm22;
    property m23: Double read _Getm23 write _Setm23;
    property m24: Double read _Getm24 write _Setm24;
    property m31: Double read _Getm31 write _Setm31;
    property m32: Double read _Getm32 write _Setm32;
    property m33: Double read _Getm33 write _Setm33;
    property m34: Double read _Getm34 write _Setm34;
    property m41: Double read _Getm41 write _Setm41;
    property m42: Double read _Getm42 write _Setm42;
    property m43: Double read _Getm43 write _Setm43;
    property m44: Double read _Getm44 write _Setm44;
  end;

  { --------------------------------------------------------------------
    TJSCSSStyleSheet
    --------------------------------------------------------------------}

  IJSCSSStyleSheet = interface(IJSStyleSheet)
    ['{CCE1DD0C-6115-383C-95A7-FE88E67443F3}']
    function _GetownerRule: IJSCSSRule;
    function _GetcssRules: IJSCSSRuleList;
    function _GetparsingMode: TCSSStyleSheetParsingMode;
    function _Getrules: IJSCSSRuleList;
    function insertRule(const aRule: UTF8String; aIndex: LongWord): LongWord; overload;
    function insertRule(const aRule: UTF8String): LongWord; overload;
    procedure deleteRule(aIndex: LongWord);
    procedure replaceSync(const aText: UTF8String);
    procedure removeRule(aIndex: LongWord); overload;
    procedure removeRule; overload;
    function addRule(const aSelector: UTF8String; const aStyle: UTF8String; aIndex: LongWord): Integer; overload;
    function addRule: Integer; overload;
    function addRule(const aSelector: UTF8String): Integer; overload;
    function addRule(const aSelector: UTF8String; const aStyle: UTF8String): Integer; overload;
    property ownerRule: IJSCSSRule read _GetownerRule;
    property cssRules: IJSCSSRuleList read _GetcssRules;
    property parsingMode: TCSSStyleSheetParsingMode read _GetparsingMode;
    property rules: IJSCSSRuleList read _Getrules;
  end;

  TJSCSSStyleSheet = class(TJSStyleSheet,IJSCSSStyleSheet)
  Private
    function _GetownerRule: IJSCSSRule;
    function _GetcssRules: IJSCSSRuleList;
    function _GetparsingMode: TCSSStyleSheetParsingMode;
    function _Getrules: IJSCSSRuleList;
  Public
    function insertRule(const aRule: UTF8String; aIndex: LongWord): LongWord; overload;
    function insertRule(const aRule: UTF8String): LongWord; overload;
    procedure deleteRule(aIndex: LongWord);
    procedure replaceSync(const aText: UTF8String);
    procedure removeRule(aIndex: LongWord); overload;
    procedure removeRule; overload;
    function addRule(const aSelector: UTF8String; const aStyle: UTF8String; aIndex: LongWord): Integer; overload;
    function addRule: Integer; overload;
    function addRule(const aSelector: UTF8String): Integer; overload;
    function addRule(const aSelector: UTF8String; const aStyle: UTF8String): Integer; overload;
    class function Cast(Intf: IJSObject): IJSCSSStyleSheet;
    property ownerRule: IJSCSSRule read _GetownerRule;
    property cssRules: IJSCSSRuleList read _GetcssRules;
    property parsingMode: TCSSStyleSheetParsingMode read _GetparsingMode;
    property rules: IJSCSSRuleList read _Getrules;
  end;

  { --------------------------------------------------------------------
    TJSDOMPoint
    --------------------------------------------------------------------}

  IJSDOMPoint = interface(IJSDOMPointReadOnly)
    ['{E8310D67-5965-36FE-81F8-D057D0DF6634}']
    function _Getx: Double;
    function _Gety: Double;
    function _Getz: Double;
    function _Getw: Double;
    procedure _Setx(const aValue: Double);
    procedure _Sety(const aValue: Double);
    procedure _Setz(const aValue: Double);
    procedure _Setw(const aValue: Double);
    function fromPoint(const aOther: TJSDOMPointInit): IJSDOMPoint; overload;
    function fromPoint: IJSDOMPoint; overload;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property z: Double read _Getz write _Setz;
    property w: Double read _Getw write _Setw;
  end;

  TJSDOMPoint = class(TJSDOMPointReadOnly,IJSDOMPoint)
  Private
    function _Getx: Double;
    function _Gety: Double;
    function _Getz: Double;
    function _Getw: Double;
    procedure _Setx(const aValue: Double);
    procedure _Sety(const aValue: Double);
    procedure _Setz(const aValue: Double);
    procedure _Setw(const aValue: Double);
  Public
    function fromPoint(const aOther: TJSDOMPointInit): IJSDOMPoint; overload;
    function fromPoint: IJSDOMPoint; overload;
    class function Cast(Intf: IJSObject): IJSDOMPoint;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property z: Double read _Getz write _Setz;
    property w: Double read _Getw write _Setw;
  end;

  { --------------------------------------------------------------------
    TJSOffscreenCanvas
    --------------------------------------------------------------------}

  IJSOffscreenCanvas = interface(IJSEventTarget)
    ['{6F1CFA6C-A57B-3962-973F-68AF3902ED2D}']
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    procedure _Setwidth(const aValue: LongWord);
    procedure _Setheight(const aValue: LongWord);
    function getContext(aContextId: TOffscreenRenderingContextId; aContextOptions: TJOB_JSValue): TOffscreenRenderingContext; overload;
    function getContext(aContextId: TOffscreenRenderingContextId): TOffscreenRenderingContext; overload;
    function transferToImageBitmap: IJSImageBitmap;
    property width: LongWord read _Getwidth write _Setwidth;
    property height: LongWord read _Getheight write _Setheight;
    // property oncontextlost: TEventHandler read _Getoncontextlost write _Setoncontextlost;
    // property oncontextrestored: TEventHandler read _Getoncontextrestored write _Setoncontextrestored;
  end;

  TJSOffscreenCanvas = class(TJSEventTarget,IJSOffscreenCanvas)
  Private
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    procedure _Setwidth(const aValue: LongWord);
    procedure _Setheight(const aValue: LongWord);
  Public
    function getContext(aContextId: TOffscreenRenderingContextId; aContextOptions: TJOB_JSValue): TOffscreenRenderingContext; overload;
    function getContext(aContextId: TOffscreenRenderingContextId): TOffscreenRenderingContext; overload;
    function transferToImageBitmap: IJSImageBitmap;
    class function Cast(Intf: IJSObject): IJSOffscreenCanvas;
    property width: LongWord read _Getwidth write _Setwidth;
    property height: LongWord read _Getheight write _Setheight;
    // property oncontextlost: TEventHandler read _Getoncontextlost write _Setoncontextlost;
    // property oncontextrestored: TEventHandler read _Getoncontextrestored write _Setoncontextrestored;
  end;

  { --------------------------------------------------------------------
    TJSFile
    --------------------------------------------------------------------}

  IJSFile = interface(IJSBlob)
    ['{01F41F57-BE6F-37DF-B5EA-750A759D1B16}']
    function _Getname: UnicodeString;
    function _GetlastModified: Int64;
    function _GetwebkitRelativePath: UnicodeString;
    function _GetmozFullPath: UnicodeString;
    property name: UnicodeString read _Getname;
    property lastModified: Int64 read _GetlastModified;
    property webkitRelativePath: UnicodeString read _GetwebkitRelativePath;
    property mozFullPath: UnicodeString read _GetmozFullPath;
  end;

  TJSFile = class(TJSBlob,IJSFile)
  Private
    function _Getname: UnicodeString;
    function _GetlastModified: Int64;
    function _GetwebkitRelativePath: UnicodeString;
    function _GetmozFullPath: UnicodeString;
  Public
    class function Cast(Intf: IJSObject): IJSFile;
    property name: UnicodeString read _Getname;
    property lastModified: Int64 read _GetlastModified;
    property webkitRelativePath: UnicodeString read _GetwebkitRelativePath;
    property mozFullPath: UnicodeString read _GetmozFullPath;
  end;

  { --------------------------------------------------------------------
    TJSFileSystemDirectoryEntry
    --------------------------------------------------------------------}

  IJSFileSystemDirectoryEntry = interface(IJSFileSystemEntry)
    ['{54EBD578-41E5-3944-8ABB-BD524B6FF36B}']
    function createReader: IJSFileSystemDirectoryReader;
  end;

  TJSFileSystemDirectoryEntry = class(TJSFileSystemEntry,IJSFileSystemDirectoryEntry)
  Private
  Public
    function createReader: IJSFileSystemDirectoryReader;
    class function Cast(Intf: IJSObject): IJSFileSystemDirectoryEntry;
  end;

  { --------------------------------------------------------------------
    TJSDocument
    --------------------------------------------------------------------}

  IJSDocument = interface(IJSNode)
    ['{A1B6B3EF-9623-307D-B3D8-CB586BF44C96}']
    function _Getimplementation_: IJSDOMImplementation;
    function _GetURL: UnicodeString;
    function _GetdocumentURI: UnicodeString;
    function _GetcompatMode: UnicodeString;
    function _GetcharacterSet: UnicodeString;
    function _Getcharset: UnicodeString;
    function _GetinputEncoding: UnicodeString;
    function _GetcontentType: UnicodeString;
    function _Getdoctype: IJSDocumentType;
    function _GetdocumentElement: IJSElement;
    function _Getlocation: IJSLocation;
    function _Getdomain: UnicodeString;
    function _Getreferrer: UnicodeString;
    function _Getcookie: UnicodeString;
    function _GetlastModified: UnicodeString;
    function _GetreadyState: UnicodeString;
    function _Gettitle: UnicodeString;
    function _Getdir: UnicodeString;
    function _Getbody: IJSHTMLElement;
    function _Gethead: IJSHTMLHeadElement;
    function _Getimages: IJSHTMLCollection;
    function _Getembeds: IJSHTMLCollection;
    function _Getplugins: IJSHTMLCollection;
    function _Getlinks: IJSHTMLCollection;
    function _Getforms: IJSHTMLCollection;
    function _Getscripts: IJSHTMLCollection;
    function _GetdefaultView: IJSWindowProxy;
    function _GetdesignMode: UnicodeString;
    function _GetmozSyntheticDocument: Boolean;
    function _GetcurrentScript: IJSElement;
    function _GetdocumentURIObject: IJSURI;
    function _GetreferrerPolicy: TReferrerPolicy;
    function _GetreferrerInfo: IJSnsIReferrerInfo;
    function _GetfgColor: UnicodeString;
    function _GetlinkColor: UnicodeString;
    function _GetvlinkColor: UnicodeString;
    function _GetalinkColor: UnicodeString;
    function _GetbgColor: UnicodeString;
    function _Getanchors: IJSHTMLCollection;
    function _Getapplets: IJSHTMLCollection;
    function _Getall: IJSHTMLAllCollection;
    function _Getfullscreen: Boolean;
    function _GetmozFullScreen: Boolean;
    function _GetfullscreenEnabled: Boolean;
    function _GetmozFullScreenEnabled: Boolean;
    function _GetallowDeprecatedTls: Boolean;
    function _Gethidden: Boolean;
    function _GetvisibilityState: TVisibilityState;
    function _GetselectedStyleSheetSet: UnicodeString;
    function _GetlastStyleSheetSet: UnicodeString;
    function _GetpreferredStyleSheetSet: UnicodeString;
    function _GetstyleSheetSets: IJSDOMStringList;
    function _GetscrollingElement: IJSElement;
    function _GetrootElement: IJSSVGSVGElement;
    function _GetloadedFromPrototype: Boolean;
    function _GeteffectiveStoragePrincipal: IJSPrincipal;
    function _GetpartitionedPrincipal: IJSPrincipal;
    function _GetcookieJarSettings: IJSnsICookieJarSettings;
    function _GetstyleSheetChangeEventsEnabled: Boolean;
    function _GetshadowRootAttachedEventEnabled: Boolean;
    function _GetcontentLanguage: UnicodeString;
    function _GetdocumentLoadGroup: IJSnsILoadGroup;
    function _GetmozDocumentURIIfNotForErrorPages: IJSURI;
    function _GetdocumentReadyForIdle: IJSPromise;
    function _GetcommandDispatcher: IJSXULCommandDispatcher;
    function _GetdevToolsWatchingDOMMutations: Boolean;
    function _GetisSrcdocDocument: Boolean;
    function _GetsandboxFlagsAsString: UnicodeString;
    function _GetautoplayPolicy: TDocumentAutoplayPolicy;
    function _GetuserHasInteracted: Boolean;
    function _GethasBeenUserGestureActivated: Boolean;
    function _GethasValidTransientUserGestureActivation: Boolean;
    function _Getcsp: IJSContentSecurityPolicy;
    function _GetcspJSON: UnicodeString;
    function _GetdocumentFlashClassification: TFlashClassification;
    function _GetblockedNodeByClassifierCount: Integer;
    function _GetblockedNodesByClassifier: IJSNodeList;
    function _GetpermDelegateHandler: IJSnsIPermissionDelegateHandler;
    function _GetisInitialDocument: Boolean;
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
    function _GetactiveElement: IJSElement;
    function _GetpointerLockElement: IJSElement;
    function _GetfullscreenElement: IJSElement;
    function _GetmozFullScreenElement: IJSElement;
    procedure _Setdomain(const aValue: UnicodeString);
    procedure _Setcookie(const aValue: UnicodeString);
    procedure _Settitle(const aValue: UnicodeString);
    procedure _Setdir(const aValue: UnicodeString);
    procedure _Setbody(const aValue: IJSHTMLElement);
    procedure _SetdesignMode(const aValue: UnicodeString);
    procedure _SetfgColor(const aValue: UnicodeString);
    procedure _SetlinkColor(const aValue: UnicodeString);
    procedure _SetvlinkColor(const aValue: UnicodeString);
    procedure _SetalinkColor(const aValue: UnicodeString);
    procedure _SetbgColor(const aValue: UnicodeString);
    procedure _SetallowDeprecatedTls(const aValue: Boolean);
    procedure _SetselectedStyleSheetSet(const aValue: UnicodeString);
    procedure _SetstyleSheetChangeEventsEnabled(const aValue: Boolean);
    procedure _SetshadowRootAttachedEventEnabled(const aValue: Boolean);
    procedure _SetdevToolsWatchingDOMMutations(const aValue: Boolean);
    function getElementsByTagName(const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByTagNameNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByClassName(const aClassNames: UnicodeString): IJSHTMLCollection;
    function getElementById(const aElementId: UnicodeString): IJSElement;
    function createElement(const aLocalName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
    function createElement(const aLocalName: UnicodeString): IJSElement; overload;
    function createElement(const aLocalName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
    function createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
    function createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString): IJSElement; overload;
    function createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
    function createDocumentFragment: IJSDocumentFragment;
    function createTextNode(const aData: UnicodeString): IJSText;
    function createComment(const aData: UnicodeString): IJSComment;
    function importNode(aNode: IJSNode; aDeep: Boolean): IJSNode; overload;
    function importNode(aNode: IJSNode): IJSNode; overload;
    function adoptNode(aNode: IJSNode): IJSNode;
    function createEvent(const aInterface_: UnicodeString): IJSEvent;
    function createCDATASection(const aData: UnicodeString): IJSCDATASection;
    function createAttribute(const aName: UnicodeString): IJSAttr;
    function createAttributeNS(const aNamespace: UnicodeString; const aName: UnicodeString): IJSAttr;
    function getElementsByName(const aElementName: UnicodeString): IJSNodeList;
    function open(const aUnused1: UnicodeString; const aUnused2: UnicodeString): IJSDocument; overload;
    function open: IJSDocument; overload;
    function open(const aUnused1: UnicodeString): IJSDocument; overload;
    function open(const aUrl: UnicodeString; const aName: UnicodeString; const aFeatures: UnicodeString): IJSWindowProxy;
    procedure close;
    procedure write(const aText: UnicodeString){; ToDo:varargs};
    procedure writeln(const aText: UnicodeString){; ToDo:varargs};
    function hasFocus: Boolean;
    function execCommand(const aCommandId: UnicodeString; aShowUI: Boolean; const aValue: UnicodeString): Boolean; overload;
    function execCommand(const aCommandId: UnicodeString): Boolean; overload;
    function execCommand(const aCommandId: UnicodeString; aShowUI: Boolean): Boolean; overload;
    function queryCommandEnabled(const aCommandId: UnicodeString): Boolean;
    function queryCommandIndeterm(const aCommandId: UnicodeString): Boolean;
    function queryCommandState(const aCommandId: UnicodeString): Boolean;
    function queryCommandSupported(const aCommandId: UnicodeString): Boolean;
    function queryCommandValue(const aCommandId: UnicodeString): UnicodeString;
    procedure releaseCapture;
    procedure mozSetImageElement(const aImageElementId: UnicodeString; aImageElement: IJSElement);
    procedure clear;
    procedure captureEvents;
    procedure releaseEvents;
    procedure exitPointerLock;
    procedure reloadWithHttpsOnlyException;
    procedure enableStyleSheetsForSet(const aName: UnicodeString);
    function caretPositionFromPoint(aX: Single; aY: Single): IJSCaretPosition;
    function querySelector(const aSelectors: UTF8String): IJSElement;
    function querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
    function createXULElement(const aLocalName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
    function createXULElement(const aLocalName: UnicodeString): IJSElement; overload;
    function createXULElement(const aLocalName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
    procedure blockUnblockOnload(aBlock: Boolean);
    function getSelection: IJSSelection;
    procedure notifyUserGestureActivation;
    procedure clearUserGestureActivation;
    function consumeTransientUserGestureActivation: Boolean;
    procedure setSuppressedEventListener(const aListener: TEventListener);
    procedure setKeyPressEventModel(aKeyPressEventModel: Word);
    procedure userInteractionForTesting;
    procedure setNotifyFetchSuccess(aShouldNotify: Boolean);
    procedure setNotifyFormOrPasswordRemoved(aShouldNotify: Boolean);
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    function elementFromPoint(aX: Single; aY: Single): IJSElement;
    function elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
    function nodeFromPoint(aX: Single; aY: Single): IJSNode;
    function nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
    property implementation_: IJSDOMImplementation read _Getimplementation_;
    property URL: UnicodeString read _GetURL;
    property documentURI: UnicodeString read _GetdocumentURI;
    property compatMode: UnicodeString read _GetcompatMode;
    property characterSet: UnicodeString read _GetcharacterSet;
    property charset: UnicodeString read _Getcharset;
    property inputEncoding: UnicodeString read _GetinputEncoding;
    property contentType: UnicodeString read _GetcontentType;
    property doctype: IJSDocumentType read _Getdoctype;
    property documentElement: IJSElement read _GetdocumentElement;
    property location: IJSLocation read _Getlocation;
    property domain: UnicodeString read _Getdomain write _Setdomain;
    property referrer: UnicodeString read _Getreferrer;
    property cookie: UnicodeString read _Getcookie write _Setcookie;
    property lastModified: UnicodeString read _GetlastModified;
    property readyState: UnicodeString read _GetreadyState;
    property title: UnicodeString read _Gettitle write _Settitle;
    property dir: UnicodeString read _Getdir write _Setdir;
    property body: IJSHTMLElement read _Getbody write _Setbody;
    property head: IJSHTMLHeadElement read _Gethead;
    property images: IJSHTMLCollection read _Getimages;
    property embeds: IJSHTMLCollection read _Getembeds;
    property plugins: IJSHTMLCollection read _Getplugins;
    property links: IJSHTMLCollection read _Getlinks;
    property forms: IJSHTMLCollection read _Getforms;
    property scripts: IJSHTMLCollection read _Getscripts;
    property defaultView: IJSWindowProxy read _GetdefaultView;
    property designMode: UnicodeString read _GetdesignMode write _SetdesignMode;
    // property onreadystatechange: TEventHandler read _Getonreadystatechange write _Setonreadystatechange;
    // property onbeforescriptexecute: TEventHandler read _Getonbeforescriptexecute write _Setonbeforescriptexecute;
    // property onafterscriptexecute: TEventHandler read _Getonafterscriptexecute write _Setonafterscriptexecute;
    property mozSyntheticDocument: Boolean read _GetmozSyntheticDocument;
    property currentScript: IJSElement read _GetcurrentScript;
    property documentURIObject: IJSURI read _GetdocumentURIObject;
    property referrerPolicy: TReferrerPolicy read _GetreferrerPolicy;
    property referrerInfo: IJSnsIReferrerInfo read _GetreferrerInfo;
    property fgColor: UnicodeString read _GetfgColor write _SetfgColor;
    property linkColor: UnicodeString read _GetlinkColor write _SetlinkColor;
    property vlinkColor: UnicodeString read _GetvlinkColor write _SetvlinkColor;
    property alinkColor: UnicodeString read _GetalinkColor write _SetalinkColor;
    property bgColor: UnicodeString read _GetbgColor write _SetbgColor;
    property anchors: IJSHTMLCollection read _Getanchors;
    property applets: IJSHTMLCollection read _Getapplets;
    property all: IJSHTMLAllCollection read _Getall;
    property fullscreen: Boolean read _Getfullscreen;
    property mozFullScreen: Boolean read _GetmozFullScreen;
    property fullscreenEnabled: Boolean read _GetfullscreenEnabled;
    property mozFullScreenEnabled: Boolean read _GetmozFullScreenEnabled;
    // property onfullscreenchange: TEventHandler read _Getonfullscreenchange write _Setonfullscreenchange;
    // property onfullscreenerror: TEventHandler read _Getonfullscreenerror write _Setonfullscreenerror;
    // property onpointerlockchange: TEventHandler read _Getonpointerlockchange write _Setonpointerlockchange;
    // property onpointerlockerror: TEventHandler read _Getonpointerlockerror write _Setonpointerlockerror;
    property allowDeprecatedTls: Boolean read _GetallowDeprecatedTls write _SetallowDeprecatedTls;
    property hidden: Boolean read _Gethidden;
    property visibilityState: TVisibilityState read _GetvisibilityState;
    // property onvisibilitychange: TEventHandler read _Getonvisibilitychange write _Setonvisibilitychange;
    property selectedStyleSheetSet: UnicodeString read _GetselectedStyleSheetSet write _SetselectedStyleSheetSet;
    property lastStyleSheetSet: UnicodeString read _GetlastStyleSheetSet;
    property preferredStyleSheetSet: UnicodeString read _GetpreferredStyleSheetSet;
    property styleSheetSets: IJSDOMStringList read _GetstyleSheetSets;
    property scrollingElement: IJSElement read _GetscrollingElement;
    property rootElement: IJSSVGSVGElement read _GetrootElement;
    property loadedFromPrototype: Boolean read _GetloadedFromPrototype;
    property effectiveStoragePrincipal: IJSPrincipal read _GeteffectiveStoragePrincipal;
    property partitionedPrincipal: IJSPrincipal read _GetpartitionedPrincipal;
    property cookieJarSettings: IJSnsICookieJarSettings read _GetcookieJarSettings;
    property styleSheetChangeEventsEnabled: Boolean read _GetstyleSheetChangeEventsEnabled write _SetstyleSheetChangeEventsEnabled;
    property shadowRootAttachedEventEnabled: Boolean read _GetshadowRootAttachedEventEnabled write _SetshadowRootAttachedEventEnabled;
    property contentLanguage: UnicodeString read _GetcontentLanguage;
    property documentLoadGroup: IJSnsILoadGroup read _GetdocumentLoadGroup;
    property mozDocumentURIIfNotForErrorPages: IJSURI read _GetmozDocumentURIIfNotForErrorPages;
    property documentReadyForIdle: IJSPromise read _GetdocumentReadyForIdle;
    property commandDispatcher: IJSXULCommandDispatcher read _GetcommandDispatcher;
    property devToolsWatchingDOMMutations: Boolean read _GetdevToolsWatchingDOMMutations write _SetdevToolsWatchingDOMMutations;
    property isSrcdocDocument: Boolean read _GetisSrcdocDocument;
    property sandboxFlagsAsString: UnicodeString read _GetsandboxFlagsAsString;
    property autoplayPolicy: TDocumentAutoplayPolicy read _GetautoplayPolicy;
    property userHasInteracted: Boolean read _GetuserHasInteracted;
    property hasBeenUserGestureActivated: Boolean read _GethasBeenUserGestureActivated;
    property hasValidTransientUserGestureActivation: Boolean read _GethasValidTransientUserGestureActivation;
    property csp: IJSContentSecurityPolicy read _Getcsp;
    property cspJSON: UnicodeString read _GetcspJSON;
    property documentFlashClassification: TFlashClassification read _GetdocumentFlashClassification;
    property blockedNodeByClassifierCount: Integer read _GetblockedNodeByClassifierCount;
    property blockedNodesByClassifier: IJSNodeList read _GetblockedNodesByClassifier;
    property permDelegateHandler: IJSnsIPermissionDelegateHandler read _GetpermDelegateHandler;
    property isInitialDocument: Boolean read _GetisInitialDocument;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
    property activeElement: IJSElement read _GetactiveElement;
    property pointerLockElement: IJSElement read _GetpointerLockElement;
    property fullscreenElement: IJSElement read _GetfullscreenElement;
    property mozFullScreenElement: IJSElement read _GetmozFullScreenElement;
  end;

  TJSDocument = class(TJSNode,IJSDocument)
  Private
    function _Getimplementation_: IJSDOMImplementation;
    function _GetURL: UnicodeString;
    function _GetdocumentURI: UnicodeString;
    function _GetcompatMode: UnicodeString;
    function _GetcharacterSet: UnicodeString;
    function _Getcharset: UnicodeString;
    function _GetinputEncoding: UnicodeString;
    function _GetcontentType: UnicodeString;
    function _Getdoctype: IJSDocumentType;
    function _GetdocumentElement: IJSElement;
    function _Getlocation: IJSLocation;
    function _Getdomain: UnicodeString;
    function _Getreferrer: UnicodeString;
    function _Getcookie: UnicodeString;
    function _GetlastModified: UnicodeString;
    function _GetreadyState: UnicodeString;
    function _Gettitle: UnicodeString;
    function _Getdir: UnicodeString;
    function _Getbody: IJSHTMLElement;
    function _Gethead: IJSHTMLHeadElement;
    function _Getimages: IJSHTMLCollection;
    function _Getembeds: IJSHTMLCollection;
    function _Getplugins: IJSHTMLCollection;
    function _Getlinks: IJSHTMLCollection;
    function _Getforms: IJSHTMLCollection;
    function _Getscripts: IJSHTMLCollection;
    function _GetdefaultView: IJSWindowProxy;
    function _GetdesignMode: UnicodeString;
    function _GetmozSyntheticDocument: Boolean;
    function _GetcurrentScript: IJSElement;
    function _GetdocumentURIObject: IJSURI;
    function _GetreferrerPolicy: TReferrerPolicy;
    function _GetreferrerInfo: IJSnsIReferrerInfo;
    function _GetfgColor: UnicodeString;
    function _GetlinkColor: UnicodeString;
    function _GetvlinkColor: UnicodeString;
    function _GetalinkColor: UnicodeString;
    function _GetbgColor: UnicodeString;
    function _Getanchors: IJSHTMLCollection;
    function _Getapplets: IJSHTMLCollection;
    function _Getall: IJSHTMLAllCollection;
    function _Getfullscreen: Boolean;
    function _GetmozFullScreen: Boolean;
    function _GetfullscreenEnabled: Boolean;
    function _GetmozFullScreenEnabled: Boolean;
    function _GetallowDeprecatedTls: Boolean;
    function _Gethidden: Boolean;
    function _GetvisibilityState: TVisibilityState;
    function _GetselectedStyleSheetSet: UnicodeString;
    function _GetlastStyleSheetSet: UnicodeString;
    function _GetpreferredStyleSheetSet: UnicodeString;
    function _GetstyleSheetSets: IJSDOMStringList;
    function _GetscrollingElement: IJSElement;
    function _GetrootElement: IJSSVGSVGElement;
    function _GetloadedFromPrototype: Boolean;
    function _GeteffectiveStoragePrincipal: IJSPrincipal;
    function _GetpartitionedPrincipal: IJSPrincipal;
    function _GetcookieJarSettings: IJSnsICookieJarSettings;
    function _GetstyleSheetChangeEventsEnabled: Boolean;
    function _GetshadowRootAttachedEventEnabled: Boolean;
    function _GetcontentLanguage: UnicodeString;
    function _GetdocumentLoadGroup: IJSnsILoadGroup;
    function _GetmozDocumentURIIfNotForErrorPages: IJSURI;
    function _GetdocumentReadyForIdle: IJSPromise;
    function _GetcommandDispatcher: IJSXULCommandDispatcher;
    function _GetdevToolsWatchingDOMMutations: Boolean;
    function _GetisSrcdocDocument: Boolean;
    function _GetsandboxFlagsAsString: UnicodeString;
    function _GetautoplayPolicy: TDocumentAutoplayPolicy;
    function _GetuserHasInteracted: Boolean;
    function _GethasBeenUserGestureActivated: Boolean;
    function _GethasValidTransientUserGestureActivation: Boolean;
    function _Getcsp: IJSContentSecurityPolicy;
    function _GetcspJSON: UnicodeString;
    function _GetdocumentFlashClassification: TFlashClassification;
    function _GetblockedNodeByClassifierCount: Integer;
    function _GetblockedNodesByClassifier: IJSNodeList;
    function _GetpermDelegateHandler: IJSnsIPermissionDelegateHandler;
    function _GetisInitialDocument: Boolean;
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
    function _GetactiveElement: IJSElement;
    function _GetpointerLockElement: IJSElement;
    function _GetfullscreenElement: IJSElement;
    function _GetmozFullScreenElement: IJSElement;
    procedure _Setdomain(const aValue: UnicodeString);
    procedure _Setcookie(const aValue: UnicodeString);
    procedure _Settitle(const aValue: UnicodeString);
    procedure _Setdir(const aValue: UnicodeString);
    procedure _Setbody(const aValue: IJSHTMLElement);
    procedure _SetdesignMode(const aValue: UnicodeString);
    procedure _SetfgColor(const aValue: UnicodeString);
    procedure _SetlinkColor(const aValue: UnicodeString);
    procedure _SetvlinkColor(const aValue: UnicodeString);
    procedure _SetalinkColor(const aValue: UnicodeString);
    procedure _SetbgColor(const aValue: UnicodeString);
    procedure _SetallowDeprecatedTls(const aValue: Boolean);
    procedure _SetselectedStyleSheetSet(const aValue: UnicodeString);
    procedure _SetstyleSheetChangeEventsEnabled(const aValue: Boolean);
    procedure _SetshadowRootAttachedEventEnabled(const aValue: Boolean);
    procedure _SetdevToolsWatchingDOMMutations(const aValue: Boolean);
  Public
    Const
      KEYPRESS_EVENT_MODEL_DEFAULT = 0;
      KEYPRESS_EVENT_MODEL_SPLIT = 1;
      KEYPRESS_EVENT_MODEL_CONFLATED = 2;
  Public
    function getElementsByTagName(const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByTagNameNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByClassName(const aClassNames: UnicodeString): IJSHTMLCollection;
    function getElementById(const aElementId: UnicodeString): IJSElement;
    function createElement(const aLocalName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
    function createElement(const aLocalName: UnicodeString): IJSElement; overload;
    function createElement(const aLocalName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
    function createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
    function createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString): IJSElement; overload;
    function createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
    function createDocumentFragment: IJSDocumentFragment;
    function createTextNode(const aData: UnicodeString): IJSText;
    function createComment(const aData: UnicodeString): IJSComment;
    function importNode(aNode: IJSNode; aDeep: Boolean): IJSNode; overload;
    function importNode(aNode: IJSNode): IJSNode; overload;
    function adoptNode(aNode: IJSNode): IJSNode;
    function createEvent(const aInterface_: UnicodeString): IJSEvent;
    function createCDATASection(const aData: UnicodeString): IJSCDATASection;
    function createAttribute(const aName: UnicodeString): IJSAttr;
    function createAttributeNS(const aNamespace: UnicodeString; const aName: UnicodeString): IJSAttr;
    function getElementsByName(const aElementName: UnicodeString): IJSNodeList;
    function open(const aUnused1: UnicodeString; const aUnused2: UnicodeString): IJSDocument; overload;
    function open: IJSDocument; overload;
    function open(const aUnused1: UnicodeString): IJSDocument; overload;
    function open(const aUrl: UnicodeString; const aName: UnicodeString; const aFeatures: UnicodeString): IJSWindowProxy;
    procedure close;
    procedure write(const aText: UnicodeString){; ToDo:varargs};
    procedure writeln(const aText: UnicodeString){; ToDo:varargs};
    function hasFocus: Boolean;
    function execCommand(const aCommandId: UnicodeString; aShowUI: Boolean; const aValue: UnicodeString): Boolean; overload;
    function execCommand(const aCommandId: UnicodeString): Boolean; overload;
    function execCommand(const aCommandId: UnicodeString; aShowUI: Boolean): Boolean; overload;
    function queryCommandEnabled(const aCommandId: UnicodeString): Boolean;
    function queryCommandIndeterm(const aCommandId: UnicodeString): Boolean;
    function queryCommandState(const aCommandId: UnicodeString): Boolean;
    function queryCommandSupported(const aCommandId: UnicodeString): Boolean;
    function queryCommandValue(const aCommandId: UnicodeString): UnicodeString;
    procedure releaseCapture;
    procedure mozSetImageElement(const aImageElementId: UnicodeString; aImageElement: IJSElement);
    procedure clear;
    procedure captureEvents;
    procedure releaseEvents;
    procedure exitPointerLock;
    procedure reloadWithHttpsOnlyException;
    procedure enableStyleSheetsForSet(const aName: UnicodeString);
    function caretPositionFromPoint(aX: Single; aY: Single): IJSCaretPosition;
    function querySelector(const aSelectors: UTF8String): IJSElement;
    function querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
    function createXULElement(const aLocalName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
    function createXULElement(const aLocalName: UnicodeString): IJSElement; overload;
    function createXULElement(const aLocalName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
    procedure blockUnblockOnload(aBlock: Boolean);
    function getSelection: IJSSelection;
    procedure notifyUserGestureActivation;
    procedure clearUserGestureActivation;
    function consumeTransientUserGestureActivation: Boolean;
    procedure setSuppressedEventListener(const aListener: TEventListener);
    procedure setKeyPressEventModel(aKeyPressEventModel: Word);
    procedure userInteractionForTesting;
    procedure setNotifyFetchSuccess(aShouldNotify: Boolean);
    procedure setNotifyFormOrPasswordRemoved(aShouldNotify: Boolean);
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    function elementFromPoint(aX: Single; aY: Single): IJSElement;
    function elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
    function nodeFromPoint(aX: Single; aY: Single): IJSNode;
    function nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
    class function Cast(Intf: IJSObject): IJSDocument;
    property implementation_: IJSDOMImplementation read _Getimplementation_;
    property URL: UnicodeString read _GetURL;
    property documentURI: UnicodeString read _GetdocumentURI;
    property compatMode: UnicodeString read _GetcompatMode;
    property characterSet: UnicodeString read _GetcharacterSet;
    property charset: UnicodeString read _Getcharset;
    property inputEncoding: UnicodeString read _GetinputEncoding;
    property contentType: UnicodeString read _GetcontentType;
    property doctype: IJSDocumentType read _Getdoctype;
    property documentElement: IJSElement read _GetdocumentElement;
    property location: IJSLocation read _Getlocation;
    property domain: UnicodeString read _Getdomain write _Setdomain;
    property referrer: UnicodeString read _Getreferrer;
    property cookie: UnicodeString read _Getcookie write _Setcookie;
    property lastModified: UnicodeString read _GetlastModified;
    property readyState: UnicodeString read _GetreadyState;
    property title: UnicodeString read _Gettitle write _Settitle;
    property dir: UnicodeString read _Getdir write _Setdir;
    property body: IJSHTMLElement read _Getbody write _Setbody;
    property head: IJSHTMLHeadElement read _Gethead;
    property images: IJSHTMLCollection read _Getimages;
    property embeds: IJSHTMLCollection read _Getembeds;
    property plugins: IJSHTMLCollection read _Getplugins;
    property links: IJSHTMLCollection read _Getlinks;
    property forms: IJSHTMLCollection read _Getforms;
    property scripts: IJSHTMLCollection read _Getscripts;
    property defaultView: IJSWindowProxy read _GetdefaultView;
    property designMode: UnicodeString read _GetdesignMode write _SetdesignMode;
    // property onreadystatechange: TEventHandler read _Getonreadystatechange write _Setonreadystatechange;
    // property onbeforescriptexecute: TEventHandler read _Getonbeforescriptexecute write _Setonbeforescriptexecute;
    // property onafterscriptexecute: TEventHandler read _Getonafterscriptexecute write _Setonafterscriptexecute;
    property mozSyntheticDocument: Boolean read _GetmozSyntheticDocument;
    property currentScript: IJSElement read _GetcurrentScript;
    property documentURIObject: IJSURI read _GetdocumentURIObject;
    property referrerPolicy: TReferrerPolicy read _GetreferrerPolicy;
    property referrerInfo: IJSnsIReferrerInfo read _GetreferrerInfo;
    property fgColor: UnicodeString read _GetfgColor write _SetfgColor;
    property linkColor: UnicodeString read _GetlinkColor write _SetlinkColor;
    property vlinkColor: UnicodeString read _GetvlinkColor write _SetvlinkColor;
    property alinkColor: UnicodeString read _GetalinkColor write _SetalinkColor;
    property bgColor: UnicodeString read _GetbgColor write _SetbgColor;
    property anchors: IJSHTMLCollection read _Getanchors;
    property applets: IJSHTMLCollection read _Getapplets;
    property all: IJSHTMLAllCollection read _Getall;
    property fullscreen: Boolean read _Getfullscreen;
    property mozFullScreen: Boolean read _GetmozFullScreen;
    property fullscreenEnabled: Boolean read _GetfullscreenEnabled;
    property mozFullScreenEnabled: Boolean read _GetmozFullScreenEnabled;
    // property onfullscreenchange: TEventHandler read _Getonfullscreenchange write _Setonfullscreenchange;
    // property onfullscreenerror: TEventHandler read _Getonfullscreenerror write _Setonfullscreenerror;
    // property onpointerlockchange: TEventHandler read _Getonpointerlockchange write _Setonpointerlockchange;
    // property onpointerlockerror: TEventHandler read _Getonpointerlockerror write _Setonpointerlockerror;
    property allowDeprecatedTls: Boolean read _GetallowDeprecatedTls write _SetallowDeprecatedTls;
    property hidden: Boolean read _Gethidden;
    property visibilityState: TVisibilityState read _GetvisibilityState;
    // property onvisibilitychange: TEventHandler read _Getonvisibilitychange write _Setonvisibilitychange;
    property selectedStyleSheetSet: UnicodeString read _GetselectedStyleSheetSet write _SetselectedStyleSheetSet;
    property lastStyleSheetSet: UnicodeString read _GetlastStyleSheetSet;
    property preferredStyleSheetSet: UnicodeString read _GetpreferredStyleSheetSet;
    property styleSheetSets: IJSDOMStringList read _GetstyleSheetSets;
    property scrollingElement: IJSElement read _GetscrollingElement;
    property rootElement: IJSSVGSVGElement read _GetrootElement;
    property loadedFromPrototype: Boolean read _GetloadedFromPrototype;
    property effectiveStoragePrincipal: IJSPrincipal read _GeteffectiveStoragePrincipal;
    property partitionedPrincipal: IJSPrincipal read _GetpartitionedPrincipal;
    property cookieJarSettings: IJSnsICookieJarSettings read _GetcookieJarSettings;
    property styleSheetChangeEventsEnabled: Boolean read _GetstyleSheetChangeEventsEnabled write _SetstyleSheetChangeEventsEnabled;
    property shadowRootAttachedEventEnabled: Boolean read _GetshadowRootAttachedEventEnabled write _SetshadowRootAttachedEventEnabled;
    property contentLanguage: UnicodeString read _GetcontentLanguage;
    property documentLoadGroup: IJSnsILoadGroup read _GetdocumentLoadGroup;
    property mozDocumentURIIfNotForErrorPages: IJSURI read _GetmozDocumentURIIfNotForErrorPages;
    property documentReadyForIdle: IJSPromise read _GetdocumentReadyForIdle;
    property commandDispatcher: IJSXULCommandDispatcher read _GetcommandDispatcher;
    property devToolsWatchingDOMMutations: Boolean read _GetdevToolsWatchingDOMMutations write _SetdevToolsWatchingDOMMutations;
    property isSrcdocDocument: Boolean read _GetisSrcdocDocument;
    property sandboxFlagsAsString: UnicodeString read _GetsandboxFlagsAsString;
    property autoplayPolicy: TDocumentAutoplayPolicy read _GetautoplayPolicy;
    property userHasInteracted: Boolean read _GetuserHasInteracted;
    property hasBeenUserGestureActivated: Boolean read _GethasBeenUserGestureActivated;
    property hasValidTransientUserGestureActivation: Boolean read _GethasValidTransientUserGestureActivation;
    property csp: IJSContentSecurityPolicy read _Getcsp;
    property cspJSON: UnicodeString read _GetcspJSON;
    property documentFlashClassification: TFlashClassification read _GetdocumentFlashClassification;
    property blockedNodeByClassifierCount: Integer read _GetblockedNodeByClassifierCount;
    property blockedNodesByClassifier: IJSNodeList read _GetblockedNodesByClassifier;
    property permDelegateHandler: IJSnsIPermissionDelegateHandler read _GetpermDelegateHandler;
    property isInitialDocument: Boolean read _GetisInitialDocument;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
    property activeElement: IJSElement read _GetactiveElement;
    property pointerLockElement: IJSElement read _GetpointerLockElement;
    property fullscreenElement: IJSElement read _GetfullscreenElement;
    property mozFullScreenElement: IJSElement read _GetmozFullScreenElement;
  end;

  { --------------------------------------------------------------------
    TJSDocumentType
    --------------------------------------------------------------------}

  IJSDocumentType = interface(IJSNode)
    ['{2FAEE4D9-7540-3A02-A396-83FAE809699C}']
    function _Getname: UnicodeString;
    function _GetpublicId: UnicodeString;
    function _GetsystemId: UnicodeString;
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    property name: UnicodeString read _Getname;
    property publicId: UnicodeString read _GetpublicId;
    property systemId: UnicodeString read _GetsystemId;
  end;

  TJSDocumentType = class(TJSNode,IJSDocumentType)
  Private
    function _Getname: UnicodeString;
    function _GetpublicId: UnicodeString;
    function _GetsystemId: UnicodeString;
  Public
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    class function Cast(Intf: IJSObject): IJSDocumentType;
    property name: UnicodeString read _Getname;
    property publicId: UnicodeString read _GetpublicId;
    property systemId: UnicodeString read _GetsystemId;
  end;

  { --------------------------------------------------------------------
    TJSElement
    --------------------------------------------------------------------}

  IJSElement = interface(IJSNode)
    ['{509EC8E5-B2F0-3FDB-8B1E-ED96E018A43D}']
    function _GetnamespaceURI: UnicodeString;
    function _Getprefix: UnicodeString;
    function _GetlocalName: UnicodeString;
    function _GettagName: UnicodeString;
    function _Getid: UnicodeString;
    function _GetclassName_: UnicodeString;
    function _GetclassList: IJSDOMTokenList;
    function _Getpart: IJSDOMTokenList;
    function _Getattributes: IJSNamedNodeMap;
    function _GetfontSizeInflation: Single;
    function _GetimplementedPseudoElement: UnicodeString;
    function _GetscrollTop: Integer;
    function _GetscrollLeft: Integer;
    function _GetscrollWidth: Integer;
    function _GetscrollHeight: Integer;
    function _GetclientTop: Integer;
    function _GetclientLeft: Integer;
    function _GetclientWidth: Integer;
    function _GetclientHeight: Integer;
    function _GetscreenX: Integer;
    function _GetscreenY: Integer;
    function _Getscreen: IJSnsIScreen;
    function _GetscrollTopMin: Integer;
    function _GetscrollTopMax: Integer;
    function _GetscrollLeftMin: Integer;
    function _GetscrollLeftMax: Integer;
    function _GetinnerHTML: UnicodeString;
    function _GetouterHTML: UnicodeString;
    function _Getslot: UnicodeString;
    function _GethasVisibleScrollbars: Boolean;
    function _GetclientHeightDouble: Double;
    function _GetclientWidthDouble: Double;
    function _GetfirstLineBoxBSize: Double;
    function _GetpreviousElementSibling: IJSElement;
    function _GetnextElementSibling: IJSElement;
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
    procedure _Setid(const aValue: UnicodeString);
    procedure _SetclassName_(const aValue: UnicodeString);
    procedure _SetscrollTop(const aValue: Integer);
    procedure _SetscrollLeft(const aValue: Integer);
    procedure _SetinnerHTML(const aValue: UnicodeString);
    procedure _SetouterHTML(const aValue: UnicodeString);
    procedure _Setslot(const aValue: UnicodeString);
    function getAttributeNames: TUnicodeStringDynArray;
    function getAttribute(const aName: UnicodeString): UnicodeString;
    function getAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): UnicodeString;
    function toggleAttribute(const aName: UnicodeString; aForce: Boolean): Boolean; overload;
    function toggleAttribute(const aName: UnicodeString): Boolean; overload;
    procedure setAttribute(const aName: UnicodeString; const aValue: UnicodeString);
    procedure setAttributeNS(const aNamespace: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString);
    procedure removeAttribute(const aName: UnicodeString);
    procedure removeAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString);
    function hasAttribute(const aName: UnicodeString): Boolean;
    function hasAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): Boolean;
    function hasAttributes: Boolean;
    function closest(const aSelector: UTF8String): IJSElement;
    function matches(const aSelector: UTF8String): Boolean;
    function webkitMatchesSelector(const aSelector: UTF8String): Boolean;
    function getElementsByTagName(const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByTagNameNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByClassName(const aClassNames: UnicodeString): IJSHTMLCollection;
    function insertAdjacentElement(const aWhere: UnicodeString; aElement: IJSElement): IJSElement;
    procedure insertAdjacentText(const aWhere: UnicodeString; const aData: UnicodeString);
    function mozMatchesSelector(const aSelector: UTF8String): Boolean;
    procedure setPointerCapture(aPointerId: Integer);
    procedure releasePointerCapture(aPointerId: Integer);
    function hasPointerCapture(aPointerId: Integer): Boolean;
    procedure setCapture(aRetargetToElement: Boolean); overload;
    procedure setCapture; overload;
    procedure releaseCapture;
    procedure setCaptureAlways(aRetargetToElement: Boolean); overload;
    procedure setCaptureAlways; overload;
    function getAttributeNode(const aName: UnicodeString): IJSAttr;
    function setAttributeNode(aNewAttr: IJSAttr): IJSAttr;
    function removeAttributeNode(aOldAttr: IJSAttr): IJSAttr;
    function getAttributeNodeNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
    function setAttributeNodeNS(aNewAttr: IJSAttr): IJSAttr;
    function getClientRects: IJSDOMRectList;
    function getBoundingClientRect: IJSDOMRect;
    procedure scrollIntoView(arg: Boolean); overload;
    procedure scrollIntoView; overload;
    procedure scrollIntoView(const arg: TJSScrollIntoViewOptions); overload;
    procedure scroll(aX: Double; aY: Double);
    procedure scroll(const aOptions: TJSScrollToOptions); overload;
    procedure scroll; overload;
    procedure scrollTo(aX: Double; aY: Double);
    procedure scrollTo(const aOptions: TJSScrollToOptions); overload;
    procedure scrollTo; overload;
    procedure scrollBy(aX: Double; aY: Double);
    procedure scrollBy(const aOptions: TJSScrollToOptions); overload;
    procedure scrollBy; overload;
    procedure mozScrollSnap;
    procedure insertAdjacentHTML(const aPosition: UnicodeString; const aText: UnicodeString);
    function querySelector(const aSelectors: UTF8String): IJSElement;
    function querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
    procedure requestPointerLock;
    function hasGridFragments: Boolean;
    function getElementsWithGrid: TJSElementDynArray;
    procedure setAttributeDevtools(const aName: UnicodeString; const aValue: UnicodeString);
    procedure setAttributeDevtoolsNS(const aNamespace: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString);
    procedure setHTML(const aInnerHTML: UnicodeString; const aOptions: TJSSetHTMLOptions); overload;
    procedure setHTML(const aInnerHTML: UnicodeString); overload;
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    property namespaceURI: UnicodeString read _GetnamespaceURI;
    property prefix: UnicodeString read _Getprefix;
    property localName: UnicodeString read _GetlocalName;
    property tagName: UnicodeString read _GettagName;
    property id: UnicodeString read _Getid write _Setid;
    property className_: UnicodeString read _GetclassName_ write _SetclassName_;
    property classList: IJSDOMTokenList read _GetclassList;
    property part: IJSDOMTokenList read _Getpart;
    property attributes: IJSNamedNodeMap read _Getattributes;
    property fontSizeInflation: Single read _GetfontSizeInflation;
    property implementedPseudoElement: UnicodeString read _GetimplementedPseudoElement;
    property scrollTop: Integer read _GetscrollTop write _SetscrollTop;
    property scrollLeft: Integer read _GetscrollLeft write _SetscrollLeft;
    property scrollWidth: Integer read _GetscrollWidth;
    property scrollHeight: Integer read _GetscrollHeight;
    property clientTop: Integer read _GetclientTop;
    property clientLeft: Integer read _GetclientLeft;
    property clientWidth: Integer read _GetclientWidth;
    property clientHeight: Integer read _GetclientHeight;
    property screenX: Integer read _GetscreenX;
    property screenY: Integer read _GetscreenY;
    property screen: IJSnsIScreen read _Getscreen;
    property scrollTopMin: Integer read _GetscrollTopMin;
    property scrollTopMax: Integer read _GetscrollTopMax;
    property scrollLeftMin: Integer read _GetscrollLeftMin;
    property scrollLeftMax: Integer read _GetscrollLeftMax;
    property innerHTML: UnicodeString read _GetinnerHTML write _SetinnerHTML;
    property outerHTML: UnicodeString read _GetouterHTML write _SetouterHTML;
    property slot: UnicodeString read _Getslot write _Setslot;
    // property onfullscreenchange: TEventHandler read _Getonfullscreenchange write _Setonfullscreenchange;
    // property onfullscreenerror: TEventHandler read _Getonfullscreenerror write _Setonfullscreenerror;
    property hasVisibleScrollbars: Boolean read _GethasVisibleScrollbars;
    property clientHeightDouble: Double read _GetclientHeightDouble;
    property clientWidthDouble: Double read _GetclientWidthDouble;
    property firstLineBoxBSize: Double read _GetfirstLineBoxBSize;
    property previousElementSibling: IJSElement read _GetpreviousElementSibling;
    property nextElementSibling: IJSElement read _GetnextElementSibling;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
  end;

  TJSElement = class(TJSNode,IJSElement)
  Private
    function _GetnamespaceURI: UnicodeString;
    function _Getprefix: UnicodeString;
    function _GetlocalName: UnicodeString;
    function _GettagName: UnicodeString;
    function _Getid: UnicodeString;
    function _GetclassName_: UnicodeString;
    function _GetclassList: IJSDOMTokenList;
    function _Getpart: IJSDOMTokenList;
    function _Getattributes: IJSNamedNodeMap;
    function _GetfontSizeInflation: Single;
    function _GetimplementedPseudoElement: UnicodeString;
    function _GetscrollTop: Integer;
    function _GetscrollLeft: Integer;
    function _GetscrollWidth: Integer;
    function _GetscrollHeight: Integer;
    function _GetclientTop: Integer;
    function _GetclientLeft: Integer;
    function _GetclientWidth: Integer;
    function _GetclientHeight: Integer;
    function _GetscreenX: Integer;
    function _GetscreenY: Integer;
    function _Getscreen: IJSnsIScreen;
    function _GetscrollTopMin: Integer;
    function _GetscrollTopMax: Integer;
    function _GetscrollLeftMin: Integer;
    function _GetscrollLeftMax: Integer;
    function _GetinnerHTML: UnicodeString;
    function _GetouterHTML: UnicodeString;
    function _Getslot: UnicodeString;
    function _GethasVisibleScrollbars: Boolean;
    function _GetclientHeightDouble: Double;
    function _GetclientWidthDouble: Double;
    function _GetfirstLineBoxBSize: Double;
    function _GetpreviousElementSibling: IJSElement;
    function _GetnextElementSibling: IJSElement;
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
    procedure _Setid(const aValue: UnicodeString);
    procedure _SetclassName_(const aValue: UnicodeString);
    procedure _SetscrollTop(const aValue: Integer);
    procedure _SetscrollLeft(const aValue: Integer);
    procedure _SetinnerHTML(const aValue: UnicodeString);
    procedure _SetouterHTML(const aValue: UnicodeString);
    procedure _Setslot(const aValue: UnicodeString);
  Public
    function getAttributeNames: TUnicodeStringDynArray;
    function getAttribute(const aName: UnicodeString): UnicodeString;
    function getAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): UnicodeString;
    function toggleAttribute(const aName: UnicodeString; aForce: Boolean): Boolean; overload;
    function toggleAttribute(const aName: UnicodeString): Boolean; overload;
    procedure setAttribute(const aName: UnicodeString; const aValue: UnicodeString);
    procedure setAttributeNS(const aNamespace: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString);
    procedure removeAttribute(const aName: UnicodeString);
    procedure removeAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString);
    function hasAttribute(const aName: UnicodeString): Boolean;
    function hasAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): Boolean;
    function hasAttributes: Boolean;
    function closest(const aSelector: UTF8String): IJSElement;
    function matches(const aSelector: UTF8String): Boolean;
    function webkitMatchesSelector(const aSelector: UTF8String): Boolean;
    function getElementsByTagName(const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByTagNameNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): IJSHTMLCollection;
    function getElementsByClassName(const aClassNames: UnicodeString): IJSHTMLCollection;
    function insertAdjacentElement(const aWhere: UnicodeString; aElement: IJSElement): IJSElement;
    procedure insertAdjacentText(const aWhere: UnicodeString; const aData: UnicodeString);
    function mozMatchesSelector(const aSelector: UTF8String): Boolean;
    procedure setPointerCapture(aPointerId: Integer);
    procedure releasePointerCapture(aPointerId: Integer);
    function hasPointerCapture(aPointerId: Integer): Boolean;
    procedure setCapture(aRetargetToElement: Boolean); overload;
    procedure setCapture; overload;
    procedure releaseCapture;
    procedure setCaptureAlways(aRetargetToElement: Boolean); overload;
    procedure setCaptureAlways; overload;
    function getAttributeNode(const aName: UnicodeString): IJSAttr;
    function setAttributeNode(aNewAttr: IJSAttr): IJSAttr;
    function removeAttributeNode(aOldAttr: IJSAttr): IJSAttr;
    function getAttributeNodeNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
    function setAttributeNodeNS(aNewAttr: IJSAttr): IJSAttr;
    function getClientRects: IJSDOMRectList;
    function getBoundingClientRect: IJSDOMRect;
    procedure scrollIntoView(arg: Boolean); overload;
    procedure scrollIntoView; overload;
    procedure scrollIntoView(const arg: TJSScrollIntoViewOptions); overload;
    procedure scroll(aX: Double; aY: Double);
    procedure scroll(const aOptions: TJSScrollToOptions); overload;
    procedure scroll; overload;
    procedure scrollTo(aX: Double; aY: Double);
    procedure scrollTo(const aOptions: TJSScrollToOptions); overload;
    procedure scrollTo; overload;
    procedure scrollBy(aX: Double; aY: Double);
    procedure scrollBy(const aOptions: TJSScrollToOptions); overload;
    procedure scrollBy; overload;
    procedure mozScrollSnap;
    procedure insertAdjacentHTML(const aPosition: UnicodeString; const aText: UnicodeString);
    function querySelector(const aSelectors: UTF8String): IJSElement;
    function querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
    procedure requestPointerLock;
    function hasGridFragments: Boolean;
    function getElementsWithGrid: TJSElementDynArray;
    procedure setAttributeDevtools(const aName: UnicodeString; const aValue: UnicodeString);
    procedure setAttributeDevtoolsNS(const aNamespace: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString);
    procedure setHTML(const aInnerHTML: UnicodeString; const aOptions: TJSSetHTMLOptions); overload;
    procedure setHTML(const aInnerHTML: UnicodeString); overload;
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    class function Cast(Intf: IJSObject): IJSElement;
    property namespaceURI: UnicodeString read _GetnamespaceURI;
    property prefix: UnicodeString read _Getprefix;
    property localName: UnicodeString read _GetlocalName;
    property tagName: UnicodeString read _GettagName;
    property id: UnicodeString read _Getid write _Setid;
    property className_: UnicodeString read _GetclassName_ write _SetclassName_;
    property classList: IJSDOMTokenList read _GetclassList;
    property part: IJSDOMTokenList read _Getpart;
    property attributes: IJSNamedNodeMap read _Getattributes;
    property fontSizeInflation: Single read _GetfontSizeInflation;
    property implementedPseudoElement: UnicodeString read _GetimplementedPseudoElement;
    property scrollTop: Integer read _GetscrollTop write _SetscrollTop;
    property scrollLeft: Integer read _GetscrollLeft write _SetscrollLeft;
    property scrollWidth: Integer read _GetscrollWidth;
    property scrollHeight: Integer read _GetscrollHeight;
    property clientTop: Integer read _GetclientTop;
    property clientLeft: Integer read _GetclientLeft;
    property clientWidth: Integer read _GetclientWidth;
    property clientHeight: Integer read _GetclientHeight;
    property screenX: Integer read _GetscreenX;
    property screenY: Integer read _GetscreenY;
    property screen: IJSnsIScreen read _Getscreen;
    property scrollTopMin: Integer read _GetscrollTopMin;
    property scrollTopMax: Integer read _GetscrollTopMax;
    property scrollLeftMin: Integer read _GetscrollLeftMin;
    property scrollLeftMax: Integer read _GetscrollLeftMax;
    property innerHTML: UnicodeString read _GetinnerHTML write _SetinnerHTML;
    property outerHTML: UnicodeString read _GetouterHTML write _SetouterHTML;
    property slot: UnicodeString read _Getslot write _Setslot;
    // property onfullscreenchange: TEventHandler read _Getonfullscreenchange write _Setonfullscreenchange;
    // property onfullscreenerror: TEventHandler read _Getonfullscreenerror write _Setonfullscreenerror;
    property hasVisibleScrollbars: Boolean read _GethasVisibleScrollbars;
    property clientHeightDouble: Double read _GetclientHeightDouble;
    property clientWidthDouble: Double read _GetclientWidthDouble;
    property firstLineBoxBSize: Double read _GetfirstLineBoxBSize;
    property previousElementSibling: IJSElement read _GetpreviousElementSibling;
    property nextElementSibling: IJSElement read _GetnextElementSibling;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
  end;

  { --------------------------------------------------------------------
    TJSDocumentFragment
    --------------------------------------------------------------------}

  IJSDocumentFragment = interface(IJSNode)
    ['{E1BEA8D7-FE6E-3D6E-B835-B9DD95C2D14F}']
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
    function getElementById(const aElementId: UnicodeString): IJSElement;
    function querySelector(const aSelectors: UTF8String): IJSElement;
    function querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
  end;

  TJSDocumentFragment = class(TJSNode,IJSDocumentFragment)
  Private
    function _Getchildren: IJSHTMLCollection;
    function _GetfirstElementChild: IJSElement;
    function _GetlastElementChild: IJSElement;
    function _GetchildElementCount: LongWord;
  Public
    function getElementById(const aElementId: UnicodeString): IJSElement;
    function querySelector(const aSelectors: UTF8String): IJSElement;
    function querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
    function getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    function getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
    procedure prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure append(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
    class function Cast(Intf: IJSObject): IJSDocumentFragment;
    property children: IJSHTMLCollection read _Getchildren;
    property firstElementChild: IJSElement read _GetfirstElementChild;
    property lastElementChild: IJSElement read _GetlastElementChild;
    property childElementCount: LongWord read _GetchildElementCount;
  end;

  { --------------------------------------------------------------------
    TJSAttr
    --------------------------------------------------------------------}

  IJSAttr = interface(IJSNode)
    ['{5E7C485A-A253-39DB-A212-7943807F5E88}']
    function _GetlocalName: UnicodeString;
    function _Getvalue: UnicodeString;
    function _Getname: UnicodeString;
    function _GetnamespaceURI: UnicodeString;
    function _Getprefix: UnicodeString;
    function _Getspecified: Boolean;
    function _GetownerElement: IJSElement;
    procedure _Setvalue(const aValue: UnicodeString);
    property localName: UnicodeString read _GetlocalName;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property name: UnicodeString read _Getname;
    property namespaceURI: UnicodeString read _GetnamespaceURI;
    property prefix: UnicodeString read _Getprefix;
    property specified: Boolean read _Getspecified;
    property ownerElement: IJSElement read _GetownerElement;
  end;

  TJSAttr = class(TJSNode,IJSAttr)
  Private
    function _GetlocalName: UnicodeString;
    function _Getvalue: UnicodeString;
    function _Getname: UnicodeString;
    function _GetnamespaceURI: UnicodeString;
    function _Getprefix: UnicodeString;
    function _Getspecified: Boolean;
    function _GetownerElement: IJSElement;
    procedure _Setvalue(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSAttr;
    property localName: UnicodeString read _GetlocalName;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property name: UnicodeString read _Getname;
    property namespaceURI: UnicodeString read _GetnamespaceURI;
    property prefix: UnicodeString read _Getprefix;
    property specified: Boolean read _Getspecified;
    property ownerElement: IJSElement read _GetownerElement;
  end;

  { --------------------------------------------------------------------
    TJSCharacterData
    --------------------------------------------------------------------}

  IJSCharacterData = interface(IJSNode)
    ['{E7B1D985-8387-3859-B74B-619FE4E335D0}']
    function _Getdata: UnicodeString;
    function _Getlength_: LongWord;
    function _GetpreviousElementSibling: IJSElement;
    function _GetnextElementSibling: IJSElement;
    procedure _Setdata(const aValue: UnicodeString);
    function substringData(aOffset: LongWord; aCount: LongWord): UnicodeString;
    procedure appendData(const aData: UnicodeString);
    procedure insertData(aOffset: LongWord; const aData: UnicodeString);
    procedure deleteData(aOffset: LongWord; aCount: LongWord);
    procedure replaceData(aOffset: LongWord; aCount: LongWord; const aData: UnicodeString);
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    property data: UnicodeString read _Getdata write _Setdata;
    property length_: LongWord read _Getlength_;
    property previousElementSibling: IJSElement read _GetpreviousElementSibling;
    property nextElementSibling: IJSElement read _GetnextElementSibling;
  end;

  TJSCharacterData = class(TJSNode,IJSCharacterData)
  Private
    function _Getdata: UnicodeString;
    function _Getlength_: LongWord;
    function _GetpreviousElementSibling: IJSElement;
    function _GetnextElementSibling: IJSElement;
    procedure _Setdata(const aValue: UnicodeString);
  Public
    function substringData(aOffset: LongWord; aCount: LongWord): UnicodeString;
    procedure appendData(const aData: UnicodeString);
    procedure insertData(aOffset: LongWord; const aData: UnicodeString);
    procedure deleteData(aOffset: LongWord; aCount: LongWord);
    procedure replaceData(aOffset: LongWord; aCount: LongWord; const aData: UnicodeString);
    procedure before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure before(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure after(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
    procedure replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
    procedure remove;
    class function Cast(Intf: IJSObject): IJSCharacterData;
    property data: UnicodeString read _Getdata write _Setdata;
    property length_: LongWord read _Getlength_;
    property previousElementSibling: IJSElement read _GetpreviousElementSibling;
    property nextElementSibling: IJSElement read _GetnextElementSibling;
  end;

  { --------------------------------------------------------------------
    TJSText
    --------------------------------------------------------------------}

  IJSText = interface(IJSCharacterData)
    ['{DA553EFA-C7AA-33E5-BBF8-619235F07EFF}']
    function _GetwholeText: UnicodeString;
    function splitText(aOffset: LongWord): IJSText;
    property wholeText: UnicodeString read _GetwholeText;
  end;

  TJSText = class(TJSCharacterData,IJSText)
  Private
    function _GetwholeText: UnicodeString;
  Public
    function splitText(aOffset: LongWord): IJSText;
    class function Cast(Intf: IJSObject): IJSText;
    property wholeText: UnicodeString read _GetwholeText;
  end;

  { --------------------------------------------------------------------
    TJSComment
    --------------------------------------------------------------------}

  IJSComment = interface(IJSCharacterData)
    ['{A6DABBC4-BEB5-343C-BA9F-E6DDC1194965}']
  end;

  TJSComment = class(TJSCharacterData,IJSComment)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSComment;
  end;

  { --------------------------------------------------------------------
    TJSHTMLElement
    --------------------------------------------------------------------}

  IJSHTMLElement = interface(IJSElement)
    ['{439D79D3-285D-3A49-91BB-0311FD3C604C}']
    function _Gettitle: UnicodeString;
    function _Getlang: UnicodeString;
    function _Getdir: UnicodeString;
    function _GetinnerText: UnicodeString;
    function _GetouterText: UnicodeString;
    function _Gethidden: Boolean;
    function _Getinert: Boolean;
    function _GetaccessKey: UnicodeString;
    function _GetaccessKeyLabel: UnicodeString;
    function _Getdraggable: Boolean;
    function _GetcontentEditable: UnicodeString;
    function _GetisContentEditable: Boolean;
    function _GetcontextMenu: IJSHTMLMenuElement;
    function _Getspellcheck: Boolean;
    function _GetinputMode: UnicodeString;
    function _GetenterKeyHint: UnicodeString;
    function _Getautocapitalize: UnicodeString;
    function _Getnonce: UnicodeString;
    function _GetoffsetParent: IJSElement;
    function _GetoffsetTop: Integer;
    function _GetoffsetLeft: Integer;
    function _GetoffsetWidth: Integer;
    function _GetoffsetHeight: Integer;
    function _Getinternals: IJSElementInternals;
    function _GetisFormAssociatedCustomElements: Boolean;
    function _Getdataset: IJSDOMStringMap;
    function _GettabIndex: Integer;
    function _Getstyle: IJSCSSStyleDeclaration;
    procedure _Settitle(const aValue: UnicodeString);
    procedure _Setlang(const aValue: UnicodeString);
    procedure _Setdir(const aValue: UnicodeString);
    procedure _SetinnerText(const aValue: UnicodeString);
    procedure _SetouterText(const aValue: UnicodeString);
    procedure _Sethidden(const aValue: Boolean);
    procedure _Setinert(const aValue: Boolean);
    procedure _SetaccessKey(const aValue: UnicodeString);
    procedure _Setdraggable(const aValue: Boolean);
    procedure _SetcontentEditable(const aValue: UnicodeString);
    procedure _Setspellcheck(const aValue: Boolean);
    procedure _SetinputMode(const aValue: UnicodeString);
    procedure _SetenterKeyHint(const aValue: UnicodeString);
    procedure _Setautocapitalize(const aValue: UnicodeString);
    procedure _Setnonce(const aValue: UnicodeString);
    procedure _SettabIndex(const aValue: Integer);
    procedure click;
    function attachInternals: IJSElementInternals;
    procedure focus(const aOptions: TJSFocusOptions); overload;
    procedure focus; overload;
    procedure blur;
    property title: UnicodeString read _Gettitle write _Settitle;
    property lang: UnicodeString read _Getlang write _Setlang;
    property dir: UnicodeString read _Getdir write _Setdir;
    property innerText: UnicodeString read _GetinnerText write _SetinnerText;
    property outerText: UnicodeString read _GetouterText write _SetouterText;
    property hidden: Boolean read _Gethidden write _Sethidden;
    property inert: Boolean read _Getinert write _Setinert;
    property accessKey: UnicodeString read _GetaccessKey write _SetaccessKey;
    property accessKeyLabel: UnicodeString read _GetaccessKeyLabel;
    property draggable: Boolean read _Getdraggable write _Setdraggable;
    property contentEditable: UnicodeString read _GetcontentEditable write _SetcontentEditable;
    property isContentEditable: Boolean read _GetisContentEditable;
    property contextMenu: IJSHTMLMenuElement read _GetcontextMenu;
    property spellcheck: Boolean read _Getspellcheck write _Setspellcheck;
    property inputMode: UnicodeString read _GetinputMode write _SetinputMode;
    property enterKeyHint: UnicodeString read _GetenterKeyHint write _SetenterKeyHint;
    property autocapitalize: UnicodeString read _Getautocapitalize write _Setautocapitalize;
    property nonce: UnicodeString read _Getnonce write _Setnonce;
    property offsetParent: IJSElement read _GetoffsetParent;
    property offsetTop: Integer read _GetoffsetTop;
    property offsetLeft: Integer read _GetoffsetLeft;
    property offsetWidth: Integer read _GetoffsetWidth;
    property offsetHeight: Integer read _GetoffsetHeight;
    property internals: IJSElementInternals read _Getinternals;
    property isFormAssociatedCustomElements: Boolean read _GetisFormAssociatedCustomElements;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    property dataset: IJSDOMStringMap read _Getdataset;
    property tabIndex: Integer read _GettabIndex write _SettabIndex;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property style: IJSCSSStyleDeclaration read _Getstyle;
    // property ontouchstart: TEventHandler read _Getontouchstart write _Setontouchstart;
    // property ontouchend: TEventHandler read _Getontouchend write _Setontouchend;
    // property ontouchmove: TEventHandler read _Getontouchmove write _Setontouchmove;
    // property ontouchcancel: TEventHandler read _Getontouchcancel write _Setontouchcancel;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;

  TJSHTMLElement = class(TJSElement,IJSHTMLElement)
  Private
    function _Gettitle: UnicodeString;
    function _Getlang: UnicodeString;
    function _Getdir: UnicodeString;
    function _GetinnerText: UnicodeString;
    function _GetouterText: UnicodeString;
    function _Gethidden: Boolean;
    function _Getinert: Boolean;
    function _GetaccessKey: UnicodeString;
    function _GetaccessKeyLabel: UnicodeString;
    function _Getdraggable: Boolean;
    function _GetcontentEditable: UnicodeString;
    function _GetisContentEditable: Boolean;
    function _GetcontextMenu: IJSHTMLMenuElement;
    function _Getspellcheck: Boolean;
    function _GetinputMode: UnicodeString;
    function _GetenterKeyHint: UnicodeString;
    function _Getautocapitalize: UnicodeString;
    function _Getnonce: UnicodeString;
    function _GetoffsetParent: IJSElement;
    function _GetoffsetTop: Integer;
    function _GetoffsetLeft: Integer;
    function _GetoffsetWidth: Integer;
    function _GetoffsetHeight: Integer;
    function _Getinternals: IJSElementInternals;
    function _GetisFormAssociatedCustomElements: Boolean;
    function _Getdataset: IJSDOMStringMap;
    function _GettabIndex: Integer;
    function _Getstyle: IJSCSSStyleDeclaration;
    procedure _Settitle(const aValue: UnicodeString);
    procedure _Setlang(const aValue: UnicodeString);
    procedure _Setdir(const aValue: UnicodeString);
    procedure _SetinnerText(const aValue: UnicodeString);
    procedure _SetouterText(const aValue: UnicodeString);
    procedure _Sethidden(const aValue: Boolean);
    procedure _Setinert(const aValue: Boolean);
    procedure _SetaccessKey(const aValue: UnicodeString);
    procedure _Setdraggable(const aValue: Boolean);
    procedure _SetcontentEditable(const aValue: UnicodeString);
    procedure _Setspellcheck(const aValue: Boolean);
    procedure _SetinputMode(const aValue: UnicodeString);
    procedure _SetenterKeyHint(const aValue: UnicodeString);
    procedure _Setautocapitalize(const aValue: UnicodeString);
    procedure _Setnonce(const aValue: UnicodeString);
    procedure _SettabIndex(const aValue: Integer);
  Public
    procedure click;
    function attachInternals: IJSElementInternals;
    procedure focus(const aOptions: TJSFocusOptions); overload;
    procedure focus; overload;
    procedure blur;
    class function Cast(Intf: IJSObject): IJSHTMLElement;
    property title: UnicodeString read _Gettitle write _Settitle;
    property lang: UnicodeString read _Getlang write _Setlang;
    property dir: UnicodeString read _Getdir write _Setdir;
    property innerText: UnicodeString read _GetinnerText write _SetinnerText;
    property outerText: UnicodeString read _GetouterText write _SetouterText;
    property hidden: Boolean read _Gethidden write _Sethidden;
    property inert: Boolean read _Getinert write _Setinert;
    property accessKey: UnicodeString read _GetaccessKey write _SetaccessKey;
    property accessKeyLabel: UnicodeString read _GetaccessKeyLabel;
    property draggable: Boolean read _Getdraggable write _Setdraggable;
    property contentEditable: UnicodeString read _GetcontentEditable write _SetcontentEditable;
    property isContentEditable: Boolean read _GetisContentEditable;
    property contextMenu: IJSHTMLMenuElement read _GetcontextMenu;
    property spellcheck: Boolean read _Getspellcheck write _Setspellcheck;
    property inputMode: UnicodeString read _GetinputMode write _SetinputMode;
    property enterKeyHint: UnicodeString read _GetenterKeyHint write _SetenterKeyHint;
    property autocapitalize: UnicodeString read _Getautocapitalize write _Setautocapitalize;
    property nonce: UnicodeString read _Getnonce write _Setnonce;
    property offsetParent: IJSElement read _GetoffsetParent;
    property offsetTop: Integer read _GetoffsetTop;
    property offsetLeft: Integer read _GetoffsetLeft;
    property offsetWidth: Integer read _GetoffsetWidth;
    property offsetHeight: Integer read _GetoffsetHeight;
    property internals: IJSElementInternals read _Getinternals;
    property isFormAssociatedCustomElements: Boolean read _GetisFormAssociatedCustomElements;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    property dataset: IJSDOMStringMap read _Getdataset;
    property tabIndex: Integer read _GettabIndex write _SettabIndex;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property style: IJSCSSStyleDeclaration read _Getstyle;
    // property ontouchstart: TEventHandler read _Getontouchstart write _Setontouchstart;
    // property ontouchend: TEventHandler read _Getontouchend write _Setontouchend;
    // property ontouchmove: TEventHandler read _Getontouchmove write _Setontouchmove;
    // property ontouchcancel: TEventHandler read _Getontouchcancel write _Setontouchcancel;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;

  { --------------------------------------------------------------------
    TJSShadowRoot
    --------------------------------------------------------------------}

  IJSShadowRoot = interface(IJSDocumentFragment)
    ['{81AF455B-5115-3C44-9AA7-E5B7F77531A0}']
    function _Getmode: TShadowRootMode;
    function _GetdelegatesFocus: Boolean;
    function _GetslotAssignment: TSlotAssignmentMode;
    function _Gethost: IJSElement;
    function _GetinnerHTML: UnicodeString;
    function _GetactiveElement: IJSElement;
    function _GetpointerLockElement: IJSElement;
    function _GetfullscreenElement: IJSElement;
    function _GetmozFullScreenElement: IJSElement;
    procedure _SetinnerHTML(const aValue: UnicodeString);
    function getElementById(const aElementId: UnicodeString): IJSElement;
    function importNodeAndAppendChildAt(aParentNode: IJSNode; aNode: IJSNode; aDeep: Boolean): IJSNode; overload;
    function importNodeAndAppendChildAt(aParentNode: IJSNode; aNode: IJSNode): IJSNode; overload;
    function createElementAndAppendChildAt(aParentNode: IJSNode; const aLocalName: UnicodeString): IJSNode;
    procedure setIsUAWidget;
    function isUAWidget: Boolean;
    function elementFromPoint(aX: Single; aY: Single): IJSElement;
    function elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
    function nodeFromPoint(aX: Single; aY: Single): IJSNode;
    function nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
    property mode: TShadowRootMode read _Getmode;
    property delegatesFocus: Boolean read _GetdelegatesFocus;
    property slotAssignment: TSlotAssignmentMode read _GetslotAssignment;
    property host: IJSElement read _Gethost;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    property innerHTML: UnicodeString read _GetinnerHTML write _SetinnerHTML;
    property activeElement: IJSElement read _GetactiveElement;
    property pointerLockElement: IJSElement read _GetpointerLockElement;
    property fullscreenElement: IJSElement read _GetfullscreenElement;
    property mozFullScreenElement: IJSElement read _GetmozFullScreenElement;
  end;

  TJSShadowRoot = class(TJSDocumentFragment,IJSShadowRoot)
  Private
    function _Getmode: TShadowRootMode;
    function _GetdelegatesFocus: Boolean;
    function _GetslotAssignment: TSlotAssignmentMode;
    function _Gethost: IJSElement;
    function _GetinnerHTML: UnicodeString;
    function _GetactiveElement: IJSElement;
    function _GetpointerLockElement: IJSElement;
    function _GetfullscreenElement: IJSElement;
    function _GetmozFullScreenElement: IJSElement;
    procedure _SetinnerHTML(const aValue: UnicodeString);
  Public
    function getElementById(const aElementId: UnicodeString): IJSElement;
    function importNodeAndAppendChildAt(aParentNode: IJSNode; aNode: IJSNode; aDeep: Boolean): IJSNode; overload;
    function importNodeAndAppendChildAt(aParentNode: IJSNode; aNode: IJSNode): IJSNode; overload;
    function createElementAndAppendChildAt(aParentNode: IJSNode; const aLocalName: UnicodeString): IJSNode;
    procedure setIsUAWidget;
    function isUAWidget: Boolean;
    function elementFromPoint(aX: Single; aY: Single): IJSElement;
    function elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
    function nodeFromPoint(aX: Single; aY: Single): IJSNode;
    function nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
    class function Cast(Intf: IJSObject): IJSShadowRoot;
    property mode: TShadowRootMode read _Getmode;
    property delegatesFocus: Boolean read _GetdelegatesFocus;
    property slotAssignment: TSlotAssignmentMode read _GetslotAssignment;
    property host: IJSElement read _Gethost;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    property innerHTML: UnicodeString read _GetinnerHTML write _SetinnerHTML;
    property activeElement: IJSElement read _GetactiveElement;
    property pointerLockElement: IJSElement read _GetpointerLockElement;
    property fullscreenElement: IJSElement read _GetfullscreenElement;
    property mozFullScreenElement: IJSElement read _GetmozFullScreenElement;
  end;

  { --------------------------------------------------------------------
    TJSSVGElement
    --------------------------------------------------------------------}

  IJSSVGElement = interface(IJSElement)
    ['{B2D89D3C-0B7B-384E-8766-991C38CB8483}']
    function _Getid: UnicodeString;
    function _GetownerSVGElement: IJSSVGSVGElement;
    function _GetviewportElement: IJSSVGElement;
    function _Getnonce: UnicodeString;
    function _Getdataset: IJSDOMStringMap;
    function _GettabIndex: Integer;
    function _Getstyle: IJSCSSStyleDeclaration;
    procedure _Setid(const aValue: UnicodeString);
    procedure _Setnonce(const aValue: UnicodeString);
    procedure _SettabIndex(const aValue: Integer);
    procedure focus(const aOptions: TJSFocusOptions); overload;
    procedure focus; overload;
    procedure blur;
    property id: UnicodeString read _Getid write _Setid;
    property ownerSVGElement: IJSSVGSVGElement read _GetownerSVGElement;
    property viewportElement: IJSSVGElement read _GetviewportElement;
    property nonce: UnicodeString read _Getnonce write _Setnonce;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    property dataset: IJSDOMStringMap read _Getdataset;
    property tabIndex: Integer read _GettabIndex write _SettabIndex;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property style: IJSCSSStyleDeclaration read _Getstyle;
    // property ontouchstart: TEventHandler read _Getontouchstart write _Setontouchstart;
    // property ontouchend: TEventHandler read _Getontouchend write _Setontouchend;
    // property ontouchmove: TEventHandler read _Getontouchmove write _Setontouchmove;
    // property ontouchcancel: TEventHandler read _Getontouchcancel write _Setontouchcancel;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;

  TJSSVGElement = class(TJSElement,IJSSVGElement)
  Private
    function _Getid: UnicodeString;
    function _GetownerSVGElement: IJSSVGSVGElement;
    function _GetviewportElement: IJSSVGElement;
    function _Getnonce: UnicodeString;
    function _Getdataset: IJSDOMStringMap;
    function _GettabIndex: Integer;
    function _Getstyle: IJSCSSStyleDeclaration;
    procedure _Setid(const aValue: UnicodeString);
    procedure _Setnonce(const aValue: UnicodeString);
    procedure _SettabIndex(const aValue: Integer);
  Public
    procedure focus(const aOptions: TJSFocusOptions); overload;
    procedure focus; overload;
    procedure blur;
    class function Cast(Intf: IJSObject): IJSSVGElement;
    property id: UnicodeString read _Getid write _Setid;
    property ownerSVGElement: IJSSVGSVGElement read _GetownerSVGElement;
    property viewportElement: IJSSVGElement read _GetviewportElement;
    property nonce: UnicodeString read _Getnonce write _Setnonce;
    // property onabort: TEventHandler read _Getonabort write _Setonabort;
    // property onblur: TEventHandler read _Getonblur write _Setonblur;
    // property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    // property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    // property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    // property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    // property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    // property onchange: TEventHandler read _Getonchange write _Setonchange;
    // property onclick: TEventHandler read _Getonclick write _Setonclick;
    // property onclose: TEventHandler read _Getonclose write _Setonclose;
    // property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    // property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    // property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    // property ondrag: TEventHandler read _Getondrag write _Setondrag;
    // property ondragend: TEventHandler read _Getondragend write _Setondragend;
    // property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    // property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    // property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    // property ondragover: TEventHandler read _Getondragover write _Setondragover;
    // property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    // property ondrop: TEventHandler read _Getondrop write _Setondrop;
    // property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    // property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    // property onended: TEventHandler read _Getonended write _Setonended;
    // property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    // property oninput: TEventHandler read _Getoninput write _Setoninput;
    // property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    // property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    // property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    // property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    // property onload: TEventHandler read _Getonload write _Setonload;
    // property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    // property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    // property onloadend: TEventHandler read _Getonloadend write _Setonloadend;
    // property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    // property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    // property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    // property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    // property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    // property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    // property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    // property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    // property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    // property onpause: TEventHandler read _Getonpause write _Setonpause;
    // property onplay: TEventHandler read _Getonplay write _Setonplay;
    // property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    // property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    // property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    // property onreset: TEventHandler read _Getonreset write _Setonreset;
    // property onresize: TEventHandler read _Getonresize write _Setonresize;
    // property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    // property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    // property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    // property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    // property onselect: TEventHandler read _Getonselect write _Setonselect;
    // property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    // property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    // property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    // property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    // property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    // property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    // property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    // property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    // property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    // property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    // property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    // property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    // property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    // property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    // property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    // property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    // property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    // property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    // property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    // property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    // property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    // property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    // property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    // property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    // property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    // property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    // property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    // property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    // property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    // property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    // property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    // property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    // property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    // property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
    property dataset: IJSDOMStringMap read _Getdataset;
    property tabIndex: Integer read _GettabIndex write _SettabIndex;
    // property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    // property oncut: TEventHandler read _Getoncut write _Setoncut;
    // property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property style: IJSCSSStyleDeclaration read _Getstyle;
    // property ontouchstart: TEventHandler read _Getontouchstart write _Setontouchstart;
    // property ontouchend: TEventHandler read _Getontouchend write _Setontouchend;
    // property ontouchmove: TEventHandler read _Getontouchmove write _Setontouchmove;
    // property ontouchcancel: TEventHandler read _Getontouchcancel write _Setontouchcancel;
    // property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;

  { --------------------------------------------------------------------
    TJSCDATASection
    --------------------------------------------------------------------}

  IJSCDATASection = interface(IJSText)
    ['{E6CE04C2-079B-3374-89E0-F0D81AA2A621}']
  end;

  TJSCDATASection = class(TJSText,IJSCDATASection)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSCDATASection;
  end;

  { --------------------------------------------------------------------
    TJSHTMLUnknownElement
    --------------------------------------------------------------------}

  IJSHTMLUnknownElement = interface(IJSHTMLElement)
    ['{C8B34EC0-0721-3BD5-8D7D-61963CBB1363}']
  end;

  TJSHTMLUnknownElement = class(TJSHTMLElement,IJSHTMLUnknownElement)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSHTMLUnknownElement;
  end;

  { --------------------------------------------------------------------
    TJSHTMLHeadElement
    --------------------------------------------------------------------}

  IJSHTMLHeadElement = interface(IJSHTMLElement)
    ['{AC6CC386-2D95-333F-AD48-9E5D8A5DBD72}']
  end;

  TJSHTMLHeadElement = class(TJSHTMLElement,IJSHTMLHeadElement)
  Private
  Public
    class function Cast(Intf: IJSObject): IJSHTMLHeadElement;
  end;

  { --------------------------------------------------------------------
    TJSHTMLMenuElement
    --------------------------------------------------------------------}

  IJSHTMLMenuElement = interface(IJSHTMLElement)
    ['{154C1DC9-E661-3EB1-8292-0C0888147FA8}']
    function _Gettype_: UnicodeString;
    function _Getlabel_: UnicodeString;
    function _Getcompact: Boolean;
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setlabel_(const aValue: UnicodeString);
    procedure _Setcompact(const aValue: Boolean);
    procedure sendShowEvent;
    function createBuilder: IJSMenuBuilder;
    procedure build(aBuilder: IJSMenuBuilder);
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property label_: UnicodeString read _Getlabel_ write _Setlabel_;
    property compact: Boolean read _Getcompact write _Setcompact;
  end;

  TJSHTMLMenuElement = class(TJSHTMLElement,IJSHTMLMenuElement)
  Private
    function _Gettype_: UnicodeString;
    function _Getlabel_: UnicodeString;
    function _Getcompact: Boolean;
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setlabel_(const aValue: UnicodeString);
    procedure _Setcompact(const aValue: Boolean);
  Public
    procedure sendShowEvent;
    function createBuilder: IJSMenuBuilder;
    procedure build(aBuilder: IJSMenuBuilder);
    class function Cast(Intf: IJSObject): IJSHTMLMenuElement;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property label_: UnicodeString read _Getlabel_ write _Setlabel_;
    property compact: Boolean read _Getcompact write _Setcompact;
  end;

  { --------------------------------------------------------------------
    TJSHTMLFormElement
    --------------------------------------------------------------------}

  IJSHTMLFormElement = interface(IJSHTMLElement)
    ['{A7B472CB-7EF5-3AF7-8131-95A0396900A3}']
    function _GetacceptCharset: UnicodeString;
    function _Getaction: UnicodeString;
    function _Getautocomplete: UnicodeString;
    function _Getenctype: UnicodeString;
    function _Getencoding: UnicodeString;
    function _Getmethod: UnicodeString;
    function _Getname: UnicodeString;
    function _GetnoValidate: Boolean;
    function _Gettarget: UnicodeString;
    function _Getelements: IJSHTMLCollection;
    function _Getlength_: Integer;
    procedure _SetacceptCharset(const aValue: UnicodeString);
    procedure _Setaction(const aValue: UnicodeString);
    procedure _Setautocomplete(const aValue: UnicodeString);
    procedure _Setenctype(const aValue: UnicodeString);
    procedure _Setencoding(const aValue: UnicodeString);
    procedure _Setmethod(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _SetnoValidate(const aValue: Boolean);
    procedure _Settarget(const aValue: UnicodeString);
    procedure submit;
    procedure requestSubmit(aSubmitter: IJSHTMLElement); overload;
    procedure requestSubmit; overload;
    procedure reset;
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    property acceptCharset: UnicodeString read _GetacceptCharset write _SetacceptCharset;
    property action: UnicodeString read _Getaction write _Setaction;
    property autocomplete: UnicodeString read _Getautocomplete write _Setautocomplete;
    property enctype: UnicodeString read _Getenctype write _Setenctype;
    property encoding: UnicodeString read _Getencoding write _Setencoding;
    property method: UnicodeString read _Getmethod write _Setmethod;
    property name: UnicodeString read _Getname write _Setname;
    property noValidate: Boolean read _GetnoValidate write _SetnoValidate;
    property target: UnicodeString read _Gettarget write _Settarget;
    property elements: IJSHTMLCollection read _Getelements;
    property length_: Integer read _Getlength_;
  end;

  TJSHTMLFormElement = class(TJSHTMLElement,IJSHTMLFormElement)
  Private
    function _GetacceptCharset: UnicodeString;
    function _Getaction: UnicodeString;
    function _Getautocomplete: UnicodeString;
    function _Getenctype: UnicodeString;
    function _Getencoding: UnicodeString;
    function _Getmethod: UnicodeString;
    function _Getname: UnicodeString;
    function _GetnoValidate: Boolean;
    function _Gettarget: UnicodeString;
    function _Getelements: IJSHTMLCollection;
    function _Getlength_: Integer;
    procedure _SetacceptCharset(const aValue: UnicodeString);
    procedure _Setaction(const aValue: UnicodeString);
    procedure _Setautocomplete(const aValue: UnicodeString);
    procedure _Setenctype(const aValue: UnicodeString);
    procedure _Setencoding(const aValue: UnicodeString);
    procedure _Setmethod(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _SetnoValidate(const aValue: Boolean);
    procedure _Settarget(const aValue: UnicodeString);
  Public
    procedure submit;
    procedure requestSubmit(aSubmitter: IJSHTMLElement); overload;
    procedure requestSubmit; overload;
    procedure reset;
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    class function Cast(Intf: IJSObject): IJSHTMLFormElement;
    property acceptCharset: UnicodeString read _GetacceptCharset write _SetacceptCharset;
    property action: UnicodeString read _Getaction write _Setaction;
    property autocomplete: UnicodeString read _Getautocomplete write _Setautocomplete;
    property enctype: UnicodeString read _Getenctype write _Setenctype;
    property encoding: UnicodeString read _Getencoding write _Setencoding;
    property method: UnicodeString read _Getmethod write _Setmethod;
    property name: UnicodeString read _Getname write _Setname;
    property noValidate: Boolean read _GetnoValidate write _SetnoValidate;
    property target: UnicodeString read _Gettarget write _Settarget;
    property elements: IJSHTMLCollection read _Getelements;
    property length_: Integer read _Getlength_;
  end;

  { --------------------------------------------------------------------
    TJSSVGGraphicsElement
    --------------------------------------------------------------------}

  IJSSVGGraphicsElement = interface(IJSSVGElement)
    ['{A5CAC004-05F9-34CF-9156-33CC2DED682D}']
    function _Getautofocus: Boolean;
    function _Gettransform: IJSSVGAnimatedTransformList;
    function _GetnearestViewportElement: IJSSVGElement;
    function _GetfarthestViewportElement: IJSSVGElement;
    function _GetrequiredExtensions: IJSSVGStringList;
    function _GetsystemLanguage: IJSSVGStringList;
    procedure _Setautofocus(const aValue: Boolean);
    function getBBox(const aOptions: TJSSVGBoundingBoxOptions): IJSSVGRect; overload;
    function getBBox: IJSSVGRect; overload;
    function getCTM: IJSSVGMatrix;
    function getScreenCTM: IJSSVGMatrix;
    function getTransformToElement(aElement: IJSSVGGraphicsElement): IJSSVGMatrix;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property transform: IJSSVGAnimatedTransformList read _Gettransform;
    property nearestViewportElement: IJSSVGElement read _GetnearestViewportElement;
    property farthestViewportElement: IJSSVGElement read _GetfarthestViewportElement;
    property requiredExtensions: IJSSVGStringList read _GetrequiredExtensions;
    property systemLanguage: IJSSVGStringList read _GetsystemLanguage;
  end;

  TJSSVGGraphicsElement = class(TJSSVGElement,IJSSVGGraphicsElement)
  Private
    function _Getautofocus: Boolean;
    function _Gettransform: IJSSVGAnimatedTransformList;
    function _GetnearestViewportElement: IJSSVGElement;
    function _GetfarthestViewportElement: IJSSVGElement;
    function _GetrequiredExtensions: IJSSVGStringList;
    function _GetsystemLanguage: IJSSVGStringList;
    procedure _Setautofocus(const aValue: Boolean);
  Public
    function getBBox(const aOptions: TJSSVGBoundingBoxOptions): IJSSVGRect; overload;
    function getBBox: IJSSVGRect; overload;
    function getCTM: IJSSVGMatrix;
    function getScreenCTM: IJSSVGMatrix;
    function getTransformToElement(aElement: IJSSVGGraphicsElement): IJSSVGMatrix;
    class function Cast(Intf: IJSObject): IJSSVGGraphicsElement;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property transform: IJSSVGAnimatedTransformList read _Gettransform;
    property nearestViewportElement: IJSSVGElement read _GetnearestViewportElement;
    property farthestViewportElement: IJSSVGElement read _GetfarthestViewportElement;
    property requiredExtensions: IJSSVGStringList read _GetrequiredExtensions;
    property systemLanguage: IJSSVGStringList read _GetsystemLanguage;
  end;

  { --------------------------------------------------------------------
    TJSHTMLAnchorElement
    --------------------------------------------------------------------}

  IJSHTMLAnchorElement = interface(IJSHTMLElement)
    ['{2CB500A1-2FE7-363E-B96B-8FDB7AA4A994}']
    function _Gettarget: UnicodeString;
    function _Getdownload: UnicodeString;
    function _Getping: UnicodeString;
    function _Getrel: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _GetrelList: IJSDOMTokenList;
    function _Gethreflang: UnicodeString;
    function _Gettype_: UnicodeString;
    function _Gettext: UnicodeString;
    function _Getcoords: UnicodeString;
    function _Getcharset: UnicodeString;
    function _Getname: UnicodeString;
    function _Getrev: UnicodeString;
    function _Getshape: UnicodeString;
    function _Gethref: UnicodeString;
    function _Getorigin: UnicodeString;
    function _Getprotocol: UnicodeString;
    function _Getusername: UnicodeString;
    function _Getpassword: UnicodeString;
    function _Gethost: UnicodeString;
    function _Gethostname: UnicodeString;
    function _Getport: UnicodeString;
    function _Getpathname: UnicodeString;
    function _Getsearch: UnicodeString;
    function _Gethash: UnicodeString;
    procedure _Settarget(const aValue: UnicodeString);
    procedure _Setdownload(const aValue: UnicodeString);
    procedure _Setping(const aValue: UnicodeString);
    procedure _Setrel(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _Sethreflang(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Settext(const aValue: UnicodeString);
    procedure _Setcoords(const aValue: UnicodeString);
    procedure _Setcharset(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setrev(const aValue: UnicodeString);
    procedure _Setshape(const aValue: UnicodeString);
    procedure _Sethref(const aValue: UnicodeString);
    procedure _Setprotocol(const aValue: UnicodeString);
    procedure _Setusername(const aValue: UnicodeString);
    procedure _Setpassword(const aValue: UnicodeString);
    procedure _Sethost(const aValue: UnicodeString);
    procedure _Sethostname(const aValue: UnicodeString);
    procedure _Setport(const aValue: UnicodeString);
    procedure _Setpathname(const aValue: UnicodeString);
    procedure _Setsearch(const aValue: UnicodeString);
    procedure _Sethash(const aValue: UnicodeString);
    property target: UnicodeString read _Gettarget write _Settarget;
    property download: UnicodeString read _Getdownload write _Setdownload;
    property ping: UnicodeString read _Getping write _Setping;
    property rel: UnicodeString read _Getrel write _Setrel;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property relList: IJSDOMTokenList read _GetrelList;
    property hreflang: UnicodeString read _Gethreflang write _Sethreflang;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property text: UnicodeString read _Gettext write _Settext;
    property coords: UnicodeString read _Getcoords write _Setcoords;
    property charset: UnicodeString read _Getcharset write _Setcharset;
    property name: UnicodeString read _Getname write _Setname;
    property rev: UnicodeString read _Getrev write _Setrev;
    property shape: UnicodeString read _Getshape write _Setshape;
    property href: UnicodeString read _Gethref write _Sethref;
    property origin: UnicodeString read _Getorigin;
    property protocol: UnicodeString read _Getprotocol write _Setprotocol;
    property username: UnicodeString read _Getusername write _Setusername;
    property password: UnicodeString read _Getpassword write _Setpassword;
    property host: UnicodeString read _Gethost write _Sethost;
    property hostname: UnicodeString read _Gethostname write _Sethostname;
    property port: UnicodeString read _Getport write _Setport;
    property pathname: UnicodeString read _Getpathname write _Setpathname;
    property search: UnicodeString read _Getsearch write _Setsearch;
    property hash: UnicodeString read _Gethash write _Sethash;
  end;

  TJSHTMLAnchorElement = class(TJSHTMLElement,IJSHTMLAnchorElement)
  Private
    function _Gettarget: UnicodeString;
    function _Getdownload: UnicodeString;
    function _Getping: UnicodeString;
    function _Getrel: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _GetrelList: IJSDOMTokenList;
    function _Gethreflang: UnicodeString;
    function _Gettype_: UnicodeString;
    function _Gettext: UnicodeString;
    function _Getcoords: UnicodeString;
    function _Getcharset: UnicodeString;
    function _Getname: UnicodeString;
    function _Getrev: UnicodeString;
    function _Getshape: UnicodeString;
    function _Gethref: UnicodeString;
    function _Getorigin: UnicodeString;
    function _Getprotocol: UnicodeString;
    function _Getusername: UnicodeString;
    function _Getpassword: UnicodeString;
    function _Gethost: UnicodeString;
    function _Gethostname: UnicodeString;
    function _Getport: UnicodeString;
    function _Getpathname: UnicodeString;
    function _Getsearch: UnicodeString;
    function _Gethash: UnicodeString;
    procedure _Settarget(const aValue: UnicodeString);
    procedure _Setdownload(const aValue: UnicodeString);
    procedure _Setping(const aValue: UnicodeString);
    procedure _Setrel(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _Sethreflang(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Settext(const aValue: UnicodeString);
    procedure _Setcoords(const aValue: UnicodeString);
    procedure _Setcharset(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setrev(const aValue: UnicodeString);
    procedure _Setshape(const aValue: UnicodeString);
    procedure _Sethref(const aValue: UnicodeString);
    procedure _Setprotocol(const aValue: UnicodeString);
    procedure _Setusername(const aValue: UnicodeString);
    procedure _Setpassword(const aValue: UnicodeString);
    procedure _Sethost(const aValue: UnicodeString);
    procedure _Sethostname(const aValue: UnicodeString);
    procedure _Setport(const aValue: UnicodeString);
    procedure _Setpathname(const aValue: UnicodeString);
    procedure _Setsearch(const aValue: UnicodeString);
    procedure _Sethash(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLAnchorElement;
    property target: UnicodeString read _Gettarget write _Settarget;
    property download: UnicodeString read _Getdownload write _Setdownload;
    property ping: UnicodeString read _Getping write _Setping;
    property rel: UnicodeString read _Getrel write _Setrel;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property relList: IJSDOMTokenList read _GetrelList;
    property hreflang: UnicodeString read _Gethreflang write _Sethreflang;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property text: UnicodeString read _Gettext write _Settext;
    property coords: UnicodeString read _Getcoords write _Setcoords;
    property charset: UnicodeString read _Getcharset write _Setcharset;
    property name: UnicodeString read _Getname write _Setname;
    property rev: UnicodeString read _Getrev write _Setrev;
    property shape: UnicodeString read _Getshape write _Setshape;
    property href: UnicodeString read _Gethref write _Sethref;
    property origin: UnicodeString read _Getorigin;
    property protocol: UnicodeString read _Getprotocol write _Setprotocol;
    property username: UnicodeString read _Getusername write _Setusername;
    property password: UnicodeString read _Getpassword write _Setpassword;
    property host: UnicodeString read _Gethost write _Sethost;
    property hostname: UnicodeString read _Gethostname write _Sethostname;
    property port: UnicodeString read _Getport write _Setport;
    property pathname: UnicodeString read _Getpathname write _Setpathname;
    property search: UnicodeString read _Getsearch write _Setsearch;
    property hash: UnicodeString read _Gethash write _Sethash;
  end;

  { --------------------------------------------------------------------
    TJSHTMLButtonElement
    --------------------------------------------------------------------}

  IJSHTMLButtonElement = interface(IJSHTMLElement)
    ['{4A781247-90AA-3699-A8DB-B6032BD119E7}']
    function _Getautofocus: Boolean;
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _GetformAction: UnicodeString;
    function _GetformEnctype: UnicodeString;
    function _GetformMethod: UnicodeString;
    function _GetformNoValidate: Boolean;
    function _GetformTarget: UnicodeString;
    function _Getname: UnicodeString;
    function _Gettype_: UnicodeString;
    function _Getvalue: UnicodeString;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    procedure _Setautofocus(const aValue: Boolean);
    procedure _Setdisabled(const aValue: Boolean);
    procedure _SetformAction(const aValue: UnicodeString);
    procedure _SetformEnctype(const aValue: UnicodeString);
    procedure _SetformMethod(const aValue: UnicodeString);
    procedure _SetformNoValidate(const aValue: Boolean);
    procedure _SetformTarget(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setvalue(const aValue: UnicodeString);
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    procedure setCustomValidity(const aError: UnicodeString);
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property formAction: UnicodeString read _GetformAction write _SetformAction;
    property formEnctype: UnicodeString read _GetformEnctype write _SetformEnctype;
    property formMethod: UnicodeString read _GetformMethod write _SetformMethod;
    property formNoValidate: Boolean read _GetformNoValidate write _SetformNoValidate;
    property formTarget: UnicodeString read _GetformTarget write _SetformTarget;
    property name: UnicodeString read _Getname write _Setname;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
  end;

  TJSHTMLButtonElement = class(TJSHTMLElement,IJSHTMLButtonElement)
  Private
    function _Getautofocus: Boolean;
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _GetformAction: UnicodeString;
    function _GetformEnctype: UnicodeString;
    function _GetformMethod: UnicodeString;
    function _GetformNoValidate: Boolean;
    function _GetformTarget: UnicodeString;
    function _Getname: UnicodeString;
    function _Gettype_: UnicodeString;
    function _Getvalue: UnicodeString;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    procedure _Setautofocus(const aValue: Boolean);
    procedure _Setdisabled(const aValue: Boolean);
    procedure _SetformAction(const aValue: UnicodeString);
    procedure _SetformEnctype(const aValue: UnicodeString);
    procedure _SetformMethod(const aValue: UnicodeString);
    procedure _SetformNoValidate(const aValue: Boolean);
    procedure _SetformTarget(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setvalue(const aValue: UnicodeString);
  Public
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    procedure setCustomValidity(const aError: UnicodeString);
    class function Cast(Intf: IJSObject): IJSHTMLButtonElement;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property formAction: UnicodeString read _GetformAction write _SetformAction;
    property formEnctype: UnicodeString read _GetformEnctype write _SetformEnctype;
    property formMethod: UnicodeString read _GetformMethod write _SetformMethod;
    property formNoValidate: Boolean read _GetformNoValidate write _SetformNoValidate;
    property formTarget: UnicodeString read _GetformTarget write _SetformTarget;
    property name: UnicodeString read _Getname write _Setname;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
  end;

  { --------------------------------------------------------------------
    TJSHTMLCanvasElement
    --------------------------------------------------------------------}

  IJSHTMLCanvasElement = interface(IJSHTMLElement)
    ['{1002B500-84B0-3B0D-B5A7-98DAC3A83402}']
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    function _GetmozOpaque: Boolean;
    procedure _Setwidth(const aValue: LongWord);
    procedure _Setheight(const aValue: LongWord);
    procedure _SetmozOpaque(const aValue: Boolean);
    function getContext(const aContextId: UnicodeString; aContextOptions: TJOB_JSValue): IJSnsISupports; overload;
    function getContext(const aContextId: UnicodeString): IJSnsISupports; overload;
    function toDataURL(const aType_: UnicodeString; aEncoderOptions: TJOB_JSValue): UnicodeString; overload;
    function toDataURL: UnicodeString; overload;
    function toDataURL(const aType_: UnicodeString): UnicodeString; overload;
    procedure toBlob(const aCallback: TBlobCallback; const aType_: UnicodeString; aEncoderOptions: TJOB_JSValue); overload;
    procedure toBlob(const aCallback: TBlobCallback); overload;
    procedure toBlob(const aCallback: TBlobCallback; const aType_: UnicodeString); overload;
    function MozGetIPCContext(const aContextId: UnicodeString): IJSnsISupports;
    function transferControlToOffscreen: IJSOffscreenCanvas;
    property width: LongWord read _Getwidth write _Setwidth;
    property height: LongWord read _Getheight write _Setheight;
    property mozOpaque: Boolean read _GetmozOpaque write _SetmozOpaque;
    // property mozPrintCallback: TPrintCallback read _GetmozPrintCallback write _SetmozPrintCallback;
  end;

  TJSHTMLCanvasElement = class(TJSHTMLElement,IJSHTMLCanvasElement)
  Private
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    function _GetmozOpaque: Boolean;
    procedure _Setwidth(const aValue: LongWord);
    procedure _Setheight(const aValue: LongWord);
    procedure _SetmozOpaque(const aValue: Boolean);
  Public
    function getContext(const aContextId: UnicodeString; aContextOptions: TJOB_JSValue): IJSnsISupports; overload;
    function getContext(const aContextId: UnicodeString): IJSnsISupports; overload;
    function toDataURL(const aType_: UnicodeString; aEncoderOptions: TJOB_JSValue): UnicodeString; overload;
    function toDataURL: UnicodeString; overload;
    function toDataURL(const aType_: UnicodeString): UnicodeString; overload;
    procedure toBlob(const aCallback: TBlobCallback; const aType_: UnicodeString; aEncoderOptions: TJOB_JSValue); overload;
    procedure toBlob(const aCallback: TBlobCallback); overload;
    procedure toBlob(const aCallback: TBlobCallback; const aType_: UnicodeString); overload;
    function MozGetIPCContext(const aContextId: UnicodeString): IJSnsISupports;
    function transferControlToOffscreen: IJSOffscreenCanvas;
    class function Cast(Intf: IJSObject): IJSHTMLCanvasElement;
    property width: LongWord read _Getwidth write _Setwidth;
    property height: LongWord read _Getheight write _Setheight;
    property mozOpaque: Boolean read _GetmozOpaque write _SetmozOpaque;
    // property mozPrintCallback: TPrintCallback read _GetmozPrintCallback write _SetmozPrintCallback;
  end;

  { --------------------------------------------------------------------
    TJSHTMLDivElement
    --------------------------------------------------------------------}

  IJSHTMLDivElement = interface(IJSHTMLElement)
    ['{9FEFFCDB-FC8A-34BC-AFAC-DE2C69275FBE}']
    function _Getalign: UnicodeString;
    procedure _Setalign(const aValue: UnicodeString);
    property align: UnicodeString read _Getalign write _Setalign;
  end;

  TJSHTMLDivElement = class(TJSHTMLElement,IJSHTMLDivElement)
  Private
    function _Getalign: UnicodeString;
    procedure _Setalign(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLDivElement;
    property align: UnicodeString read _Getalign write _Setalign;
  end;

  { --------------------------------------------------------------------
    TJSHTMLEmbedElement
    --------------------------------------------------------------------}

  IJSHTMLEmbedElement = interface(IJSHTMLElement)
    ['{C09660EF-06EB-3BA1-90A7-A9EC15F905B9}']
    function _Getsrc: UnicodeString;
    function _Gettype_: UnicodeString;
    function _Getwidth: UnicodeString;
    function _Getheight: UnicodeString;
    function _Getalign: UnicodeString;
    function _Getname: UnicodeString;
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setwidth(const aValue: UnicodeString);
    procedure _Setheight(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _SetloadingEnabled(const aValue: Boolean);
    function getSVGDocument: IJSDocument;
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property width: UnicodeString read _Getwidth write _Setwidth;
    property height: UnicodeString read _Getheight write _Setheight;
    property align: UnicodeString read _Getalign write _Setalign;
    property name: UnicodeString read _Getname write _Setname;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  TJSHTMLEmbedElement = class(TJSHTMLElement,IJSHTMLEmbedElement)
  Private
    function _Getsrc: UnicodeString;
    function _Gettype_: UnicodeString;
    function _Getwidth: UnicodeString;
    function _Getheight: UnicodeString;
    function _Getalign: UnicodeString;
    function _Getname: UnicodeString;
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setwidth(const aValue: UnicodeString);
    procedure _Setheight(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _SetloadingEnabled(const aValue: Boolean);
  Public
    Const
      UNKNOWN_REQUEST = -1;
      CURRENT_REQUEST = 0;
      PENDING_REQUEST = 1;
  Public
    function getSVGDocument: IJSDocument;
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    class function Cast(Intf: IJSObject): IJSHTMLEmbedElement;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property width: UnicodeString read _Getwidth write _Setwidth;
    property height: UnicodeString read _Getheight write _Setheight;
    property align: UnicodeString read _Getalign write _Setalign;
    property name: UnicodeString read _Getname write _Setname;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  { --------------------------------------------------------------------
    TJSHTMLIFrameElement
    --------------------------------------------------------------------}

  IJSHTMLIFrameElement = interface(IJSHTMLElement)
    ['{82EDA62E-22E9-39B3-875A-E7F7807552E9}']
    function _Getsrc: UnicodeString;
    function _Getsrcdoc: UnicodeString;
    function _Getname: UnicodeString;
    function _Getsandbox: IJSDOMTokenList;
    function _GetallowFullscreen: Boolean;
    function _Getwidth: UnicodeString;
    function _Getheight: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _GetcontentDocument: IJSDocument;
    function _GetcontentWindow: IJSWindowProxy;
    function _Getalign: UnicodeString;
    function _Getscrolling: UnicodeString;
    function _GetframeBorder: UnicodeString;
    function _GetlongDesc: UnicodeString;
    function _GetmarginHeight: UnicodeString;
    function _GetmarginWidth: UnicodeString;
    function _Getallow: UnicodeString;
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setsrcdoc(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _SetallowFullscreen(const aValue: Boolean);
    procedure _Setwidth(const aValue: UnicodeString);
    procedure _Setheight(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Setscrolling(const aValue: UnicodeString);
    procedure _SetframeBorder(const aValue: UnicodeString);
    procedure _SetlongDesc(const aValue: UnicodeString);
    procedure _SetmarginHeight(const aValue: UnicodeString);
    procedure _SetmarginWidth(const aValue: UnicodeString);
    procedure _Setallow(const aValue: UnicodeString);
    function getSVGDocument: IJSDocument;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property srcdoc: UnicodeString read _Getsrcdoc write _Setsrcdoc;
    property name: UnicodeString read _Getname write _Setname;
    property sandbox: IJSDOMTokenList read _Getsandbox;
    property allowFullscreen: Boolean read _GetallowFullscreen write _SetallowFullscreen;
    property width: UnicodeString read _Getwidth write _Setwidth;
    property height: UnicodeString read _Getheight write _Setheight;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property contentDocument: IJSDocument read _GetcontentDocument;
    property contentWindow: IJSWindowProxy read _GetcontentWindow;
    property align: UnicodeString read _Getalign write _Setalign;
    property scrolling: UnicodeString read _Getscrolling write _Setscrolling;
    property frameBorder: UnicodeString read _GetframeBorder write _SetframeBorder;
    property longDesc: UnicodeString read _GetlongDesc write _SetlongDesc;
    property marginHeight: UnicodeString read _GetmarginHeight write _SetmarginHeight;
    property marginWidth: UnicodeString read _GetmarginWidth write _SetmarginWidth;
    property allow: UnicodeString read _Getallow write _Setallow;
  end;

  TJSHTMLIFrameElement = class(TJSHTMLElement,IJSHTMLIFrameElement)
  Private
    function _Getsrc: UnicodeString;
    function _Getsrcdoc: UnicodeString;
    function _Getname: UnicodeString;
    function _Getsandbox: IJSDOMTokenList;
    function _GetallowFullscreen: Boolean;
    function _Getwidth: UnicodeString;
    function _Getheight: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _GetcontentDocument: IJSDocument;
    function _GetcontentWindow: IJSWindowProxy;
    function _Getalign: UnicodeString;
    function _Getscrolling: UnicodeString;
    function _GetframeBorder: UnicodeString;
    function _GetlongDesc: UnicodeString;
    function _GetmarginHeight: UnicodeString;
    function _GetmarginWidth: UnicodeString;
    function _Getallow: UnicodeString;
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setsrcdoc(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _SetallowFullscreen(const aValue: Boolean);
    procedure _Setwidth(const aValue: UnicodeString);
    procedure _Setheight(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Setscrolling(const aValue: UnicodeString);
    procedure _SetframeBorder(const aValue: UnicodeString);
    procedure _SetlongDesc(const aValue: UnicodeString);
    procedure _SetmarginHeight(const aValue: UnicodeString);
    procedure _SetmarginWidth(const aValue: UnicodeString);
    procedure _Setallow(const aValue: UnicodeString);
  Public
    function getSVGDocument: IJSDocument;
    class function Cast(Intf: IJSObject): IJSHTMLIFrameElement;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property srcdoc: UnicodeString read _Getsrcdoc write _Setsrcdoc;
    property name: UnicodeString read _Getname write _Setname;
    property sandbox: IJSDOMTokenList read _Getsandbox;
    property allowFullscreen: Boolean read _GetallowFullscreen write _SetallowFullscreen;
    property width: UnicodeString read _Getwidth write _Setwidth;
    property height: UnicodeString read _Getheight write _Setheight;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property contentDocument: IJSDocument read _GetcontentDocument;
    property contentWindow: IJSWindowProxy read _GetcontentWindow;
    property align: UnicodeString read _Getalign write _Setalign;
    property scrolling: UnicodeString read _Getscrolling write _Setscrolling;
    property frameBorder: UnicodeString read _GetframeBorder write _SetframeBorder;
    property longDesc: UnicodeString read _GetlongDesc write _SetlongDesc;
    property marginHeight: UnicodeString read _GetmarginHeight write _SetmarginHeight;
    property marginWidth: UnicodeString read _GetmarginWidth write _SetmarginWidth;
    property allow: UnicodeString read _Getallow write _Setallow;
  end;

  { --------------------------------------------------------------------
    TJSHTMLImageElement
    --------------------------------------------------------------------}

  IJSHTMLImageElement = interface(IJSHTMLElement)
    ['{162A22FC-FD97-3B4C-BFB1-C54764E54E4A}']
    function _Getalt: UnicodeString;
    function _Getsrc: UnicodeString;
    function _Getsrcset: UnicodeString;
    function _GetcrossOrigin: UnicodeString;
    function _GetuseMap: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _GetisMap: Boolean;
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    function _Getdecoding: UnicodeString;
    function _Getloading: UnicodeString;
    function _GetnaturalWidth: LongWord;
    function _GetnaturalHeight: LongWord;
    function _Getcomplete: Boolean;
    function _Getname: UnicodeString;
    function _Getalign: UnicodeString;
    function _Gethspace: LongWord;
    function _Getvspace: LongWord;
    function _GetlongDesc: UnicodeString;
    function _Getborder: UnicodeString;
    function _Getsizes: UnicodeString;
    function _GetcurrentSrc: UnicodeString;
    function _Getlowsrc: UnicodeString;
    function _Getx: Integer;
    function _Gety: Integer;
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _Setalt(const aValue: UnicodeString);
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setsrcset(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _SetuseMap(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _SetisMap(const aValue: Boolean);
    procedure _Setwidth(const aValue: LongWord);
    procedure _Setheight(const aValue: LongWord);
    procedure _Setdecoding(const aValue: UnicodeString);
    procedure _Setloading(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Sethspace(const aValue: LongWord);
    procedure _Setvspace(const aValue: LongWord);
    procedure _SetlongDesc(const aValue: UnicodeString);
    procedure _Setborder(const aValue: UnicodeString);
    procedure _Setsizes(const aValue: UnicodeString);
    procedure _Setlowsrc(const aValue: UnicodeString);
    procedure _SetloadingEnabled(const aValue: Boolean);
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    property alt: UnicodeString read _Getalt write _Setalt;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property srcset: UnicodeString read _Getsrcset write _Setsrcset;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property useMap: UnicodeString read _GetuseMap write _SetuseMap;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property isMap: Boolean read _GetisMap write _SetisMap;
    property width: LongWord read _Getwidth write _Setwidth;
    property height: LongWord read _Getheight write _Setheight;
    property decoding: UnicodeString read _Getdecoding write _Setdecoding;
    property loading: UnicodeString read _Getloading write _Setloading;
    property naturalWidth: LongWord read _GetnaturalWidth;
    property naturalHeight: LongWord read _GetnaturalHeight;
    property complete: Boolean read _Getcomplete;
    property name: UnicodeString read _Getname write _Setname;
    property align: UnicodeString read _Getalign write _Setalign;
    property hspace: LongWord read _Gethspace write _Sethspace;
    property vspace: LongWord read _Getvspace write _Setvspace;
    property longDesc: UnicodeString read _GetlongDesc write _SetlongDesc;
    property border: UnicodeString read _Getborder write _Setborder;
    property sizes: UnicodeString read _Getsizes write _Setsizes;
    property currentSrc: UnicodeString read _GetcurrentSrc;
    property lowsrc: UnicodeString read _Getlowsrc write _Setlowsrc;
    property x: Integer read _Getx;
    property y: Integer read _Gety;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  TJSHTMLImageElement = class(TJSHTMLElement,IJSHTMLImageElement)
  Private
    function _Getalt: UnicodeString;
    function _Getsrc: UnicodeString;
    function _Getsrcset: UnicodeString;
    function _GetcrossOrigin: UnicodeString;
    function _GetuseMap: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _GetisMap: Boolean;
    function _Getwidth: LongWord;
    function _Getheight: LongWord;
    function _Getdecoding: UnicodeString;
    function _Getloading: UnicodeString;
    function _GetnaturalWidth: LongWord;
    function _GetnaturalHeight: LongWord;
    function _Getcomplete: Boolean;
    function _Getname: UnicodeString;
    function _Getalign: UnicodeString;
    function _Gethspace: LongWord;
    function _Getvspace: LongWord;
    function _GetlongDesc: UnicodeString;
    function _Getborder: UnicodeString;
    function _Getsizes: UnicodeString;
    function _GetcurrentSrc: UnicodeString;
    function _Getlowsrc: UnicodeString;
    function _Getx: Integer;
    function _Gety: Integer;
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _Setalt(const aValue: UnicodeString);
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setsrcset(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _SetuseMap(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _SetisMap(const aValue: Boolean);
    procedure _Setwidth(const aValue: LongWord);
    procedure _Setheight(const aValue: LongWord);
    procedure _Setdecoding(const aValue: UnicodeString);
    procedure _Setloading(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Sethspace(const aValue: LongWord);
    procedure _Setvspace(const aValue: LongWord);
    procedure _SetlongDesc(const aValue: UnicodeString);
    procedure _Setborder(const aValue: UnicodeString);
    procedure _Setsizes(const aValue: UnicodeString);
    procedure _Setlowsrc(const aValue: UnicodeString);
    procedure _SetloadingEnabled(const aValue: Boolean);
  Public
    Const
      UNKNOWN_REQUEST = -1;
      CURRENT_REQUEST = 0;
      PENDING_REQUEST = 1;
  Public
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    class function Cast(Intf: IJSObject): IJSHTMLImageElement;
    property alt: UnicodeString read _Getalt write _Setalt;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property srcset: UnicodeString read _Getsrcset write _Setsrcset;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property useMap: UnicodeString read _GetuseMap write _SetuseMap;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property isMap: Boolean read _GetisMap write _SetisMap;
    property width: LongWord read _Getwidth write _Setwidth;
    property height: LongWord read _Getheight write _Setheight;
    property decoding: UnicodeString read _Getdecoding write _Setdecoding;
    property loading: UnicodeString read _Getloading write _Setloading;
    property naturalWidth: LongWord read _GetnaturalWidth;
    property naturalHeight: LongWord read _GetnaturalHeight;
    property complete: Boolean read _Getcomplete;
    property name: UnicodeString read _Getname write _Setname;
    property align: UnicodeString read _Getalign write _Setalign;
    property hspace: LongWord read _Gethspace write _Sethspace;
    property vspace: LongWord read _Getvspace write _Setvspace;
    property longDesc: UnicodeString read _GetlongDesc write _SetlongDesc;
    property border: UnicodeString read _Getborder write _Setborder;
    property sizes: UnicodeString read _Getsizes write _Setsizes;
    property currentSrc: UnicodeString read _GetcurrentSrc;
    property lowsrc: UnicodeString read _Getlowsrc write _Setlowsrc;
    property x: Integer read _Getx;
    property y: Integer read _Gety;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  { --------------------------------------------------------------------
    TJSHTMLInputElement
    --------------------------------------------------------------------}

  TJSFileSystemEntryDynArray = IJSArray; // array of TJSFileSystemEntry
  IJSHTMLInputElement = interface(IJSHTMLElement)
    ['{89A72942-54D2-3A87-A50F-CEB5D61D99E2}']
    function _Getaccept: UnicodeString;
    function _Getalt: UnicodeString;
    function _Getautocomplete: UnicodeString;
    function _Getautofocus: Boolean;
    function _Getcapture: UnicodeString;
    function _GetdefaultChecked: Boolean;
    function _Getchecked: Boolean;
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _Getfiles: IJSFileList;
    function _GetformAction: UnicodeString;
    function _GetformEnctype: UnicodeString;
    function _GetformMethod: UnicodeString;
    function _GetformNoValidate: Boolean;
    function _GetformTarget: UnicodeString;
    function _Getheight: LongWord;
    function _Getindeterminate: Boolean;
    function _Getlist: IJSHTMLElement;
    function _Getmax: UnicodeString;
    function _GetmaxLength: Integer;
    function _Getmin: UnicodeString;
    function _GetminLength: Integer;
    function _Getmultiple: Boolean;
    function _Getname: UnicodeString;
    function _Getpattern: UnicodeString;
    function _Getplaceholder: UnicodeString;
    function _GetreadOnly: Boolean;
    function _Getrequired: Boolean;
    function _Getsize: LongWord;
    function _Getsrc: UnicodeString;
    function _Getstep: UnicodeString;
    function _Gettype_: UnicodeString;
    function _GetdefaultValue: UnicodeString;
    function _Getvalue: UnicodeString;
    function _GetvalueAsDate: IJSObject;
    function _GetvalueAsNumber: Double;
    function _Getwidth: LongWord;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    function _GetselectionStart: LongWord;
    function _GetselectionEnd: LongWord;
    function _GetselectionDirection: UnicodeString;
    function _Getalign: UnicodeString;
    function _GetuseMap: UnicodeString;
    function _Getcontrollers: IJSXULControllers;
    function _GettextLength: Integer;
    function _GethasBeenTypePassword: Boolean;
    function _GetpreviewValue: UnicodeString;
    function _GetrevealPassword: Boolean;
    function _GetwebkitEntries: TJSFileSystemEntryDynArray;
    function _Getwebkitdirectory: Boolean;
    function _GetdateTimeBoxElement: IJSElement;
    function _Geteditor: IJSnsIEditor;
    function _GethasEditor: Boolean;
    function _GetisInputEventTarget: Boolean;
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _Setaccept(const aValue: UnicodeString);
    procedure _Setalt(const aValue: UnicodeString);
    procedure _Setautocomplete(const aValue: UnicodeString);
    procedure _Setautofocus(const aValue: Boolean);
    procedure _Setcapture(const aValue: UnicodeString);
    procedure _SetdefaultChecked(const aValue: Boolean);
    procedure _Setchecked(const aValue: Boolean);
    procedure _Setdisabled(const aValue: Boolean);
    procedure _Setfiles(const aValue: IJSFileList);
    procedure _SetformAction(const aValue: UnicodeString);
    procedure _SetformEnctype(const aValue: UnicodeString);
    procedure _SetformMethod(const aValue: UnicodeString);
    procedure _SetformNoValidate(const aValue: Boolean);
    procedure _SetformTarget(const aValue: UnicodeString);
    procedure _Setheight(const aValue: LongWord);
    procedure _Setindeterminate(const aValue: Boolean);
    procedure _Setmax(const aValue: UnicodeString);
    procedure _SetmaxLength(const aValue: Integer);
    procedure _Setmin(const aValue: UnicodeString);
    procedure _SetminLength(const aValue: Integer);
    procedure _Setmultiple(const aValue: Boolean);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setpattern(const aValue: UnicodeString);
    procedure _Setplaceholder(const aValue: UnicodeString);
    procedure _SetreadOnly(const aValue: Boolean);
    procedure _Setrequired(const aValue: Boolean);
    procedure _Setsize(const aValue: LongWord);
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setstep(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _SetdefaultValue(const aValue: UnicodeString);
    procedure _Setvalue(const aValue: UnicodeString);
    procedure _SetvalueAsDate(const aValue: IJSObject);
    procedure _SetvalueAsNumber(const aValue: Double);
    procedure _Setwidth(const aValue: LongWord);
    procedure _SetselectionStart(const aValue: LongWord);
    procedure _SetselectionEnd(const aValue: LongWord);
    procedure _SetselectionDirection(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _SetuseMap(const aValue: UnicodeString);
    procedure _SetpreviewValue(const aValue: UnicodeString);
    procedure _SetrevealPassword(const aValue: Boolean);
    procedure _Setwebkitdirectory(const aValue: Boolean);
    procedure _SetloadingEnabled(const aValue: Boolean);
    procedure stepUp(aN: Integer); overload;
    procedure stepUp; overload;
    procedure stepDown(aN: Integer); overload;
    procedure stepDown; overload;
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    procedure setCustomValidity(const aError: UnicodeString);
    procedure select;
    procedure setRangeText(const aReplacement: UnicodeString);
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord; aSelectionMode: TSelectionMode); overload;
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord; const aDirection: UnicodeString); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord); overload;
    procedure showPicker;
    function mozGetFileNameArray: TUnicodeStringDynArray;
    procedure mozSetDirectory(const aDirectoryPath: UnicodeString);
    function mozIsTextField(aExcludePassword: Boolean): Boolean;
    function getMinimum: Double;
    function getMaximum: Double;
    procedure openDateTimePicker(const aInitialValue: TJSDateTimeValue); overload;
    procedure openDateTimePicker; overload;
    procedure updateDateTimePicker(const aValue: TJSDateTimeValue); overload;
    procedure updateDateTimePicker; overload;
    procedure closeDateTimePicker;
    procedure setFocusState(aIsFocused: Boolean);
    procedure updateValidityState;
    function getStep: Double;
    function getStepBase: Double;
    procedure setUserInput(const aInput: UnicodeString);
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    property accept: UnicodeString read _Getaccept write _Setaccept;
    property alt: UnicodeString read _Getalt write _Setalt;
    property autocomplete: UnicodeString read _Getautocomplete write _Setautocomplete;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property capture: UnicodeString read _Getcapture write _Setcapture;
    property defaultChecked: Boolean read _GetdefaultChecked write _SetdefaultChecked;
    property checked: Boolean read _Getchecked write _Setchecked;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property files: IJSFileList read _Getfiles write _Setfiles;
    property formAction: UnicodeString read _GetformAction write _SetformAction;
    property formEnctype: UnicodeString read _GetformEnctype write _SetformEnctype;
    property formMethod: UnicodeString read _GetformMethod write _SetformMethod;
    property formNoValidate: Boolean read _GetformNoValidate write _SetformNoValidate;
    property formTarget: UnicodeString read _GetformTarget write _SetformTarget;
    property height: LongWord read _Getheight write _Setheight;
    property indeterminate: Boolean read _Getindeterminate write _Setindeterminate;
    property list: IJSHTMLElement read _Getlist;
    property max: UnicodeString read _Getmax write _Setmax;
    property maxLength: Integer read _GetmaxLength write _SetmaxLength;
    property min: UnicodeString read _Getmin write _Setmin;
    property minLength: Integer read _GetminLength write _SetminLength;
    property multiple: Boolean read _Getmultiple write _Setmultiple;
    property name: UnicodeString read _Getname write _Setname;
    property pattern: UnicodeString read _Getpattern write _Setpattern;
    property placeholder: UnicodeString read _Getplaceholder write _Setplaceholder;
    property readOnly: Boolean read _GetreadOnly write _SetreadOnly;
    property required: Boolean read _Getrequired write _Setrequired;
    property size: LongWord read _Getsize write _Setsize;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property step: UnicodeString read _Getstep write _Setstep;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property defaultValue: UnicodeString read _GetdefaultValue write _SetdefaultValue;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property valueAsDate: IJSObject read _GetvalueAsDate write _SetvalueAsDate;
    property valueAsNumber: Double read _GetvalueAsNumber write _SetvalueAsNumber;
    property width: LongWord read _Getwidth write _Setwidth;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
    property selectionStart: LongWord read _GetselectionStart write _SetselectionStart;
    property selectionEnd: LongWord read _GetselectionEnd write _SetselectionEnd;
    property selectionDirection: UnicodeString read _GetselectionDirection write _SetselectionDirection;
    property align: UnicodeString read _Getalign write _Setalign;
    property useMap: UnicodeString read _GetuseMap write _SetuseMap;
    property controllers: IJSXULControllers read _Getcontrollers;
    property textLength: Integer read _GettextLength;
    property hasBeenTypePassword: Boolean read _GethasBeenTypePassword;
    property previewValue: UnicodeString read _GetpreviewValue write _SetpreviewValue;
    property revealPassword: Boolean read _GetrevealPassword write _SetrevealPassword;
    property webkitEntries: TJSFileSystemEntryDynArray read _GetwebkitEntries;
    property webkitdirectory: Boolean read _Getwebkitdirectory write _Setwebkitdirectory;
    property dateTimeBoxElement: IJSElement read _GetdateTimeBoxElement;
    property editor: IJSnsIEditor read _Geteditor;
    property hasEditor: Boolean read _GethasEditor;
    property isInputEventTarget: Boolean read _GetisInputEventTarget;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  TJSHTMLInputElement = class(TJSHTMLElement,IJSHTMLInputElement)
  Private
    function _Getaccept: UnicodeString;
    function _Getalt: UnicodeString;
    function _Getautocomplete: UnicodeString;
    function _Getautofocus: Boolean;
    function _Getcapture: UnicodeString;
    function _GetdefaultChecked: Boolean;
    function _Getchecked: Boolean;
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _Getfiles: IJSFileList;
    function _GetformAction: UnicodeString;
    function _GetformEnctype: UnicodeString;
    function _GetformMethod: UnicodeString;
    function _GetformNoValidate: Boolean;
    function _GetformTarget: UnicodeString;
    function _Getheight: LongWord;
    function _Getindeterminate: Boolean;
    function _Getlist: IJSHTMLElement;
    function _Getmax: UnicodeString;
    function _GetmaxLength: Integer;
    function _Getmin: UnicodeString;
    function _GetminLength: Integer;
    function _Getmultiple: Boolean;
    function _Getname: UnicodeString;
    function _Getpattern: UnicodeString;
    function _Getplaceholder: UnicodeString;
    function _GetreadOnly: Boolean;
    function _Getrequired: Boolean;
    function _Getsize: LongWord;
    function _Getsrc: UnicodeString;
    function _Getstep: UnicodeString;
    function _Gettype_: UnicodeString;
    function _GetdefaultValue: UnicodeString;
    function _Getvalue: UnicodeString;
    function _GetvalueAsDate: IJSObject;
    function _GetvalueAsNumber: Double;
    function _Getwidth: LongWord;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    function _GetselectionStart: LongWord;
    function _GetselectionEnd: LongWord;
    function _GetselectionDirection: UnicodeString;
    function _Getalign: UnicodeString;
    function _GetuseMap: UnicodeString;
    function _Getcontrollers: IJSXULControllers;
    function _GettextLength: Integer;
    function _GethasBeenTypePassword: Boolean;
    function _GetpreviewValue: UnicodeString;
    function _GetrevealPassword: Boolean;
    function _GetwebkitEntries: TJSFileSystemEntryDynArray;
    function _Getwebkitdirectory: Boolean;
    function _GetdateTimeBoxElement: IJSElement;
    function _Geteditor: IJSnsIEditor;
    function _GethasEditor: Boolean;
    function _GetisInputEventTarget: Boolean;
    function _GetloadingEnabled: Boolean;
    function _GetcurrentURI: IJSURI;
    function _GetcurrentRequestFinalURI: IJSURI;
    procedure _Setaccept(const aValue: UnicodeString);
    procedure _Setalt(const aValue: UnicodeString);
    procedure _Setautocomplete(const aValue: UnicodeString);
    procedure _Setautofocus(const aValue: Boolean);
    procedure _Setcapture(const aValue: UnicodeString);
    procedure _SetdefaultChecked(const aValue: Boolean);
    procedure _Setchecked(const aValue: Boolean);
    procedure _Setdisabled(const aValue: Boolean);
    procedure _Setfiles(const aValue: IJSFileList);
    procedure _SetformAction(const aValue: UnicodeString);
    procedure _SetformEnctype(const aValue: UnicodeString);
    procedure _SetformMethod(const aValue: UnicodeString);
    procedure _SetformNoValidate(const aValue: Boolean);
    procedure _SetformTarget(const aValue: UnicodeString);
    procedure _Setheight(const aValue: LongWord);
    procedure _Setindeterminate(const aValue: Boolean);
    procedure _Setmax(const aValue: UnicodeString);
    procedure _SetmaxLength(const aValue: Integer);
    procedure _Setmin(const aValue: UnicodeString);
    procedure _SetminLength(const aValue: Integer);
    procedure _Setmultiple(const aValue: Boolean);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setpattern(const aValue: UnicodeString);
    procedure _Setplaceholder(const aValue: UnicodeString);
    procedure _SetreadOnly(const aValue: Boolean);
    procedure _Setrequired(const aValue: Boolean);
    procedure _Setsize(const aValue: LongWord);
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setstep(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _SetdefaultValue(const aValue: UnicodeString);
    procedure _Setvalue(const aValue: UnicodeString);
    procedure _SetvalueAsDate(const aValue: IJSObject);
    procedure _SetvalueAsNumber(const aValue: Double);
    procedure _Setwidth(const aValue: LongWord);
    procedure _SetselectionStart(const aValue: LongWord);
    procedure _SetselectionEnd(const aValue: LongWord);
    procedure _SetselectionDirection(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _SetuseMap(const aValue: UnicodeString);
    procedure _SetpreviewValue(const aValue: UnicodeString);
    procedure _SetrevealPassword(const aValue: Boolean);
    procedure _Setwebkitdirectory(const aValue: Boolean);
    procedure _SetloadingEnabled(const aValue: Boolean);
  Public
    Const
      UNKNOWN_REQUEST = -1;
      CURRENT_REQUEST = 0;
      PENDING_REQUEST = 1;
  Public
    procedure stepUp(aN: Integer); overload;
    procedure stepUp; overload;
    procedure stepDown(aN: Integer); overload;
    procedure stepDown; overload;
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    procedure setCustomValidity(const aError: UnicodeString);
    procedure select;
    procedure setRangeText(const aReplacement: UnicodeString);
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord; aSelectionMode: TSelectionMode); overload;
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord; const aDirection: UnicodeString); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord); overload;
    procedure showPicker;
    function mozGetFileNameArray: TUnicodeStringDynArray;
    procedure mozSetDirectory(const aDirectoryPath: UnicodeString);
    function mozIsTextField(aExcludePassword: Boolean): Boolean;
    function getMinimum: Double;
    function getMaximum: Double;
    procedure openDateTimePicker(const aInitialValue: TJSDateTimeValue); overload;
    procedure openDateTimePicker; overload;
    procedure updateDateTimePicker(const aValue: TJSDateTimeValue); overload;
    procedure updateDateTimePicker; overload;
    procedure closeDateTimePicker;
    procedure setFocusState(aIsFocused: Boolean);
    procedure updateValidityState;
    function getStep: Double;
    function getStepBase: Double;
    procedure setUserInput(const aInput: UnicodeString);
    procedure addObserver(aObserver: IJSimgINotificationObserver);
    procedure removeObserver(aObserver: IJSimgINotificationObserver);
    function getRequest(aRequestType: Integer): IJSimgIRequest;
    function getRequestType(aRequest: IJSimgIRequest): Integer;
    procedure forceReload(aNotify: Boolean); overload;
    procedure forceReload; overload;
    class function Cast(Intf: IJSObject): IJSHTMLInputElement;
    property accept: UnicodeString read _Getaccept write _Setaccept;
    property alt: UnicodeString read _Getalt write _Setalt;
    property autocomplete: UnicodeString read _Getautocomplete write _Setautocomplete;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property capture: UnicodeString read _Getcapture write _Setcapture;
    property defaultChecked: Boolean read _GetdefaultChecked write _SetdefaultChecked;
    property checked: Boolean read _Getchecked write _Setchecked;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property files: IJSFileList read _Getfiles write _Setfiles;
    property formAction: UnicodeString read _GetformAction write _SetformAction;
    property formEnctype: UnicodeString read _GetformEnctype write _SetformEnctype;
    property formMethod: UnicodeString read _GetformMethod write _SetformMethod;
    property formNoValidate: Boolean read _GetformNoValidate write _SetformNoValidate;
    property formTarget: UnicodeString read _GetformTarget write _SetformTarget;
    property height: LongWord read _Getheight write _Setheight;
    property indeterminate: Boolean read _Getindeterminate write _Setindeterminate;
    property list: IJSHTMLElement read _Getlist;
    property max: UnicodeString read _Getmax write _Setmax;
    property maxLength: Integer read _GetmaxLength write _SetmaxLength;
    property min: UnicodeString read _Getmin write _Setmin;
    property minLength: Integer read _GetminLength write _SetminLength;
    property multiple: Boolean read _Getmultiple write _Setmultiple;
    property name: UnicodeString read _Getname write _Setname;
    property pattern: UnicodeString read _Getpattern write _Setpattern;
    property placeholder: UnicodeString read _Getplaceholder write _Setplaceholder;
    property readOnly: Boolean read _GetreadOnly write _SetreadOnly;
    property required: Boolean read _Getrequired write _Setrequired;
    property size: LongWord read _Getsize write _Setsize;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property step: UnicodeString read _Getstep write _Setstep;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property defaultValue: UnicodeString read _GetdefaultValue write _SetdefaultValue;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property valueAsDate: IJSObject read _GetvalueAsDate write _SetvalueAsDate;
    property valueAsNumber: Double read _GetvalueAsNumber write _SetvalueAsNumber;
    property width: LongWord read _Getwidth write _Setwidth;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
    property selectionStart: LongWord read _GetselectionStart write _SetselectionStart;
    property selectionEnd: LongWord read _GetselectionEnd write _SetselectionEnd;
    property selectionDirection: UnicodeString read _GetselectionDirection write _SetselectionDirection;
    property align: UnicodeString read _Getalign write _Setalign;
    property useMap: UnicodeString read _GetuseMap write _SetuseMap;
    property controllers: IJSXULControllers read _Getcontrollers;
    property textLength: Integer read _GettextLength;
    property hasBeenTypePassword: Boolean read _GethasBeenTypePassword;
    property previewValue: UnicodeString read _GetpreviewValue write _SetpreviewValue;
    property revealPassword: Boolean read _GetrevealPassword write _SetrevealPassword;
    property webkitEntries: TJSFileSystemEntryDynArray read _GetwebkitEntries;
    property webkitdirectory: Boolean read _Getwebkitdirectory write _Setwebkitdirectory;
    property dateTimeBoxElement: IJSElement read _GetdateTimeBoxElement;
    property editor: IJSnsIEditor read _Geteditor;
    property hasEditor: Boolean read _GethasEditor;
    property isInputEventTarget: Boolean read _GetisInputEventTarget;
    property loadingEnabled: Boolean read _GetloadingEnabled write _SetloadingEnabled;
    property currentURI: IJSURI read _GetcurrentURI;
    property currentRequestFinalURI: IJSURI read _GetcurrentRequestFinalURI;
  end;

  { --------------------------------------------------------------------
    TJSHTMLLabelElement
    --------------------------------------------------------------------}

  IJSHTMLLabelElement = interface(IJSHTMLElement)
    ['{71E7BBF1-BF5B-3CA6-A893-D19CFC859516}']
    function _Getform: IJSHTMLFormElement;
    function _GethtmlFor: UnicodeString;
    function _Getcontrol: IJSHTMLElement;
    procedure _SethtmlFor(const aValue: UnicodeString);
    property form: IJSHTMLFormElement read _Getform;
    property htmlFor: UnicodeString read _GethtmlFor write _SethtmlFor;
    property control: IJSHTMLElement read _Getcontrol;
  end;

  TJSHTMLLabelElement = class(TJSHTMLElement,IJSHTMLLabelElement)
  Private
    function _Getform: IJSHTMLFormElement;
    function _GethtmlFor: UnicodeString;
    function _Getcontrol: IJSHTMLElement;
    procedure _SethtmlFor(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLLabelElement;
    property form: IJSHTMLFormElement read _Getform;
    property htmlFor: UnicodeString read _GethtmlFor write _SethtmlFor;
    property control: IJSHTMLElement read _Getcontrol;
  end;

  { --------------------------------------------------------------------
    TJSHTMLLinkElement
    --------------------------------------------------------------------}

  IJSHTMLLinkElement = interface(IJSHTMLElement)
    ['{2BC56900-44E8-3883-8F64-8758F3BE3D84}']
    function _Getdisabled: Boolean;
    function _Gethref: UnicodeString;
    function _GetcrossOrigin: UnicodeString;
    function _Getrel: UnicodeString;
    function _GetrelList: IJSDOMTokenList;
    function _Getmedia: UnicodeString;
    function _Gethreflang: UnicodeString;
    function _Gettype_: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _Getsizes: IJSDOMTokenList;
    function _GetimageSrcset: UnicodeString;
    function _GetimageSizes: UnicodeString;
    function _Getcharset: UnicodeString;
    function _Getrev: UnicodeString;
    function _Gettarget: UnicodeString;
    function _Getintegrity: UnicodeString;
    function _Getas_: UnicodeString;
    function _Getsheet: IJSStyleSheet;
    procedure _Setdisabled(const aValue: Boolean);
    procedure _Sethref(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _Setrel(const aValue: UnicodeString);
    procedure _Setmedia(const aValue: UnicodeString);
    procedure _Sethreflang(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _SetimageSrcset(const aValue: UnicodeString);
    procedure _SetimageSizes(const aValue: UnicodeString);
    procedure _Setcharset(const aValue: UnicodeString);
    procedure _Setrev(const aValue: UnicodeString);
    procedure _Settarget(const aValue: UnicodeString);
    procedure _Setintegrity(const aValue: UnicodeString);
    procedure _Setas_(const aValue: UnicodeString);
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property href: UnicodeString read _Gethref write _Sethref;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property rel: UnicodeString read _Getrel write _Setrel;
    property relList: IJSDOMTokenList read _GetrelList;
    property media: UnicodeString read _Getmedia write _Setmedia;
    property hreflang: UnicodeString read _Gethreflang write _Sethreflang;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property sizes: IJSDOMTokenList read _Getsizes;
    property imageSrcset: UnicodeString read _GetimageSrcset write _SetimageSrcset;
    property imageSizes: UnicodeString read _GetimageSizes write _SetimageSizes;
    property charset: UnicodeString read _Getcharset write _Setcharset;
    property rev: UnicodeString read _Getrev write _Setrev;
    property target: UnicodeString read _Gettarget write _Settarget;
    property integrity: UnicodeString read _Getintegrity write _Setintegrity;
    property as_: UnicodeString read _Getas_ write _Setas_;
    property sheet: IJSStyleSheet read _Getsheet;
  end;

  TJSHTMLLinkElement = class(TJSHTMLElement,IJSHTMLLinkElement)
  Private
    function _Getdisabled: Boolean;
    function _Gethref: UnicodeString;
    function _GetcrossOrigin: UnicodeString;
    function _Getrel: UnicodeString;
    function _GetrelList: IJSDOMTokenList;
    function _Getmedia: UnicodeString;
    function _Gethreflang: UnicodeString;
    function _Gettype_: UnicodeString;
    function _GetreferrerPolicy: UnicodeString;
    function _Getsizes: IJSDOMTokenList;
    function _GetimageSrcset: UnicodeString;
    function _GetimageSizes: UnicodeString;
    function _Getcharset: UnicodeString;
    function _Getrev: UnicodeString;
    function _Gettarget: UnicodeString;
    function _Getintegrity: UnicodeString;
    function _Getas_: UnicodeString;
    function _Getsheet: IJSStyleSheet;
    procedure _Setdisabled(const aValue: Boolean);
    procedure _Sethref(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _Setrel(const aValue: UnicodeString);
    procedure _Setmedia(const aValue: UnicodeString);
    procedure _Sethreflang(const aValue: UnicodeString);
    procedure _Settype_(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _SetimageSrcset(const aValue: UnicodeString);
    procedure _SetimageSizes(const aValue: UnicodeString);
    procedure _Setcharset(const aValue: UnicodeString);
    procedure _Setrev(const aValue: UnicodeString);
    procedure _Settarget(const aValue: UnicodeString);
    procedure _Setintegrity(const aValue: UnicodeString);
    procedure _Setas_(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLLinkElement;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property href: UnicodeString read _Gethref write _Sethref;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property rel: UnicodeString read _Getrel write _Setrel;
    property relList: IJSDOMTokenList read _GetrelList;
    property media: UnicodeString read _Getmedia write _Setmedia;
    property hreflang: UnicodeString read _Gethreflang write _Sethreflang;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property sizes: IJSDOMTokenList read _Getsizes;
    property imageSrcset: UnicodeString read _GetimageSrcset write _SetimageSrcset;
    property imageSizes: UnicodeString read _GetimageSizes write _SetimageSizes;
    property charset: UnicodeString read _Getcharset write _Setcharset;
    property rev: UnicodeString read _Getrev write _Setrev;
    property target: UnicodeString read _Gettarget write _Settarget;
    property integrity: UnicodeString read _Getintegrity write _Setintegrity;
    property as_: UnicodeString read _Getas_ write _Setas_;
    property sheet: IJSStyleSheet read _Getsheet;
  end;

  { --------------------------------------------------------------------
    TJSHTMLOptionElement
    --------------------------------------------------------------------}

  IJSHTMLOptionElement = interface(IJSHTMLElement)
    ['{63D0277A-90D2-31D1-806F-057D9ACC0578}']
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _Getlabel_: UnicodeString;
    function _GetdefaultSelected: Boolean;
    function _Getselected: Boolean;
    function _Getvalue: UnicodeString;
    function _Gettext: UnicodeString;
    function _Getindex: Integer;
    procedure _Setdisabled(const aValue: Boolean);
    procedure _Setlabel_(const aValue: UnicodeString);
    procedure _SetdefaultSelected(const aValue: Boolean);
    procedure _Setselected(const aValue: Boolean);
    procedure _Setvalue(const aValue: UnicodeString);
    procedure _Settext(const aValue: UnicodeString);
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property label_: UnicodeString read _Getlabel_ write _Setlabel_;
    property defaultSelected: Boolean read _GetdefaultSelected write _SetdefaultSelected;
    property selected: Boolean read _Getselected write _Setselected;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property text: UnicodeString read _Gettext write _Settext;
    property index: Integer read _Getindex;
  end;

  TJSHTMLOptionElement = class(TJSHTMLElement,IJSHTMLOptionElement)
  Private
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _Getlabel_: UnicodeString;
    function _GetdefaultSelected: Boolean;
    function _Getselected: Boolean;
    function _Getvalue: UnicodeString;
    function _Gettext: UnicodeString;
    function _Getindex: Integer;
    procedure _Setdisabled(const aValue: Boolean);
    procedure _Setlabel_(const aValue: UnicodeString);
    procedure _SetdefaultSelected(const aValue: Boolean);
    procedure _Setselected(const aValue: Boolean);
    procedure _Setvalue(const aValue: UnicodeString);
    procedure _Settext(const aValue: UnicodeString);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLOptionElement;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property label_: UnicodeString read _Getlabel_ write _Setlabel_;
    property defaultSelected: Boolean read _GetdefaultSelected write _SetdefaultSelected;
    property selected: Boolean read _Getselected write _Setselected;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property text: UnicodeString read _Gettext write _Settext;
    property index: Integer read _Getindex;
  end;

  { --------------------------------------------------------------------
    TJSHTMLProgressElement
    --------------------------------------------------------------------}

  IJSHTMLProgressElement = interface(IJSHTMLElement)
    ['{F458E43E-91E5-38C9-96C6-11ED4A13516B}']
    function _Getvalue: Double;
    function _Getmax: Double;
    function _Getposition: Double;
    function _Getlabels: IJSNodeList;
    procedure _Setvalue(const aValue: Double);
    procedure _Setmax(const aValue: Double);
    property value: Double read _Getvalue write _Setvalue;
    property max: Double read _Getmax write _Setmax;
    property position: Double read _Getposition;
    property labels: IJSNodeList read _Getlabels;
  end;

  TJSHTMLProgressElement = class(TJSHTMLElement,IJSHTMLProgressElement)
  Private
    function _Getvalue: Double;
    function _Getmax: Double;
    function _Getposition: Double;
    function _Getlabels: IJSNodeList;
    procedure _Setvalue(const aValue: Double);
    procedure _Setmax(const aValue: Double);
  Public
    class function Cast(Intf: IJSObject): IJSHTMLProgressElement;
    property value: Double read _Getvalue write _Setvalue;
    property max: Double read _Getmax write _Setmax;
    property position: Double read _Getposition;
    property labels: IJSNodeList read _Getlabels;
  end;

  { --------------------------------------------------------------------
    TJSHTMLTextAreaElement
    --------------------------------------------------------------------}

  IJSHTMLTextAreaElement = interface(IJSHTMLElement)
    ['{64209EA9-223E-32C3-90FA-DDAC32C47B09}']
    function _Getautocomplete: UnicodeString;
    function _Getautofocus: Boolean;
    function _Getcols: LongWord;
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _GetmaxLength: Integer;
    function _GetminLength: Integer;
    function _Getname: UnicodeString;
    function _Getplaceholder: UnicodeString;
    function _GetreadOnly: Boolean;
    function _Getrequired: Boolean;
    function _Getrows: LongWord;
    function _Getwrap: UnicodeString;
    function _Gettype_: UnicodeString;
    function _GetdefaultValue: UnicodeString;
    function _Getvalue: UnicodeString;
    function _GettextLength: LongWord;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    function _GetselectionStart: LongWord;
    function _GetselectionEnd: LongWord;
    function _GetselectionDirection: UnicodeString;
    function _Getcontrollers: IJSXULControllers;
    function _GetpreviewValue: UnicodeString;
    function _Geteditor: IJSnsIEditor;
    function _GethasEditor: Boolean;
    function _GetisInputEventTarget: Boolean;
    procedure _Setautocomplete(const aValue: UnicodeString);
    procedure _Setautofocus(const aValue: Boolean);
    procedure _Setcols(const aValue: LongWord);
    procedure _Setdisabled(const aValue: Boolean);
    procedure _SetmaxLength(const aValue: Integer);
    procedure _SetminLength(const aValue: Integer);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setplaceholder(const aValue: UnicodeString);
    procedure _SetreadOnly(const aValue: Boolean);
    procedure _Setrequired(const aValue: Boolean);
    procedure _Setrows(const aValue: LongWord);
    procedure _Setwrap(const aValue: UnicodeString);
    procedure _SetdefaultValue(const aValue: UnicodeString);
    procedure _Setvalue(const aValue: UnicodeString);
    procedure _SetselectionStart(const aValue: LongWord);
    procedure _SetselectionEnd(const aValue: LongWord);
    procedure _SetselectionDirection(const aValue: UnicodeString);
    procedure _SetpreviewValue(const aValue: UnicodeString);
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    procedure setCustomValidity(const aError: UnicodeString);
    procedure select;
    procedure setRangeText(const aReplacement: UnicodeString);
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord; aSelectionMode: TSelectionMode); overload;
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord; const aDirection: UnicodeString); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord); overload;
    procedure setUserInput(const aInput: UnicodeString);
    property autocomplete: UnicodeString read _Getautocomplete write _Setautocomplete;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property cols: LongWord read _Getcols write _Setcols;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property maxLength: Integer read _GetmaxLength write _SetmaxLength;
    property minLength: Integer read _GetminLength write _SetminLength;
    property name: UnicodeString read _Getname write _Setname;
    property placeholder: UnicodeString read _Getplaceholder write _Setplaceholder;
    property readOnly: Boolean read _GetreadOnly write _SetreadOnly;
    property required: Boolean read _Getrequired write _Setrequired;
    property rows: LongWord read _Getrows write _Setrows;
    property wrap: UnicodeString read _Getwrap write _Setwrap;
    property type_: UnicodeString read _Gettype_;
    property defaultValue: UnicodeString read _GetdefaultValue write _SetdefaultValue;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property textLength: LongWord read _GettextLength;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
    property selectionStart: LongWord read _GetselectionStart write _SetselectionStart;
    property selectionEnd: LongWord read _GetselectionEnd write _SetselectionEnd;
    property selectionDirection: UnicodeString read _GetselectionDirection write _SetselectionDirection;
    property controllers: IJSXULControllers read _Getcontrollers;
    property previewValue: UnicodeString read _GetpreviewValue write _SetpreviewValue;
    property editor: IJSnsIEditor read _Geteditor;
    property hasEditor: Boolean read _GethasEditor;
    property isInputEventTarget: Boolean read _GetisInputEventTarget;
  end;

  TJSHTMLTextAreaElement = class(TJSHTMLElement,IJSHTMLTextAreaElement)
  Private
    function _Getautocomplete: UnicodeString;
    function _Getautofocus: Boolean;
    function _Getcols: LongWord;
    function _Getdisabled: Boolean;
    function _Getform: IJSHTMLFormElement;
    function _GetmaxLength: Integer;
    function _GetminLength: Integer;
    function _Getname: UnicodeString;
    function _Getplaceholder: UnicodeString;
    function _GetreadOnly: Boolean;
    function _Getrequired: Boolean;
    function _Getrows: LongWord;
    function _Getwrap: UnicodeString;
    function _Gettype_: UnicodeString;
    function _GetdefaultValue: UnicodeString;
    function _Getvalue: UnicodeString;
    function _GettextLength: LongWord;
    function _GetwillValidate: Boolean;
    function _Getvalidity: IJSValidityState;
    function _GetvalidationMessage: UnicodeString;
    function _Getlabels: IJSNodeList;
    function _GetselectionStart: LongWord;
    function _GetselectionEnd: LongWord;
    function _GetselectionDirection: UnicodeString;
    function _Getcontrollers: IJSXULControllers;
    function _GetpreviewValue: UnicodeString;
    function _Geteditor: IJSnsIEditor;
    function _GethasEditor: Boolean;
    function _GetisInputEventTarget: Boolean;
    procedure _Setautocomplete(const aValue: UnicodeString);
    procedure _Setautofocus(const aValue: Boolean);
    procedure _Setcols(const aValue: LongWord);
    procedure _Setdisabled(const aValue: Boolean);
    procedure _SetmaxLength(const aValue: Integer);
    procedure _SetminLength(const aValue: Integer);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setplaceholder(const aValue: UnicodeString);
    procedure _SetreadOnly(const aValue: Boolean);
    procedure _Setrequired(const aValue: Boolean);
    procedure _Setrows(const aValue: LongWord);
    procedure _Setwrap(const aValue: UnicodeString);
    procedure _SetdefaultValue(const aValue: UnicodeString);
    procedure _Setvalue(const aValue: UnicodeString);
    procedure _SetselectionStart(const aValue: LongWord);
    procedure _SetselectionEnd(const aValue: LongWord);
    procedure _SetselectionDirection(const aValue: UnicodeString);
    procedure _SetpreviewValue(const aValue: UnicodeString);
  Public
    function checkValidity: Boolean;
    function reportValidity: Boolean;
    procedure setCustomValidity(const aError: UnicodeString);
    procedure select;
    procedure setRangeText(const aReplacement: UnicodeString);
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord; aSelectionMode: TSelectionMode); overload;
    procedure setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord; const aDirection: UnicodeString); overload;
    procedure setSelectionRange(aStart: LongWord; aEnd_: LongWord); overload;
    procedure setUserInput(const aInput: UnicodeString);
    class function Cast(Intf: IJSObject): IJSHTMLTextAreaElement;
    property autocomplete: UnicodeString read _Getautocomplete write _Setautocomplete;
    property autofocus: Boolean read _Getautofocus write _Setautofocus;
    property cols: LongWord read _Getcols write _Setcols;
    property disabled: Boolean read _Getdisabled write _Setdisabled;
    property form: IJSHTMLFormElement read _Getform;
    property maxLength: Integer read _GetmaxLength write _SetmaxLength;
    property minLength: Integer read _GetminLength write _SetminLength;
    property name: UnicodeString read _Getname write _Setname;
    property placeholder: UnicodeString read _Getplaceholder write _Setplaceholder;
    property readOnly: Boolean read _GetreadOnly write _SetreadOnly;
    property required: Boolean read _Getrequired write _Setrequired;
    property rows: LongWord read _Getrows write _Setrows;
    property wrap: UnicodeString read _Getwrap write _Setwrap;
    property type_: UnicodeString read _Gettype_;
    property defaultValue: UnicodeString read _GetdefaultValue write _SetdefaultValue;
    property value: UnicodeString read _Getvalue write _Setvalue;
    property textLength: LongWord read _GettextLength;
    property willValidate: Boolean read _GetwillValidate;
    property validity: IJSValidityState read _Getvalidity;
    property validationMessage: UnicodeString read _GetvalidationMessage;
    property labels: IJSNodeList read _Getlabels;
    property selectionStart: LongWord read _GetselectionStart write _SetselectionStart;
    property selectionEnd: LongWord read _GetselectionEnd write _SetselectionEnd;
    property selectionDirection: UnicodeString read _GetselectionDirection write _SetselectionDirection;
    property controllers: IJSXULControllers read _Getcontrollers;
    property previewValue: UnicodeString read _GetpreviewValue write _SetpreviewValue;
    property editor: IJSnsIEditor read _Geteditor;
    property hasEditor: Boolean read _GethasEditor;
    property isInputEventTarget: Boolean read _GetisInputEventTarget;
  end;

  { --------------------------------------------------------------------
    TJSSVGSVGElement
    --------------------------------------------------------------------}

  IJSSVGSVGElement = interface(IJSSVGGraphicsElement)
    ['{9D2B320B-86B9-39F4-A309-83D1B2F6B0FE}']
    function _Getx: IJSSVGAnimatedLength;
    function _Gety: IJSSVGAnimatedLength;
    function _Getwidth: IJSSVGAnimatedLength;
    function _Getheight: IJSSVGAnimatedLength;
    function _GetuseCurrentView: Boolean;
    function _GetcurrentScale: Single;
    function _GetcurrentTranslate: IJSSVGPoint;
    function _GetviewBox: IJSSVGAnimatedRect;
    function _GetpreserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio;
    function _GetzoomAndPan: Word;
    procedure _SetcurrentScale(const aValue: Single);
    procedure _SetzoomAndPan(const aValue: Word);
    function suspendRedraw(aMaxWaitMilliseconds: LongWord): LongWord;
    procedure unsuspendRedraw(aSuspendHandleID: LongWord);
    procedure unsuspendRedrawAll;
    procedure forceRedraw;
    procedure pauseAnimations;
    procedure unpauseAnimations;
    function animationsPaused: Boolean;
    function getCurrentTime: Single;
    procedure setCurrentTime(aSeconds: Single);
    procedure deselectAll;
    function createSVGNumber: IJSSVGNumber;
    function createSVGLength: IJSSVGLength;
    function createSVGAngle: IJSSVGAngle;
    function createSVGPoint: IJSSVGPoint;
    function createSVGMatrix: IJSSVGMatrix;
    function createSVGRect: IJSSVGRect;
    function createSVGTransform: IJSSVGTransform;
    function createSVGTransformFromMatrix(const aMatrix: TJSDOMMatrix2DInit): IJSSVGTransform; overload;
    function createSVGTransformFromMatrix: IJSSVGTransform; overload;
    function getElementById(const aElementId: UnicodeString): IJSElement;
    property x: IJSSVGAnimatedLength read _Getx;
    property y: IJSSVGAnimatedLength read _Gety;
    property width: IJSSVGAnimatedLength read _Getwidth;
    property height: IJSSVGAnimatedLength read _Getheight;
    property useCurrentView: Boolean read _GetuseCurrentView;
    property currentScale: Single read _GetcurrentScale write _SetcurrentScale;
    property currentTranslate: IJSSVGPoint read _GetcurrentTranslate;
    property viewBox: IJSSVGAnimatedRect read _GetviewBox;
    property preserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio read _GetpreserveAspectRatio;
    property zoomAndPan: Word read _GetzoomAndPan write _SetzoomAndPan;
  end;

  TJSSVGSVGElement = class(TJSSVGGraphicsElement,IJSSVGSVGElement)
  Private
    function _Getx: IJSSVGAnimatedLength;
    function _Gety: IJSSVGAnimatedLength;
    function _Getwidth: IJSSVGAnimatedLength;
    function _Getheight: IJSSVGAnimatedLength;
    function _GetuseCurrentView: Boolean;
    function _GetcurrentScale: Single;
    function _GetcurrentTranslate: IJSSVGPoint;
    function _GetviewBox: IJSSVGAnimatedRect;
    function _GetpreserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio;
    function _GetzoomAndPan: Word;
    procedure _SetcurrentScale(const aValue: Single);
    procedure _SetzoomAndPan(const aValue: Word);
  Public
    Const
      SVG_ZOOMANDPAN_UNKNOWN = 0;
      SVG_ZOOMANDPAN_DISABLE = 1;
      SVG_ZOOMANDPAN_MAGNIFY = 2;
  Public
    function suspendRedraw(aMaxWaitMilliseconds: LongWord): LongWord;
    procedure unsuspendRedraw(aSuspendHandleID: LongWord);
    procedure unsuspendRedrawAll;
    procedure forceRedraw;
    procedure pauseAnimations;
    procedure unpauseAnimations;
    function animationsPaused: Boolean;
    function getCurrentTime: Single;
    procedure setCurrentTime(aSeconds: Single);
    procedure deselectAll;
    function createSVGNumber: IJSSVGNumber;
    function createSVGLength: IJSSVGLength;
    function createSVGAngle: IJSSVGAngle;
    function createSVGPoint: IJSSVGPoint;
    function createSVGMatrix: IJSSVGMatrix;
    function createSVGRect: IJSSVGRect;
    function createSVGTransform: IJSSVGTransform;
    function createSVGTransformFromMatrix(const aMatrix: TJSDOMMatrix2DInit): IJSSVGTransform; overload;
    function createSVGTransformFromMatrix: IJSSVGTransform; overload;
    function getElementById(const aElementId: UnicodeString): IJSElement;
    class function Cast(Intf: IJSObject): IJSSVGSVGElement;
    property x: IJSSVGAnimatedLength read _Getx;
    property y: IJSSVGAnimatedLength read _Gety;
    property width: IJSSVGAnimatedLength read _Getwidth;
    property height: IJSSVGAnimatedLength read _Getheight;
    property useCurrentView: Boolean read _GetuseCurrentView;
    property currentScale: Single read _GetcurrentScale write _SetcurrentScale;
    property currentTranslate: IJSSVGPoint read _GetcurrentTranslate;
    property viewBox: IJSSVGAnimatedRect read _GetviewBox;
    property preserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio read _GetpreserveAspectRatio;
    property zoomAndPan: Word read _GetzoomAndPan write _SetzoomAndPan;
  end;

var
  JSDocument: TJSDocument;
  JSWindow: TJSWindow;

implementation

function JOBCallTEventListener(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: IJSEvent;
begin
  event:=H.GetObject(TJSEvent) as IJSEvent;
  Result:=H.AllocBool(TEventListener(aMethod)(event));
end;

function JOBCallTEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: IJSEvent;
begin
  event:=H.GetObject(TJSEvent) as IJSEvent;
  Result:=H.AllocJSValue(TEventHandlerNonNull(aMethod)(event));
end;

function JOBCallTOnBeforeUnloadEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: IJSEvent;
begin
  event:=H.GetObject(TJSEvent) as IJSEvent;
  Result:=H.AllocString(TOnBeforeUnloadEventHandlerNonNull(aMethod)(event));
end;

function JOBCallTOnErrorEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: TJOB_JSValue;
  source: UnicodeString;
  lineno: LongWord;
  column: LongWord;
  error: TJOB_JSValue;
begin
  event:=H.GetValue;  source:=H.GetString;  lineno:=H.GetMaxInt;  column:=H.GetMaxInt;  error:=H.GetValue;
  Result:=H.AllocJSValue(TOnErrorEventHandlerNonNull(aMethod)(event,source,lineno,column,error));
end;

function JOBCallTPromiseDocumentFlushedCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
begin
  Result:=H.AllocJSValue(TPromiseDocumentFlushedCallback(aMethod)());
end;

function JOBCallTCustomElementCreationCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  name: UnicodeString;
begin
  name:=H.GetString;
  TCustomElementCreationCallback(aMethod)(name);
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleConnectedCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
begin
  TLifecycleConnectedCallback(aMethod)();
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleDisconnectedCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
begin
  TLifecycleDisconnectedCallback(aMethod)();
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleAdoptedCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  oldDocument: IJSDocument;
  newDocment: IJSDocument;
begin
  oldDocument:=H.GetObject(TJSDocument) as IJSDocument;  newDocment:=H.GetObject(TJSDocument) as IJSDocument;
  TLifecycleAdoptedCallback(aMethod)(oldDocument,newDocment);
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleAttributeChangedCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  attrName: UnicodeString;
  oldValue: UnicodeString;
  newValue: UnicodeString;
  namespaceURI: UnicodeString;
begin
  attrName:=H.GetString;  oldValue:=H.GetString;  newValue:=H.GetString;  namespaceURI:=H.GetString;
  TLifecycleAttributeChangedCallback(aMethod)(attrName,oldValue,newValue,namespaceURI);
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleFormAssociatedCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  form: IJSHTMLFormElement;
begin
  form:=H.GetObject(TJSHTMLFormElement) as IJSHTMLFormElement;
  TLifecycleFormAssociatedCallback(aMethod)(form);
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleFormResetCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
begin
  TLifecycleFormResetCallback(aMethod)();
  Result:=H.AllocUndefined;
end;

function JOBCallTLifecycleFormDisabledCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  disabled: Boolean;
begin
  disabled:=H.GetBoolean;
  TLifecycleFormDisabledCallback(aMethod)(disabled);
  Result:=H.AllocUndefined;
end;

function JOBCallTPrintCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  ctx: IJSMozCanvasPrintState;
begin
  ctx:=H.GetObject(TJSMozCanvasPrintState) as IJSMozCanvasPrintState;
  TPrintCallback(aMethod)(ctx);
  Result:=H.AllocUndefined;
end;

function JOBCallTBlobCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  blob: IJSBlob;
begin
  blob:=H.GetObject(TJSBlob) as IJSBlob;
  TBlobCallback(aMethod)(blob);
  Result:=H.AllocUndefined;
end;

function JOBCallTFileSystemEntryCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  entry: IJSFileSystemEntry;
begin
  entry:=H.GetObject(TJSFileSystemEntry) as IJSFileSystemEntry;
  TFileSystemEntryCallback(aMethod)(entry);
  Result:=H.AllocUndefined;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSAddEventListenerOptions; aWantsUntrusted: Boolean); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions,aWantsUntrusted]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean; aWantsUntrusted: Boolean); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions,aWantsUntrusted]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSAddEventListenerOptions); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('removeEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.removeEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('removeEventListener',[aType_,m]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: TJSEventListenerOptions); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('removeEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

function TJSEventTarget.dispatchEvent(aEvent: IJSEvent): Boolean;
begin
  Result:=InvokeJSBooleanResult('dispatchEvent',[aEvent]);
end;

procedure TJSEventTarget.setEventHandler(const aType_: UnicodeString; const aHandler: TEventHandler);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aHandler),@JOBCallTEventHandlerNonNull);
  try
    InvokeJSNoResult('setEventHandler',[aType_,m]);
  finally
    m.free;
  end;
end;

class function TJSEventTarget.Cast(Intf: IJSObject): IJSEventTarget;
begin
  Result:=TJSEventTarget.JOBCast(Intf);
end;

function TJSEvent._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSEvent._Gettarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('target',TJSEventTarget) as IJSEventTarget;
end;

function TJSEvent._GetsrcElement: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('srcElement',TJSEventTarget) as IJSEventTarget;
end;

function TJSEvent._GetcurrentTarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('currentTarget',TJSEventTarget) as IJSEventTarget;
end;

function TJSEvent._GeteventPhase: Word;
begin
  Result:=ReadJSPropertyLongInt('eventPhase');
end;

function TJSEvent._GetcancelBubble: Boolean;
begin
  Result:=ReadJSPropertyBoolean('cancelBubble');
end;

function TJSEvent._Getbubbles: Boolean;
begin
  Result:=ReadJSPropertyBoolean('bubbles');
end;

function TJSEvent._Getcancelable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('cancelable');
end;

function TJSEvent._GetreturnValue: Boolean;
begin
  Result:=ReadJSPropertyBoolean('returnValue');
end;

function TJSEvent._GetdefaultPrevented: Boolean;
begin
  Result:=ReadJSPropertyBoolean('defaultPrevented');
end;

function TJSEvent._Getcomposed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('composed');
end;

procedure TJSEvent._SetcancelBubble(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('cancelBubble',aValue);
end;

procedure TJSEvent._SetreturnValue(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('returnValue',aValue);
end;

function TJSEvent.composedPath: TJSEventTargetDynArray;
begin
  Result:=InvokeJSObjectResult('composedPath',[],TJSArray) as TJSEventTargetDynArray;
end;

procedure TJSEvent.stopPropagation;
begin
  InvokeJSNoResult('stopPropagation',[]);
end;

procedure TJSEvent.stopImmediatePropagation;
begin
  InvokeJSNoResult('stopImmediatePropagation',[]);
end;

procedure TJSEvent.preventDefault;
begin
  InvokeJSNoResult('preventDefault',[]);
end;

procedure TJSEvent.initEvent(const aType_: UnicodeString; aBubbles: Boolean; aCancelable: Boolean); overload;
begin
  InvokeJSNoResult('initEvent',[aType_,aBubbles,aCancelable]);
end;

procedure TJSEvent.initEvent(const aType_: UnicodeString); overload;
begin
  InvokeJSNoResult('initEvent',[aType_]);
end;

procedure TJSEvent.initEvent(const aType_: UnicodeString; aBubbles: Boolean); overload;
begin
  InvokeJSNoResult('initEvent',[aType_,aBubbles]);
end;

class function TJSEvent.Cast(Intf: IJSObject): IJSEvent;
begin
  Result:=TJSEvent.JOBCast(Intf);
end;

class function TJSGlobalEventHandlers.Cast(Intf: IJSObject): IJSGlobalEventHandlers;
begin
  Result:=TJSGlobalEventHandlers.JOBCast(Intf);
end;

class function TJSWindowEventHandlers.Cast(Intf: IJSObject): IJSWindowEventHandlers;
begin
  Result:=TJSWindowEventHandlers.JOBCast(Intf);
end;

class function TJSDocumentAndElementEventHandlers.Cast(Intf: IJSObject): IJSDocumentAndElementEventHandlers;
begin
  Result:=TJSDocumentAndElementEventHandlers.JOBCast(Intf);
end;

class function TJSOnErrorEventHandlerForNodes.Cast(Intf: IJSObject): IJSOnErrorEventHandlerForNodes;
begin
  Result:=TJSOnErrorEventHandlerForNodes.JOBCast(Intf);
end;

class function TJSOnErrorEventHandlerForWindow.Cast(Intf: IJSObject): IJSOnErrorEventHandlerForWindow;
begin
  Result:=TJSOnErrorEventHandlerForWindow.JOBCast(Intf);
end;

class function TJSContentSecurityPolicy.Cast(Intf: IJSObject): IJSContentSecurityPolicy;
begin
  Result:=TJSContentSecurityPolicy.JOBCast(Intf);
end;

class function TJSPrincipal.Cast(Intf: IJSObject): IJSPrincipal;
begin
  Result:=TJSPrincipal.JOBCast(Intf);
end;

class function TJSWindowProxy.Cast(Intf: IJSObject): IJSWindowProxy;
begin
  Result:=TJSWindowProxy.JOBCast(Intf);
end;

class function TJSnsISupports.Cast(Intf: IJSObject): IJSnsISupports;
begin
  Result:=TJSnsISupports.JOBCast(Intf);
end;

class function TJSURI.Cast(Intf: IJSObject): IJSURI;
begin
  Result:=TJSURI.JOBCast(Intf);
end;

class function TJSnsIDocShell.Cast(Intf: IJSObject): IJSnsIDocShell;
begin
  Result:=TJSnsIDocShell.JOBCast(Intf);
end;

class function TJSnsILoadGroup.Cast(Intf: IJSObject): IJSnsILoadGroup;
begin
  Result:=TJSnsILoadGroup.JOBCast(Intf);
end;

class function TJSnsIReferrerInfo.Cast(Intf: IJSObject): IJSnsIReferrerInfo;
begin
  Result:=TJSnsIReferrerInfo.JOBCast(Intf);
end;

class function TJSnsICookieJarSettings.Cast(Intf: IJSObject): IJSnsICookieJarSettings;
begin
  Result:=TJSnsICookieJarSettings.JOBCast(Intf);
end;

class function TJSnsIPermissionDelegateHandler.Cast(Intf: IJSObject): IJSnsIPermissionDelegateHandler;
begin
  Result:=TJSnsIPermissionDelegateHandler.JOBCast(Intf);
end;

class function TJSXULCommandDispatcher.Cast(Intf: IJSObject): IJSXULCommandDispatcher;
begin
  Result:=TJSXULCommandDispatcher.JOBCast(Intf);
end;

class function TJSXPathExpression.Cast(Intf: IJSObject): IJSXPathExpression;
begin
  Result:=TJSXPathExpression.JOBCast(Intf);
end;

function TJSParentNode._Getchildren: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('children',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSParentNode._GetfirstElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('firstElementChild',TJSElement) as IJSElement;
end;

function TJSParentNode._GetlastElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('lastElementChild',TJSElement) as IJSElement;
end;

function TJSParentNode._GetchildElementCount: LongWord;
begin
  Result:=ReadJSPropertyInt64('childElementCount');
end;

function TJSParentNode.getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttribute',[aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSParentNode.getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttributeNS',[aNamespaceURI,aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

procedure TJSParentNode.prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSParentNode.prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSParentNode.append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSParentNode.append(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSParentNode.replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

procedure TJSParentNode.replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

class function TJSParentNode.Cast(Intf: IJSObject): IJSParentNode;
begin
  Result:=TJSParentNode.JOBCast(Intf);
end;

class function TJSFontFaceSource.Cast(Intf: IJSObject): IJSFontFaceSource;
begin
  Result:=TJSFontFaceSource.JOBCast(Intf);
end;

function TJSDocumentOrShadowRoot._GetactiveElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('activeElement',TJSElement) as IJSElement;
end;

function TJSDocumentOrShadowRoot._GetpointerLockElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('pointerLockElement',TJSElement) as IJSElement;
end;

function TJSDocumentOrShadowRoot._GetfullscreenElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('fullscreenElement',TJSElement) as IJSElement;
end;

function TJSDocumentOrShadowRoot._GetmozFullScreenElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('mozFullScreenElement',TJSElement) as IJSElement;
end;

function TJSDocumentOrShadowRoot.elementFromPoint(aX: Single; aY: Single): IJSElement;
begin
  Result:=InvokeJSObjectResult('elementFromPoint',[aX,aY],TJSElement) as IJSElement;
end;

function TJSDocumentOrShadowRoot.elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
begin
  Result:=InvokeJSObjectResult('elementsFromPoint',[aX,aY],TJSArray) as TJSElementDynArray;
end;

function TJSDocumentOrShadowRoot.nodeFromPoint(aX: Single; aY: Single): IJSNode;
begin
  Result:=InvokeJSObjectResult('nodeFromPoint',[aX,aY],TJSNode) as IJSNode;
end;

function TJSDocumentOrShadowRoot.nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
begin
  Result:=InvokeJSObjectResult('nodesFromPoint',[aX,aY],TJSArray) as TJSNodeDynArray;
end;

class function TJSDocumentOrShadowRoot.Cast(Intf: IJSObject): IJSDocumentOrShadowRoot;
begin
  Result:=TJSDocumentOrShadowRoot.JOBCast(Intf);
end;

function TJSDOMImplementation.hasFeature: Boolean;
begin
  Result:=InvokeJSBooleanResult('hasFeature',[]);
end;

function TJSDOMImplementation.createDocumentType(const aQualifiedName: UnicodeString; const aPublicId: UnicodeString; const aSystemId: UnicodeString): IJSDocumentType;
begin
  Result:=InvokeJSObjectResult('createDocumentType',[aQualifiedName,aPublicId,aSystemId],TJSDocumentType) as IJSDocumentType;
end;

function TJSDOMImplementation.createDocument(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; aDoctype: IJSDocumentType): IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('createDocument',[aNamespace,aQualifiedName,aDoctype],TJSDocument) as IJSDocument;
end;

function TJSDOMImplementation.createDocument(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString): IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('createDocument',[aNamespace,aQualifiedName],TJSDocument) as IJSDocument;
end;

function TJSDOMImplementation.createHTMLDocument(const aTitle: UnicodeString): IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('createHTMLDocument',[aTitle],TJSDocument) as IJSDocument;
end;

function TJSDOMImplementation.createHTMLDocument: IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('createHTMLDocument',[],TJSDocument) as IJSDocument;
end;

class function TJSDOMImplementation.Cast(Intf: IJSObject): IJSDOMImplementation;
begin
  Result:=TJSDOMImplementation.JOBCast(Intf);
end;

procedure TJSChildNode.before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSChildNode.before(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSChildNode.after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSChildNode.after(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSChildNode.replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSChildNode.replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSChildNode.remove;
begin
  InvokeJSNoResult('remove',[]);
end;

class function TJSChildNode.Cast(Intf: IJSObject): IJSChildNode;
begin
  Result:=TJSChildNode.JOBCast(Intf);
end;

function TJSNonDocumentTypeChildNode._GetpreviousElementSibling: IJSElement;
begin
  Result:=ReadJSPropertyObject('previousElementSibling',TJSElement) as IJSElement;
end;

function TJSNonDocumentTypeChildNode._GetnextElementSibling: IJSElement;
begin
  Result:=ReadJSPropertyObject('nextElementSibling',TJSElement) as IJSElement;
end;

class function TJSNonDocumentTypeChildNode.Cast(Intf: IJSObject): IJSNonDocumentTypeChildNode;
begin
  Result:=TJSNonDocumentTypeChildNode.JOBCast(Intf);
end;

class function TJSnsIScreen.Cast(Intf: IJSObject): IJSnsIScreen;
begin
  Result:=TJSnsIScreen.JOBCast(Intf);
end;

function TJSHTMLOrForeignElement._Getdataset: IJSDOMStringMap;
begin
  Result:=ReadJSPropertyObject('dataset',TJSDOMStringMap) as IJSDOMStringMap;
end;

function TJSHTMLOrForeignElement._GettabIndex: Integer;
begin
  Result:=ReadJSPropertyLongInt('tabIndex');
end;

procedure TJSHTMLOrForeignElement._SettabIndex(const aValue: Integer);
begin
  WriteJSPropertyLongInt('tabIndex',aValue);
end;

procedure TJSHTMLOrForeignElement.focus(const aOptions: TJSFocusOptions); overload;
begin
  InvokeJSNoResult('focus',[aOptions]);
end;

procedure TJSHTMLOrForeignElement.focus; overload;
begin
  InvokeJSNoResult('focus',[]);
end;

procedure TJSHTMLOrForeignElement.blur;
begin
  InvokeJSNoResult('blur',[]);
end;

class function TJSHTMLOrForeignElement.Cast(Intf: IJSObject): IJSHTMLOrForeignElement;
begin
  Result:=TJSHTMLOrForeignElement.JOBCast(Intf);
end;

function TJSElementCSSInlineStyle._Getstyle: IJSCSSStyleDeclaration;
begin
  Result:=ReadJSPropertyObject('style',TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

class function TJSElementCSSInlineStyle.Cast(Intf: IJSObject): IJSElementCSSInlineStyle;
begin
  Result:=TJSElementCSSInlineStyle.JOBCast(Intf);
end;

function TJSHTMLCollection._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSHTMLCollection.item(aIndex: LongWord): IJSElement;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSElement) as IJSElement;
end;

function TJSHTMLCollection.namedItem(const aName: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('namedItem',[aName],TJSElement) as IJSElement;
end;

class function TJSHTMLCollection.Cast(Intf: IJSObject): IJSHTMLCollection;
begin
  Result:=TJSHTMLCollection.JOBCast(Intf);
end;

function TJSLocation._Gethref: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('href');
end;

function TJSLocation._Getorigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('origin');
end;

function TJSLocation._Getprotocol: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('protocol');
end;

function TJSLocation._Gethost: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('host');
end;

function TJSLocation._Gethostname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hostname');
end;

function TJSLocation._Getport: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('port');
end;

function TJSLocation._Getpathname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('pathname');
end;

function TJSLocation._Getsearch: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('search');
end;

function TJSLocation._Gethash: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hash');
end;

procedure TJSLocation._Sethref(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('href',aValue);
end;

procedure TJSLocation._Setprotocol(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('protocol',aValue);
end;

procedure TJSLocation._Sethost(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('host',aValue);
end;

procedure TJSLocation._Sethostname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hostname',aValue);
end;

procedure TJSLocation._Setport(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('port',aValue);
end;

procedure TJSLocation._Setpathname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('pathname',aValue);
end;

procedure TJSLocation._Setsearch(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('search',aValue);
end;

procedure TJSLocation._Sethash(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hash',aValue);
end;

procedure TJSLocation.assign(const aUrl: UnicodeString);
begin
  InvokeJSNoResult('assign',[aUrl]);
end;

procedure TJSLocation.replace(const aUrl: UnicodeString);
begin
  InvokeJSNoResult('replace',[aUrl]);
end;

procedure TJSLocation.reload(aForceget: Boolean); overload;
begin
  InvokeJSNoResult('reload',[aForceget]);
end;

procedure TJSLocation.reload; overload;
begin
  InvokeJSNoResult('reload',[]);
end;

class function TJSLocation.Cast(Intf: IJSObject): IJSLocation;
begin
  Result:=TJSLocation.JOBCast(Intf);
end;

function TJSNodeList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSNodeList.item(aIndex: LongWord): IJSNode;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSNode) as IJSNode;
end;

class function TJSNodeList.Cast(Intf: IJSObject): IJSNodeList;
begin
  Result:=TJSNodeList.JOBCast(Intf);
end;

function TJSDOMStringList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSDOMStringList.item(aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('item',[aIndex]);
end;

function TJSDOMStringList.contains(const aString_: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('contains',[aString_]);
end;

class function TJSDOMStringList.Cast(Intf: IJSObject): IJSDOMStringList;
begin
  Result:=TJSDOMStringList.JOBCast(Intf);
end;

function TJSCaretPosition._GetoffsetNode: IJSNode;
begin
  Result:=ReadJSPropertyObject('offsetNode',TJSNode) as IJSNode;
end;

function TJSCaretPosition._Getoffset: LongWord;
begin
  Result:=ReadJSPropertyInt64('offset');
end;

function TJSCaretPosition.getClientRect: IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('getClientRect',[],TJSDOMRect) as IJSDOMRect;
end;

class function TJSCaretPosition.Cast(Intf: IJSObject): IJSCaretPosition;
begin
  Result:=TJSCaretPosition.JOBCast(Intf);
end;

function TJSDOMTokenList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSDOMTokenList._Getvalue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('value');
end;

procedure TJSDOMTokenList._Setvalue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('value',aValue);
end;

function TJSDOMTokenList.item(aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('item',[aIndex]);
end;

function TJSDOMTokenList.contains(const aToken: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('contains',[aToken]);
end;

procedure TJSDOMTokenList.add(const aTokens: UnicodeString){; ToDo:varargs};
begin
  InvokeJSNoResult('add',[aTokens]);
end;

procedure TJSDOMTokenList.remove(const aTokens: UnicodeString){; ToDo:varargs};
begin
  InvokeJSNoResult('remove',[aTokens]);
end;

function TJSDOMTokenList.replace(const aToken: UnicodeString; const aNewToken: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('replace',[aToken,aNewToken]);
end;

function TJSDOMTokenList.toggle(const aToken: UnicodeString; aForce: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('toggle',[aToken,aForce]);
end;

function TJSDOMTokenList.toggle(const aToken: UnicodeString): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('toggle',[aToken]);
end;

function TJSDOMTokenList.supports(const aToken: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('supports',[aToken]);
end;

class function TJSDOMTokenList.Cast(Intf: IJSObject): IJSDOMTokenList;
begin
  Result:=TJSDOMTokenList.JOBCast(Intf);
end;

function TJSNamedNodeMap._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSNamedNodeMap.getNamedItem(const aName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('getNamedItem',[aName],TJSAttr) as IJSAttr;
end;

function TJSNamedNodeMap.setNamedItem(arg: IJSAttr): IJSAttr;
begin
  Result:=InvokeJSObjectResult('setNamedItem',[arg],TJSAttr) as IJSAttr;
end;

function TJSNamedNodeMap.removeNamedItem(const aName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('removeNamedItem',[aName],TJSAttr) as IJSAttr;
end;

function TJSNamedNodeMap.item(aIndex: LongWord): IJSAttr;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSAttr) as IJSAttr;
end;

function TJSNamedNodeMap.getNamedItemNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('getNamedItemNS',[aNamespaceURI,aLocalName],TJSAttr) as IJSAttr;
end;

function TJSNamedNodeMap.setNamedItemNS(arg: IJSAttr): IJSAttr;
begin
  Result:=InvokeJSObjectResult('setNamedItemNS',[arg],TJSAttr) as IJSAttr;
end;

function TJSNamedNodeMap.removeNamedItemNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('removeNamedItemNS',[aNamespaceURI,aLocalName],TJSAttr) as IJSAttr;
end;

class function TJSNamedNodeMap.Cast(Intf: IJSObject): IJSNamedNodeMap;
begin
  Result:=TJSNamedNodeMap.JOBCast(Intf);
end;

class function TJSDOMStringMap.Cast(Intf: IJSObject): IJSDOMStringMap;
begin
  Result:=TJSDOMStringMap.JOBCast(Intf);
end;

function TJSCSSStyleDeclaration._GetcssText: UTF8String;
begin
  Result:=ReadJSPropertyUTF8String('cssText');
end;

function TJSCSSStyleDeclaration._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSCSSStyleDeclaration._GetparentRule: IJSCSSRule;
begin
  Result:=ReadJSPropertyObject('parentRule',TJSCSSRule) as IJSCSSRule;
end;

procedure TJSCSSStyleDeclaration._SetcssText(const aValue: UTF8String);
begin
  WriteJSPropertyUTF8String('cssText',aValue);
end;

function TJSCSSStyleDeclaration.item(aIndex: LongWord): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('item',[aIndex]);
end;

function TJSCSSStyleDeclaration.getCSSImageURLs(const aProperty_: UTF8String): TUTF8StringDynArray;
begin
  Result:=InvokeJSObjectResult('getCSSImageURLs',[aProperty_],TJSArray) as TUTF8StringDynArray;
end;

function TJSCSSStyleDeclaration.getPropertyValue(const aProperty_: UTF8String): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('getPropertyValue',[aProperty_]);
end;

function TJSCSSStyleDeclaration.getPropertyPriority(const aProperty_: UTF8String): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('getPropertyPriority',[aProperty_]);
end;

procedure TJSCSSStyleDeclaration.setProperty(const aProperty_: UTF8String; const aValue: UTF8String; const aPriority: UTF8String); overload;
begin
  InvokeJSNoResult('setProperty',[aProperty_,aValue,aPriority]);
end;

procedure TJSCSSStyleDeclaration.setProperty(const aProperty_: UTF8String; const aValue: UTF8String); overload;
begin
  InvokeJSNoResult('setProperty',[aProperty_,aValue]);
end;

function TJSCSSStyleDeclaration.removeProperty(const aProperty_: UTF8String): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('removeProperty',[aProperty_]);
end;

class function TJSCSSStyleDeclaration.Cast(Intf: IJSObject): IJSCSSStyleDeclaration;
begin
  Result:=TJSCSSStyleDeclaration.JOBCast(Intf);
end;

class function TJSnsIBrowserDOMWindow.Cast(Intf: IJSObject): IJSnsIBrowserDOMWindow;
begin
  Result:=TJSnsIBrowserDOMWindow.JOBCast(Intf);
end;

class function TJSXULControllers.Cast(Intf: IJSObject): IJSXULControllers;
begin
  Result:=TJSXULControllers.JOBCast(Intf);
end;

class function TJSnsIDOMWindowUtils.Cast(Intf: IJSObject): IJSnsIDOMWindowUtils;
begin
  Result:=TJSnsIDOMWindowUtils.JOBCast(Intf);
end;

class function TJSnsIPrintSettings.Cast(Intf: IJSObject): IJSnsIPrintSettings;
begin
  Result:=TJSnsIPrintSettings.JOBCast(Intf);
end;

function TJSWindowSessionStorage._GetsessionStorage: IJSStorage;
begin
  Result:=ReadJSPropertyObject('sessionStorage',TJSStorage) as IJSStorage;
end;

class function TJSWindowSessionStorage.Cast(Intf: IJSObject): IJSWindowSessionStorage;
begin
  Result:=TJSWindowSessionStorage.JOBCast(Intf);
end;

function TJSWindowLocalStorage._GetlocalStorage: IJSStorage;
begin
  Result:=ReadJSPropertyObject('localStorage',TJSStorage) as IJSStorage;
end;

class function TJSWindowLocalStorage.Cast(Intf: IJSObject): IJSWindowLocalStorage;
begin
  Result:=TJSWindowLocalStorage.JOBCast(Intf);
end;

function TJSTouch._Getidentifier: Integer;
begin
  Result:=ReadJSPropertyLongInt('identifier');
end;

function TJSTouch._Gettarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('target',TJSEventTarget) as IJSEventTarget;
end;

function TJSTouch._GetscreenX: Integer;
begin
  Result:=ReadJSPropertyLongInt('screenX');
end;

function TJSTouch._GetscreenY: Integer;
begin
  Result:=ReadJSPropertyLongInt('screenY');
end;

function TJSTouch._GetclientX: Integer;
begin
  Result:=ReadJSPropertyLongInt('clientX');
end;

function TJSTouch._GetclientY: Integer;
begin
  Result:=ReadJSPropertyLongInt('clientY');
end;

function TJSTouch._GetpageX: Integer;
begin
  Result:=ReadJSPropertyLongInt('pageX');
end;

function TJSTouch._GetpageY: Integer;
begin
  Result:=ReadJSPropertyLongInt('pageY');
end;

function TJSTouch._GetradiusX: Integer;
begin
  Result:=ReadJSPropertyLongInt('radiusX');
end;

function TJSTouch._GetradiusY: Integer;
begin
  Result:=ReadJSPropertyLongInt('radiusY');
end;

function TJSTouch._GetrotationAngle: Single;
begin
  Result:=ReadJSPropertyDouble('rotationAngle');
end;

function TJSTouch._Getforce: Single;
begin
  Result:=ReadJSPropertyDouble('force');
end;

class function TJSTouch.Cast(Intf: IJSObject): IJSTouch;
begin
  Result:=TJSTouch.JOBCast(Intf);
end;

function TJSTouchList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSTouchList.item(aIndex: LongWord): IJSTouch;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSTouch) as IJSTouch;
end;

class function TJSTouchList.Cast(Intf: IJSObject): IJSTouchList;
begin
  Result:=TJSTouchList.JOBCast(Intf);
end;

function TJSHistory._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSHistory._GetscrollRestoration: TScrollRestoration;
begin
  Result:=ReadJSPropertyUnicodeString('scrollRestoration');
end;

function TJSHistory._Getstate: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('state');
end;

procedure TJSHistory._SetscrollRestoration(const aValue: TScrollRestoration);
begin
  WriteJSPropertyUnicodeString('scrollRestoration',aValue);
end;

procedure TJSHistory.go(aDelta: Integer); overload;
begin
  InvokeJSNoResult('go',[aDelta]);
end;

procedure TJSHistory.go; overload;
begin
  InvokeJSNoResult('go',[]);
end;

procedure TJSHistory.back;
begin
  InvokeJSNoResult('back',[]);
end;

procedure TJSHistory.forward;
begin
  InvokeJSNoResult('forward',[]);
end;

procedure TJSHistory.pushState(aData: TJOB_JSValue; const aTitle: UnicodeString; const aUrl: UnicodeString); overload;
begin
  InvokeJSNoResult('pushState',[aData,aTitle,aUrl]);
end;

procedure TJSHistory.pushState(aData: TJOB_JSValue; const aTitle: UnicodeString); overload;
begin
  InvokeJSNoResult('pushState',[aData,aTitle]);
end;

procedure TJSHistory.replaceState(aData: TJOB_JSValue; const aTitle: UnicodeString; const aUrl: UnicodeString); overload;
begin
  InvokeJSNoResult('replaceState',[aData,aTitle,aUrl]);
end;

procedure TJSHistory.replaceState(aData: TJOB_JSValue; const aTitle: UnicodeString); overload;
begin
  InvokeJSNoResult('replaceState',[aData,aTitle]);
end;

class function TJSHistory.Cast(Intf: IJSObject): IJSHistory;
begin
  Result:=TJSHistory.JOBCast(Intf);
end;

procedure TJSCustomElementRegistry.setElementCreationCallback(const aName: UnicodeString; const aCallback: TCustomElementCreationCallback);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aCallback),@JOBCallTCustomElementCreationCallback);
  try
    InvokeJSNoResult('setElementCreationCallback',[aName,m]);
  finally
    m.free;
  end;
end;

function TJSCustomElementRegistry.get(const aName: UnicodeString): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult('get',[aName]);
end;

procedure TJSCustomElementRegistry.upgrade(aRoot: IJSNode);
begin
  InvokeJSNoResult('upgrade',[aRoot]);
end;

class function TJSCustomElementRegistry.Cast(Intf: IJSObject): IJSCustomElementRegistry;
begin
  Result:=TJSCustomElementRegistry.JOBCast(Intf);
end;

function TJSBarProp._Getvisible: Boolean;
begin
  Result:=ReadJSPropertyBoolean('visible');
end;

procedure TJSBarProp._Setvisible(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('visible',aValue);
end;

class function TJSBarProp.Cast(Intf: IJSObject): IJSBarProp;
begin
  Result:=TJSBarProp.JOBCast(Intf);
end;

function TJSNavigator._GetpdfViewerEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('pdfViewerEnabled');
end;

function TJSNavigator._GetdoNotTrack: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('doNotTrack');
end;

function TJSNavigator._GetglobalPrivacyControl: Boolean;
begin
  Result:=ReadJSPropertyBoolean('globalPrivacyControl');
end;

function TJSNavigator._GetmaxTouchPoints: Integer;
begin
  Result:=ReadJSPropertyLongInt('maxTouchPoints');
end;

function TJSNavigator._Getoscpu: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('oscpu');
end;

function TJSNavigator._Getvendor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('vendor');
end;

function TJSNavigator._GetvendorSub: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('vendorSub');
end;

function TJSNavigator._GetproductSub: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('productSub');
end;

function TJSNavigator._GetcookieEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('cookieEnabled');
end;

function TJSNavigator._GetbuildID: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('buildID');
end;

function TJSNavigator._GetisWebVRContentDetected: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isWebVRContentDetected');
end;

function TJSNavigator._GetisWebVRContentPresenting: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isWebVRContentPresenting');
end;

function TJSNavigator._Getclipboard: IJSClipboard;
begin
  Result:=ReadJSPropertyObject('clipboard',TJSClipboard) as IJSClipboard;
end;

function TJSNavigator._GettestTrialGatedAttribute: Boolean;
begin
  Result:=ReadJSPropertyBoolean('testTrialGatedAttribute');
end;

function TJSNavigator._GetappCodeName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('appCodeName');
end;

function TJSNavigator._GetappName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('appName');
end;

function TJSNavigator._GetappVersion: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('appVersion');
end;

function TJSNavigator._Getplatform: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('platform');
end;

function TJSNavigator._GetuserAgent: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('userAgent');
end;

function TJSNavigator._Getproduct: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('product');
end;

function TJSNavigator._Getlanguage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('language');
end;

function TJSNavigator._Getlanguages: TUnicodeStringDynArray;
begin
  Result:=ReadJSPropertyObject('languages',TJSArray) as TUnicodeStringDynArray;
end;

function TJSNavigator._GetonLine: Boolean;
begin
  Result:=ReadJSPropertyBoolean('onLine');
end;

function TJSNavigator._GethardwareConcurrency: QWord;
begin
  Result:=ReadJSPropertyInt64('hardwareConcurrency');
end;

function TJSNavigator._Getwebdriver: Boolean;
begin
  Result:=ReadJSPropertyBoolean('webdriver');
end;

function TJSNavigator.vibrate(aDuration: LongWord): Boolean;
begin
  Result:=InvokeJSBooleanResult('vibrate',[aDuration]);
end;

procedure TJSNavigator.setVibrationPermission(aPermitted: Boolean; aPersistent: Boolean); overload;
begin
  InvokeJSNoResult('setVibrationPermission',[aPermitted,aPersistent]);
end;

procedure TJSNavigator.setVibrationPermission(aPermitted: Boolean); overload;
begin
  InvokeJSNoResult('setVibrationPermission',[aPermitted]);
end;

function TJSNavigator.javaEnabled: Boolean;
begin
  Result:=InvokeJSBooleanResult('javaEnabled',[]);
end;

function TJSNavigator.canShare(const aData: TJSShareData): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('canShare',[aData]);
end;

function TJSNavigator.canShare: Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('canShare',[]);
end;

function TJSNavigator.taintEnabled: Boolean;
begin
  Result:=InvokeJSBooleanResult('taintEnabled',[]);
end;

procedure TJSNavigator.checkProtocolHandlerAllowed(const aScheme: UnicodeString; aHandlerURI: IJSURI; aDocumentURI: IJSURI);
begin
  InvokeJSNoResult('checkProtocolHandlerAllowed',[aScheme,aHandlerURI,aDocumentURI]);
end;

procedure TJSNavigator.registerProtocolHandler(const aScheme: UnicodeString; const aUrl: UnicodeString);
begin
  InvokeJSNoResult('registerProtocolHandler',[aScheme,aUrl]);
end;

class function TJSNavigator.Cast(Intf: IJSObject): IJSNavigator;
begin
  Result:=TJSNavigator.JOBCast(Intf);
end;

function TJSNavigatorID._GetappCodeName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('appCodeName');
end;

function TJSNavigatorID._GetappName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('appName');
end;

function TJSNavigatorID._GetappVersion: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('appVersion');
end;

function TJSNavigatorID._Getplatform: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('platform');
end;

function TJSNavigatorID._GetuserAgent: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('userAgent');
end;

function TJSNavigatorID._Getproduct: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('product');
end;

function TJSNavigatorID.taintEnabled: Boolean;
begin
  Result:=InvokeJSBooleanResult('taintEnabled',[]);
end;

class function TJSNavigatorID.Cast(Intf: IJSObject): IJSNavigatorID;
begin
  Result:=TJSNavigatorID.JOBCast(Intf);
end;

function TJSNavigatorLanguage._Getlanguage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('language');
end;

function TJSNavigatorLanguage._Getlanguages: TUnicodeStringDynArray;
begin
  Result:=ReadJSPropertyObject('languages',TJSArray) as TUnicodeStringDynArray;
end;

class function TJSNavigatorLanguage.Cast(Intf: IJSObject): IJSNavigatorLanguage;
begin
  Result:=TJSNavigatorLanguage.JOBCast(Intf);
end;

function TJSNavigatorOnLine._GetonLine: Boolean;
begin
  Result:=ReadJSPropertyBoolean('onLine');
end;

class function TJSNavigatorOnLine.Cast(Intf: IJSObject): IJSNavigatorOnLine;
begin
  Result:=TJSNavigatorOnLine.JOBCast(Intf);
end;

procedure TJSNavigatorContentUtils.checkProtocolHandlerAllowed(const aScheme: UnicodeString; aHandlerURI: IJSURI; aDocumentURI: IJSURI);
begin
  InvokeJSNoResult('checkProtocolHandlerAllowed',[aScheme,aHandlerURI,aDocumentURI]);
end;

procedure TJSNavigatorContentUtils.registerProtocolHandler(const aScheme: UnicodeString; const aUrl: UnicodeString);
begin
  InvokeJSNoResult('registerProtocolHandler',[aScheme,aUrl]);
end;

class function TJSNavigatorContentUtils.Cast(Intf: IJSObject): IJSNavigatorContentUtils;
begin
  Result:=TJSNavigatorContentUtils.JOBCast(Intf);
end;

class function TJSNavigatorStorage.Cast(Intf: IJSObject): IJSNavigatorStorage;
begin
  Result:=TJSNavigatorStorage.JOBCast(Intf);
end;

class function TJSNavigatorStorageUtils.Cast(Intf: IJSObject): IJSNavigatorStorageUtils;
begin
  Result:=TJSNavigatorStorageUtils.JOBCast(Intf);
end;

class function TJSNavigatorGeolocation.Cast(Intf: IJSObject): IJSNavigatorGeolocation;
begin
  Result:=TJSNavigatorGeolocation.JOBCast(Intf);
end;

function TJSNavigatorConcurrentHardware._GethardwareConcurrency: QWord;
begin
  Result:=ReadJSPropertyInt64('hardwareConcurrency');
end;

class function TJSNavigatorConcurrentHardware.Cast(Intf: IJSObject): IJSNavigatorConcurrentHardware;
begin
  Result:=TJSNavigatorConcurrentHardware.JOBCast(Intf);
end;

function TJSNavigatorAutomationInformation._Getwebdriver: Boolean;
begin
  Result:=ReadJSPropertyBoolean('webdriver');
end;

class function TJSNavigatorAutomationInformation.Cast(Intf: IJSObject): IJSNavigatorAutomationInformation;
begin
  Result:=TJSNavigatorAutomationInformation.JOBCast(Intf);
end;

class function TJSNavigatorLocks.Cast(Intf: IJSObject): IJSNavigatorLocks;
begin
  Result:=TJSNavigatorLocks.JOBCast(Intf);
end;

function TJSStorage._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSStorage._GetisSessionOnly: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isSessionOnly');
end;

function TJSStorage._GethasSnapshot: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasSnapshot');
end;

function TJSStorage._GetsnapshotUsage: Int64;
begin
  Result:=ReadJSPropertyInt64('snapshotUsage');
end;

function TJSStorage.key(aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('key',[aIndex]);
end;

function TJSStorage.getItem(const aKey: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getItem',[aKey]);
end;

procedure TJSStorage.setItem(const aKey: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('setItem',[aKey,aValue]);
end;

procedure TJSStorage.removeItem(const aKey: UnicodeString);
begin
  InvokeJSNoResult('removeItem',[aKey]);
end;

procedure TJSStorage.clear;
begin
  InvokeJSNoResult('clear',[]);
end;

procedure TJSStorage.open;
begin
  InvokeJSNoResult('open',[]);
end;

procedure TJSStorage.close;
begin
  InvokeJSNoResult('close',[]);
end;

procedure TJSStorage.beginExplicitSnapshot;
begin
  InvokeJSNoResult('beginExplicitSnapshot',[]);
end;

procedure TJSStorage.checkpointExplicitSnapshot;
begin
  InvokeJSNoResult('checkpointExplicitSnapshot',[]);
end;

procedure TJSStorage.endExplicitSnapshot;
begin
  InvokeJSNoResult('endExplicitSnapshot',[]);
end;

class function TJSStorage.Cast(Intf: IJSObject): IJSStorage;
begin
  Result:=TJSStorage.JOBCast(Intf);
end;

function TJSSelection._GetanchorNode: IJSNode;
begin
  Result:=ReadJSPropertyObject('anchorNode',TJSNode) as IJSNode;
end;

function TJSSelection._GetanchorOffset: LongWord;
begin
  Result:=ReadJSPropertyInt64('anchorOffset');
end;

function TJSSelection._GetfocusNode: IJSNode;
begin
  Result:=ReadJSPropertyObject('focusNode',TJSNode) as IJSNode;
end;

function TJSSelection._GetfocusOffset: LongWord;
begin
  Result:=ReadJSPropertyInt64('focusOffset');
end;

function TJSSelection._GetisCollapsed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isCollapsed');
end;

function TJSSelection._GetrangeCount: LongWord;
begin
  Result:=ReadJSPropertyInt64('rangeCount');
end;

function TJSSelection._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSSelection._GetinterlinePosition: Boolean;
begin
  Result:=ReadJSPropertyBoolean('interlinePosition');
end;

function TJSSelection._GetcaretBidiLevel: SmallInt;
begin
  Result:=ReadJSPropertyLongInt('caretBidiLevel');
end;

function TJSSelection._GetselectionType: SmallInt;
begin
  Result:=ReadJSPropertyLongInt('selectionType');
end;

procedure TJSSelection._SetinterlinePosition(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('interlinePosition',aValue);
end;

procedure TJSSelection._SetcaretBidiLevel(const aValue: SmallInt);
begin
  WriteJSPropertyLongInt('caretBidiLevel',aValue);
end;

function TJSSelection.getRangeAt(aIndex: LongWord): IJSRange;
begin
  Result:=InvokeJSObjectResult('getRangeAt',[aIndex],TJSRange) as IJSRange;
end;

procedure TJSSelection.addRange(aRange: IJSRange);
begin
  InvokeJSNoResult('addRange',[aRange]);
end;

procedure TJSSelection.removeRange(aRange: IJSRange);
begin
  InvokeJSNoResult('removeRange',[aRange]);
end;

procedure TJSSelection.removeAllRanges;
begin
  InvokeJSNoResult('removeAllRanges',[]);
end;

procedure TJSSelection.empty;
begin
  InvokeJSNoResult('empty',[]);
end;

procedure TJSSelection.collapse(aNode: IJSNode; aOffset: LongWord); overload;
begin
  InvokeJSNoResult('collapse',[aNode,aOffset]);
end;

procedure TJSSelection.collapse(aNode: IJSNode); overload;
begin
  InvokeJSNoResult('collapse',[aNode]);
end;

procedure TJSSelection.setPosition(aNode: IJSNode; aOffset: LongWord); overload;
begin
  InvokeJSNoResult('setPosition',[aNode,aOffset]);
end;

procedure TJSSelection.setPosition(aNode: IJSNode); overload;
begin
  InvokeJSNoResult('setPosition',[aNode]);
end;

procedure TJSSelection.collapseToStart;
begin
  InvokeJSNoResult('collapseToStart',[]);
end;

procedure TJSSelection.collapseToEnd;
begin
  InvokeJSNoResult('collapseToEnd',[]);
end;

procedure TJSSelection.extend(aNode: IJSNode; aOffset: LongWord); overload;
begin
  InvokeJSNoResult('extend',[aNode,aOffset]);
end;

procedure TJSSelection.extend(aNode: IJSNode); overload;
begin
  InvokeJSNoResult('extend',[aNode]);
end;

procedure TJSSelection.setBaseAndExtent(_anchorNode: IJSNode; _anchorOffset: LongWord; aFocusNode: IJSNode; aFocusOffset: LongWord);
begin
  InvokeJSNoResult('setBaseAndExtent',[_anchorNode,_anchorOffset,aFocusNode,aFocusOffset]);
end;

procedure TJSSelection.selectAllChildren(aNode: IJSNode);
begin
  InvokeJSNoResult('selectAllChildren',[aNode]);
end;

procedure TJSSelection.deleteFromDocument;
begin
  InvokeJSNoResult('deleteFromDocument',[]);
end;

function TJSSelection.containsNode(aNode: IJSNode; allowPartialContainment: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('containsNode',[aNode,allowPartialContainment]);
end;

function TJSSelection.containsNode(aNode: IJSNode): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('containsNode',[aNode]);
end;

procedure TJSSelection.modify(const alter: UnicodeString; const aDirection: UnicodeString; const aGranularity: UnicodeString);
begin
  InvokeJSNoResult('modify',[alter,aDirection,aGranularity]);
end;

function TJSSelection.toStringWithFormat(const aFormatType: UnicodeString; aFlags: LongWord; aWrapColumn: Integer): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toStringWithFormat',[aFormatType,aFlags,aWrapColumn]);
end;

procedure TJSSelection.addSelectionListener(aNewListener: IJSnsISelectionListener);
begin
  InvokeJSNoResult('addSelectionListener',[aNewListener]);
end;

procedure TJSSelection.removeSelectionListener(aListenerToRemove: IJSnsISelectionListener);
begin
  InvokeJSNoResult('removeSelectionListener',[aListenerToRemove]);
end;

function TJSSelection.GetRangesForInterval(aBeginNode: IJSNode; aBeginOffset: Integer; aEndNode: IJSNode; aEndOffset: Integer; allowAdjacent: Boolean): TJSRangeDynArray;
begin
  Result:=InvokeJSObjectResult('GetRangesForInterval',[aBeginNode,aBeginOffset,aEndNode,aEndOffset,allowAdjacent],TJSArray) as TJSRangeDynArray;
end;

procedure TJSSelection.scrollIntoView(aRegion: SmallInt; aIsSynchronous: Boolean; aVPercent: SmallInt; aHPercent: SmallInt);
begin
  InvokeJSNoResult('scrollIntoView',[aRegion,aIsSynchronous,aVPercent,aHPercent]);
end;

procedure TJSSelection.setColors(const aForegroundColor: UnicodeString; const aBackgroundColor: UnicodeString; const aAltForegroundColor: UnicodeString; const aAltBackgroundColor: UnicodeString);
begin
  InvokeJSNoResult('setColors',[aForegroundColor,aBackgroundColor,aAltForegroundColor,aAltBackgroundColor]);
end;

procedure TJSSelection.resetColors;
begin
  InvokeJSNoResult('resetColors',[]);
end;

class function TJSSelection.Cast(Intf: IJSObject): IJSSelection;
begin
  Result:=TJSSelection.JOBCast(Intf);
end;

class function TJSnsISelectionListener.Cast(Intf: IJSObject): IJSnsISelectionListener;
begin
  Result:=TJSnsISelectionListener.JOBCast(Intf);
end;

function TJSScreenLuminance._Getmin: Double;
begin
  Result:=ReadJSPropertyDouble('min');
end;

function TJSScreenLuminance._Getmax: Double;
begin
  Result:=ReadJSPropertyDouble('max');
end;

function TJSScreenLuminance._GetmaxAverage: Double;
begin
  Result:=ReadJSPropertyDouble('maxAverage');
end;

class function TJSScreenLuminance.Cast(Intf: IJSObject): IJSScreenLuminance;
begin
  Result:=TJSScreenLuminance.JOBCast(Intf);
end;

function TJSClipboardItem._GetpresentationStyle: TPresentationStyle;
begin
  Result:=ReadJSPropertyUnicodeString('presentationStyle');
end;

function TJSClipboardItem._Gettypes: TUnicodeStringDynArray;
begin
  Result:=ReadJSPropertyObject('types',TJSArray) as TUnicodeStringDynArray;
end;

class function TJSClipboardItem.Cast(Intf: IJSObject): IJSClipboardItem;
begin
  Result:=TJSClipboardItem.JOBCast(Intf);
end;

function TJSAbstractRange._GetstartContainer: IJSNode;
begin
  Result:=ReadJSPropertyObject('startContainer',TJSNode) as IJSNode;
end;

function TJSAbstractRange._GetstartOffset: LongWord;
begin
  Result:=ReadJSPropertyInt64('startOffset');
end;

function TJSAbstractRange._GetendContainer: IJSNode;
begin
  Result:=ReadJSPropertyObject('endContainer',TJSNode) as IJSNode;
end;

function TJSAbstractRange._GetendOffset: LongWord;
begin
  Result:=ReadJSPropertyInt64('endOffset');
end;

function TJSAbstractRange._Getcollapsed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('collapsed');
end;

class function TJSAbstractRange.Cast(Intf: IJSObject): IJSAbstractRange;
begin
  Result:=TJSAbstractRange.JOBCast(Intf);
end;

function TJSDOMRectList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSDOMRectList.item(aIndex: LongWord): IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSDOMRect) as IJSDOMRect;
end;

class function TJSDOMRectList.Cast(Intf: IJSObject): IJSDOMRectList;
begin
  Result:=TJSDOMRectList.JOBCast(Intf);
end;

function TJSDOMRectReadOnly._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMRectReadOnly._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMRectReadOnly._Getwidth: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSDOMRectReadOnly._Getheight: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

function TJSDOMRectReadOnly._Gettop: Double;
begin
  Result:=ReadJSPropertyDouble('top');
end;

function TJSDOMRectReadOnly._Getright: Double;
begin
  Result:=ReadJSPropertyDouble('right');
end;

function TJSDOMRectReadOnly._Getbottom: Double;
begin
  Result:=ReadJSPropertyDouble('bottom');
end;

function TJSDOMRectReadOnly._Getleft: Double;
begin
  Result:=ReadJSPropertyDouble('left');
end;

function TJSDOMRectReadOnly.fromRect(const aOther: TJSDOMRectInit): IJSDOMRectReadOnly; overload;
begin
  Result:=InvokeJSObjectResult('fromRect',[aOther],TJSDOMRectReadOnly) as IJSDOMRectReadOnly;
end;

function TJSDOMRectReadOnly.fromRect: IJSDOMRectReadOnly; overload;
begin
  Result:=InvokeJSObjectResult('fromRect',[],TJSDOMRectReadOnly) as IJSDOMRectReadOnly;
end;

function TJSDOMRectReadOnly.toJSON: IJSObject;
begin
  Result:=InvokeJSObjectResult('toJSON',[],TJSObject) as IJSObject;
end;

class function TJSDOMRectReadOnly.Cast(Intf: IJSObject): IJSDOMRectReadOnly;
begin
  Result:=TJSDOMRectReadOnly.JOBCast(Intf);
end;

class function TJSTouchEventHandlers.Cast(Intf: IJSObject): IJSTouchEventHandlers;
begin
  Result:=TJSTouchEventHandlers.JOBCast(Intf);
end;

function TJSHTMLAllCollection._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSHTMLAllCollection.namedItem(const aName: UnicodeString): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult('namedItem',[aName]);
end;

function TJSHTMLAllCollection.item(const aNameOrIndex: UnicodeString): TJOB_JSValue; overload;
begin
  Result:=InvokeJSValueResult('item',[aNameOrIndex]);
end;

function TJSHTMLAllCollection.item: TJOB_JSValue; overload;
begin
  Result:=InvokeJSValueResult('item',[]);
end;

class function TJSHTMLAllCollection.Cast(Intf: IJSObject): IJSHTMLAllCollection;
begin
  Result:=TJSHTMLAllCollection.JOBCast(Intf);
end;

class function TJSMenuBuilder.Cast(Intf: IJSObject): IJSMenuBuilder;
begin
  Result:=TJSMenuBuilder.JOBCast(Intf);
end;

function TJSElementInternals._GetshadowRoot: IJSShadowRoot;
begin
  Result:=ReadJSPropertyObject('shadowRoot',TJSShadowRoot) as IJSShadowRoot;
end;

function TJSElementInternals._Getform: IJSHTMLFormElement;
begin
  Result:=ReadJSPropertyObject('form',TJSHTMLFormElement) as IJSHTMLFormElement;
end;

function TJSElementInternals._GetwillValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('willValidate');
end;

function TJSElementInternals._Getvalidity: IJSValidityState;
begin
  Result:=ReadJSPropertyObject('validity',TJSValidityState) as IJSValidityState;
end;

function TJSElementInternals._GetvalidationMessage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('validationMessage');
end;

function TJSElementInternals._Getlabels: IJSNodeList;
begin
  Result:=ReadJSPropertyObject('labels',TJSNodeList) as IJSNodeList;
end;

function TJSElementInternals._GetvalidationAnchor: IJSHTMLElement;
begin
  Result:=ReadJSPropertyObject('validationAnchor',TJSHTMLElement) as IJSHTMLElement;
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFile; aState: IJSFile); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFormData; aState: IJSFile); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(const aValue: UnicodeString; aState: IJSFile); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(const aValue: UnicodeString); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFormData); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFile); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue]);
end;

procedure TJSElementInternals.setFormValue(const aValue: UnicodeString; aState: IJSFormData); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFormData; aState: IJSFormData); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFile; aState: IJSFormData); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(const aValue: UnicodeString; const aState: UnicodeString); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFormData; const aState: UnicodeString); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setFormValue(aValue: IJSFile; const aState: UnicodeString); overload;
begin
  InvokeJSNoResult('setFormValue',[aValue,aState]);
end;

procedure TJSElementInternals.setValidity(const aFlags: TJSValidityStateFlags; const aMessage: UnicodeString; anchor: IJSHTMLElement); overload;
begin
  InvokeJSNoResult('setValidity',[aFlags,aMessage,anchor]);
end;

procedure TJSElementInternals.setValidity; overload;
begin
  InvokeJSNoResult('setValidity',[]);
end;

procedure TJSElementInternals.setValidity(const aFlags: TJSValidityStateFlags); overload;
begin
  InvokeJSNoResult('setValidity',[aFlags]);
end;

procedure TJSElementInternals.setValidity(const aFlags: TJSValidityStateFlags; const aMessage: UnicodeString); overload;
begin
  InvokeJSNoResult('setValidity',[aFlags,aMessage]);
end;

function TJSElementInternals.checkValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('checkValidity',[]);
end;

function TJSElementInternals.reportValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('reportValidity',[]);
end;

class function TJSElementInternals.Cast(Intf: IJSObject): IJSElementInternals;
begin
  Result:=TJSElementInternals.JOBCast(Intf);
end;

function TJSValidityState._GetvalueMissing: Boolean;
begin
  Result:=ReadJSPropertyBoolean('valueMissing');
end;

function TJSValidityState._GettypeMismatch: Boolean;
begin
  Result:=ReadJSPropertyBoolean('typeMismatch');
end;

function TJSValidityState._GetpatternMismatch: Boolean;
begin
  Result:=ReadJSPropertyBoolean('patternMismatch');
end;

function TJSValidityState._GettooLong: Boolean;
begin
  Result:=ReadJSPropertyBoolean('tooLong');
end;

function TJSValidityState._GettooShort: Boolean;
begin
  Result:=ReadJSPropertyBoolean('tooShort');
end;

function TJSValidityState._GetrangeUnderflow: Boolean;
begin
  Result:=ReadJSPropertyBoolean('rangeUnderflow');
end;

function TJSValidityState._GetrangeOverflow: Boolean;
begin
  Result:=ReadJSPropertyBoolean('rangeOverflow');
end;

function TJSValidityState._GetstepMismatch: Boolean;
begin
  Result:=ReadJSPropertyBoolean('stepMismatch');
end;

function TJSValidityState._GetbadInput: Boolean;
begin
  Result:=ReadJSPropertyBoolean('badInput');
end;

function TJSValidityState._GetcustomError: Boolean;
begin
  Result:=ReadJSPropertyBoolean('customError');
end;

function TJSValidityState._Getvalid: Boolean;
begin
  Result:=ReadJSPropertyBoolean('valid');
end;

class function TJSValidityState.Cast(Intf: IJSObject): IJSValidityState;
begin
  Result:=TJSValidityState.JOBCast(Intf);
end;

class function TJSSVGViewSpec.Cast(Intf: IJSObject): IJSSVGViewSpec;
begin
  Result:=TJSSVGViewSpec.JOBCast(Intf);
end;

function TJSSVGFitToViewBox._GetviewBox: IJSSVGAnimatedRect;
begin
  Result:=ReadJSPropertyObject('viewBox',TJSSVGAnimatedRect) as IJSSVGAnimatedRect;
end;

function TJSSVGFitToViewBox._GetpreserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio;
begin
  Result:=ReadJSPropertyObject('preserveAspectRatio',TJSSVGAnimatedPreserveAspectRatio) as IJSSVGAnimatedPreserveAspectRatio;
end;

class function TJSSVGFitToViewBox.Cast(Intf: IJSObject): IJSSVGFitToViewBox;
begin
  Result:=TJSSVGFitToViewBox.JOBCast(Intf);
end;

function TJSSVGZoomAndPan._GetzoomAndPan: Word;
begin
  Result:=ReadJSPropertyLongInt('zoomAndPan');
end;

procedure TJSSVGZoomAndPan._SetzoomAndPan(const aValue: Word);
begin
  WriteJSPropertyLongInt('zoomAndPan',aValue);
end;

class function TJSSVGZoomAndPan.Cast(Intf: IJSObject): IJSSVGZoomAndPan;
begin
  Result:=TJSSVGZoomAndPan.JOBCast(Intf);
end;

function TJSCSSRule._Gettype_: Word;
begin
  Result:=ReadJSPropertyLongInt('type');
end;

function TJSCSSRule._GetcssText: UTF8String;
begin
  Result:=ReadJSPropertyUTF8String('cssText');
end;

function TJSCSSRule._GetparentRule: IJSCSSRule;
begin
  Result:=ReadJSPropertyObject('parentRule',TJSCSSRule) as IJSCSSRule;
end;

function TJSCSSRule._GetparentStyleSheet: IJSCSSStyleSheet;
begin
  Result:=ReadJSPropertyObject('parentStyleSheet',TJSCSSStyleSheet) as IJSCSSStyleSheet;
end;

procedure TJSCSSRule._SetcssText(const aValue: UTF8String);
begin
  WriteJSPropertyUTF8String('cssText',aValue);
end;

class function TJSCSSRule.Cast(Intf: IJSObject): IJSCSSRule;
begin
  Result:=TJSCSSRule.JOBCast(Intf);
end;

function TJSSVGAnimatedLength._GetbaseVal: IJSSVGLength;
begin
  Result:=ReadJSPropertyObject('baseVal',TJSSVGLength) as IJSSVGLength;
end;

function TJSSVGAnimatedLength._GetanimVal: IJSSVGLength;
begin
  Result:=ReadJSPropertyObject('animVal',TJSSVGLength) as IJSSVGLength;
end;

class function TJSSVGAnimatedLength.Cast(Intf: IJSObject): IJSSVGAnimatedLength;
begin
  Result:=TJSSVGAnimatedLength.JOBCast(Intf);
end;

function TJSSVGPoint._Getx: Single;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSSVGPoint._Gety: Single;
begin
  Result:=ReadJSPropertyDouble('y');
end;

procedure TJSSVGPoint._Setx(const aValue: Single);
begin
  WriteJSPropertyDouble('x',aValue);
end;

procedure TJSSVGPoint._Sety(const aValue: Single);
begin
  WriteJSPropertyDouble('y',aValue);
end;

function TJSSVGPoint.matrixTransform(const aMatrix: TJSDOMMatrix2DInit): IJSSVGPoint; overload;
begin
  Result:=InvokeJSObjectResult('matrixTransform',[aMatrix],TJSSVGPoint) as IJSSVGPoint;
end;

function TJSSVGPoint.matrixTransform: IJSSVGPoint; overload;
begin
  Result:=InvokeJSObjectResult('matrixTransform',[],TJSSVGPoint) as IJSSVGPoint;
end;

class function TJSSVGPoint.Cast(Intf: IJSObject): IJSSVGPoint;
begin
  Result:=TJSSVGPoint.JOBCast(Intf);
end;

function TJSSVGNumber._Getvalue: Single;
begin
  Result:=ReadJSPropertyDouble('value');
end;

procedure TJSSVGNumber._Setvalue(const aValue: Single);
begin
  WriteJSPropertyDouble('value',aValue);
end;

class function TJSSVGNumber.Cast(Intf: IJSObject): IJSSVGNumber;
begin
  Result:=TJSSVGNumber.JOBCast(Intf);
end;

function TJSSVGLength._GetunitType: Word;
begin
  Result:=ReadJSPropertyLongInt('unitType');
end;

function TJSSVGLength._Getvalue: Single;
begin
  Result:=ReadJSPropertyDouble('value');
end;

function TJSSVGLength._GetvalueInSpecifiedUnits: Single;
begin
  Result:=ReadJSPropertyDouble('valueInSpecifiedUnits');
end;

function TJSSVGLength._GetvalueAsString: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('valueAsString');
end;

procedure TJSSVGLength._Setvalue(const aValue: Single);
begin
  WriteJSPropertyDouble('value',aValue);
end;

procedure TJSSVGLength._SetvalueInSpecifiedUnits(const aValue: Single);
begin
  WriteJSPropertyDouble('valueInSpecifiedUnits',aValue);
end;

procedure TJSSVGLength._SetvalueAsString(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('valueAsString',aValue);
end;

procedure TJSSVGLength.newValueSpecifiedUnits(aUnitType: Word; aValueInSpecifiedUnits: Single);
begin
  InvokeJSNoResult('newValueSpecifiedUnits',[aUnitType,aValueInSpecifiedUnits]);
end;

procedure TJSSVGLength.convertToSpecifiedUnits(aUnitType: Word);
begin
  InvokeJSNoResult('convertToSpecifiedUnits',[aUnitType]);
end;

class function TJSSVGLength.Cast(Intf: IJSObject): IJSSVGLength;
begin
  Result:=TJSSVGLength.JOBCast(Intf);
end;

function TJSSVGAngle._GetunitType: Word;
begin
  Result:=ReadJSPropertyLongInt('unitType');
end;

function TJSSVGAngle._Getvalue: Single;
begin
  Result:=ReadJSPropertyDouble('value');
end;

function TJSSVGAngle._GetvalueInSpecifiedUnits: Single;
begin
  Result:=ReadJSPropertyDouble('valueInSpecifiedUnits');
end;

function TJSSVGAngle._GetvalueAsString: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('valueAsString');
end;

procedure TJSSVGAngle._Setvalue(const aValue: Single);
begin
  WriteJSPropertyDouble('value',aValue);
end;

procedure TJSSVGAngle._SetvalueInSpecifiedUnits(const aValue: Single);
begin
  WriteJSPropertyDouble('valueInSpecifiedUnits',aValue);
end;

procedure TJSSVGAngle._SetvalueAsString(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('valueAsString',aValue);
end;

procedure TJSSVGAngle.newValueSpecifiedUnits(aUnitType: Word; aValueInSpecifiedUnits: Single);
begin
  InvokeJSNoResult('newValueSpecifiedUnits',[aUnitType,aValueInSpecifiedUnits]);
end;

procedure TJSSVGAngle.convertToSpecifiedUnits(aUnitType: Word);
begin
  InvokeJSNoResult('convertToSpecifiedUnits',[aUnitType]);
end;

class function TJSSVGAngle.Cast(Intf: IJSObject): IJSSVGAngle;
begin
  Result:=TJSSVGAngle.JOBCast(Intf);
end;

function TJSSVGMatrix._Geta: Single;
begin
  Result:=ReadJSPropertyDouble('a');
end;

function TJSSVGMatrix._Getb: Single;
begin
  Result:=ReadJSPropertyDouble('b');
end;

function TJSSVGMatrix._Getc: Single;
begin
  Result:=ReadJSPropertyDouble('c');
end;

function TJSSVGMatrix._Getd: Single;
begin
  Result:=ReadJSPropertyDouble('d');
end;

function TJSSVGMatrix._Gete: Single;
begin
  Result:=ReadJSPropertyDouble('e');
end;

function TJSSVGMatrix._Getf: Single;
begin
  Result:=ReadJSPropertyDouble('f');
end;

procedure TJSSVGMatrix._Seta(const aValue: Single);
begin
  WriteJSPropertyDouble('a',aValue);
end;

procedure TJSSVGMatrix._Setb(const aValue: Single);
begin
  WriteJSPropertyDouble('b',aValue);
end;

procedure TJSSVGMatrix._Setc(const aValue: Single);
begin
  WriteJSPropertyDouble('c',aValue);
end;

procedure TJSSVGMatrix._Setd(const aValue: Single);
begin
  WriteJSPropertyDouble('d',aValue);
end;

procedure TJSSVGMatrix._Sete(const aValue: Single);
begin
  WriteJSPropertyDouble('e',aValue);
end;

procedure TJSSVGMatrix._Setf(const aValue: Single);
begin
  WriteJSPropertyDouble('f',aValue);
end;

function TJSSVGMatrix.multiply(aSecondMatrix: IJSSVGMatrix): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('multiply',[aSecondMatrix],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.inverse: IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('inverse',[],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.translate(aX: Single; aY: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('translate',[aX,aY],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.scale(aScaleFactor: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleFactor],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.scaleNonUniform(aScaleFactorX: Single; aScaleFactorY: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('scaleNonUniform',[aScaleFactorX,aScaleFactorY],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.rotate(angle: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('rotate',[angle],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.rotateFromVector(aX: Single; aY: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('rotateFromVector',[aX,aY],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.flipX: IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('flipX',[],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.flipY: IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('flipY',[],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.skewX(angle: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('skewX',[angle],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGMatrix.skewY(angle: Single): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('skewY',[angle],TJSSVGMatrix) as IJSSVGMatrix;
end;

class function TJSSVGMatrix.Cast(Intf: IJSObject): IJSSVGMatrix;
begin
  Result:=TJSSVGMatrix.JOBCast(Intf);
end;

function TJSSVGRect._Getx: Single;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSSVGRect._Gety: Single;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSSVGRect._Getwidth: Single;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSSVGRect._Getheight: Single;
begin
  Result:=ReadJSPropertyDouble('height');
end;

procedure TJSSVGRect._Setx(const aValue: Single);
begin
  WriteJSPropertyDouble('x',aValue);
end;

procedure TJSSVGRect._Sety(const aValue: Single);
begin
  WriteJSPropertyDouble('y',aValue);
end;

procedure TJSSVGRect._Setwidth(const aValue: Single);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSSVGRect._Setheight(const aValue: Single);
begin
  WriteJSPropertyDouble('height',aValue);
end;

class function TJSSVGRect.Cast(Intf: IJSObject): IJSSVGRect;
begin
  Result:=TJSSVGRect.JOBCast(Intf);
end;

function TJSSVGTransform._Gettype_: Word;
begin
  Result:=ReadJSPropertyLongInt('type');
end;

function TJSSVGTransform._Getmatrix: IJSSVGMatrix;
begin
  Result:=ReadJSPropertyObject('matrix',TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGTransform._Getangle: Single;
begin
  Result:=ReadJSPropertyDouble('angle');
end;

procedure TJSSVGTransform.setMatrix(const aMatrix: TJSDOMMatrix2DInit); overload;
begin
  InvokeJSNoResult('setMatrix',[aMatrix]);
end;

procedure TJSSVGTransform.setMatrix; overload;
begin
  InvokeJSNoResult('setMatrix',[]);
end;

procedure TJSSVGTransform.setTranslate(aTx: Single; aTy: Single);
begin
  InvokeJSNoResult('setTranslate',[aTx,aTy]);
end;

procedure TJSSVGTransform.setScale(aSx: Single; aSy: Single);
begin
  InvokeJSNoResult('setScale',[aSx,aSy]);
end;

procedure TJSSVGTransform.setRotate(_angle: Single; aCx: Single; aCy: Single);
begin
  InvokeJSNoResult('setRotate',[_angle,aCx,aCy]);
end;

procedure TJSSVGTransform.setSkewX(_angle: Single);
begin
  InvokeJSNoResult('setSkewX',[_angle]);
end;

procedure TJSSVGTransform.setSkewY(_angle: Single);
begin
  InvokeJSNoResult('setSkewY',[_angle]);
end;

class function TJSSVGTransform.Cast(Intf: IJSObject): IJSSVGTransform;
begin
  Result:=TJSSVGTransform.JOBCast(Intf);
end;

function TJSDOMMatrixReadOnly._Geta: Double;
begin
  Result:=ReadJSPropertyDouble('a');
end;

function TJSDOMMatrixReadOnly._Getb: Double;
begin
  Result:=ReadJSPropertyDouble('b');
end;

function TJSDOMMatrixReadOnly._Getc: Double;
begin
  Result:=ReadJSPropertyDouble('c');
end;

function TJSDOMMatrixReadOnly._Getd: Double;
begin
  Result:=ReadJSPropertyDouble('d');
end;

function TJSDOMMatrixReadOnly._Gete: Double;
begin
  Result:=ReadJSPropertyDouble('e');
end;

function TJSDOMMatrixReadOnly._Getf: Double;
begin
  Result:=ReadJSPropertyDouble('f');
end;

function TJSDOMMatrixReadOnly._Getm11: Double;
begin
  Result:=ReadJSPropertyDouble('m11');
end;

function TJSDOMMatrixReadOnly._Getm12: Double;
begin
  Result:=ReadJSPropertyDouble('m12');
end;

function TJSDOMMatrixReadOnly._Getm13: Double;
begin
  Result:=ReadJSPropertyDouble('m13');
end;

function TJSDOMMatrixReadOnly._Getm14: Double;
begin
  Result:=ReadJSPropertyDouble('m14');
end;

function TJSDOMMatrixReadOnly._Getm21: Double;
begin
  Result:=ReadJSPropertyDouble('m21');
end;

function TJSDOMMatrixReadOnly._Getm22: Double;
begin
  Result:=ReadJSPropertyDouble('m22');
end;

function TJSDOMMatrixReadOnly._Getm23: Double;
begin
  Result:=ReadJSPropertyDouble('m23');
end;

function TJSDOMMatrixReadOnly._Getm24: Double;
begin
  Result:=ReadJSPropertyDouble('m24');
end;

function TJSDOMMatrixReadOnly._Getm31: Double;
begin
  Result:=ReadJSPropertyDouble('m31');
end;

function TJSDOMMatrixReadOnly._Getm32: Double;
begin
  Result:=ReadJSPropertyDouble('m32');
end;

function TJSDOMMatrixReadOnly._Getm33: Double;
begin
  Result:=ReadJSPropertyDouble('m33');
end;

function TJSDOMMatrixReadOnly._Getm34: Double;
begin
  Result:=ReadJSPropertyDouble('m34');
end;

function TJSDOMMatrixReadOnly._Getm41: Double;
begin
  Result:=ReadJSPropertyDouble('m41');
end;

function TJSDOMMatrixReadOnly._Getm42: Double;
begin
  Result:=ReadJSPropertyDouble('m42');
end;

function TJSDOMMatrixReadOnly._Getm43: Double;
begin
  Result:=ReadJSPropertyDouble('m43');
end;

function TJSDOMMatrixReadOnly._Getm44: Double;
begin
  Result:=ReadJSPropertyDouble('m44');
end;

function TJSDOMMatrixReadOnly._Getis2D: Boolean;
begin
  Result:=ReadJSPropertyBoolean('is2D');
end;

function TJSDOMMatrixReadOnly._GetisIdentity: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isIdentity');
end;

function TJSDOMMatrixReadOnly.fromMatrix(const aOther: TJSDOMMatrixInit): IJSDOMMatrixReadOnly; overload;
begin
  Result:=InvokeJSObjectResult('fromMatrix',[aOther],TJSDOMMatrixReadOnly) as IJSDOMMatrixReadOnly;
end;

function TJSDOMMatrixReadOnly.fromMatrix: IJSDOMMatrixReadOnly; overload;
begin
  Result:=InvokeJSObjectResult('fromMatrix',[],TJSDOMMatrixReadOnly) as IJSDOMMatrixReadOnly;
end;

function TJSDOMMatrixReadOnly.translate(aTx: Double; aTy: Double; aTz: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translate',[aTx,aTy,aTz],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.translate: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translate',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.translate(aTx: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translate',[aTx],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.translate(aTx: Double; aTy: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translate',[aTx,aTy],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleX,aScaleY,aScaleZ,aOriginX,aOriginY,aOriginZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale(aScaleX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleX,aScaleY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleX,aScaleY,aScaleZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleX,aScaleY,aScaleZ,aOriginX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale',[aScaleX,aScaleY,aScaleZ,aOriginX,aOriginY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scaleNonUniform(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleNonUniform',[aScaleX,aScaleY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scaleNonUniform: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleNonUniform',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scaleNonUniform(aScaleX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleNonUniform',[aScaleX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale3d(aScale: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3d',[aScale,aOriginX,aOriginY,aOriginZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale3d: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3d',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale3d(aScale: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3d',[aScale],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale3d(aScale: Double; aOriginX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3d',[aScale,aOriginX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.scale3d(aScale: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3d',[aScale,aOriginX,aOriginY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotate(aRotX: Double; aRotY: Double; aRotZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotate',[aRotX,aRotY,aRotZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotate: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotate',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotate(aRotX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotate',[aRotX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotate(aRotX: Double; aRotY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotate',[aRotX,aRotY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateFromVector(aX: Double; aY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateFromVector',[aX,aY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateFromVector: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateFromVector',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateFromVector(aX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateFromVector',[aX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateAxisAngle(aX: Double; aY: Double; aZ: Double; angle: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngle',[aX,aY,aZ,angle],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateAxisAngle: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngle',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateAxisAngle(aX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngle',[aX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateAxisAngle(aX: Double; aY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngle',[aX,aY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.rotateAxisAngle(aX: Double; aY: Double; aZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngle',[aX,aY,aZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.skewX(aSx: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewX',[aSx],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.skewX: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewX',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.skewY(aSy: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewY',[aSy],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.skewY: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewY',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.multiply(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('multiply',[aOther],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.multiply: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('multiply',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.flipX: IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('flipX',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.flipY: IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('flipY',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.inverse: IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('inverse',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrixReadOnly.transformPoint(const aPoint: TJSDOMPointInit): IJSDOMPoint; overload;
begin
  Result:=InvokeJSObjectResult('transformPoint',[aPoint],TJSDOMPoint) as IJSDOMPoint;
end;

function TJSDOMMatrixReadOnly.transformPoint: IJSDOMPoint; overload;
begin
  Result:=InvokeJSObjectResult('transformPoint',[],TJSDOMPoint) as IJSDOMPoint;
end;

function TJSDOMMatrixReadOnly.toJSON: IJSObject;
begin
  Result:=InvokeJSObjectResult('toJSON',[],TJSObject) as IJSObject;
end;

class function TJSDOMMatrixReadOnly.Cast(Intf: IJSObject): IJSDOMMatrixReadOnly;
begin
  Result:=TJSDOMMatrixReadOnly.JOBCast(Intf);
end;

function TJSSVGAnimatedRect._GetbaseVal: IJSSVGRect;
begin
  Result:=ReadJSPropertyObject('baseVal',TJSSVGRect) as IJSSVGRect;
end;

function TJSSVGAnimatedRect._GetanimVal: IJSSVGRect;
begin
  Result:=ReadJSPropertyObject('animVal',TJSSVGRect) as IJSSVGRect;
end;

class function TJSSVGAnimatedRect.Cast(Intf: IJSObject): IJSSVGAnimatedRect;
begin
  Result:=TJSSVGAnimatedRect.JOBCast(Intf);
end;

function TJSSVGAnimatedPreserveAspectRatio._GetbaseVal: IJSSVGPreserveAspectRatio;
begin
  Result:=ReadJSPropertyObject('baseVal',TJSSVGPreserveAspectRatio) as IJSSVGPreserveAspectRatio;
end;

function TJSSVGAnimatedPreserveAspectRatio._GetanimVal: IJSSVGPreserveAspectRatio;
begin
  Result:=ReadJSPropertyObject('animVal',TJSSVGPreserveAspectRatio) as IJSSVGPreserveAspectRatio;
end;

class function TJSSVGAnimatedPreserveAspectRatio.Cast(Intf: IJSObject): IJSSVGAnimatedPreserveAspectRatio;
begin
  Result:=TJSSVGAnimatedPreserveAspectRatio.JOBCast(Intf);
end;

function TJSDOMPointReadOnly._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMPointReadOnly._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMPointReadOnly._Getz: Double;
begin
  Result:=ReadJSPropertyDouble('z');
end;

function TJSDOMPointReadOnly._Getw: Double;
begin
  Result:=ReadJSPropertyDouble('w');
end;

function TJSDOMPointReadOnly.fromPoint(const aOther: TJSDOMPointInit): IJSDOMPointReadOnly; overload;
begin
  Result:=InvokeJSObjectResult('fromPoint',[aOther],TJSDOMPointReadOnly) as IJSDOMPointReadOnly;
end;

function TJSDOMPointReadOnly.fromPoint: IJSDOMPointReadOnly; overload;
begin
  Result:=InvokeJSObjectResult('fromPoint',[],TJSDOMPointReadOnly) as IJSDOMPointReadOnly;
end;

function TJSDOMPointReadOnly.matrixTransform(const aMatrix: TJSDOMMatrixInit): IJSDOMPoint; overload;
begin
  Result:=InvokeJSObjectResult('matrixTransform',[aMatrix],TJSDOMPoint) as IJSDOMPoint;
end;

function TJSDOMPointReadOnly.matrixTransform: IJSDOMPoint; overload;
begin
  Result:=InvokeJSObjectResult('matrixTransform',[],TJSDOMPoint) as IJSDOMPoint;
end;

function TJSDOMPointReadOnly.toJSON: IJSObject;
begin
  Result:=InvokeJSObjectResult('toJSON',[],TJSObject) as IJSObject;
end;

class function TJSDOMPointReadOnly.Cast(Intf: IJSObject): IJSDOMPointReadOnly;
begin
  Result:=TJSDOMPointReadOnly.JOBCast(Intf);
end;

function TJSSVGPreserveAspectRatio._Getalign: Word;
begin
  Result:=ReadJSPropertyLongInt('align');
end;

function TJSSVGPreserveAspectRatio._GetmeetOrSlice: Word;
begin
  Result:=ReadJSPropertyLongInt('meetOrSlice');
end;

procedure TJSSVGPreserveAspectRatio._Setalign(const aValue: Word);
begin
  WriteJSPropertyLongInt('align',aValue);
end;

procedure TJSSVGPreserveAspectRatio._SetmeetOrSlice(const aValue: Word);
begin
  WriteJSPropertyLongInt('meetOrSlice',aValue);
end;

class function TJSSVGPreserveAspectRatio.Cast(Intf: IJSObject): IJSSVGPreserveAspectRatio;
begin
  Result:=TJSSVGPreserveAspectRatio.JOBCast(Intf);
end;

function TJSCSSRuleList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSCSSRuleList.item(aIndex: LongWord): IJSCSSRule;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSCSSRule) as IJSCSSRule;
end;

class function TJSCSSRuleList.Cast(Intf: IJSObject): IJSCSSRuleList;
begin
  Result:=TJSCSSRuleList.JOBCast(Intf);
end;

function TJSSVGTests._GetrequiredExtensions: IJSSVGStringList;
begin
  Result:=ReadJSPropertyObject('requiredExtensions',TJSSVGStringList) as IJSSVGStringList;
end;

function TJSSVGTests._GetsystemLanguage: IJSSVGStringList;
begin
  Result:=ReadJSPropertyObject('systemLanguage',TJSSVGStringList) as IJSSVGStringList;
end;

class function TJSSVGTests.Cast(Intf: IJSObject): IJSSVGTests;
begin
  Result:=TJSSVGTests.JOBCast(Intf);
end;

function TJSSVGAnimatedTransformList._GetbaseVal: IJSSVGTransformList;
begin
  Result:=ReadJSPropertyObject('baseVal',TJSSVGTransformList) as IJSSVGTransformList;
end;

function TJSSVGAnimatedTransformList._GetanimVal: IJSSVGTransformList;
begin
  Result:=ReadJSPropertyObject('animVal',TJSSVGTransformList) as IJSSVGTransformList;
end;

class function TJSSVGAnimatedTransformList.Cast(Intf: IJSObject): IJSSVGAnimatedTransformList;
begin
  Result:=TJSSVGAnimatedTransformList.JOBCast(Intf);
end;

function TJSSVGStringList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSSVGStringList._GetnumberOfItems: LongWord;
begin
  Result:=ReadJSPropertyInt64('numberOfItems');
end;

procedure TJSSVGStringList.clear;
begin
  InvokeJSNoResult('clear',[]);
end;

function TJSSVGStringList.initialize(const aNewItem: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('initialize',[aNewItem]);
end;

function TJSSVGStringList.getItem(aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getItem',[aIndex]);
end;

function TJSSVGStringList.insertItemBefore(const aNewItem: UnicodeString; aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('insertItemBefore',[aNewItem,aIndex]);
end;

function TJSSVGStringList.replaceItem(const aNewItem: UnicodeString; aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('replaceItem',[aNewItem,aIndex]);
end;

function TJSSVGStringList.removeItem(aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('removeItem',[aIndex]);
end;

function TJSSVGStringList.appendItem(const aNewItem: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('appendItem',[aNewItem]);
end;

class function TJSSVGStringList.Cast(Intf: IJSObject): IJSSVGStringList;
begin
  Result:=TJSSVGStringList.JOBCast(Intf);
end;

function TJSSVGTransformList._GetnumberOfItems: LongWord;
begin
  Result:=ReadJSPropertyInt64('numberOfItems');
end;

function TJSSVGTransformList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

procedure TJSSVGTransformList.clear;
begin
  InvokeJSNoResult('clear',[]);
end;

function TJSSVGTransformList.initialize(aNewItem: IJSSVGTransform): IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('initialize',[aNewItem],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.getItem(aIndex: LongWord): IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('getItem',[aIndex],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.insertItemBefore(aNewItem: IJSSVGTransform; aIndex: LongWord): IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('insertItemBefore',[aNewItem,aIndex],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.replaceItem(aNewItem: IJSSVGTransform; aIndex: LongWord): IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('replaceItem',[aNewItem,aIndex],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.removeItem(aIndex: LongWord): IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('removeItem',[aIndex],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.appendItem(aNewItem: IJSSVGTransform): IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('appendItem',[aNewItem],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.createSVGTransformFromMatrix(const aMatrix: TJSDOMMatrix2DInit): IJSSVGTransform; overload;
begin
  Result:=InvokeJSObjectResult('createSVGTransformFromMatrix',[aMatrix],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.createSVGTransformFromMatrix: IJSSVGTransform; overload;
begin
  Result:=InvokeJSObjectResult('createSVGTransformFromMatrix',[],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGTransformList.consolidate: IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('consolidate',[],TJSSVGTransform) as IJSSVGTransform;
end;

class function TJSSVGTransformList.Cast(Intf: IJSObject): IJSSVGTransformList;
begin
  Result:=TJSSVGTransformList.JOBCast(Intf);
end;

function TJSSVGAnimatedString._GetbaseVal: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('baseVal');
end;

function TJSSVGAnimatedString._GetanimVal: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('animVal');
end;

procedure TJSSVGAnimatedString._SetbaseVal(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('baseVal',aValue);
end;

class function TJSSVGAnimatedString.Cast(Intf: IJSObject): IJSSVGAnimatedString;
begin
  Result:=TJSSVGAnimatedString.JOBCast(Intf);
end;

function TJSStyleSheet._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSStyleSheet._Gethref: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('href');
end;

function TJSStyleSheet._GetownerNode: IJSNode;
begin
  Result:=ReadJSPropertyObject('ownerNode',TJSNode) as IJSNode;
end;

function TJSStyleSheet._GetparentStyleSheet: IJSStyleSheet;
begin
  Result:=ReadJSPropertyObject('parentStyleSheet',TJSStyleSheet) as IJSStyleSheet;
end;

function TJSStyleSheet._Gettitle: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('title');
end;

function TJSStyleSheet._Getdisabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('disabled');
end;

function TJSStyleSheet._GetsourceMapURL: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('sourceMapURL');
end;

function TJSStyleSheet._GetsourceURL: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('sourceURL');
end;

function TJSStyleSheet._GetassociatedDocument: IJSDocument;
begin
  Result:=ReadJSPropertyObject('associatedDocument',TJSDocument) as IJSDocument;
end;

function TJSStyleSheet._Getconstructed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('constructed');
end;

procedure TJSStyleSheet._Setdisabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('disabled',aValue);
end;

class function TJSStyleSheet.Cast(Intf: IJSObject): IJSStyleSheet;
begin
  Result:=TJSStyleSheet.JOBCast(Intf);
end;

function TJSMozCanvasPrintState._Getcontext: IJSnsISupports;
begin
  Result:=ReadJSPropertyObject('context',TJSnsISupports) as IJSnsISupports;
end;

procedure TJSMozCanvasPrintState.done;
begin
  InvokeJSNoResult('done',[]);
end;

class function TJSMozCanvasPrintState.Cast(Intf: IJSObject): IJSMozCanvasPrintState;
begin
  Result:=TJSMozCanvasPrintState.JOBCast(Intf);
end;

class function TJSimgINotificationObserver.Cast(Intf: IJSObject): IJSimgINotificationObserver;
begin
  Result:=TJSimgINotificationObserver.JOBCast(Intf);
end;

class function TJSimgIRequest.Cast(Intf: IJSObject): IJSimgIRequest;
begin
  Result:=TJSimgIRequest.JOBCast(Intf);
end;

class function TJSnsIStreamListener.Cast(Intf: IJSObject): IJSnsIStreamListener;
begin
  Result:=TJSnsIStreamListener.JOBCast(Intf);
end;

function TJSMozImageLoadingContent._GetloadingEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('loadingEnabled');
end;

function TJSMozImageLoadingContent._GetcurrentURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentURI',TJSURI) as IJSURI;
end;

function TJSMozImageLoadingContent._GetcurrentRequestFinalURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentRequestFinalURI',TJSURI) as IJSURI;
end;

procedure TJSMozImageLoadingContent._SetloadingEnabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('loadingEnabled',aValue);
end;

procedure TJSMozImageLoadingContent.addObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('addObserver',[aObserver]);
end;

procedure TJSMozImageLoadingContent.removeObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('removeObserver',[aObserver]);
end;

function TJSMozImageLoadingContent.getRequest(aRequestType: Integer): IJSimgIRequest;
begin
  Result:=InvokeJSObjectResult('getRequest',[aRequestType],TJSimgIRequest) as IJSimgIRequest;
end;

function TJSMozImageLoadingContent.getRequestType(aRequest: IJSimgIRequest): Integer;
begin
  Result:=InvokeJSLongIntResult('getRequestType',[aRequest]);
end;

procedure TJSMozImageLoadingContent.forceReload(aNotify: Boolean); overload;
begin
  InvokeJSNoResult('forceReload',[aNotify]);
end;

procedure TJSMozImageLoadingContent.forceReload; overload;
begin
  InvokeJSNoResult('forceReload',[]);
end;

class function TJSMozImageLoadingContent.Cast(Intf: IJSObject): IJSMozImageLoadingContent;
begin
  Result:=TJSMozImageLoadingContent.JOBCast(Intf);
end;

function TJSMozEditableElement._Geteditor: IJSnsIEditor;
begin
  Result:=ReadJSPropertyObject('editor',TJSnsIEditor) as IJSnsIEditor;
end;

function TJSMozEditableElement._GethasEditor: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasEditor');
end;

function TJSMozEditableElement._GetisInputEventTarget: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isInputEventTarget');
end;

procedure TJSMozEditableElement.setUserInput(const aInput: UnicodeString);
begin
  InvokeJSNoResult('setUserInput',[aInput]);
end;

class function TJSMozEditableElement.Cast(Intf: IJSObject): IJSMozEditableElement;
begin
  Result:=TJSMozEditableElement.JOBCast(Intf);
end;

class function TJSnsIEditor.Cast(Intf: IJSObject): IJSnsIEditor;
begin
  Result:=TJSnsIEditor.JOBCast(Intf);
end;

function TJSHTMLHyperlinkElementUtils._Gethref: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('href');
end;

function TJSHTMLHyperlinkElementUtils._Getorigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('origin');
end;

function TJSHTMLHyperlinkElementUtils._Getprotocol: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('protocol');
end;

function TJSHTMLHyperlinkElementUtils._Getusername: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('username');
end;

function TJSHTMLHyperlinkElementUtils._Getpassword: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('password');
end;

function TJSHTMLHyperlinkElementUtils._Gethost: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('host');
end;

function TJSHTMLHyperlinkElementUtils._Gethostname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hostname');
end;

function TJSHTMLHyperlinkElementUtils._Getport: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('port');
end;

function TJSHTMLHyperlinkElementUtils._Getpathname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('pathname');
end;

function TJSHTMLHyperlinkElementUtils._Getsearch: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('search');
end;

function TJSHTMLHyperlinkElementUtils._Gethash: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hash');
end;

procedure TJSHTMLHyperlinkElementUtils._Sethref(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('href',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Setprotocol(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('protocol',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Setusername(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('username',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Setpassword(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('password',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Sethost(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('host',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Sethostname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hostname',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Setport(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('port',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Setpathname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('pathname',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Setsearch(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('search',aValue);
end;

procedure TJSHTMLHyperlinkElementUtils._Sethash(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hash',aValue);
end;

class function TJSHTMLHyperlinkElementUtils.Cast(Intf: IJSObject): IJSHTMLHyperlinkElementUtils;
begin
  Result:=TJSHTMLHyperlinkElementUtils.JOBCast(Intf);
end;

function TJSLinkStyle._Getsheet: IJSStyleSheet;
begin
  Result:=ReadJSPropertyObject('sheet',TJSStyleSheet) as IJSStyleSheet;
end;

class function TJSLinkStyle.Cast(Intf: IJSObject): IJSLinkStyle;
begin
  Result:=TJSLinkStyle.JOBCast(Intf);
end;

function TJSBlob._Getsize: QWord;
begin
  Result:=ReadJSPropertyInt64('size');
end;

function TJSBlob._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSBlob._GetblobImplType: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('blobImplType');
end;

function TJSBlob.slice(aStart: Int64; aEnd_: Int64; const aContentType: UnicodeString): IJSBlob; overload;
begin
  Result:=InvokeJSObjectResult('slice',[aStart,aEnd_,aContentType],TJSBlob) as IJSBlob;
end;

function TJSBlob.slice: IJSBlob; overload;
begin
  Result:=InvokeJSObjectResult('slice',[],TJSBlob) as IJSBlob;
end;

function TJSBlob.slice(aStart: Int64): IJSBlob; overload;
begin
  Result:=InvokeJSObjectResult('slice',[aStart],TJSBlob) as IJSBlob;
end;

function TJSBlob.slice(aStart: Int64; aEnd_: Int64): IJSBlob; overload;
begin
  Result:=InvokeJSObjectResult('slice',[aStart,aEnd_],TJSBlob) as IJSBlob;
end;

class function TJSBlob.Cast(Intf: IJSObject): IJSBlob;
begin
  Result:=TJSBlob.JOBCast(Intf);
end;

function TJSFileList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSFileList.item(aIndex: LongWord): IJSFile;
begin
  Result:=InvokeJSObjectResult('item',[aIndex],TJSFile) as IJSFile;
end;

class function TJSFileList.Cast(Intf: IJSObject): IJSFileList;
begin
  Result:=TJSFileList.JOBCast(Intf);
end;

class function TJSnsIFile.Cast(Intf: IJSObject): IJSnsIFile;
begin
  Result:=TJSnsIFile.JOBCast(Intf);
end;

function TJSFileSystem._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSFileSystem._Getroot: IJSFileSystemDirectoryEntry;
begin
  Result:=ReadJSPropertyObject('root',TJSFileSystemDirectoryEntry) as IJSFileSystemDirectoryEntry;
end;

class function TJSFileSystem.Cast(Intf: IJSObject): IJSFileSystem;
begin
  Result:=TJSFileSystem.JOBCast(Intf);
end;

function TJSFileSystemEntry._GetisFile: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isFile');
end;

function TJSFileSystemEntry._GetisDirectory: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isDirectory');
end;

function TJSFileSystemEntry._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSFileSystemEntry._GetfullPath: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('fullPath');
end;

function TJSFileSystemEntry._Getfilesystem: IJSFileSystem;
begin
  Result:=ReadJSPropertyObject('filesystem',TJSFileSystem) as IJSFileSystem;
end;

class function TJSFileSystemEntry.Cast(Intf: IJSObject): IJSFileSystemEntry;
begin
  Result:=TJSFileSystemEntry.JOBCast(Intf);
end;

function TJSImageBitmap._Getwidth: LongWord;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSImageBitmap._Getheight: LongWord;
begin
  Result:=ReadJSPropertyInt64('height');
end;

procedure TJSImageBitmap.close;
begin
  InvokeJSNoResult('close',[]);
end;

class function TJSImageBitmap.Cast(Intf: IJSObject): IJSImageBitmap;
begin
  Result:=TJSImageBitmap.JOBCast(Intf);
end;

class function TJSFileSystemDirectoryReader.Cast(Intf: IJSObject): IJSFileSystemDirectoryReader;
begin
  Result:=TJSFileSystemDirectoryReader.JOBCast(Intf);
end;

procedure TJSFormData.append(const aName: UnicodeString; aValue: IJSBlob; const aFilename: UnicodeString); overload;
begin
  InvokeJSNoResult('append',[aName,aValue,aFilename]);
end;

procedure TJSFormData.append(const aName: UnicodeString; aValue: IJSBlob); overload;
begin
  InvokeJSNoResult('append',[aName,aValue]);
end;

procedure TJSFormData.append(const aName: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('append',[aName,aValue]);
end;

procedure TJSFormData.delete(const aName: UnicodeString);
begin
  InvokeJSNoResult('delete',[aName]);
end;

function TJSFormData.get(const aName: UnicodeString): TFormDataEntryValue;
begin
  Result:=InvokeJSValueResult('get',[aName]);
end;

function TJSFormData.getAll(const aName: UnicodeString): TFormDataEntryValueDynArray;
begin
  Result:=InvokeJSObjectResult('getAll',[aName],TJSArray) as TFormDataEntryValueDynArray;
end;

function TJSFormData.has(const aName: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('has',[aName]);
end;

procedure TJSFormData.set_(const aName: UnicodeString; aValue: IJSBlob; const aFilename: UnicodeString); overload;
begin
  InvokeJSNoResult('set',[aName,aValue,aFilename]);
end;

procedure TJSFormData.set_(const aName: UnicodeString; aValue: IJSBlob); overload;
begin
  InvokeJSNoResult('set',[aName,aValue]);
end;

procedure TJSFormData.set_(const aName: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('set',[aName,aValue]);
end;

class function TJSFormData.Cast(Intf: IJSObject): IJSFormData;
begin
  Result:=TJSFormData.JOBCast(Intf);
end;

function TJSNode._GetnodeType: Word;
begin
  Result:=ReadJSPropertyLongInt('nodeType');
end;

function TJSNode._GetnodeName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nodeName');
end;

function TJSNode._GetbaseURI: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('baseURI');
end;

function TJSNode._GetisConnected: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isConnected');
end;

function TJSNode._GetparentNode: IJSNode;
begin
  Result:=ReadJSPropertyObject('parentNode',TJSNode) as IJSNode;
end;

function TJSNode._GetfirstChild: IJSNode;
begin
  Result:=ReadJSPropertyObject('firstChild',TJSNode) as IJSNode;
end;

function TJSNode._GetlastChild: IJSNode;
begin
  Result:=ReadJSPropertyObject('lastChild',TJSNode) as IJSNode;
end;

function TJSNode._GetpreviousSibling: IJSNode;
begin
  Result:=ReadJSPropertyObject('previousSibling',TJSNode) as IJSNode;
end;

function TJSNode._GetnextSibling: IJSNode;
begin
  Result:=ReadJSPropertyObject('nextSibling',TJSNode) as IJSNode;
end;

function TJSNode._GetnodeValue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nodeValue');
end;

function TJSNode._GettextContent: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('textContent');
end;

procedure TJSNode._SetnodeValue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('nodeValue',aValue);
end;

procedure TJSNode._SettextContent(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('textContent',aValue);
end;

function TJSNode.getRootNode(const aOptions: TJSGetRootNodeOptions): IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('getRootNode',[aOptions],TJSNode) as IJSNode;
end;

function TJSNode.getRootNode: IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('getRootNode',[],TJSNode) as IJSNode;
end;

function TJSNode.hasChildNodes: Boolean;
begin
  Result:=InvokeJSBooleanResult('hasChildNodes',[]);
end;

procedure TJSNode.normalize;
begin
  InvokeJSNoResult('normalize',[]);
end;

function TJSNode.cloneNode(aDeep: Boolean): IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('cloneNode',[aDeep],TJSNode) as IJSNode;
end;

function TJSNode.cloneNode: IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('cloneNode',[],TJSNode) as IJSNode;
end;

function TJSNode.isEqualNode(aOtherNode: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('isEqualNode',[aOtherNode]);
end;

function TJSNode.isSameNode(aOtherNode: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('isSameNode',[aOtherNode]);
end;

function TJSNode.compareDocumentPosition(aOther: IJSNode): Word;
begin
  Result:=InvokeJSLongIntResult('compareDocumentPosition',[aOther]);
end;

function TJSNode.contains(aOther: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('contains',[aOther]);
end;

function TJSNode.lookupPrefix(const aNamespace: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('lookupPrefix',[aNamespace]);
end;

function TJSNode.lookupNamespaceURI(const aPrefix: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('lookupNamespaceURI',[aPrefix]);
end;

function TJSNode.isDefaultNamespace(const aNamespace: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('isDefaultNamespace',[aNamespace]);
end;

function TJSNode.insertBefore(aNode: IJSNode; aChild: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('insertBefore',[aNode,aChild],TJSNode) as IJSNode;
end;

function TJSNode.appendChild(aNode: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('appendChild',[aNode],TJSNode) as IJSNode;
end;

function TJSNode.replaceChild(aNode: IJSNode; aChild: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('replaceChild',[aNode,aChild],TJSNode) as IJSNode;
end;

function TJSNode.removeChild(aChild: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('removeChild',[aChild],TJSNode) as IJSNode;
end;

class function TJSNode.Cast(Intf: IJSObject): IJSNode;
begin
  Result:=TJSNode.JOBCast(Intf);
end;

function TJSWindow._Getwindow: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('window',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow._Getself_: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('self',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow._Getdocument: IJSDocument;
begin
  Result:=ReadJSPropertyObject('document',TJSDocument) as IJSDocument;
end;

function TJSWindow._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSWindow._Getlocation: IJSLocation;
begin
  Result:=ReadJSPropertyObject('location',TJSLocation) as IJSLocation;
end;

function TJSWindow._Gethistory: IJSHistory;
begin
  Result:=ReadJSPropertyObject('history',TJSHistory) as IJSHistory;
end;

function TJSWindow._GetcustomElements: IJSCustomElementRegistry;
begin
  Result:=ReadJSPropertyObject('customElements',TJSCustomElementRegistry) as IJSCustomElementRegistry;
end;

function TJSWindow._Getlocationbar: IJSBarProp;
begin
  Result:=ReadJSPropertyObject('locationbar',TJSBarProp) as IJSBarProp;
end;

function TJSWindow._Getmenubar: IJSBarProp;
begin
  Result:=ReadJSPropertyObject('menubar',TJSBarProp) as IJSBarProp;
end;

function TJSWindow._Getpersonalbar: IJSBarProp;
begin
  Result:=ReadJSPropertyObject('personalbar',TJSBarProp) as IJSBarProp;
end;

function TJSWindow._Getscrollbars: IJSBarProp;
begin
  Result:=ReadJSPropertyObject('scrollbars',TJSBarProp) as IJSBarProp;
end;

function TJSWindow._Getstatusbar: IJSBarProp;
begin
  Result:=ReadJSPropertyObject('statusbar',TJSBarProp) as IJSBarProp;
end;

function TJSWindow._Gettoolbar: IJSBarProp;
begin
  Result:=ReadJSPropertyObject('toolbar',TJSBarProp) as IJSBarProp;
end;

function TJSWindow._Getstatus: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('status');
end;

function TJSWindow._Getclosed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('closed');
end;

function TJSWindow._Getevent: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('event');
end;

function TJSWindow._Getframes: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('frames',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSWindow._Gettop: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('top',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow._Getopener: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('opener');
end;

function TJSWindow._Getparent: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('parent',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow._GetframeElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('frameElement',TJSElement) as IJSElement;
end;

function TJSWindow._Getnavigator: IJSNavigator;
begin
  Result:=ReadJSPropertyObject('navigator',TJSNavigator) as IJSNavigator;
end;

function TJSWindow._GetclientInformation: IJSNavigator;
begin
  Result:=ReadJSPropertyObject('clientInformation',TJSNavigator) as IJSNavigator;
end;

function TJSWindow._GetapplicationCache: IJSOfflineResourceList;
begin
  Result:=ReadJSPropertyObject('applicationCache',TJSOfflineResourceList) as IJSOfflineResourceList;
end;

function TJSWindow._Getscreen: IJSScreen;
begin
  Result:=ReadJSPropertyObject('screen',TJSScreen) as IJSScreen;
end;

function TJSWindow._GetinnerWidth: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('innerWidth');
end;

function TJSWindow._GetinnerHeight: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('innerHeight');
end;

function TJSWindow._GetscrollX: Double;
begin
  Result:=ReadJSPropertyDouble('scrollX');
end;

function TJSWindow._GetpageXOffset: Double;
begin
  Result:=ReadJSPropertyDouble('pageXOffset');
end;

function TJSWindow._GetscrollY: Double;
begin
  Result:=ReadJSPropertyDouble('scrollY');
end;

function TJSWindow._GetpageYOffset: Double;
begin
  Result:=ReadJSPropertyDouble('pageYOffset');
end;

function TJSWindow._GetscreenLeft: Double;
begin
  Result:=ReadJSPropertyDouble('screenLeft');
end;

function TJSWindow._GetscreenTop: Double;
begin
  Result:=ReadJSPropertyDouble('screenTop');
end;

function TJSWindow._GetscreenX: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('screenX');
end;

function TJSWindow._GetscreenY: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('screenY');
end;

function TJSWindow._GetouterWidth: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('outerWidth');
end;

function TJSWindow._GetouterHeight: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('outerHeight');
end;

function TJSWindow._Getcontrollers: IJSXULControllers;
begin
  Result:=ReadJSPropertyObject('controllers',TJSXULControllers) as IJSXULControllers;
end;

function TJSWindow._GetrealFrameElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('realFrameElement',TJSElement) as IJSElement;
end;

function TJSWindow._GetdocShell: IJSnsIDocShell;
begin
  Result:=ReadJSPropertyObject('docShell',TJSnsIDocShell) as IJSnsIDocShell;
end;

function TJSWindow._GetmozInnerScreenX: Single;
begin
  Result:=ReadJSPropertyDouble('mozInnerScreenX');
end;

function TJSWindow._GetmozInnerScreenY: Single;
begin
  Result:=ReadJSPropertyDouble('mozInnerScreenY');
end;

function TJSWindow._GetdevicePixelRatio: Double;
begin
  Result:=ReadJSPropertyDouble('devicePixelRatio');
end;

function TJSWindow._GetdesktopToDeviceScale: Double;
begin
  Result:=ReadJSPropertyDouble('desktopToDeviceScale');
end;

function TJSWindow._GetscrollMinX: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollMinX');
end;

function TJSWindow._GetscrollMinY: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollMinY');
end;

function TJSWindow._GetscrollMaxX: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollMaxX');
end;

function TJSWindow._GetscrollMaxY: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollMaxY');
end;

function TJSWindow._GetfullScreen: Boolean;
begin
  Result:=ReadJSPropertyBoolean('fullScreen');
end;

function TJSWindow._Getcontent: IJSObject;
begin
  Result:=ReadJSPropertyObject('content',TJSObject) as IJSObject;
end;

function TJSWindow._GetwindowUtils: IJSnsIDOMWindowUtils;
begin
  Result:=ReadJSPropertyObject('windowUtils',TJSnsIDOMWindowUtils) as IJSnsIDOMWindowUtils;
end;

function TJSWindow._GetclientPrincipal: IJSPrincipal;
begin
  Result:=ReadJSPropertyObject('clientPrincipal',TJSPrincipal) as IJSPrincipal;
end;

function TJSWindow._Getsidebar: TJOB_JSValue;
begin
  Result:=ReadJSPropertyValue('sidebar');
end;

function TJSWindow._GetwindowState: Word;
begin
  Result:=ReadJSPropertyLongInt('windowState');
end;

function TJSWindow._GetisFullyOccluded: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isFullyOccluded');
end;

function TJSWindow._GetbrowserDOMWindow: IJSnsIBrowserDOMWindow;
begin
  Result:=ReadJSPropertyObject('browserDOMWindow',TJSnsIBrowserDOMWindow) as IJSnsIBrowserDOMWindow;
end;

function TJSWindow._GetisChromeWindow: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isChromeWindow');
end;

function TJSWindow._GetsessionStorage: IJSStorage;
begin
  Result:=ReadJSPropertyObject('sessionStorage',TJSStorage) as IJSStorage;
end;

function TJSWindow._GetlocalStorage: IJSStorage;
begin
  Result:=ReadJSPropertyObject('localStorage',TJSStorage) as IJSStorage;
end;

procedure TJSWindow._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSWindow._Setstatus(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('status',aValue);
end;

procedure TJSWindow._Setopener(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('opener',aValue);
end;

procedure TJSWindow._SetinnerWidth(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('innerWidth',aValue);
end;

procedure TJSWindow._SetinnerHeight(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('innerHeight',aValue);
end;

procedure TJSWindow._SetscreenX(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('screenX',aValue);
end;

procedure TJSWindow._SetscreenY(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('screenY',aValue);
end;

procedure TJSWindow._SetouterWidth(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('outerWidth',aValue);
end;

procedure TJSWindow._SetouterHeight(const aValue: TJOB_JSValue);
begin
  WriteJSPropertyValue('outerHeight',aValue);
end;

procedure TJSWindow._SetfullScreen(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('fullScreen',aValue);
end;

procedure TJSWindow._SetbrowserDOMWindow(const aValue: IJSnsIBrowserDOMWindow);
begin
  WriteJSPropertyObject('browserDOMWindow',aValue);
end;

procedure TJSWindow.close;
begin
  InvokeJSNoResult('close',[]);
end;

procedure TJSWindow.stop;
begin
  InvokeJSNoResult('stop',[]);
end;

procedure TJSWindow.focus;
begin
  InvokeJSNoResult('focus',[]);
end;

procedure TJSWindow.blur;
begin
  InvokeJSNoResult('blur',[]);
end;

function TJSWindow.open(const aUrl: UnicodeString; const aTarget: UnicodeString; const aFeatures: UnicodeString): IJSWindowProxy; overload;
begin
  Result:=InvokeJSObjectResult('open',[aUrl,aTarget,aFeatures],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.open: IJSWindowProxy; overload;
begin
  Result:=InvokeJSObjectResult('open',[],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.open(const aUrl: UnicodeString): IJSWindowProxy; overload;
begin
  Result:=InvokeJSObjectResult('open',[aUrl],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.open(const aUrl: UnicodeString; const aTarget: UnicodeString): IJSWindowProxy; overload;
begin
  Result:=InvokeJSObjectResult('open',[aUrl,aTarget],TJSWindowProxy) as IJSWindowProxy;
end;

procedure TJSWindow.alert;
begin
  InvokeJSNoResult('alert',[]);
end;

procedure TJSWindow.alert(const aMessage: UnicodeString);
begin
  InvokeJSNoResult('alert',[aMessage]);
end;

function TJSWindow.confirm(const aMessage: UnicodeString): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('confirm',[aMessage]);
end;

function TJSWindow.confirm: Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('confirm',[]);
end;

function TJSWindow.prompt(const aMessage: UnicodeString; const aDefault: UnicodeString): UnicodeString; overload;
begin
  Result:=InvokeJSUnicodeStringResult('prompt',[aMessage,aDefault]);
end;

function TJSWindow.prompt: UnicodeString; overload;
begin
  Result:=InvokeJSUnicodeStringResult('prompt',[]);
end;

function TJSWindow.prompt(const aMessage: UnicodeString): UnicodeString; overload;
begin
  Result:=InvokeJSUnicodeStringResult('prompt',[aMessage]);
end;

procedure TJSWindow.print;
begin
  InvokeJSNoResult('print',[]);
end;

procedure TJSWindow.postMessage(aMessage: TJOB_JSValue; const aOptions: TJSWindowPostMessageOptions); overload;
begin
  InvokeJSNoResult('postMessage',[aMessage,aOptions]);
end;

procedure TJSWindow.postMessage(aMessage: TJOB_JSValue); overload;
begin
  InvokeJSNoResult('postMessage',[aMessage]);
end;

procedure TJSWindow.captureEvents;
begin
  InvokeJSNoResult('captureEvents',[]);
end;

procedure TJSWindow.releaseEvents;
begin
  InvokeJSNoResult('releaseEvents',[]);
end;

function TJSWindow.getSelection: IJSSelection;
begin
  Result:=InvokeJSObjectResult('getSelection',[],TJSSelection) as IJSSelection;
end;

function TJSWindow.getComputedStyle(aElt: IJSElement; const aPseudoElt: UnicodeString): IJSCSSStyleDeclaration; overload;
begin
  Result:=InvokeJSObjectResult('getComputedStyle',[aElt,aPseudoElt],TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

function TJSWindow.getComputedStyle(aElt: IJSElement): IJSCSSStyleDeclaration; overload;
begin
  Result:=InvokeJSObjectResult('getComputedStyle',[aElt],TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

procedure TJSWindow.moveTo(aX: Integer; aY: Integer);
begin
  InvokeJSNoResult('moveTo',[aX,aY]);
end;

procedure TJSWindow.moveBy(aX: Integer; aY: Integer);
begin
  InvokeJSNoResult('moveBy',[aX,aY]);
end;

procedure TJSWindow.resizeTo(aX: Integer; aY: Integer);
begin
  InvokeJSNoResult('resizeTo',[aX,aY]);
end;

procedure TJSWindow.resizeBy(aX: Integer; aY: Integer);
begin
  InvokeJSNoResult('resizeBy',[aX,aY]);
end;

procedure TJSWindow.scroll(aX: Double; aY: Double);
begin
  InvokeJSNoResult('scroll',[aX,aY]);
end;

procedure TJSWindow.scroll(const aOptions: TJSScrollToOptions); overload;
begin
  InvokeJSNoResult('scroll',[aOptions]);
end;

procedure TJSWindow.scroll; overload;
begin
  InvokeJSNoResult('scroll',[]);
end;

procedure TJSWindow.scrollTo(aX: Double; aY: Double);
begin
  InvokeJSNoResult('scrollTo',[aX,aY]);
end;

procedure TJSWindow.scrollTo(const aOptions: TJSScrollToOptions); overload;
begin
  InvokeJSNoResult('scrollTo',[aOptions]);
end;

procedure TJSWindow.scrollTo; overload;
begin
  InvokeJSNoResult('scrollTo',[]);
end;

procedure TJSWindow.scrollBy(aX: Double; aY: Double);
begin
  InvokeJSNoResult('scrollBy',[aX,aY]);
end;

procedure TJSWindow.scrollBy(const aOptions: TJSScrollToOptions); overload;
begin
  InvokeJSNoResult('scrollBy',[aOptions]);
end;

procedure TJSWindow.scrollBy; overload;
begin
  InvokeJSNoResult('scrollBy',[]);
end;

procedure TJSWindow.mozScrollSnap;
begin
  InvokeJSNoResult('mozScrollSnap',[]);
end;

function TJSWindow.getDefaultComputedStyle(aElt: IJSElement; const aPseudoElt: UnicodeString): IJSCSSStyleDeclaration; overload;
begin
  Result:=InvokeJSObjectResult('getDefaultComputedStyle',[aElt,aPseudoElt],TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

function TJSWindow.getDefaultComputedStyle(aElt: IJSElement): IJSCSSStyleDeclaration; overload;
begin
  Result:=InvokeJSObjectResult('getDefaultComputedStyle',[aElt],TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

procedure TJSWindow.scrollByLines(aNumLines: Integer; const aOptions: TJSScrollOptions); overload;
begin
  InvokeJSNoResult('scrollByLines',[aNumLines,aOptions]);
end;

procedure TJSWindow.scrollByLines(aNumLines: Integer); overload;
begin
  InvokeJSNoResult('scrollByLines',[aNumLines]);
end;

procedure TJSWindow.scrollByPages(aNumPages: Integer; const aOptions: TJSScrollOptions); overload;
begin
  InvokeJSNoResult('scrollByPages',[aNumPages,aOptions]);
end;

procedure TJSWindow.scrollByPages(aNumPages: Integer); overload;
begin
  InvokeJSNoResult('scrollByPages',[aNumPages]);
end;

procedure TJSWindow.sizeToContent;
begin
  InvokeJSNoResult('sizeToContent',[]);
end;

procedure TJSWindow.updateCommands(const action: UnicodeString; aSel: IJSSelection; aReason: SmallInt); overload;
begin
  InvokeJSNoResult('updateCommands',[action,aSel,aReason]);
end;

procedure TJSWindow.updateCommands(const action: UnicodeString); overload;
begin
  InvokeJSNoResult('updateCommands',[action]);
end;

procedure TJSWindow.updateCommands(const action: UnicodeString; aSel: IJSSelection); overload;
begin
  InvokeJSNoResult('updateCommands',[action,aSel]);
end;

function TJSWindow.find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean; aSearchInFrames: Boolean; aShowDialog: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr,aCaseSensitive,aBackwards,aWrapAround,aWholeWord,aSearchInFrames,aShowDialog]);
end;

function TJSWindow.find: Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[]);
end;

function TJSWindow.find(const aStr: UnicodeString): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr]);
end;

function TJSWindow.find(const aStr: UnicodeString; aCaseSensitive: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr,aCaseSensitive]);
end;

function TJSWindow.find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr,aCaseSensitive,aBackwards]);
end;

function TJSWindow.find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr,aCaseSensitive,aBackwards,aWrapAround]);
end;

function TJSWindow.find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr,aCaseSensitive,aBackwards,aWrapAround,aWholeWord]);
end;

function TJSWindow.find(const aStr: UnicodeString; aCaseSensitive: Boolean; aBackwards: Boolean; aWrapAround: Boolean; aWholeWord: Boolean; aSearchInFrames: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('find',[aStr,aCaseSensitive,aBackwards,aWrapAround,aWholeWord,aSearchInFrames]);
end;

procedure TJSWindow.dump(const aStr: UnicodeString);
begin
  InvokeJSNoResult('dump',[aStr]);
end;

procedure TJSWindow.setResizable(aResizable: Boolean);
begin
  InvokeJSNoResult('setResizable',[aResizable]);
end;

function TJSWindow.openDialog(const aUrl: UnicodeString; const aName: UnicodeString; const aOptions: UnicodeString; aExtraArguments: TJOB_JSValue): IJSWindowProxy{; ToDo:varargs}; overload;
begin
  Result:=InvokeJSObjectResult('openDialog',[aUrl,aName,aOptions,aExtraArguments],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.openDialog: IJSWindowProxy{; ToDo:varargs}; overload;
begin
  Result:=InvokeJSObjectResult('openDialog',[],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.openDialog(const aUrl: UnicodeString): IJSWindowProxy{; ToDo:varargs}; overload;
begin
  Result:=InvokeJSObjectResult('openDialog',[aUrl],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.openDialog(const aUrl: UnicodeString; const aName: UnicodeString): IJSWindowProxy{; ToDo:varargs}; overload;
begin
  Result:=InvokeJSObjectResult('openDialog',[aUrl,aName],TJSWindowProxy) as IJSWindowProxy;
end;

function TJSWindow.getInterface(aIid: TJOB_JSValue): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult('getInterface',[aIid]);
end;

function TJSWindow.shouldReportForServiceWorkerScope(const aScope: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('shouldReportForServiceWorkerScope',[aScope]);
end;

procedure TJSWindow.getAttention;
begin
  InvokeJSNoResult('getAttention',[]);
end;

procedure TJSWindow.getAttentionWithCycleCount(aCycleCount: Integer);
begin
  InvokeJSNoResult('getAttentionWithCycleCount',[aCycleCount]);
end;

procedure TJSWindow.setCursor(const aCursor: UTF8String);
begin
  InvokeJSNoResult('setCursor',[aCursor]);
end;

procedure TJSWindow.maximize;
begin
  InvokeJSNoResult('maximize',[]);
end;

procedure TJSWindow.minimize;
begin
  InvokeJSNoResult('minimize',[]);
end;

procedure TJSWindow.restore;
begin
  InvokeJSNoResult('restore',[]);
end;

function TJSWindow.getWorkspaceID: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getWorkspaceID',[]);
end;

procedure TJSWindow.moveToWorkspace(const aWorkspaceID: UnicodeString);
begin
  InvokeJSNoResult('moveToWorkspace',[aWorkspaceID]);
end;

procedure TJSWindow.notifyDefaultButtonLoaded(aDefaultButton: IJSElement);
begin
  InvokeJSNoResult('notifyDefaultButtonLoaded',[aDefaultButton]);
end;

procedure TJSWindow.cancelIdleCallback(aHandle: LongWord);
begin
  InvokeJSNoResult('cancelIdleCallback',[aHandle]);
end;

function TJSWindow.getRegionalPrefsLocales: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('getRegionalPrefsLocales',[],TJSArray) as TUnicodeStringDynArray;
end;

function TJSWindow.getWebExposedLocales: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('getWebExposedLocales',[],TJSArray) as TUnicodeStringDynArray;
end;

class function TJSWindow.Cast(Intf: IJSObject): IJSWindow;
begin
  Result:=TJSWindow.JOBCast(Intf);
end;

function TJSOfflineResourceList._Getstatus: Word;
begin
  Result:=ReadJSPropertyLongInt('status');
end;

function TJSOfflineResourceList._GetmozItems: IJSDOMStringList;
begin
  Result:=ReadJSPropertyObject('mozItems',TJSDOMStringList) as IJSDOMStringList;
end;

function TJSOfflineResourceList._GetmozLength: LongWord;
begin
  Result:=ReadJSPropertyInt64('mozLength');
end;

function TJSOfflineResourceList._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

procedure TJSOfflineResourceList.update;
begin
  InvokeJSNoResult('update',[]);
end;

procedure TJSOfflineResourceList.swapCache;
begin
  InvokeJSNoResult('swapCache',[]);
end;

function TJSOfflineResourceList.mozHasItem(const aUri: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('mozHasItem',[aUri]);
end;

function TJSOfflineResourceList.mozItem(aIndex: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('mozItem',[aIndex]);
end;

procedure TJSOfflineResourceList.mozAdd(const aUri: UnicodeString);
begin
  InvokeJSNoResult('mozAdd',[aUri]);
end;

procedure TJSOfflineResourceList.mozRemove(const aUri: UnicodeString);
begin
  InvokeJSNoResult('mozRemove',[aUri]);
end;

class function TJSOfflineResourceList.Cast(Intf: IJSObject): IJSOfflineResourceList;
begin
  Result:=TJSOfflineResourceList.JOBCast(Intf);
end;

function TJSScreen._GetavailWidth: Integer;
begin
  Result:=ReadJSPropertyLongInt('availWidth');
end;

function TJSScreen._GetavailHeight: Integer;
begin
  Result:=ReadJSPropertyLongInt('availHeight');
end;

function TJSScreen._Getwidth: Integer;
begin
  Result:=ReadJSPropertyLongInt('width');
end;

function TJSScreen._Getheight: Integer;
begin
  Result:=ReadJSPropertyLongInt('height');
end;

function TJSScreen._GetcolorDepth: Integer;
begin
  Result:=ReadJSPropertyLongInt('colorDepth');
end;

function TJSScreen._GetpixelDepth: Integer;
begin
  Result:=ReadJSPropertyLongInt('pixelDepth');
end;

function TJSScreen._Gettop: Integer;
begin
  Result:=ReadJSPropertyLongInt('top');
end;

function TJSScreen._Getleft: Integer;
begin
  Result:=ReadJSPropertyLongInt('left');
end;

function TJSScreen._GetavailTop: Integer;
begin
  Result:=ReadJSPropertyLongInt('availTop');
end;

function TJSScreen._GetavailLeft: Integer;
begin
  Result:=ReadJSPropertyLongInt('availLeft');
end;

function TJSScreen._GetmozOrientation: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('mozOrientation');
end;

function TJSScreen._Getorientation: IJSScreenOrientation;
begin
  Result:=ReadJSPropertyObject('orientation',TJSScreenOrientation) as IJSScreenOrientation;
end;

function TJSScreen._GetcolorGamut: TScreenColorGamut;
begin
  Result:=ReadJSPropertyUnicodeString('colorGamut');
end;

function TJSScreen._Getluminance: IJSScreenLuminance;
begin
  Result:=ReadJSPropertyObject('luminance',TJSScreenLuminance) as IJSScreenLuminance;
end;

function TJSScreen.mozLockOrientation(const aOrientation: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('mozLockOrientation',[aOrientation]);
end;

procedure TJSScreen.mozUnlockOrientation;
begin
  InvokeJSNoResult('mozUnlockOrientation',[]);
end;

class function TJSScreen.Cast(Intf: IJSObject): IJSScreen;
begin
  Result:=TJSScreen.JOBCast(Intf);
end;

procedure TJSClipboard.onUserReactedToPasteMenuPopup(allowed: Boolean);
begin
  InvokeJSNoResult('onUserReactedToPasteMenuPopup',[allowed]);
end;

class function TJSClipboard.Cast(Intf: IJSObject): IJSClipboard;
begin
  Result:=TJSClipboard.JOBCast(Intf);
end;

function TJSRange._GetcommonAncestorContainer: IJSNode;
begin
  Result:=ReadJSPropertyObject('commonAncestorContainer',TJSNode) as IJSNode;
end;

procedure TJSRange.setStart(aRefNode: IJSNode; aOffset: LongWord);
begin
  InvokeJSNoResult('setStart',[aRefNode,aOffset]);
end;

procedure TJSRange.setEnd(aRefNode: IJSNode; aOffset: LongWord);
begin
  InvokeJSNoResult('setEnd',[aRefNode,aOffset]);
end;

procedure TJSRange.setStartBefore(aRefNode: IJSNode);
begin
  InvokeJSNoResult('setStartBefore',[aRefNode]);
end;

procedure TJSRange.setStartAfter(aRefNode: IJSNode);
begin
  InvokeJSNoResult('setStartAfter',[aRefNode]);
end;

procedure TJSRange.setEndBefore(aRefNode: IJSNode);
begin
  InvokeJSNoResult('setEndBefore',[aRefNode]);
end;

procedure TJSRange.setEndAfter(aRefNode: IJSNode);
begin
  InvokeJSNoResult('setEndAfter',[aRefNode]);
end;

procedure TJSRange.collapse(aToStart: Boolean); overload;
begin
  InvokeJSNoResult('collapse',[aToStart]);
end;

procedure TJSRange.collapse; overload;
begin
  InvokeJSNoResult('collapse',[]);
end;

procedure TJSRange.selectNode(aRefNode: IJSNode);
begin
  InvokeJSNoResult('selectNode',[aRefNode]);
end;

procedure TJSRange.selectNodeContents(aRefNode: IJSNode);
begin
  InvokeJSNoResult('selectNodeContents',[aRefNode]);
end;

function TJSRange.compareBoundaryPoints(aHow: Word; aSourceRange: IJSRange): SmallInt;
begin
  Result:=InvokeJSLongIntResult('compareBoundaryPoints',[aHow,aSourceRange]);
end;

procedure TJSRange.deleteContents;
begin
  InvokeJSNoResult('deleteContents',[]);
end;

function TJSRange.extractContents: IJSDocumentFragment;
begin
  Result:=InvokeJSObjectResult('extractContents',[],TJSDocumentFragment) as IJSDocumentFragment;
end;

function TJSRange.cloneContents: IJSDocumentFragment;
begin
  Result:=InvokeJSObjectResult('cloneContents',[],TJSDocumentFragment) as IJSDocumentFragment;
end;

procedure TJSRange.insertNode(aNode: IJSNode);
begin
  InvokeJSNoResult('insertNode',[aNode]);
end;

procedure TJSRange.surroundContents(aNewParent: IJSNode);
begin
  InvokeJSNoResult('surroundContents',[aNewParent]);
end;

function TJSRange.cloneRange: IJSRange;
begin
  Result:=InvokeJSObjectResult('cloneRange',[],TJSRange) as IJSRange;
end;

procedure TJSRange.detach;
begin
  InvokeJSNoResult('detach',[]);
end;

function TJSRange.isPointInRange(aNode: IJSNode; aOffset: LongWord): Boolean;
begin
  Result:=InvokeJSBooleanResult('isPointInRange',[aNode,aOffset]);
end;

function TJSRange.comparePoint(aNode: IJSNode; aOffset: LongWord): SmallInt;
begin
  Result:=InvokeJSLongIntResult('comparePoint',[aNode,aOffset]);
end;

function TJSRange.intersectsNode(aNode: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('intersectsNode',[aNode]);
end;

function TJSRange.createContextualFragment(const aFragment: UnicodeString): IJSDocumentFragment;
begin
  Result:=InvokeJSObjectResult('createContextualFragment',[aFragment],TJSDocumentFragment) as IJSDocumentFragment;
end;

function TJSRange.getClientRects: IJSDOMRectList;
begin
  Result:=InvokeJSObjectResult('getClientRects',[],TJSDOMRectList) as IJSDOMRectList;
end;

function TJSRange.getBoundingClientRect: IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('getBoundingClientRect',[],TJSDOMRect) as IJSDOMRect;
end;

class function TJSRange.Cast(Intf: IJSObject): IJSRange;
begin
  Result:=TJSRange.JOBCast(Intf);
end;

function TJSScreenOrientation._Gettype_: TOrientationType;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSScreenOrientation._Getangle: Word;
begin
  Result:=ReadJSPropertyLongInt('angle');
end;

procedure TJSScreenOrientation.unlock;
begin
  InvokeJSNoResult('unlock',[]);
end;

class function TJSScreenOrientation.Cast(Intf: IJSObject): IJSScreenOrientation;
begin
  Result:=TJSScreenOrientation.JOBCast(Intf);
end;

function TJSDOMRect._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMRect._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMRect._Getwidth: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSDOMRect._Getheight: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

procedure TJSDOMRect._Setx(const aValue: Double);
begin
  WriteJSPropertyDouble('x',aValue);
end;

procedure TJSDOMRect._Sety(const aValue: Double);
begin
  WriteJSPropertyDouble('y',aValue);
end;

procedure TJSDOMRect._Setwidth(const aValue: Double);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSDOMRect._Setheight(const aValue: Double);
begin
  WriteJSPropertyDouble('height',aValue);
end;

function TJSDOMRect.fromRect(const aOther: TJSDOMRectInit): IJSDOMRect; overload;
begin
  Result:=InvokeJSObjectResult('fromRect',[aOther],TJSDOMRect) as IJSDOMRect;
end;

function TJSDOMRect.fromRect: IJSDOMRect; overload;
begin
  Result:=InvokeJSObjectResult('fromRect',[],TJSDOMRect) as IJSDOMRect;
end;

class function TJSDOMRect.Cast(Intf: IJSObject): IJSDOMRect;
begin
  Result:=TJSDOMRect.JOBCast(Intf);
end;

function TJSDOMMatrix._Geta: Double;
begin
  Result:=ReadJSPropertyDouble('a');
end;

function TJSDOMMatrix._Getb: Double;
begin
  Result:=ReadJSPropertyDouble('b');
end;

function TJSDOMMatrix._Getc: Double;
begin
  Result:=ReadJSPropertyDouble('c');
end;

function TJSDOMMatrix._Getd: Double;
begin
  Result:=ReadJSPropertyDouble('d');
end;

function TJSDOMMatrix._Gete: Double;
begin
  Result:=ReadJSPropertyDouble('e');
end;

function TJSDOMMatrix._Getf: Double;
begin
  Result:=ReadJSPropertyDouble('f');
end;

function TJSDOMMatrix._Getm11: Double;
begin
  Result:=ReadJSPropertyDouble('m11');
end;

function TJSDOMMatrix._Getm12: Double;
begin
  Result:=ReadJSPropertyDouble('m12');
end;

function TJSDOMMatrix._Getm13: Double;
begin
  Result:=ReadJSPropertyDouble('m13');
end;

function TJSDOMMatrix._Getm14: Double;
begin
  Result:=ReadJSPropertyDouble('m14');
end;

function TJSDOMMatrix._Getm21: Double;
begin
  Result:=ReadJSPropertyDouble('m21');
end;

function TJSDOMMatrix._Getm22: Double;
begin
  Result:=ReadJSPropertyDouble('m22');
end;

function TJSDOMMatrix._Getm23: Double;
begin
  Result:=ReadJSPropertyDouble('m23');
end;

function TJSDOMMatrix._Getm24: Double;
begin
  Result:=ReadJSPropertyDouble('m24');
end;

function TJSDOMMatrix._Getm31: Double;
begin
  Result:=ReadJSPropertyDouble('m31');
end;

function TJSDOMMatrix._Getm32: Double;
begin
  Result:=ReadJSPropertyDouble('m32');
end;

function TJSDOMMatrix._Getm33: Double;
begin
  Result:=ReadJSPropertyDouble('m33');
end;

function TJSDOMMatrix._Getm34: Double;
begin
  Result:=ReadJSPropertyDouble('m34');
end;

function TJSDOMMatrix._Getm41: Double;
begin
  Result:=ReadJSPropertyDouble('m41');
end;

function TJSDOMMatrix._Getm42: Double;
begin
  Result:=ReadJSPropertyDouble('m42');
end;

function TJSDOMMatrix._Getm43: Double;
begin
  Result:=ReadJSPropertyDouble('m43');
end;

function TJSDOMMatrix._Getm44: Double;
begin
  Result:=ReadJSPropertyDouble('m44');
end;

procedure TJSDOMMatrix._Seta(const aValue: Double);
begin
  WriteJSPropertyDouble('a',aValue);
end;

procedure TJSDOMMatrix._Setb(const aValue: Double);
begin
  WriteJSPropertyDouble('b',aValue);
end;

procedure TJSDOMMatrix._Setc(const aValue: Double);
begin
  WriteJSPropertyDouble('c',aValue);
end;

procedure TJSDOMMatrix._Setd(const aValue: Double);
begin
  WriteJSPropertyDouble('d',aValue);
end;

procedure TJSDOMMatrix._Sete(const aValue: Double);
begin
  WriteJSPropertyDouble('e',aValue);
end;

procedure TJSDOMMatrix._Setf(const aValue: Double);
begin
  WriteJSPropertyDouble('f',aValue);
end;

procedure TJSDOMMatrix._Setm11(const aValue: Double);
begin
  WriteJSPropertyDouble('m11',aValue);
end;

procedure TJSDOMMatrix._Setm12(const aValue: Double);
begin
  WriteJSPropertyDouble('m12',aValue);
end;

procedure TJSDOMMatrix._Setm13(const aValue: Double);
begin
  WriteJSPropertyDouble('m13',aValue);
end;

procedure TJSDOMMatrix._Setm14(const aValue: Double);
begin
  WriteJSPropertyDouble('m14',aValue);
end;

procedure TJSDOMMatrix._Setm21(const aValue: Double);
begin
  WriteJSPropertyDouble('m21',aValue);
end;

procedure TJSDOMMatrix._Setm22(const aValue: Double);
begin
  WriteJSPropertyDouble('m22',aValue);
end;

procedure TJSDOMMatrix._Setm23(const aValue: Double);
begin
  WriteJSPropertyDouble('m23',aValue);
end;

procedure TJSDOMMatrix._Setm24(const aValue: Double);
begin
  WriteJSPropertyDouble('m24',aValue);
end;

procedure TJSDOMMatrix._Setm31(const aValue: Double);
begin
  WriteJSPropertyDouble('m31',aValue);
end;

procedure TJSDOMMatrix._Setm32(const aValue: Double);
begin
  WriteJSPropertyDouble('m32',aValue);
end;

procedure TJSDOMMatrix._Setm33(const aValue: Double);
begin
  WriteJSPropertyDouble('m33',aValue);
end;

procedure TJSDOMMatrix._Setm34(const aValue: Double);
begin
  WriteJSPropertyDouble('m34',aValue);
end;

procedure TJSDOMMatrix._Setm41(const aValue: Double);
begin
  WriteJSPropertyDouble('m41',aValue);
end;

procedure TJSDOMMatrix._Setm42(const aValue: Double);
begin
  WriteJSPropertyDouble('m42',aValue);
end;

procedure TJSDOMMatrix._Setm43(const aValue: Double);
begin
  WriteJSPropertyDouble('m43',aValue);
end;

procedure TJSDOMMatrix._Setm44(const aValue: Double);
begin
  WriteJSPropertyDouble('m44',aValue);
end;

function TJSDOMMatrix.fromMatrix(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('fromMatrix',[aOther],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.fromMatrix: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('fromMatrix',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.fromFloat32Array(array32: IJSFloat32Array): IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('fromFloat32Array',[array32],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.fromFloat64Array(array64: IJSFloat64Array): IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('fromFloat64Array',[array64],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.multiplySelf(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('multiplySelf',[aOther],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.multiplySelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('multiplySelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.preMultiplySelf(const aOther: TJSDOMMatrixInit): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('preMultiplySelf',[aOther],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.preMultiplySelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('preMultiplySelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.translateSelf(aTx: Double; aTy: Double; aTz: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translateSelf',[aTx,aTy,aTz],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.translateSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translateSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.translateSelf(aTx: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translateSelf',[aTx],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.translateSelf(aTx: Double; aTy: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('translateSelf',[aTx,aTy],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[aScaleX,aScaleY,aScaleZ,aOriginX,aOriginY,aOriginZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf(aScaleX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[aScaleX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf(aScaleX: Double; aScaleY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[aScaleX,aScaleY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[aScaleX,aScaleY,aScaleZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[aScaleX,aScaleY,aScaleZ,aOriginX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scaleSelf(aScaleX: Double; aScaleY: Double; aScaleZ: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scaleSelf',[aScaleX,aScaleY,aScaleZ,aOriginX,aOriginY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scale3dSelf(aScale: Double; aOriginX: Double; aOriginY: Double; aOriginZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3dSelf',[aScale,aOriginX,aOriginY,aOriginZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scale3dSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3dSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scale3dSelf(aScale: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3dSelf',[aScale],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scale3dSelf(aScale: Double; aOriginX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3dSelf',[aScale,aOriginX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.scale3dSelf(aScale: Double; aOriginX: Double; aOriginY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('scale3dSelf',[aScale,aOriginX,aOriginY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateSelf(aRotX: Double; aRotY: Double; aRotZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateSelf',[aRotX,aRotY,aRotZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateSelf(aRotX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateSelf',[aRotX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateSelf(aRotX: Double; aRotY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateSelf',[aRotX,aRotY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateFromVectorSelf(aX: Double; aY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateFromVectorSelf',[aX,aY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateFromVectorSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateFromVectorSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateFromVectorSelf(aX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateFromVectorSelf',[aX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateAxisAngleSelf(aX: Double; aY: Double; aZ: Double; angle: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngleSelf',[aX,aY,aZ,angle],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateAxisAngleSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngleSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateAxisAngleSelf(aX: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngleSelf',[aX],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateAxisAngleSelf(aX: Double; aY: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngleSelf',[aX,aY],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.rotateAxisAngleSelf(aX: Double; aY: Double; aZ: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('rotateAxisAngleSelf',[aX,aY,aZ],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.skewXSelf(aSx: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewXSelf',[aSx],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.skewXSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewXSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.skewYSelf(aSy: Double): IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewYSelf',[aSy],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.skewYSelf: IJSDOMMatrix; overload;
begin
  Result:=InvokeJSObjectResult('skewYSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.invertSelf: IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('invertSelf',[],TJSDOMMatrix) as IJSDOMMatrix;
end;

function TJSDOMMatrix.setMatrixValue(const aTransformList: UTF8String): IJSDOMMatrix;
begin
  Result:=InvokeJSObjectResult('setMatrixValue',[aTransformList],TJSDOMMatrix) as IJSDOMMatrix;
end;

class function TJSDOMMatrix.Cast(Intf: IJSObject): IJSDOMMatrix;
begin
  Result:=TJSDOMMatrix.JOBCast(Intf);
end;

function TJSCSSStyleSheet._GetownerRule: IJSCSSRule;
begin
  Result:=ReadJSPropertyObject('ownerRule',TJSCSSRule) as IJSCSSRule;
end;

function TJSCSSStyleSheet._GetcssRules: IJSCSSRuleList;
begin
  Result:=ReadJSPropertyObject('cssRules',TJSCSSRuleList) as IJSCSSRuleList;
end;

function TJSCSSStyleSheet._GetparsingMode: TCSSStyleSheetParsingMode;
begin
  Result:=ReadJSPropertyUnicodeString('parsingMode');
end;

function TJSCSSStyleSheet._Getrules: IJSCSSRuleList;
begin
  Result:=ReadJSPropertyObject('rules',TJSCSSRuleList) as IJSCSSRuleList;
end;

function TJSCSSStyleSheet.insertRule(const aRule: UTF8String; aIndex: LongWord): LongWord; overload;
begin
  Result:=InvokeJSMaxIntResult('insertRule',[aRule,aIndex]);
end;

function TJSCSSStyleSheet.insertRule(const aRule: UTF8String): LongWord; overload;
begin
  Result:=InvokeJSMaxIntResult('insertRule',[aRule]);
end;

procedure TJSCSSStyleSheet.deleteRule(aIndex: LongWord);
begin
  InvokeJSNoResult('deleteRule',[aIndex]);
end;

procedure TJSCSSStyleSheet.replaceSync(const aText: UTF8String);
begin
  InvokeJSNoResult('replaceSync',[aText]);
end;

procedure TJSCSSStyleSheet.removeRule(aIndex: LongWord); overload;
begin
  InvokeJSNoResult('removeRule',[aIndex]);
end;

procedure TJSCSSStyleSheet.removeRule; overload;
begin
  InvokeJSNoResult('removeRule',[]);
end;

function TJSCSSStyleSheet.addRule(const aSelector: UTF8String; const aStyle: UTF8String; aIndex: LongWord): Integer; overload;
begin
  Result:=InvokeJSLongIntResult('addRule',[aSelector,aStyle,aIndex]);
end;

function TJSCSSStyleSheet.addRule: Integer; overload;
begin
  Result:=InvokeJSLongIntResult('addRule',[]);
end;

function TJSCSSStyleSheet.addRule(const aSelector: UTF8String): Integer; overload;
begin
  Result:=InvokeJSLongIntResult('addRule',[aSelector]);
end;

function TJSCSSStyleSheet.addRule(const aSelector: UTF8String; const aStyle: UTF8String): Integer; overload;
begin
  Result:=InvokeJSLongIntResult('addRule',[aSelector,aStyle]);
end;

class function TJSCSSStyleSheet.Cast(Intf: IJSObject): IJSCSSStyleSheet;
begin
  Result:=TJSCSSStyleSheet.JOBCast(Intf);
end;

function TJSDOMPoint._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMPoint._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMPoint._Getz: Double;
begin
  Result:=ReadJSPropertyDouble('z');
end;

function TJSDOMPoint._Getw: Double;
begin
  Result:=ReadJSPropertyDouble('w');
end;

procedure TJSDOMPoint._Setx(const aValue: Double);
begin
  WriteJSPropertyDouble('x',aValue);
end;

procedure TJSDOMPoint._Sety(const aValue: Double);
begin
  WriteJSPropertyDouble('y',aValue);
end;

procedure TJSDOMPoint._Setz(const aValue: Double);
begin
  WriteJSPropertyDouble('z',aValue);
end;

procedure TJSDOMPoint._Setw(const aValue: Double);
begin
  WriteJSPropertyDouble('w',aValue);
end;

function TJSDOMPoint.fromPoint(const aOther: TJSDOMPointInit): IJSDOMPoint; overload;
begin
  Result:=InvokeJSObjectResult('fromPoint',[aOther],TJSDOMPoint) as IJSDOMPoint;
end;

function TJSDOMPoint.fromPoint: IJSDOMPoint; overload;
begin
  Result:=InvokeJSObjectResult('fromPoint',[],TJSDOMPoint) as IJSDOMPoint;
end;

class function TJSDOMPoint.Cast(Intf: IJSObject): IJSDOMPoint;
begin
  Result:=TJSDOMPoint.JOBCast(Intf);
end;

function TJSOffscreenCanvas._Getwidth: LongWord;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSOffscreenCanvas._Getheight: LongWord;
begin
  Result:=ReadJSPropertyInt64('height');
end;

procedure TJSOffscreenCanvas._Setwidth(const aValue: LongWord);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSOffscreenCanvas._Setheight(const aValue: LongWord);
begin
  WriteJSPropertyDouble('height',aValue);
end;

function TJSOffscreenCanvas.getContext(aContextId: TOffscreenRenderingContextId; aContextOptions: TJOB_JSValue): TOffscreenRenderingContext; overload;
begin
  Result:=InvokeJSValueResult('getContext',[aContextId,aContextOptions]);
end;

function TJSOffscreenCanvas.getContext(aContextId: TOffscreenRenderingContextId): TOffscreenRenderingContext; overload;
begin
  Result:=InvokeJSValueResult('getContext',[aContextId]);
end;

function TJSOffscreenCanvas.transferToImageBitmap: IJSImageBitmap;
begin
  Result:=InvokeJSObjectResult('transferToImageBitmap',[],TJSImageBitmap) as IJSImageBitmap;
end;

class function TJSOffscreenCanvas.Cast(Intf: IJSObject): IJSOffscreenCanvas;
begin
  Result:=TJSOffscreenCanvas.JOBCast(Intf);
end;

function TJSFile._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSFile._GetlastModified: Int64;
begin
  Result:=ReadJSPropertyInt64('lastModified');
end;

function TJSFile._GetwebkitRelativePath: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('webkitRelativePath');
end;

function TJSFile._GetmozFullPath: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('mozFullPath');
end;

class function TJSFile.Cast(Intf: IJSObject): IJSFile;
begin
  Result:=TJSFile.JOBCast(Intf);
end;

function TJSFileSystemDirectoryEntry.createReader: IJSFileSystemDirectoryReader;
begin
  Result:=InvokeJSObjectResult('createReader',[],TJSFileSystemDirectoryReader) as IJSFileSystemDirectoryReader;
end;

class function TJSFileSystemDirectoryEntry.Cast(Intf: IJSObject): IJSFileSystemDirectoryEntry;
begin
  Result:=TJSFileSystemDirectoryEntry.JOBCast(Intf);
end;

function TJSDocument._Getimplementation_: IJSDOMImplementation;
begin
  Result:=ReadJSPropertyObject('implementation',TJSDOMImplementation) as IJSDOMImplementation;
end;

function TJSDocument._GetURL: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('URL');
end;

function TJSDocument._GetdocumentURI: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('documentURI');
end;

function TJSDocument._GetcompatMode: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('compatMode');
end;

function TJSDocument._GetcharacterSet: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('characterSet');
end;

function TJSDocument._Getcharset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('charset');
end;

function TJSDocument._GetinputEncoding: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('inputEncoding');
end;

function TJSDocument._GetcontentType: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('contentType');
end;

function TJSDocument._Getdoctype: IJSDocumentType;
begin
  Result:=ReadJSPropertyObject('doctype',TJSDocumentType) as IJSDocumentType;
end;

function TJSDocument._GetdocumentElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('documentElement',TJSElement) as IJSElement;
end;

function TJSDocument._Getlocation: IJSLocation;
begin
  Result:=ReadJSPropertyObject('location',TJSLocation) as IJSLocation;
end;

function TJSDocument._Getdomain: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('domain');
end;

function TJSDocument._Getreferrer: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('referrer');
end;

function TJSDocument._Getcookie: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('cookie');
end;

function TJSDocument._GetlastModified: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('lastModified');
end;

function TJSDocument._GetreadyState: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('readyState');
end;

function TJSDocument._Gettitle: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('title');
end;

function TJSDocument._Getdir: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('dir');
end;

function TJSDocument._Getbody: IJSHTMLElement;
begin
  Result:=ReadJSPropertyObject('body',TJSHTMLElement) as IJSHTMLElement;
end;

function TJSDocument._Gethead: IJSHTMLHeadElement;
begin
  Result:=ReadJSPropertyObject('head',TJSHTMLHeadElement) as IJSHTMLHeadElement;
end;

function TJSDocument._Getimages: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('images',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getembeds: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('embeds',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getplugins: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('plugins',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getlinks: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('links',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getforms: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('forms',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getscripts: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('scripts',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._GetdefaultView: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('defaultView',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSDocument._GetdesignMode: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('designMode');
end;

function TJSDocument._GetmozSyntheticDocument: Boolean;
begin
  Result:=ReadJSPropertyBoolean('mozSyntheticDocument');
end;

function TJSDocument._GetcurrentScript: IJSElement;
begin
  Result:=ReadJSPropertyObject('currentScript',TJSElement) as IJSElement;
end;

function TJSDocument._GetdocumentURIObject: IJSURI;
begin
  Result:=ReadJSPropertyObject('documentURIObject',TJSURI) as IJSURI;
end;

function TJSDocument._GetreferrerPolicy: TReferrerPolicy;
begin
  Result:=ReadJSPropertyUnicodeString('referrerPolicy');
end;

function TJSDocument._GetreferrerInfo: IJSnsIReferrerInfo;
begin
  Result:=ReadJSPropertyObject('referrerInfo',TJSnsIReferrerInfo) as IJSnsIReferrerInfo;
end;

function TJSDocument._GetfgColor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('fgColor');
end;

function TJSDocument._GetlinkColor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('linkColor');
end;

function TJSDocument._GetvlinkColor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('vlinkColor');
end;

function TJSDocument._GetalinkColor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('alinkColor');
end;

function TJSDocument._GetbgColor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('bgColor');
end;

function TJSDocument._Getanchors: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('anchors',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getapplets: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('applets',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._Getall: IJSHTMLAllCollection;
begin
  Result:=ReadJSPropertyObject('all',TJSHTMLAllCollection) as IJSHTMLAllCollection;
end;

function TJSDocument._Getfullscreen: Boolean;
begin
  Result:=ReadJSPropertyBoolean('fullscreen');
end;

function TJSDocument._GetmozFullScreen: Boolean;
begin
  Result:=ReadJSPropertyBoolean('mozFullScreen');
end;

function TJSDocument._GetfullscreenEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('fullscreenEnabled');
end;

function TJSDocument._GetmozFullScreenEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('mozFullScreenEnabled');
end;

function TJSDocument._GetallowDeprecatedTls: Boolean;
begin
  Result:=ReadJSPropertyBoolean('allowDeprecatedTls');
end;

function TJSDocument._Gethidden: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hidden');
end;

function TJSDocument._GetvisibilityState: TVisibilityState;
begin
  Result:=ReadJSPropertyUnicodeString('visibilityState');
end;

function TJSDocument._GetselectedStyleSheetSet: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('selectedStyleSheetSet');
end;

function TJSDocument._GetlastStyleSheetSet: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('lastStyleSheetSet');
end;

function TJSDocument._GetpreferredStyleSheetSet: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('preferredStyleSheetSet');
end;

function TJSDocument._GetstyleSheetSets: IJSDOMStringList;
begin
  Result:=ReadJSPropertyObject('styleSheetSets',TJSDOMStringList) as IJSDOMStringList;
end;

function TJSDocument._GetscrollingElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('scrollingElement',TJSElement) as IJSElement;
end;

function TJSDocument._GetrootElement: IJSSVGSVGElement;
begin
  Result:=ReadJSPropertyObject('rootElement',TJSSVGSVGElement) as IJSSVGSVGElement;
end;

function TJSDocument._GetloadedFromPrototype: Boolean;
begin
  Result:=ReadJSPropertyBoolean('loadedFromPrototype');
end;

function TJSDocument._GeteffectiveStoragePrincipal: IJSPrincipal;
begin
  Result:=ReadJSPropertyObject('effectiveStoragePrincipal',TJSPrincipal) as IJSPrincipal;
end;

function TJSDocument._GetpartitionedPrincipal: IJSPrincipal;
begin
  Result:=ReadJSPropertyObject('partitionedPrincipal',TJSPrincipal) as IJSPrincipal;
end;

function TJSDocument._GetcookieJarSettings: IJSnsICookieJarSettings;
begin
  Result:=ReadJSPropertyObject('cookieJarSettings',TJSnsICookieJarSettings) as IJSnsICookieJarSettings;
end;

function TJSDocument._GetstyleSheetChangeEventsEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('styleSheetChangeEventsEnabled');
end;

function TJSDocument._GetshadowRootAttachedEventEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('shadowRootAttachedEventEnabled');
end;

function TJSDocument._GetcontentLanguage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('contentLanguage');
end;

function TJSDocument._GetdocumentLoadGroup: IJSnsILoadGroup;
begin
  Result:=ReadJSPropertyObject('documentLoadGroup',TJSnsILoadGroup) as IJSnsILoadGroup;
end;

function TJSDocument._GetmozDocumentURIIfNotForErrorPages: IJSURI;
begin
  Result:=ReadJSPropertyObject('mozDocumentURIIfNotForErrorPages',TJSURI) as IJSURI;
end;

function TJSDocument._GetdocumentReadyForIdle: IJSPromise;
begin
  Result:=ReadJSPropertyObject('documentReadyForIdle',TJSPromise) as IJSPromise;
end;

function TJSDocument._GetcommandDispatcher: IJSXULCommandDispatcher;
begin
  Result:=ReadJSPropertyObject('commandDispatcher',TJSXULCommandDispatcher) as IJSXULCommandDispatcher;
end;

function TJSDocument._GetdevToolsWatchingDOMMutations: Boolean;
begin
  Result:=ReadJSPropertyBoolean('devToolsWatchingDOMMutations');
end;

function TJSDocument._GetisSrcdocDocument: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isSrcdocDocument');
end;

function TJSDocument._GetsandboxFlagsAsString: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('sandboxFlagsAsString');
end;

function TJSDocument._GetautoplayPolicy: TDocumentAutoplayPolicy;
begin
  Result:=ReadJSPropertyUnicodeString('autoplayPolicy');
end;

function TJSDocument._GetuserHasInteracted: Boolean;
begin
  Result:=ReadJSPropertyBoolean('userHasInteracted');
end;

function TJSDocument._GethasBeenUserGestureActivated: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasBeenUserGestureActivated');
end;

function TJSDocument._GethasValidTransientUserGestureActivation: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasValidTransientUserGestureActivation');
end;

function TJSDocument._Getcsp: IJSContentSecurityPolicy;
begin
  Result:=ReadJSPropertyObject('csp',TJSContentSecurityPolicy) as IJSContentSecurityPolicy;
end;

function TJSDocument._GetcspJSON: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('cspJSON');
end;

function TJSDocument._GetdocumentFlashClassification: TFlashClassification;
begin
  Result:=ReadJSPropertyUnicodeString('documentFlashClassification');
end;

function TJSDocument._GetblockedNodeByClassifierCount: Integer;
begin
  Result:=ReadJSPropertyLongInt('blockedNodeByClassifierCount');
end;

function TJSDocument._GetblockedNodesByClassifier: IJSNodeList;
begin
  Result:=ReadJSPropertyObject('blockedNodesByClassifier',TJSNodeList) as IJSNodeList;
end;

function TJSDocument._GetpermDelegateHandler: IJSnsIPermissionDelegateHandler;
begin
  Result:=ReadJSPropertyObject('permDelegateHandler',TJSnsIPermissionDelegateHandler) as IJSnsIPermissionDelegateHandler;
end;

function TJSDocument._GetisInitialDocument: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isInitialDocument');
end;

function TJSDocument._Getchildren: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('children',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument._GetfirstElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('firstElementChild',TJSElement) as IJSElement;
end;

function TJSDocument._GetlastElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('lastElementChild',TJSElement) as IJSElement;
end;

function TJSDocument._GetchildElementCount: LongWord;
begin
  Result:=ReadJSPropertyInt64('childElementCount');
end;

function TJSDocument._GetactiveElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('activeElement',TJSElement) as IJSElement;
end;

function TJSDocument._GetpointerLockElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('pointerLockElement',TJSElement) as IJSElement;
end;

function TJSDocument._GetfullscreenElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('fullscreenElement',TJSElement) as IJSElement;
end;

function TJSDocument._GetmozFullScreenElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('mozFullScreenElement',TJSElement) as IJSElement;
end;

procedure TJSDocument._Setdomain(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('domain',aValue);
end;

procedure TJSDocument._Setcookie(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('cookie',aValue);
end;

procedure TJSDocument._Settitle(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('title',aValue);
end;

procedure TJSDocument._Setdir(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('dir',aValue);
end;

procedure TJSDocument._Setbody(const aValue: IJSHTMLElement);
begin
  WriteJSPropertyObject('body',aValue);
end;

procedure TJSDocument._SetdesignMode(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('designMode',aValue);
end;

procedure TJSDocument._SetfgColor(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('fgColor',aValue);
end;

procedure TJSDocument._SetlinkColor(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('linkColor',aValue);
end;

procedure TJSDocument._SetvlinkColor(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('vlinkColor',aValue);
end;

procedure TJSDocument._SetalinkColor(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('alinkColor',aValue);
end;

procedure TJSDocument._SetbgColor(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('bgColor',aValue);
end;

procedure TJSDocument._SetallowDeprecatedTls(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('allowDeprecatedTls',aValue);
end;

procedure TJSDocument._SetselectedStyleSheetSet(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('selectedStyleSheetSet',aValue);
end;

procedure TJSDocument._SetstyleSheetChangeEventsEnabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('styleSheetChangeEventsEnabled',aValue);
end;

procedure TJSDocument._SetshadowRootAttachedEventEnabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('shadowRootAttachedEventEnabled',aValue);
end;

procedure TJSDocument._SetdevToolsWatchingDOMMutations(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('devToolsWatchingDOMMutations',aValue);
end;

function TJSDocument.getElementsByTagName(const aLocalName: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByTagName',[aLocalName],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument.getElementsByTagNameNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByTagNameNS',[aNamespace,aLocalName],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument.getElementsByClassName(const aClassNames: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByClassName',[aClassNames],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument.getElementById(const aElementId: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aElementId],TJSElement) as IJSElement;
end;

function TJSDocument.createElement(const aLocalName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createElement',[aLocalName,aOptions],TJSElement) as IJSElement;
end;

function TJSDocument.createElement(const aLocalName: UnicodeString): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createElement',[aLocalName],TJSElement) as IJSElement;
end;

function TJSDocument.createElement(const aLocalName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createElement',[aLocalName,aOptions],TJSElement) as IJSElement;
end;

function TJSDocument.createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createElementNS',[aNamespace,aQualifiedName,aOptions],TJSElement) as IJSElement;
end;

function TJSDocument.createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createElementNS',[aNamespace,aQualifiedName],TJSElement) as IJSElement;
end;

function TJSDocument.createElementNS(const aNamespace: UnicodeString; const aQualifiedName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createElementNS',[aNamespace,aQualifiedName,aOptions],TJSElement) as IJSElement;
end;

function TJSDocument.createDocumentFragment: IJSDocumentFragment;
begin
  Result:=InvokeJSObjectResult('createDocumentFragment',[],TJSDocumentFragment) as IJSDocumentFragment;
end;

function TJSDocument.createTextNode(const aData: UnicodeString): IJSText;
begin
  Result:=InvokeJSObjectResult('createTextNode',[aData],TJSText) as IJSText;
end;

function TJSDocument.createComment(const aData: UnicodeString): IJSComment;
begin
  Result:=InvokeJSObjectResult('createComment',[aData],TJSComment) as IJSComment;
end;

function TJSDocument.importNode(aNode: IJSNode; aDeep: Boolean): IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('importNode',[aNode,aDeep],TJSNode) as IJSNode;
end;

function TJSDocument.importNode(aNode: IJSNode): IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('importNode',[aNode],TJSNode) as IJSNode;
end;

function TJSDocument.adoptNode(aNode: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('adoptNode',[aNode],TJSNode) as IJSNode;
end;

function TJSDocument.createEvent(const aInterface_: UnicodeString): IJSEvent;
begin
  Result:=InvokeJSObjectResult('createEvent',[aInterface_],TJSEvent) as IJSEvent;
end;

function TJSDocument.createCDATASection(const aData: UnicodeString): IJSCDATASection;
begin
  Result:=InvokeJSObjectResult('createCDATASection',[aData],TJSCDATASection) as IJSCDATASection;
end;

function TJSDocument.createAttribute(const aName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('createAttribute',[aName],TJSAttr) as IJSAttr;
end;

function TJSDocument.createAttributeNS(const aNamespace: UnicodeString; const aName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('createAttributeNS',[aNamespace,aName],TJSAttr) as IJSAttr;
end;

function TJSDocument.getElementsByName(const aElementName: UnicodeString): IJSNodeList;
begin
  Result:=InvokeJSObjectResult('getElementsByName',[aElementName],TJSNodeList) as IJSNodeList;
end;

function TJSDocument.open(const aUnused1: UnicodeString; const aUnused2: UnicodeString): IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('open',[aUnused1,aUnused2],TJSDocument) as IJSDocument;
end;

function TJSDocument.open: IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('open',[],TJSDocument) as IJSDocument;
end;

function TJSDocument.open(const aUnused1: UnicodeString): IJSDocument; overload;
begin
  Result:=InvokeJSObjectResult('open',[aUnused1],TJSDocument) as IJSDocument;
end;

function TJSDocument.open(const aUrl: UnicodeString; const aName: UnicodeString; const aFeatures: UnicodeString): IJSWindowProxy;
begin
  Result:=InvokeJSObjectResult('open',[aUrl,aName,aFeatures],TJSWindowProxy) as IJSWindowProxy;
end;

procedure TJSDocument.close;
begin
  InvokeJSNoResult('close',[]);
end;

procedure TJSDocument.write(const aText: UnicodeString){; ToDo:varargs};
begin
  InvokeJSNoResult('write',[aText]);
end;

procedure TJSDocument.writeln(const aText: UnicodeString){; ToDo:varargs};
begin
  InvokeJSNoResult('writeln',[aText]);
end;

function TJSDocument.hasFocus: Boolean;
begin
  Result:=InvokeJSBooleanResult('hasFocus',[]);
end;

function TJSDocument.execCommand(const aCommandId: UnicodeString; aShowUI: Boolean; const aValue: UnicodeString): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('execCommand',[aCommandId,aShowUI,aValue]);
end;

function TJSDocument.execCommand(const aCommandId: UnicodeString): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('execCommand',[aCommandId]);
end;

function TJSDocument.execCommand(const aCommandId: UnicodeString; aShowUI: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('execCommand',[aCommandId,aShowUI]);
end;

function TJSDocument.queryCommandEnabled(const aCommandId: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('queryCommandEnabled',[aCommandId]);
end;

function TJSDocument.queryCommandIndeterm(const aCommandId: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('queryCommandIndeterm',[aCommandId]);
end;

function TJSDocument.queryCommandState(const aCommandId: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('queryCommandState',[aCommandId]);
end;

function TJSDocument.queryCommandSupported(const aCommandId: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('queryCommandSupported',[aCommandId]);
end;

function TJSDocument.queryCommandValue(const aCommandId: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('queryCommandValue',[aCommandId]);
end;

procedure TJSDocument.releaseCapture;
begin
  InvokeJSNoResult('releaseCapture',[]);
end;

procedure TJSDocument.mozSetImageElement(const aImageElementId: UnicodeString; aImageElement: IJSElement);
begin
  InvokeJSNoResult('mozSetImageElement',[aImageElementId,aImageElement]);
end;

procedure TJSDocument.clear;
begin
  InvokeJSNoResult('clear',[]);
end;

procedure TJSDocument.captureEvents;
begin
  InvokeJSNoResult('captureEvents',[]);
end;

procedure TJSDocument.releaseEvents;
begin
  InvokeJSNoResult('releaseEvents',[]);
end;

procedure TJSDocument.exitPointerLock;
begin
  InvokeJSNoResult('exitPointerLock',[]);
end;

procedure TJSDocument.reloadWithHttpsOnlyException;
begin
  InvokeJSNoResult('reloadWithHttpsOnlyException',[]);
end;

procedure TJSDocument.enableStyleSheetsForSet(const aName: UnicodeString);
begin
  InvokeJSNoResult('enableStyleSheetsForSet',[aName]);
end;

function TJSDocument.caretPositionFromPoint(aX: Single; aY: Single): IJSCaretPosition;
begin
  Result:=InvokeJSObjectResult('caretPositionFromPoint',[aX,aY],TJSCaretPosition) as IJSCaretPosition;
end;

function TJSDocument.querySelector(const aSelectors: UTF8String): IJSElement;
begin
  Result:=InvokeJSObjectResult('querySelector',[aSelectors],TJSElement) as IJSElement;
end;

function TJSDocument.querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
begin
  Result:=InvokeJSObjectResult('querySelectorAll',[aSelectors],TJSNodeList) as IJSNodeList;
end;

function TJSDocument.createXULElement(const aLocalName: UnicodeString; const aOptions: UnicodeString): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createXULElement',[aLocalName,aOptions],TJSElement) as IJSElement;
end;

function TJSDocument.createXULElement(const aLocalName: UnicodeString): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createXULElement',[aLocalName],TJSElement) as IJSElement;
end;

function TJSDocument.createXULElement(const aLocalName: UnicodeString; const aOptions: TJSElementCreationOptions): IJSElement; overload;
begin
  Result:=InvokeJSObjectResult('createXULElement',[aLocalName,aOptions],TJSElement) as IJSElement;
end;

procedure TJSDocument.blockUnblockOnload(aBlock: Boolean);
begin
  InvokeJSNoResult('blockUnblockOnload',[aBlock]);
end;

function TJSDocument.getSelection: IJSSelection;
begin
  Result:=InvokeJSObjectResult('getSelection',[],TJSSelection) as IJSSelection;
end;

procedure TJSDocument.notifyUserGestureActivation;
begin
  InvokeJSNoResult('notifyUserGestureActivation',[]);
end;

procedure TJSDocument.clearUserGestureActivation;
begin
  InvokeJSNoResult('clearUserGestureActivation',[]);
end;

function TJSDocument.consumeTransientUserGestureActivation: Boolean;
begin
  Result:=InvokeJSBooleanResult('consumeTransientUserGestureActivation',[]);
end;

procedure TJSDocument.setSuppressedEventListener(const aListener: TEventListener);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTEventListener);
  try
    InvokeJSNoResult('setSuppressedEventListener',[m]);
  finally
    m.free;
  end;
end;

procedure TJSDocument.setKeyPressEventModel(aKeyPressEventModel: Word);
begin
  InvokeJSNoResult('setKeyPressEventModel',[aKeyPressEventModel]);
end;

procedure TJSDocument.userInteractionForTesting;
begin
  InvokeJSNoResult('userInteractionForTesting',[]);
end;

procedure TJSDocument.setNotifyFetchSuccess(aShouldNotify: Boolean);
begin
  InvokeJSNoResult('setNotifyFetchSuccess',[aShouldNotify]);
end;

procedure TJSDocument.setNotifyFormOrPasswordRemoved(aShouldNotify: Boolean);
begin
  InvokeJSNoResult('setNotifyFormOrPasswordRemoved',[aShouldNotify]);
end;

function TJSDocument.getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttribute',[aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocument.getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttributeNS',[aNamespaceURI,aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

procedure TJSDocument.prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSDocument.prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSDocument.append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSDocument.append(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSDocument.replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

procedure TJSDocument.replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

function TJSDocument.elementFromPoint(aX: Single; aY: Single): IJSElement;
begin
  Result:=InvokeJSObjectResult('elementFromPoint',[aX,aY],TJSElement) as IJSElement;
end;

function TJSDocument.elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
begin
  Result:=InvokeJSObjectResult('elementsFromPoint',[aX,aY],TJSArray) as TJSElementDynArray;
end;

function TJSDocument.nodeFromPoint(aX: Single; aY: Single): IJSNode;
begin
  Result:=InvokeJSObjectResult('nodeFromPoint',[aX,aY],TJSNode) as IJSNode;
end;

function TJSDocument.nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
begin
  Result:=InvokeJSObjectResult('nodesFromPoint',[aX,aY],TJSArray) as TJSNodeDynArray;
end;

class function TJSDocument.Cast(Intf: IJSObject): IJSDocument;
begin
  Result:=TJSDocument.JOBCast(Intf);
end;

function TJSDocumentType._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSDocumentType._GetpublicId: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('publicId');
end;

function TJSDocumentType._GetsystemId: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('systemId');
end;

procedure TJSDocumentType.before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSDocumentType.before(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSDocumentType.after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSDocumentType.after(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSDocumentType.replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSDocumentType.replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSDocumentType.remove;
begin
  InvokeJSNoResult('remove',[]);
end;

class function TJSDocumentType.Cast(Intf: IJSObject): IJSDocumentType;
begin
  Result:=TJSDocumentType.JOBCast(Intf);
end;

function TJSElement._GetnamespaceURI: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('namespaceURI');
end;

function TJSElement._Getprefix: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('prefix');
end;

function TJSElement._GetlocalName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('localName');
end;

function TJSElement._GettagName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('tagName');
end;

function TJSElement._Getid: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('id');
end;

function TJSElement._GetclassName_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('className');
end;

function TJSElement._GetclassList: IJSDOMTokenList;
begin
  Result:=ReadJSPropertyObject('classList',TJSDOMTokenList) as IJSDOMTokenList;
end;

function TJSElement._Getpart: IJSDOMTokenList;
begin
  Result:=ReadJSPropertyObject('part',TJSDOMTokenList) as IJSDOMTokenList;
end;

function TJSElement._Getattributes: IJSNamedNodeMap;
begin
  Result:=ReadJSPropertyObject('attributes',TJSNamedNodeMap) as IJSNamedNodeMap;
end;

function TJSElement._GetfontSizeInflation: Single;
begin
  Result:=ReadJSPropertyDouble('fontSizeInflation');
end;

function TJSElement._GetimplementedPseudoElement: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('implementedPseudoElement');
end;

function TJSElement._GetscrollTop: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollTop');
end;

function TJSElement._GetscrollLeft: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollLeft');
end;

function TJSElement._GetscrollWidth: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollWidth');
end;

function TJSElement._GetscrollHeight: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollHeight');
end;

function TJSElement._GetclientTop: Integer;
begin
  Result:=ReadJSPropertyLongInt('clientTop');
end;

function TJSElement._GetclientLeft: Integer;
begin
  Result:=ReadJSPropertyLongInt('clientLeft');
end;

function TJSElement._GetclientWidth: Integer;
begin
  Result:=ReadJSPropertyLongInt('clientWidth');
end;

function TJSElement._GetclientHeight: Integer;
begin
  Result:=ReadJSPropertyLongInt('clientHeight');
end;

function TJSElement._GetscreenX: Integer;
begin
  Result:=ReadJSPropertyLongInt('screenX');
end;

function TJSElement._GetscreenY: Integer;
begin
  Result:=ReadJSPropertyLongInt('screenY');
end;

function TJSElement._Getscreen: IJSnsIScreen;
begin
  Result:=ReadJSPropertyObject('screen',TJSnsIScreen) as IJSnsIScreen;
end;

function TJSElement._GetscrollTopMin: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollTopMin');
end;

function TJSElement._GetscrollTopMax: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollTopMax');
end;

function TJSElement._GetscrollLeftMin: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollLeftMin');
end;

function TJSElement._GetscrollLeftMax: Integer;
begin
  Result:=ReadJSPropertyLongInt('scrollLeftMax');
end;

function TJSElement._GetinnerHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerHTML');
end;

function TJSElement._GetouterHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('outerHTML');
end;

function TJSElement._Getslot: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('slot');
end;

function TJSElement._GethasVisibleScrollbars: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasVisibleScrollbars');
end;

function TJSElement._GetclientHeightDouble: Double;
begin
  Result:=ReadJSPropertyDouble('clientHeightDouble');
end;

function TJSElement._GetclientWidthDouble: Double;
begin
  Result:=ReadJSPropertyDouble('clientWidthDouble');
end;

function TJSElement._GetfirstLineBoxBSize: Double;
begin
  Result:=ReadJSPropertyDouble('firstLineBoxBSize');
end;

function TJSElement._GetpreviousElementSibling: IJSElement;
begin
  Result:=ReadJSPropertyObject('previousElementSibling',TJSElement) as IJSElement;
end;

function TJSElement._GetnextElementSibling: IJSElement;
begin
  Result:=ReadJSPropertyObject('nextElementSibling',TJSElement) as IJSElement;
end;

function TJSElement._Getchildren: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('children',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSElement._GetfirstElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('firstElementChild',TJSElement) as IJSElement;
end;

function TJSElement._GetlastElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('lastElementChild',TJSElement) as IJSElement;
end;

function TJSElement._GetchildElementCount: LongWord;
begin
  Result:=ReadJSPropertyInt64('childElementCount');
end;

procedure TJSElement._Setid(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('id',aValue);
end;

procedure TJSElement._SetclassName_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('className',aValue);
end;

procedure TJSElement._SetscrollTop(const aValue: Integer);
begin
  WriteJSPropertyLongInt('scrollTop',aValue);
end;

procedure TJSElement._SetscrollLeft(const aValue: Integer);
begin
  WriteJSPropertyLongInt('scrollLeft',aValue);
end;

procedure TJSElement._SetinnerHTML(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerHTML',aValue);
end;

procedure TJSElement._SetouterHTML(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('outerHTML',aValue);
end;

procedure TJSElement._Setslot(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('slot',aValue);
end;

function TJSElement.getAttributeNames: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('getAttributeNames',[],TJSArray) as TUnicodeStringDynArray;
end;

function TJSElement.getAttribute(const aName: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getAttribute',[aName]);
end;

function TJSElement.getAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getAttributeNS',[aNamespace,aLocalName]);
end;

function TJSElement.toggleAttribute(const aName: UnicodeString; aForce: Boolean): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('toggleAttribute',[aName,aForce]);
end;

function TJSElement.toggleAttribute(const aName: UnicodeString): Boolean; overload;
begin
  Result:=InvokeJSBooleanResult('toggleAttribute',[aName]);
end;

procedure TJSElement.setAttribute(const aName: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('setAttribute',[aName,aValue]);
end;

procedure TJSElement.setAttributeNS(const aNamespace: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('setAttributeNS',[aNamespace,aName,aValue]);
end;

procedure TJSElement.removeAttribute(const aName: UnicodeString);
begin
  InvokeJSNoResult('removeAttribute',[aName]);
end;

procedure TJSElement.removeAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString);
begin
  InvokeJSNoResult('removeAttributeNS',[aNamespace,aLocalName]);
end;

function TJSElement.hasAttribute(const aName: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('hasAttribute',[aName]);
end;

function TJSElement.hasAttributeNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('hasAttributeNS',[aNamespace,aLocalName]);
end;

function TJSElement.hasAttributes: Boolean;
begin
  Result:=InvokeJSBooleanResult('hasAttributes',[]);
end;

function TJSElement.closest(const aSelector: UTF8String): IJSElement;
begin
  Result:=InvokeJSObjectResult('closest',[aSelector],TJSElement) as IJSElement;
end;

function TJSElement.matches(const aSelector: UTF8String): Boolean;
begin
  Result:=InvokeJSBooleanResult('matches',[aSelector]);
end;

function TJSElement.webkitMatchesSelector(const aSelector: UTF8String): Boolean;
begin
  Result:=InvokeJSBooleanResult('webkitMatchesSelector',[aSelector]);
end;

function TJSElement.getElementsByTagName(const aLocalName: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByTagName',[aLocalName],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSElement.getElementsByTagNameNS(const aNamespace: UnicodeString; const aLocalName: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByTagNameNS',[aNamespace,aLocalName],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSElement.getElementsByClassName(const aClassNames: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByClassName',[aClassNames],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSElement.insertAdjacentElement(const aWhere: UnicodeString; aElement: IJSElement): IJSElement;
begin
  Result:=InvokeJSObjectResult('insertAdjacentElement',[aWhere,aElement],TJSElement) as IJSElement;
end;

procedure TJSElement.insertAdjacentText(const aWhere: UnicodeString; const aData: UnicodeString);
begin
  InvokeJSNoResult('insertAdjacentText',[aWhere,aData]);
end;

function TJSElement.mozMatchesSelector(const aSelector: UTF8String): Boolean;
begin
  Result:=InvokeJSBooleanResult('mozMatchesSelector',[aSelector]);
end;

procedure TJSElement.setPointerCapture(aPointerId: Integer);
begin
  InvokeJSNoResult('setPointerCapture',[aPointerId]);
end;

procedure TJSElement.releasePointerCapture(aPointerId: Integer);
begin
  InvokeJSNoResult('releasePointerCapture',[aPointerId]);
end;

function TJSElement.hasPointerCapture(aPointerId: Integer): Boolean;
begin
  Result:=InvokeJSBooleanResult('hasPointerCapture',[aPointerId]);
end;

procedure TJSElement.setCapture(aRetargetToElement: Boolean); overload;
begin
  InvokeJSNoResult('setCapture',[aRetargetToElement]);
end;

procedure TJSElement.setCapture; overload;
begin
  InvokeJSNoResult('setCapture',[]);
end;

procedure TJSElement.releaseCapture;
begin
  InvokeJSNoResult('releaseCapture',[]);
end;

procedure TJSElement.setCaptureAlways(aRetargetToElement: Boolean); overload;
begin
  InvokeJSNoResult('setCaptureAlways',[aRetargetToElement]);
end;

procedure TJSElement.setCaptureAlways; overload;
begin
  InvokeJSNoResult('setCaptureAlways',[]);
end;

function TJSElement.getAttributeNode(const aName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('getAttributeNode',[aName],TJSAttr) as IJSAttr;
end;

function TJSElement.setAttributeNode(aNewAttr: IJSAttr): IJSAttr;
begin
  Result:=InvokeJSObjectResult('setAttributeNode',[aNewAttr],TJSAttr) as IJSAttr;
end;

function TJSElement.removeAttributeNode(aOldAttr: IJSAttr): IJSAttr;
begin
  Result:=InvokeJSObjectResult('removeAttributeNode',[aOldAttr],TJSAttr) as IJSAttr;
end;

function TJSElement.getAttributeNodeNS(const aNamespaceURI: UnicodeString; const aLocalName: UnicodeString): IJSAttr;
begin
  Result:=InvokeJSObjectResult('getAttributeNodeNS',[aNamespaceURI,aLocalName],TJSAttr) as IJSAttr;
end;

function TJSElement.setAttributeNodeNS(aNewAttr: IJSAttr): IJSAttr;
begin
  Result:=InvokeJSObjectResult('setAttributeNodeNS',[aNewAttr],TJSAttr) as IJSAttr;
end;

function TJSElement.getClientRects: IJSDOMRectList;
begin
  Result:=InvokeJSObjectResult('getClientRects',[],TJSDOMRectList) as IJSDOMRectList;
end;

function TJSElement.getBoundingClientRect: IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('getBoundingClientRect',[],TJSDOMRect) as IJSDOMRect;
end;

procedure TJSElement.scrollIntoView(arg: Boolean); overload;
begin
  InvokeJSNoResult('scrollIntoView',[arg]);
end;

procedure TJSElement.scrollIntoView; overload;
begin
  InvokeJSNoResult('scrollIntoView',[]);
end;

procedure TJSElement.scrollIntoView(const arg: TJSScrollIntoViewOptions); overload;
begin
  InvokeJSNoResult('scrollIntoView',[arg]);
end;

procedure TJSElement.scroll(aX: Double; aY: Double);
begin
  InvokeJSNoResult('scroll',[aX,aY]);
end;

procedure TJSElement.scroll(const aOptions: TJSScrollToOptions); overload;
begin
  InvokeJSNoResult('scroll',[aOptions]);
end;

procedure TJSElement.scroll; overload;
begin
  InvokeJSNoResult('scroll',[]);
end;

procedure TJSElement.scrollTo(aX: Double; aY: Double);
begin
  InvokeJSNoResult('scrollTo',[aX,aY]);
end;

procedure TJSElement.scrollTo(const aOptions: TJSScrollToOptions); overload;
begin
  InvokeJSNoResult('scrollTo',[aOptions]);
end;

procedure TJSElement.scrollTo; overload;
begin
  InvokeJSNoResult('scrollTo',[]);
end;

procedure TJSElement.scrollBy(aX: Double; aY: Double);
begin
  InvokeJSNoResult('scrollBy',[aX,aY]);
end;

procedure TJSElement.scrollBy(const aOptions: TJSScrollToOptions); overload;
begin
  InvokeJSNoResult('scrollBy',[aOptions]);
end;

procedure TJSElement.scrollBy; overload;
begin
  InvokeJSNoResult('scrollBy',[]);
end;

procedure TJSElement.mozScrollSnap;
begin
  InvokeJSNoResult('mozScrollSnap',[]);
end;

procedure TJSElement.insertAdjacentHTML(const aPosition: UnicodeString; const aText: UnicodeString);
begin
  InvokeJSNoResult('insertAdjacentHTML',[aPosition,aText]);
end;

function TJSElement.querySelector(const aSelectors: UTF8String): IJSElement;
begin
  Result:=InvokeJSObjectResult('querySelector',[aSelectors],TJSElement) as IJSElement;
end;

function TJSElement.querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
begin
  Result:=InvokeJSObjectResult('querySelectorAll',[aSelectors],TJSNodeList) as IJSNodeList;
end;

procedure TJSElement.requestPointerLock;
begin
  InvokeJSNoResult('requestPointerLock',[]);
end;

function TJSElement.hasGridFragments: Boolean;
begin
  Result:=InvokeJSBooleanResult('hasGridFragments',[]);
end;

function TJSElement.getElementsWithGrid: TJSElementDynArray;
begin
  Result:=InvokeJSObjectResult('getElementsWithGrid',[],TJSArray) as TJSElementDynArray;
end;

procedure TJSElement.setAttributeDevtools(const aName: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('setAttributeDevtools',[aName,aValue]);
end;

procedure TJSElement.setAttributeDevtoolsNS(const aNamespace: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString);
begin
  InvokeJSNoResult('setAttributeDevtoolsNS',[aNamespace,aName,aValue]);
end;

procedure TJSElement.setHTML(const aInnerHTML: UnicodeString; const aOptions: TJSSetHTMLOptions); overload;
begin
  InvokeJSNoResult('setHTML',[aInnerHTML,aOptions]);
end;

procedure TJSElement.setHTML(const aInnerHTML: UnicodeString); overload;
begin
  InvokeJSNoResult('setHTML',[aInnerHTML]);
end;

procedure TJSElement.before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSElement.before(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSElement.after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSElement.after(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSElement.replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSElement.replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSElement.remove;
begin
  InvokeJSNoResult('remove',[]);
end;

function TJSElement.getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttribute',[aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSElement.getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttributeNS',[aNamespaceURI,aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

procedure TJSElement.prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSElement.prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSElement.append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSElement.append(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSElement.replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

procedure TJSElement.replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

class function TJSElement.Cast(Intf: IJSObject): IJSElement;
begin
  Result:=TJSElement.JOBCast(Intf);
end;

function TJSDocumentFragment._Getchildren: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('children',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocumentFragment._GetfirstElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('firstElementChild',TJSElement) as IJSElement;
end;

function TJSDocumentFragment._GetlastElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('lastElementChild',TJSElement) as IJSElement;
end;

function TJSDocumentFragment._GetchildElementCount: LongWord;
begin
  Result:=ReadJSPropertyInt64('childElementCount');
end;

function TJSDocumentFragment.getElementById(const aElementId: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aElementId],TJSElement) as IJSElement;
end;

function TJSDocumentFragment.querySelector(const aSelectors: UTF8String): IJSElement;
begin
  Result:=InvokeJSObjectResult('querySelector',[aSelectors],TJSElement) as IJSElement;
end;

function TJSDocumentFragment.querySelectorAll(const aSelectors: UTF8String): IJSNodeList;
begin
  Result:=InvokeJSObjectResult('querySelectorAll',[aSelectors],TJSNodeList) as IJSNodeList;
end;

function TJSDocumentFragment.getElementsByAttribute(const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttribute',[aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSDocumentFragment.getElementsByAttributeNS(const aNamespaceURI: UnicodeString; const aName: UnicodeString; const aValue: UnicodeString): IJSHTMLCollection;
begin
  Result:=InvokeJSObjectResult('getElementsByAttributeNS',[aNamespaceURI,aName,aValue],TJSHTMLCollection) as IJSHTMLCollection;
end;

procedure TJSDocumentFragment.prepend(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSDocumentFragment.prepend(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('prepend',[aNodes]);
end;

procedure TJSDocumentFragment.append(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSDocumentFragment.append(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('append',[aNodes]);
end;

procedure TJSDocumentFragment.replaceChildren(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

procedure TJSDocumentFragment.replaceChildren(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceChildren',[aNodes]);
end;

class function TJSDocumentFragment.Cast(Intf: IJSObject): IJSDocumentFragment;
begin
  Result:=TJSDocumentFragment.JOBCast(Intf);
end;

function TJSAttr._GetlocalName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('localName');
end;

function TJSAttr._Getvalue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('value');
end;

function TJSAttr._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSAttr._GetnamespaceURI: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('namespaceURI');
end;

function TJSAttr._Getprefix: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('prefix');
end;

function TJSAttr._Getspecified: Boolean;
begin
  Result:=ReadJSPropertyBoolean('specified');
end;

function TJSAttr._GetownerElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('ownerElement',TJSElement) as IJSElement;
end;

procedure TJSAttr._Setvalue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('value',aValue);
end;

class function TJSAttr.Cast(Intf: IJSObject): IJSAttr;
begin
  Result:=TJSAttr.JOBCast(Intf);
end;

function TJSCharacterData._Getdata: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('data');
end;

function TJSCharacterData._Getlength_: LongWord;
begin
  Result:=ReadJSPropertyInt64('length');
end;

function TJSCharacterData._GetpreviousElementSibling: IJSElement;
begin
  Result:=ReadJSPropertyObject('previousElementSibling',TJSElement) as IJSElement;
end;

function TJSCharacterData._GetnextElementSibling: IJSElement;
begin
  Result:=ReadJSPropertyObject('nextElementSibling',TJSElement) as IJSElement;
end;

procedure TJSCharacterData._Setdata(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('data',aValue);
end;

function TJSCharacterData.substringData(aOffset: LongWord; aCount: LongWord): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('substringData',[aOffset,aCount]);
end;

procedure TJSCharacterData.appendData(const aData: UnicodeString);
begin
  InvokeJSNoResult('appendData',[aData]);
end;

procedure TJSCharacterData.insertData(aOffset: LongWord; const aData: UnicodeString);
begin
  InvokeJSNoResult('insertData',[aOffset,aData]);
end;

procedure TJSCharacterData.deleteData(aOffset: LongWord; aCount: LongWord);
begin
  InvokeJSNoResult('deleteData',[aOffset,aCount]);
end;

procedure TJSCharacterData.replaceData(aOffset: LongWord; aCount: LongWord; const aData: UnicodeString);
begin
  InvokeJSNoResult('replaceData',[aOffset,aCount,aData]);
end;

procedure TJSCharacterData.before(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSCharacterData.before(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('before',[aNodes]);
end;

procedure TJSCharacterData.after(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSCharacterData.after(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('after',[aNodes]);
end;

procedure TJSCharacterData.replaceWith(const aNodes: UnicodeString){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSCharacterData.replaceWith(aNodes: IJSNode){; ToDo:varargs}; overload;
begin
  InvokeJSNoResult('replaceWith',[aNodes]);
end;

procedure TJSCharacterData.remove;
begin
  InvokeJSNoResult('remove',[]);
end;

class function TJSCharacterData.Cast(Intf: IJSObject): IJSCharacterData;
begin
  Result:=TJSCharacterData.JOBCast(Intf);
end;

function TJSText._GetwholeText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('wholeText');
end;

function TJSText.splitText(aOffset: LongWord): IJSText;
begin
  Result:=InvokeJSObjectResult('splitText',[aOffset],TJSText) as IJSText;
end;

class function TJSText.Cast(Intf: IJSObject): IJSText;
begin
  Result:=TJSText.JOBCast(Intf);
end;

class function TJSComment.Cast(Intf: IJSObject): IJSComment;
begin
  Result:=TJSComment.JOBCast(Intf);
end;

function TJSHTMLElement._Gettitle: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('title');
end;

function TJSHTMLElement._Getlang: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('lang');
end;

function TJSHTMLElement._Getdir: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('dir');
end;

function TJSHTMLElement._GetinnerText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerText');
end;

function TJSHTMLElement._GetouterText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('outerText');
end;

function TJSHTMLElement._Gethidden: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hidden');
end;

function TJSHTMLElement._Getinert: Boolean;
begin
  Result:=ReadJSPropertyBoolean('inert');
end;

function TJSHTMLElement._GetaccessKey: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('accessKey');
end;

function TJSHTMLElement._GetaccessKeyLabel: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('accessKeyLabel');
end;

function TJSHTMLElement._Getdraggable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('draggable');
end;

function TJSHTMLElement._GetcontentEditable: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('contentEditable');
end;

function TJSHTMLElement._GetisContentEditable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isContentEditable');
end;

function TJSHTMLElement._GetcontextMenu: IJSHTMLMenuElement;
begin
  Result:=ReadJSPropertyObject('contextMenu',TJSHTMLMenuElement) as IJSHTMLMenuElement;
end;

function TJSHTMLElement._Getspellcheck: Boolean;
begin
  Result:=ReadJSPropertyBoolean('spellcheck');
end;

function TJSHTMLElement._GetinputMode: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('inputMode');
end;

function TJSHTMLElement._GetenterKeyHint: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('enterKeyHint');
end;

function TJSHTMLElement._Getautocapitalize: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('autocapitalize');
end;

function TJSHTMLElement._Getnonce: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nonce');
end;

function TJSHTMLElement._GetoffsetParent: IJSElement;
begin
  Result:=ReadJSPropertyObject('offsetParent',TJSElement) as IJSElement;
end;

function TJSHTMLElement._GetoffsetTop: Integer;
begin
  Result:=ReadJSPropertyLongInt('offsetTop');
end;

function TJSHTMLElement._GetoffsetLeft: Integer;
begin
  Result:=ReadJSPropertyLongInt('offsetLeft');
end;

function TJSHTMLElement._GetoffsetWidth: Integer;
begin
  Result:=ReadJSPropertyLongInt('offsetWidth');
end;

function TJSHTMLElement._GetoffsetHeight: Integer;
begin
  Result:=ReadJSPropertyLongInt('offsetHeight');
end;

function TJSHTMLElement._Getinternals: IJSElementInternals;
begin
  Result:=ReadJSPropertyObject('internals',TJSElementInternals) as IJSElementInternals;
end;

function TJSHTMLElement._GetisFormAssociatedCustomElements: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isFormAssociatedCustomElements');
end;

function TJSHTMLElement._Getdataset: IJSDOMStringMap;
begin
  Result:=ReadJSPropertyObject('dataset',TJSDOMStringMap) as IJSDOMStringMap;
end;

function TJSHTMLElement._GettabIndex: Integer;
begin
  Result:=ReadJSPropertyLongInt('tabIndex');
end;

function TJSHTMLElement._Getstyle: IJSCSSStyleDeclaration;
begin
  Result:=ReadJSPropertyObject('style',TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

procedure TJSHTMLElement._Settitle(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('title',aValue);
end;

procedure TJSHTMLElement._Setlang(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('lang',aValue);
end;

procedure TJSHTMLElement._Setdir(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('dir',aValue);
end;

procedure TJSHTMLElement._SetinnerText(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerText',aValue);
end;

procedure TJSHTMLElement._SetouterText(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('outerText',aValue);
end;

procedure TJSHTMLElement._Sethidden(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('hidden',aValue);
end;

procedure TJSHTMLElement._Setinert(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('inert',aValue);
end;

procedure TJSHTMLElement._SetaccessKey(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('accessKey',aValue);
end;

procedure TJSHTMLElement._Setdraggable(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('draggable',aValue);
end;

procedure TJSHTMLElement._SetcontentEditable(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('contentEditable',aValue);
end;

procedure TJSHTMLElement._Setspellcheck(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('spellcheck',aValue);
end;

procedure TJSHTMLElement._SetinputMode(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('inputMode',aValue);
end;

procedure TJSHTMLElement._SetenterKeyHint(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('enterKeyHint',aValue);
end;

procedure TJSHTMLElement._Setautocapitalize(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('autocapitalize',aValue);
end;

procedure TJSHTMLElement._Setnonce(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('nonce',aValue);
end;

procedure TJSHTMLElement._SettabIndex(const aValue: Integer);
begin
  WriteJSPropertyLongInt('tabIndex',aValue);
end;

procedure TJSHTMLElement.click;
begin
  InvokeJSNoResult('click',[]);
end;

function TJSHTMLElement.attachInternals: IJSElementInternals;
begin
  Result:=InvokeJSObjectResult('attachInternals',[],TJSElementInternals) as IJSElementInternals;
end;

procedure TJSHTMLElement.focus(const aOptions: TJSFocusOptions); overload;
begin
  InvokeJSNoResult('focus',[aOptions]);
end;

procedure TJSHTMLElement.focus; overload;
begin
  InvokeJSNoResult('focus',[]);
end;

procedure TJSHTMLElement.blur;
begin
  InvokeJSNoResult('blur',[]);
end;

class function TJSHTMLElement.Cast(Intf: IJSObject): IJSHTMLElement;
begin
  Result:=TJSHTMLElement.JOBCast(Intf);
end;

function TJSShadowRoot._Getmode: TShadowRootMode;
begin
  Result:=ReadJSPropertyUnicodeString('mode');
end;

function TJSShadowRoot._GetdelegatesFocus: Boolean;
begin
  Result:=ReadJSPropertyBoolean('delegatesFocus');
end;

function TJSShadowRoot._GetslotAssignment: TSlotAssignmentMode;
begin
  Result:=ReadJSPropertyUnicodeString('slotAssignment');
end;

function TJSShadowRoot._Gethost: IJSElement;
begin
  Result:=ReadJSPropertyObject('host',TJSElement) as IJSElement;
end;

function TJSShadowRoot._GetinnerHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerHTML');
end;

function TJSShadowRoot._GetactiveElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('activeElement',TJSElement) as IJSElement;
end;

function TJSShadowRoot._GetpointerLockElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('pointerLockElement',TJSElement) as IJSElement;
end;

function TJSShadowRoot._GetfullscreenElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('fullscreenElement',TJSElement) as IJSElement;
end;

function TJSShadowRoot._GetmozFullScreenElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('mozFullScreenElement',TJSElement) as IJSElement;
end;

procedure TJSShadowRoot._SetinnerHTML(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerHTML',aValue);
end;

function TJSShadowRoot.getElementById(const aElementId: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aElementId],TJSElement) as IJSElement;
end;

function TJSShadowRoot.importNodeAndAppendChildAt(aParentNode: IJSNode; aNode: IJSNode; aDeep: Boolean): IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('importNodeAndAppendChildAt',[aParentNode,aNode,aDeep],TJSNode) as IJSNode;
end;

function TJSShadowRoot.importNodeAndAppendChildAt(aParentNode: IJSNode; aNode: IJSNode): IJSNode; overload;
begin
  Result:=InvokeJSObjectResult('importNodeAndAppendChildAt',[aParentNode,aNode],TJSNode) as IJSNode;
end;

function TJSShadowRoot.createElementAndAppendChildAt(aParentNode: IJSNode; const aLocalName: UnicodeString): IJSNode;
begin
  Result:=InvokeJSObjectResult('createElementAndAppendChildAt',[aParentNode,aLocalName],TJSNode) as IJSNode;
end;

procedure TJSShadowRoot.setIsUAWidget;
begin
  InvokeJSNoResult('setIsUAWidget',[]);
end;

function TJSShadowRoot.isUAWidget: Boolean;
begin
  Result:=InvokeJSBooleanResult('isUAWidget',[]);
end;

function TJSShadowRoot.elementFromPoint(aX: Single; aY: Single): IJSElement;
begin
  Result:=InvokeJSObjectResult('elementFromPoint',[aX,aY],TJSElement) as IJSElement;
end;

function TJSShadowRoot.elementsFromPoint(aX: Single; aY: Single): TJSElementDynArray;
begin
  Result:=InvokeJSObjectResult('elementsFromPoint',[aX,aY],TJSArray) as TJSElementDynArray;
end;

function TJSShadowRoot.nodeFromPoint(aX: Single; aY: Single): IJSNode;
begin
  Result:=InvokeJSObjectResult('nodeFromPoint',[aX,aY],TJSNode) as IJSNode;
end;

function TJSShadowRoot.nodesFromPoint(aX: Single; aY: Single): TJSNodeDynArray;
begin
  Result:=InvokeJSObjectResult('nodesFromPoint',[aX,aY],TJSArray) as TJSNodeDynArray;
end;

class function TJSShadowRoot.Cast(Intf: IJSObject): IJSShadowRoot;
begin
  Result:=TJSShadowRoot.JOBCast(Intf);
end;

function TJSSVGElement._Getid: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('id');
end;

function TJSSVGElement._GetownerSVGElement: IJSSVGSVGElement;
begin
  Result:=ReadJSPropertyObject('ownerSVGElement',TJSSVGSVGElement) as IJSSVGSVGElement;
end;

function TJSSVGElement._GetviewportElement: IJSSVGElement;
begin
  Result:=ReadJSPropertyObject('viewportElement',TJSSVGElement) as IJSSVGElement;
end;

function TJSSVGElement._Getnonce: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nonce');
end;

function TJSSVGElement._Getdataset: IJSDOMStringMap;
begin
  Result:=ReadJSPropertyObject('dataset',TJSDOMStringMap) as IJSDOMStringMap;
end;

function TJSSVGElement._GettabIndex: Integer;
begin
  Result:=ReadJSPropertyLongInt('tabIndex');
end;

function TJSSVGElement._Getstyle: IJSCSSStyleDeclaration;
begin
  Result:=ReadJSPropertyObject('style',TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

procedure TJSSVGElement._Setid(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('id',aValue);
end;

procedure TJSSVGElement._Setnonce(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('nonce',aValue);
end;

procedure TJSSVGElement._SettabIndex(const aValue: Integer);
begin
  WriteJSPropertyLongInt('tabIndex',aValue);
end;

procedure TJSSVGElement.focus(const aOptions: TJSFocusOptions); overload;
begin
  InvokeJSNoResult('focus',[aOptions]);
end;

procedure TJSSVGElement.focus; overload;
begin
  InvokeJSNoResult('focus',[]);
end;

procedure TJSSVGElement.blur;
begin
  InvokeJSNoResult('blur',[]);
end;

class function TJSSVGElement.Cast(Intf: IJSObject): IJSSVGElement;
begin
  Result:=TJSSVGElement.JOBCast(Intf);
end;

class function TJSCDATASection.Cast(Intf: IJSObject): IJSCDATASection;
begin
  Result:=TJSCDATASection.JOBCast(Intf);
end;

class function TJSHTMLUnknownElement.Cast(Intf: IJSObject): IJSHTMLUnknownElement;
begin
  Result:=TJSHTMLUnknownElement.JOBCast(Intf);
end;

class function TJSHTMLHeadElement.Cast(Intf: IJSObject): IJSHTMLHeadElement;
begin
  Result:=TJSHTMLHeadElement.JOBCast(Intf);
end;

function TJSHTMLMenuElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLMenuElement._Getlabel_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('label');
end;

function TJSHTMLMenuElement._Getcompact: Boolean;
begin
  Result:=ReadJSPropertyBoolean('compact');
end;

procedure TJSHTMLMenuElement._Settype_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSHTMLMenuElement._Setlabel_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('label',aValue);
end;

procedure TJSHTMLMenuElement._Setcompact(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('compact',aValue);
end;

procedure TJSHTMLMenuElement.sendShowEvent;
begin
  InvokeJSNoResult('sendShowEvent',[]);
end;

function TJSHTMLMenuElement.createBuilder: IJSMenuBuilder;
begin
  Result:=InvokeJSObjectResult('createBuilder',[],TJSMenuBuilder) as IJSMenuBuilder;
end;

procedure TJSHTMLMenuElement.build(aBuilder: IJSMenuBuilder);
begin
  InvokeJSNoResult('build',[aBuilder]);
end;

class function TJSHTMLMenuElement.Cast(Intf: IJSObject): IJSHTMLMenuElement;
begin
  Result:=TJSHTMLMenuElement.JOBCast(Intf);
end;

function TJSHTMLFormElement._GetacceptCharset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('acceptCharset');
end;

function TJSHTMLFormElement._Getaction: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('action');
end;

function TJSHTMLFormElement._Getautocomplete: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('autocomplete');
end;

function TJSHTMLFormElement._Getenctype: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('enctype');
end;

function TJSHTMLFormElement._Getencoding: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('encoding');
end;

function TJSHTMLFormElement._Getmethod: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('method');
end;

function TJSHTMLFormElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLFormElement._GetnoValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('noValidate');
end;

function TJSHTMLFormElement._Gettarget: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('target');
end;

function TJSHTMLFormElement._Getelements: IJSHTMLCollection;
begin
  Result:=ReadJSPropertyObject('elements',TJSHTMLCollection) as IJSHTMLCollection;
end;

function TJSHTMLFormElement._Getlength_: Integer;
begin
  Result:=ReadJSPropertyLongInt('length');
end;

procedure TJSHTMLFormElement._SetacceptCharset(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('acceptCharset',aValue);
end;

procedure TJSHTMLFormElement._Setaction(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('action',aValue);
end;

procedure TJSHTMLFormElement._Setautocomplete(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('autocomplete',aValue);
end;

procedure TJSHTMLFormElement._Setenctype(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('enctype',aValue);
end;

procedure TJSHTMLFormElement._Setencoding(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('encoding',aValue);
end;

procedure TJSHTMLFormElement._Setmethod(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('method',aValue);
end;

procedure TJSHTMLFormElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLFormElement._SetnoValidate(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('noValidate',aValue);
end;

procedure TJSHTMLFormElement._Settarget(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('target',aValue);
end;

procedure TJSHTMLFormElement.submit;
begin
  InvokeJSNoResult('submit',[]);
end;

procedure TJSHTMLFormElement.requestSubmit(aSubmitter: IJSHTMLElement); overload;
begin
  InvokeJSNoResult('requestSubmit',[aSubmitter]);
end;

procedure TJSHTMLFormElement.requestSubmit; overload;
begin
  InvokeJSNoResult('requestSubmit',[]);
end;

procedure TJSHTMLFormElement.reset;
begin
  InvokeJSNoResult('reset',[]);
end;

function TJSHTMLFormElement.checkValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('checkValidity',[]);
end;

function TJSHTMLFormElement.reportValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('reportValidity',[]);
end;

class function TJSHTMLFormElement.Cast(Intf: IJSObject): IJSHTMLFormElement;
begin
  Result:=TJSHTMLFormElement.JOBCast(Intf);
end;

function TJSSVGGraphicsElement._Getautofocus: Boolean;
begin
  Result:=ReadJSPropertyBoolean('autofocus');
end;

function TJSSVGGraphicsElement._Gettransform: IJSSVGAnimatedTransformList;
begin
  Result:=ReadJSPropertyObject('transform',TJSSVGAnimatedTransformList) as IJSSVGAnimatedTransformList;
end;

function TJSSVGGraphicsElement._GetnearestViewportElement: IJSSVGElement;
begin
  Result:=ReadJSPropertyObject('nearestViewportElement',TJSSVGElement) as IJSSVGElement;
end;

function TJSSVGGraphicsElement._GetfarthestViewportElement: IJSSVGElement;
begin
  Result:=ReadJSPropertyObject('farthestViewportElement',TJSSVGElement) as IJSSVGElement;
end;

function TJSSVGGraphicsElement._GetrequiredExtensions: IJSSVGStringList;
begin
  Result:=ReadJSPropertyObject('requiredExtensions',TJSSVGStringList) as IJSSVGStringList;
end;

function TJSSVGGraphicsElement._GetsystemLanguage: IJSSVGStringList;
begin
  Result:=ReadJSPropertyObject('systemLanguage',TJSSVGStringList) as IJSSVGStringList;
end;

procedure TJSSVGGraphicsElement._Setautofocus(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('autofocus',aValue);
end;

function TJSSVGGraphicsElement.getBBox(const aOptions: TJSSVGBoundingBoxOptions): IJSSVGRect; overload;
begin
  Result:=InvokeJSObjectResult('getBBox',[aOptions],TJSSVGRect) as IJSSVGRect;
end;

function TJSSVGGraphicsElement.getBBox: IJSSVGRect; overload;
begin
  Result:=InvokeJSObjectResult('getBBox',[],TJSSVGRect) as IJSSVGRect;
end;

function TJSSVGGraphicsElement.getCTM: IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('getCTM',[],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGGraphicsElement.getScreenCTM: IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('getScreenCTM',[],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGGraphicsElement.getTransformToElement(aElement: IJSSVGGraphicsElement): IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('getTransformToElement',[aElement],TJSSVGMatrix) as IJSSVGMatrix;
end;

class function TJSSVGGraphicsElement.Cast(Intf: IJSObject): IJSSVGGraphicsElement;
begin
  Result:=TJSSVGGraphicsElement.JOBCast(Intf);
end;

function TJSHTMLAnchorElement._Gettarget: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('target');
end;

function TJSHTMLAnchorElement._Getdownload: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('download');
end;

function TJSHTMLAnchorElement._Getping: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('ping');
end;

function TJSHTMLAnchorElement._Getrel: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('rel');
end;

function TJSHTMLAnchorElement._GetreferrerPolicy: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('referrerPolicy');
end;

function TJSHTMLAnchorElement._GetrelList: IJSDOMTokenList;
begin
  Result:=ReadJSPropertyObject('relList',TJSDOMTokenList) as IJSDOMTokenList;
end;

function TJSHTMLAnchorElement._Gethreflang: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hreflang');
end;

function TJSHTMLAnchorElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLAnchorElement._Gettext: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('text');
end;

function TJSHTMLAnchorElement._Getcoords: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('coords');
end;

function TJSHTMLAnchorElement._Getcharset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('charset');
end;

function TJSHTMLAnchorElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLAnchorElement._Getrev: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('rev');
end;

function TJSHTMLAnchorElement._Getshape: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('shape');
end;

function TJSHTMLAnchorElement._Gethref: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('href');
end;

function TJSHTMLAnchorElement._Getorigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('origin');
end;

function TJSHTMLAnchorElement._Getprotocol: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('protocol');
end;

function TJSHTMLAnchorElement._Getusername: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('username');
end;

function TJSHTMLAnchorElement._Getpassword: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('password');
end;

function TJSHTMLAnchorElement._Gethost: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('host');
end;

function TJSHTMLAnchorElement._Gethostname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hostname');
end;

function TJSHTMLAnchorElement._Getport: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('port');
end;

function TJSHTMLAnchorElement._Getpathname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('pathname');
end;

function TJSHTMLAnchorElement._Getsearch: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('search');
end;

function TJSHTMLAnchorElement._Gethash: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hash');
end;

procedure TJSHTMLAnchorElement._Settarget(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('target',aValue);
end;

procedure TJSHTMLAnchorElement._Setdownload(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('download',aValue);
end;

procedure TJSHTMLAnchorElement._Setping(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('ping',aValue);
end;

procedure TJSHTMLAnchorElement._Setrel(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('rel',aValue);
end;

procedure TJSHTMLAnchorElement._SetreferrerPolicy(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('referrerPolicy',aValue);
end;

procedure TJSHTMLAnchorElement._Sethreflang(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hreflang',aValue);
end;

procedure TJSHTMLAnchorElement._Settype_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSHTMLAnchorElement._Settext(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('text',aValue);
end;

procedure TJSHTMLAnchorElement._Setcoords(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('coords',aValue);
end;

procedure TJSHTMLAnchorElement._Setcharset(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('charset',aValue);
end;

procedure TJSHTMLAnchorElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLAnchorElement._Setrev(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('rev',aValue);
end;

procedure TJSHTMLAnchorElement._Setshape(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('shape',aValue);
end;

procedure TJSHTMLAnchorElement._Sethref(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('href',aValue);
end;

procedure TJSHTMLAnchorElement._Setprotocol(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('protocol',aValue);
end;

procedure TJSHTMLAnchorElement._Setusername(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('username',aValue);
end;

procedure TJSHTMLAnchorElement._Setpassword(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('password',aValue);
end;

procedure TJSHTMLAnchorElement._Sethost(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('host',aValue);
end;

procedure TJSHTMLAnchorElement._Sethostname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hostname',aValue);
end;

procedure TJSHTMLAnchorElement._Setport(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('port',aValue);
end;

procedure TJSHTMLAnchorElement._Setpathname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('pathname',aValue);
end;

procedure TJSHTMLAnchorElement._Setsearch(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('search',aValue);
end;

procedure TJSHTMLAnchorElement._Sethash(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hash',aValue);
end;

class function TJSHTMLAnchorElement.Cast(Intf: IJSObject): IJSHTMLAnchorElement;
begin
  Result:=TJSHTMLAnchorElement.JOBCast(Intf);
end;

function TJSHTMLButtonElement._Getautofocus: Boolean;
begin
  Result:=ReadJSPropertyBoolean('autofocus');
end;

function TJSHTMLButtonElement._Getdisabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('disabled');
end;

function TJSHTMLButtonElement._Getform: IJSHTMLFormElement;
begin
  Result:=ReadJSPropertyObject('form',TJSHTMLFormElement) as IJSHTMLFormElement;
end;

function TJSHTMLButtonElement._GetformAction: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formAction');
end;

function TJSHTMLButtonElement._GetformEnctype: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formEnctype');
end;

function TJSHTMLButtonElement._GetformMethod: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formMethod');
end;

function TJSHTMLButtonElement._GetformNoValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('formNoValidate');
end;

function TJSHTMLButtonElement._GetformTarget: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formTarget');
end;

function TJSHTMLButtonElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLButtonElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLButtonElement._Getvalue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('value');
end;

function TJSHTMLButtonElement._GetwillValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('willValidate');
end;

function TJSHTMLButtonElement._Getvalidity: IJSValidityState;
begin
  Result:=ReadJSPropertyObject('validity',TJSValidityState) as IJSValidityState;
end;

function TJSHTMLButtonElement._GetvalidationMessage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('validationMessage');
end;

function TJSHTMLButtonElement._Getlabels: IJSNodeList;
begin
  Result:=ReadJSPropertyObject('labels',TJSNodeList) as IJSNodeList;
end;

procedure TJSHTMLButtonElement._Setautofocus(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('autofocus',aValue);
end;

procedure TJSHTMLButtonElement._Setdisabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('disabled',aValue);
end;

procedure TJSHTMLButtonElement._SetformAction(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formAction',aValue);
end;

procedure TJSHTMLButtonElement._SetformEnctype(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formEnctype',aValue);
end;

procedure TJSHTMLButtonElement._SetformMethod(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formMethod',aValue);
end;

procedure TJSHTMLButtonElement._SetformNoValidate(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('formNoValidate',aValue);
end;

procedure TJSHTMLButtonElement._SetformTarget(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formTarget',aValue);
end;

procedure TJSHTMLButtonElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLButtonElement._Settype_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSHTMLButtonElement._Setvalue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('value',aValue);
end;

function TJSHTMLButtonElement.checkValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('checkValidity',[]);
end;

function TJSHTMLButtonElement.reportValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('reportValidity',[]);
end;

procedure TJSHTMLButtonElement.setCustomValidity(const aError: UnicodeString);
begin
  InvokeJSNoResult('setCustomValidity',[aError]);
end;

class function TJSHTMLButtonElement.Cast(Intf: IJSObject): IJSHTMLButtonElement;
begin
  Result:=TJSHTMLButtonElement.JOBCast(Intf);
end;

function TJSHTMLCanvasElement._Getwidth: LongWord;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSHTMLCanvasElement._Getheight: LongWord;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSHTMLCanvasElement._GetmozOpaque: Boolean;
begin
  Result:=ReadJSPropertyBoolean('mozOpaque');
end;

procedure TJSHTMLCanvasElement._Setwidth(const aValue: LongWord);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSHTMLCanvasElement._Setheight(const aValue: LongWord);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSHTMLCanvasElement._SetmozOpaque(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('mozOpaque',aValue);
end;

function TJSHTMLCanvasElement.getContext(const aContextId: UnicodeString; aContextOptions: TJOB_JSValue): IJSnsISupports; overload;
begin
  Result:=InvokeJSObjectResult('getContext',[aContextId,aContextOptions],TJSnsISupports) as IJSnsISupports;
end;

function TJSHTMLCanvasElement.getContext(const aContextId: UnicodeString): IJSnsISupports; overload;
begin
  Result:=InvokeJSObjectResult('getContext',[aContextId],TJSnsISupports) as IJSnsISupports;
end;

function TJSHTMLCanvasElement.toDataURL(const aType_: UnicodeString; aEncoderOptions: TJOB_JSValue): UnicodeString; overload;
begin
  Result:=InvokeJSUnicodeStringResult('toDataURL',[aType_,aEncoderOptions]);
end;

function TJSHTMLCanvasElement.toDataURL: UnicodeString; overload;
begin
  Result:=InvokeJSUnicodeStringResult('toDataURL',[]);
end;

function TJSHTMLCanvasElement.toDataURL(const aType_: UnicodeString): UnicodeString; overload;
begin
  Result:=InvokeJSUnicodeStringResult('toDataURL',[aType_]);
end;

procedure TJSHTMLCanvasElement.toBlob(const aCallback: TBlobCallback; const aType_: UnicodeString; aEncoderOptions: TJOB_JSValue); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aCallback),@JOBCallTBlobCallback);
  try
    InvokeJSNoResult('toBlob',[m,aType_,aEncoderOptions]);
  finally
    m.free;
  end;
end;

procedure TJSHTMLCanvasElement.toBlob(const aCallback: TBlobCallback); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aCallback),@JOBCallTBlobCallback);
  try
    InvokeJSNoResult('toBlob',[m]);
  finally
    m.free;
  end;
end;

procedure TJSHTMLCanvasElement.toBlob(const aCallback: TBlobCallback; const aType_: UnicodeString); overload;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aCallback),@JOBCallTBlobCallback);
  try
    InvokeJSNoResult('toBlob',[m,aType_]);
  finally
    m.free;
  end;
end;

function TJSHTMLCanvasElement.MozGetIPCContext(const aContextId: UnicodeString): IJSnsISupports;
begin
  Result:=InvokeJSObjectResult('MozGetIPCContext',[aContextId],TJSnsISupports) as IJSnsISupports;
end;

function TJSHTMLCanvasElement.transferControlToOffscreen: IJSOffscreenCanvas;
begin
  Result:=InvokeJSObjectResult('transferControlToOffscreen',[],TJSOffscreenCanvas) as IJSOffscreenCanvas;
end;

class function TJSHTMLCanvasElement.Cast(Intf: IJSObject): IJSHTMLCanvasElement;
begin
  Result:=TJSHTMLCanvasElement.JOBCast(Intf);
end;

function TJSHTMLDivElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

procedure TJSHTMLDivElement._Setalign(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

class function TJSHTMLDivElement.Cast(Intf: IJSObject): IJSHTMLDivElement;
begin
  Result:=TJSHTMLDivElement.JOBCast(Intf);
end;

function TJSHTMLEmbedElement._Getsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('src');
end;

function TJSHTMLEmbedElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLEmbedElement._Getwidth: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('width');
end;

function TJSHTMLEmbedElement._Getheight: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('height');
end;

function TJSHTMLEmbedElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

function TJSHTMLEmbedElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLEmbedElement._GetloadingEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('loadingEnabled');
end;

function TJSHTMLEmbedElement._GetcurrentURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentURI',TJSURI) as IJSURI;
end;

function TJSHTMLEmbedElement._GetcurrentRequestFinalURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentRequestFinalURI',TJSURI) as IJSURI;
end;

procedure TJSHTMLEmbedElement._Setsrc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('src',aValue);
end;

procedure TJSHTMLEmbedElement._Settype_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSHTMLEmbedElement._Setwidth(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('width',aValue);
end;

procedure TJSHTMLEmbedElement._Setheight(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('height',aValue);
end;

procedure TJSHTMLEmbedElement._Setalign(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

procedure TJSHTMLEmbedElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLEmbedElement._SetloadingEnabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('loadingEnabled',aValue);
end;

function TJSHTMLEmbedElement.getSVGDocument: IJSDocument;
begin
  Result:=InvokeJSObjectResult('getSVGDocument',[],TJSDocument) as IJSDocument;
end;

procedure TJSHTMLEmbedElement.addObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('addObserver',[aObserver]);
end;

procedure TJSHTMLEmbedElement.removeObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('removeObserver',[aObserver]);
end;

function TJSHTMLEmbedElement.getRequest(aRequestType: Integer): IJSimgIRequest;
begin
  Result:=InvokeJSObjectResult('getRequest',[aRequestType],TJSimgIRequest) as IJSimgIRequest;
end;

function TJSHTMLEmbedElement.getRequestType(aRequest: IJSimgIRequest): Integer;
begin
  Result:=InvokeJSLongIntResult('getRequestType',[aRequest]);
end;

procedure TJSHTMLEmbedElement.forceReload(aNotify: Boolean); overload;
begin
  InvokeJSNoResult('forceReload',[aNotify]);
end;

procedure TJSHTMLEmbedElement.forceReload; overload;
begin
  InvokeJSNoResult('forceReload',[]);
end;

class function TJSHTMLEmbedElement.Cast(Intf: IJSObject): IJSHTMLEmbedElement;
begin
  Result:=TJSHTMLEmbedElement.JOBCast(Intf);
end;

function TJSHTMLIFrameElement._Getsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('src');
end;

function TJSHTMLIFrameElement._Getsrcdoc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('srcdoc');
end;

function TJSHTMLIFrameElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLIFrameElement._Getsandbox: IJSDOMTokenList;
begin
  Result:=ReadJSPropertyObject('sandbox',TJSDOMTokenList) as IJSDOMTokenList;
end;

function TJSHTMLIFrameElement._GetallowFullscreen: Boolean;
begin
  Result:=ReadJSPropertyBoolean('allowFullscreen');
end;

function TJSHTMLIFrameElement._Getwidth: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('width');
end;

function TJSHTMLIFrameElement._Getheight: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('height');
end;

function TJSHTMLIFrameElement._GetreferrerPolicy: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('referrerPolicy');
end;

function TJSHTMLIFrameElement._GetcontentDocument: IJSDocument;
begin
  Result:=ReadJSPropertyObject('contentDocument',TJSDocument) as IJSDocument;
end;

function TJSHTMLIFrameElement._GetcontentWindow: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('contentWindow',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSHTMLIFrameElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

function TJSHTMLIFrameElement._Getscrolling: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('scrolling');
end;

function TJSHTMLIFrameElement._GetframeBorder: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('frameBorder');
end;

function TJSHTMLIFrameElement._GetlongDesc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('longDesc');
end;

function TJSHTMLIFrameElement._GetmarginHeight: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('marginHeight');
end;

function TJSHTMLIFrameElement._GetmarginWidth: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('marginWidth');
end;

function TJSHTMLIFrameElement._Getallow: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('allow');
end;

procedure TJSHTMLIFrameElement._Setsrc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('src',aValue);
end;

procedure TJSHTMLIFrameElement._Setsrcdoc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('srcdoc',aValue);
end;

procedure TJSHTMLIFrameElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLIFrameElement._SetallowFullscreen(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('allowFullscreen',aValue);
end;

procedure TJSHTMLIFrameElement._Setwidth(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('width',aValue);
end;

procedure TJSHTMLIFrameElement._Setheight(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('height',aValue);
end;

procedure TJSHTMLIFrameElement._SetreferrerPolicy(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('referrerPolicy',aValue);
end;

procedure TJSHTMLIFrameElement._Setalign(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

procedure TJSHTMLIFrameElement._Setscrolling(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('scrolling',aValue);
end;

procedure TJSHTMLIFrameElement._SetframeBorder(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('frameBorder',aValue);
end;

procedure TJSHTMLIFrameElement._SetlongDesc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('longDesc',aValue);
end;

procedure TJSHTMLIFrameElement._SetmarginHeight(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('marginHeight',aValue);
end;

procedure TJSHTMLIFrameElement._SetmarginWidth(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('marginWidth',aValue);
end;

procedure TJSHTMLIFrameElement._Setallow(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('allow',aValue);
end;

function TJSHTMLIFrameElement.getSVGDocument: IJSDocument;
begin
  Result:=InvokeJSObjectResult('getSVGDocument',[],TJSDocument) as IJSDocument;
end;

class function TJSHTMLIFrameElement.Cast(Intf: IJSObject): IJSHTMLIFrameElement;
begin
  Result:=TJSHTMLIFrameElement.JOBCast(Intf);
end;

function TJSHTMLImageElement._Getalt: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('alt');
end;

function TJSHTMLImageElement._Getsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('src');
end;

function TJSHTMLImageElement._Getsrcset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('srcset');
end;

function TJSHTMLImageElement._GetcrossOrigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('crossOrigin');
end;

function TJSHTMLImageElement._GetuseMap: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('useMap');
end;

function TJSHTMLImageElement._GetreferrerPolicy: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('referrerPolicy');
end;

function TJSHTMLImageElement._GetisMap: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isMap');
end;

function TJSHTMLImageElement._Getwidth: LongWord;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSHTMLImageElement._Getheight: LongWord;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSHTMLImageElement._Getdecoding: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('decoding');
end;

function TJSHTMLImageElement._Getloading: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('loading');
end;

function TJSHTMLImageElement._GetnaturalWidth: LongWord;
begin
  Result:=ReadJSPropertyInt64('naturalWidth');
end;

function TJSHTMLImageElement._GetnaturalHeight: LongWord;
begin
  Result:=ReadJSPropertyInt64('naturalHeight');
end;

function TJSHTMLImageElement._Getcomplete: Boolean;
begin
  Result:=ReadJSPropertyBoolean('complete');
end;

function TJSHTMLImageElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLImageElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

function TJSHTMLImageElement._Gethspace: LongWord;
begin
  Result:=ReadJSPropertyInt64('hspace');
end;

function TJSHTMLImageElement._Getvspace: LongWord;
begin
  Result:=ReadJSPropertyInt64('vspace');
end;

function TJSHTMLImageElement._GetlongDesc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('longDesc');
end;

function TJSHTMLImageElement._Getborder: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('border');
end;

function TJSHTMLImageElement._Getsizes: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('sizes');
end;

function TJSHTMLImageElement._GetcurrentSrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('currentSrc');
end;

function TJSHTMLImageElement._Getlowsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('lowsrc');
end;

function TJSHTMLImageElement._Getx: Integer;
begin
  Result:=ReadJSPropertyLongInt('x');
end;

function TJSHTMLImageElement._Gety: Integer;
begin
  Result:=ReadJSPropertyLongInt('y');
end;

function TJSHTMLImageElement._GetloadingEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('loadingEnabled');
end;

function TJSHTMLImageElement._GetcurrentURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentURI',TJSURI) as IJSURI;
end;

function TJSHTMLImageElement._GetcurrentRequestFinalURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentRequestFinalURI',TJSURI) as IJSURI;
end;

procedure TJSHTMLImageElement._Setalt(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('alt',aValue);
end;

procedure TJSHTMLImageElement._Setsrc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('src',aValue);
end;

procedure TJSHTMLImageElement._Setsrcset(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('srcset',aValue);
end;

procedure TJSHTMLImageElement._SetcrossOrigin(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('crossOrigin',aValue);
end;

procedure TJSHTMLImageElement._SetuseMap(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('useMap',aValue);
end;

procedure TJSHTMLImageElement._SetreferrerPolicy(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('referrerPolicy',aValue);
end;

procedure TJSHTMLImageElement._SetisMap(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('isMap',aValue);
end;

procedure TJSHTMLImageElement._Setwidth(const aValue: LongWord);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSHTMLImageElement._Setheight(const aValue: LongWord);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSHTMLImageElement._Setdecoding(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('decoding',aValue);
end;

procedure TJSHTMLImageElement._Setloading(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('loading',aValue);
end;

procedure TJSHTMLImageElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLImageElement._Setalign(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

procedure TJSHTMLImageElement._Sethspace(const aValue: LongWord);
begin
  WriteJSPropertyDouble('hspace',aValue);
end;

procedure TJSHTMLImageElement._Setvspace(const aValue: LongWord);
begin
  WriteJSPropertyDouble('vspace',aValue);
end;

procedure TJSHTMLImageElement._SetlongDesc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('longDesc',aValue);
end;

procedure TJSHTMLImageElement._Setborder(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('border',aValue);
end;

procedure TJSHTMLImageElement._Setsizes(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('sizes',aValue);
end;

procedure TJSHTMLImageElement._Setlowsrc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('lowsrc',aValue);
end;

procedure TJSHTMLImageElement._SetloadingEnabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('loadingEnabled',aValue);
end;

procedure TJSHTMLImageElement.addObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('addObserver',[aObserver]);
end;

procedure TJSHTMLImageElement.removeObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('removeObserver',[aObserver]);
end;

function TJSHTMLImageElement.getRequest(aRequestType: Integer): IJSimgIRequest;
begin
  Result:=InvokeJSObjectResult('getRequest',[aRequestType],TJSimgIRequest) as IJSimgIRequest;
end;

function TJSHTMLImageElement.getRequestType(aRequest: IJSimgIRequest): Integer;
begin
  Result:=InvokeJSLongIntResult('getRequestType',[aRequest]);
end;

procedure TJSHTMLImageElement.forceReload(aNotify: Boolean); overload;
begin
  InvokeJSNoResult('forceReload',[aNotify]);
end;

procedure TJSHTMLImageElement.forceReload; overload;
begin
  InvokeJSNoResult('forceReload',[]);
end;

class function TJSHTMLImageElement.Cast(Intf: IJSObject): IJSHTMLImageElement;
begin
  Result:=TJSHTMLImageElement.JOBCast(Intf);
end;

function TJSHTMLInputElement._Getaccept: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('accept');
end;

function TJSHTMLInputElement._Getalt: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('alt');
end;

function TJSHTMLInputElement._Getautocomplete: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('autocomplete');
end;

function TJSHTMLInputElement._Getautofocus: Boolean;
begin
  Result:=ReadJSPropertyBoolean('autofocus');
end;

function TJSHTMLInputElement._Getcapture: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('capture');
end;

function TJSHTMLInputElement._GetdefaultChecked: Boolean;
begin
  Result:=ReadJSPropertyBoolean('defaultChecked');
end;

function TJSHTMLInputElement._Getchecked: Boolean;
begin
  Result:=ReadJSPropertyBoolean('checked');
end;

function TJSHTMLInputElement._Getdisabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('disabled');
end;

function TJSHTMLInputElement._Getform: IJSHTMLFormElement;
begin
  Result:=ReadJSPropertyObject('form',TJSHTMLFormElement) as IJSHTMLFormElement;
end;

function TJSHTMLInputElement._Getfiles: IJSFileList;
begin
  Result:=ReadJSPropertyObject('files',TJSFileList) as IJSFileList;
end;

function TJSHTMLInputElement._GetformAction: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formAction');
end;

function TJSHTMLInputElement._GetformEnctype: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formEnctype');
end;

function TJSHTMLInputElement._GetformMethod: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formMethod');
end;

function TJSHTMLInputElement._GetformNoValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('formNoValidate');
end;

function TJSHTMLInputElement._GetformTarget: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('formTarget');
end;

function TJSHTMLInputElement._Getheight: LongWord;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSHTMLInputElement._Getindeterminate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('indeterminate');
end;

function TJSHTMLInputElement._Getlist: IJSHTMLElement;
begin
  Result:=ReadJSPropertyObject('list',TJSHTMLElement) as IJSHTMLElement;
end;

function TJSHTMLInputElement._Getmax: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('max');
end;

function TJSHTMLInputElement._GetmaxLength: Integer;
begin
  Result:=ReadJSPropertyLongInt('maxLength');
end;

function TJSHTMLInputElement._Getmin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('min');
end;

function TJSHTMLInputElement._GetminLength: Integer;
begin
  Result:=ReadJSPropertyLongInt('minLength');
end;

function TJSHTMLInputElement._Getmultiple: Boolean;
begin
  Result:=ReadJSPropertyBoolean('multiple');
end;

function TJSHTMLInputElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLInputElement._Getpattern: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('pattern');
end;

function TJSHTMLInputElement._Getplaceholder: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('placeholder');
end;

function TJSHTMLInputElement._GetreadOnly: Boolean;
begin
  Result:=ReadJSPropertyBoolean('readOnly');
end;

function TJSHTMLInputElement._Getrequired: Boolean;
begin
  Result:=ReadJSPropertyBoolean('required');
end;

function TJSHTMLInputElement._Getsize: LongWord;
begin
  Result:=ReadJSPropertyInt64('size');
end;

function TJSHTMLInputElement._Getsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('src');
end;

function TJSHTMLInputElement._Getstep: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('step');
end;

function TJSHTMLInputElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLInputElement._GetdefaultValue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('defaultValue');
end;

function TJSHTMLInputElement._Getvalue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('value');
end;

function TJSHTMLInputElement._GetvalueAsDate: IJSObject;
begin
  Result:=ReadJSPropertyObject('valueAsDate',TJSObject) as IJSObject;
end;

function TJSHTMLInputElement._GetvalueAsNumber: Double;
begin
  Result:=ReadJSPropertyDouble('valueAsNumber');
end;

function TJSHTMLInputElement._Getwidth: LongWord;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSHTMLInputElement._GetwillValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('willValidate');
end;

function TJSHTMLInputElement._Getvalidity: IJSValidityState;
begin
  Result:=ReadJSPropertyObject('validity',TJSValidityState) as IJSValidityState;
end;

function TJSHTMLInputElement._GetvalidationMessage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('validationMessage');
end;

function TJSHTMLInputElement._Getlabels: IJSNodeList;
begin
  Result:=ReadJSPropertyObject('labels',TJSNodeList) as IJSNodeList;
end;

function TJSHTMLInputElement._GetselectionStart: LongWord;
begin
  Result:=ReadJSPropertyInt64('selectionStart');
end;

function TJSHTMLInputElement._GetselectionEnd: LongWord;
begin
  Result:=ReadJSPropertyInt64('selectionEnd');
end;

function TJSHTMLInputElement._GetselectionDirection: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('selectionDirection');
end;

function TJSHTMLInputElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

function TJSHTMLInputElement._GetuseMap: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('useMap');
end;

function TJSHTMLInputElement._Getcontrollers: IJSXULControllers;
begin
  Result:=ReadJSPropertyObject('controllers',TJSXULControllers) as IJSXULControllers;
end;

function TJSHTMLInputElement._GettextLength: Integer;
begin
  Result:=ReadJSPropertyLongInt('textLength');
end;

function TJSHTMLInputElement._GethasBeenTypePassword: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasBeenTypePassword');
end;

function TJSHTMLInputElement._GetpreviewValue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('previewValue');
end;

function TJSHTMLInputElement._GetrevealPassword: Boolean;
begin
  Result:=ReadJSPropertyBoolean('revealPassword');
end;

function TJSHTMLInputElement._GetwebkitEntries: TJSFileSystemEntryDynArray;
begin
  Result:=ReadJSPropertyObject('webkitEntries',TJSArray) as TJSFileSystemEntryDynArray;
end;

function TJSHTMLInputElement._Getwebkitdirectory: Boolean;
begin
  Result:=ReadJSPropertyBoolean('webkitdirectory');
end;

function TJSHTMLInputElement._GetdateTimeBoxElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('dateTimeBoxElement',TJSElement) as IJSElement;
end;

function TJSHTMLInputElement._Geteditor: IJSnsIEditor;
begin
  Result:=ReadJSPropertyObject('editor',TJSnsIEditor) as IJSnsIEditor;
end;

function TJSHTMLInputElement._GethasEditor: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasEditor');
end;

function TJSHTMLInputElement._GetisInputEventTarget: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isInputEventTarget');
end;

function TJSHTMLInputElement._GetloadingEnabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('loadingEnabled');
end;

function TJSHTMLInputElement._GetcurrentURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentURI',TJSURI) as IJSURI;
end;

function TJSHTMLInputElement._GetcurrentRequestFinalURI: IJSURI;
begin
  Result:=ReadJSPropertyObject('currentRequestFinalURI',TJSURI) as IJSURI;
end;

procedure TJSHTMLInputElement._Setaccept(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('accept',aValue);
end;

procedure TJSHTMLInputElement._Setalt(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('alt',aValue);
end;

procedure TJSHTMLInputElement._Setautocomplete(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('autocomplete',aValue);
end;

procedure TJSHTMLInputElement._Setautofocus(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('autofocus',aValue);
end;

procedure TJSHTMLInputElement._Setcapture(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('capture',aValue);
end;

procedure TJSHTMLInputElement._SetdefaultChecked(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('defaultChecked',aValue);
end;

procedure TJSHTMLInputElement._Setchecked(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('checked',aValue);
end;

procedure TJSHTMLInputElement._Setdisabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('disabled',aValue);
end;

procedure TJSHTMLInputElement._Setfiles(const aValue: IJSFileList);
begin
  WriteJSPropertyObject('files',aValue);
end;

procedure TJSHTMLInputElement._SetformAction(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formAction',aValue);
end;

procedure TJSHTMLInputElement._SetformEnctype(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formEnctype',aValue);
end;

procedure TJSHTMLInputElement._SetformMethod(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formMethod',aValue);
end;

procedure TJSHTMLInputElement._SetformNoValidate(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('formNoValidate',aValue);
end;

procedure TJSHTMLInputElement._SetformTarget(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('formTarget',aValue);
end;

procedure TJSHTMLInputElement._Setheight(const aValue: LongWord);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSHTMLInputElement._Setindeterminate(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('indeterminate',aValue);
end;

procedure TJSHTMLInputElement._Setmax(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('max',aValue);
end;

procedure TJSHTMLInputElement._SetmaxLength(const aValue: Integer);
begin
  WriteJSPropertyLongInt('maxLength',aValue);
end;

procedure TJSHTMLInputElement._Setmin(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('min',aValue);
end;

procedure TJSHTMLInputElement._SetminLength(const aValue: Integer);
begin
  WriteJSPropertyLongInt('minLength',aValue);
end;

procedure TJSHTMLInputElement._Setmultiple(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('multiple',aValue);
end;

procedure TJSHTMLInputElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLInputElement._Setpattern(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('pattern',aValue);
end;

procedure TJSHTMLInputElement._Setplaceholder(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('placeholder',aValue);
end;

procedure TJSHTMLInputElement._SetreadOnly(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('readOnly',aValue);
end;

procedure TJSHTMLInputElement._Setrequired(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('required',aValue);
end;

procedure TJSHTMLInputElement._Setsize(const aValue: LongWord);
begin
  WriteJSPropertyDouble('size',aValue);
end;

procedure TJSHTMLInputElement._Setsrc(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('src',aValue);
end;

procedure TJSHTMLInputElement._Setstep(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('step',aValue);
end;

procedure TJSHTMLInputElement._Settype_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSHTMLInputElement._SetdefaultValue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('defaultValue',aValue);
end;

procedure TJSHTMLInputElement._Setvalue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('value',aValue);
end;

procedure TJSHTMLInputElement._SetvalueAsDate(const aValue: IJSObject);
begin
  WriteJSPropertyObject('valueAsDate',aValue);
end;

procedure TJSHTMLInputElement._SetvalueAsNumber(const aValue: Double);
begin
  WriteJSPropertyDouble('valueAsNumber',aValue);
end;

procedure TJSHTMLInputElement._Setwidth(const aValue: LongWord);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSHTMLInputElement._SetselectionStart(const aValue: LongWord);
begin
  WriteJSPropertyDouble('selectionStart',aValue);
end;

procedure TJSHTMLInputElement._SetselectionEnd(const aValue: LongWord);
begin
  WriteJSPropertyDouble('selectionEnd',aValue);
end;

procedure TJSHTMLInputElement._SetselectionDirection(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('selectionDirection',aValue);
end;

procedure TJSHTMLInputElement._Setalign(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

procedure TJSHTMLInputElement._SetuseMap(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('useMap',aValue);
end;

procedure TJSHTMLInputElement._SetpreviewValue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('previewValue',aValue);
end;

procedure TJSHTMLInputElement._SetrevealPassword(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('revealPassword',aValue);
end;

procedure TJSHTMLInputElement._Setwebkitdirectory(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('webkitdirectory',aValue);
end;

procedure TJSHTMLInputElement._SetloadingEnabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('loadingEnabled',aValue);
end;

procedure TJSHTMLInputElement.stepUp(aN: Integer); overload;
begin
  InvokeJSNoResult('stepUp',[aN]);
end;

procedure TJSHTMLInputElement.stepUp; overload;
begin
  InvokeJSNoResult('stepUp',[]);
end;

procedure TJSHTMLInputElement.stepDown(aN: Integer); overload;
begin
  InvokeJSNoResult('stepDown',[aN]);
end;

procedure TJSHTMLInputElement.stepDown; overload;
begin
  InvokeJSNoResult('stepDown',[]);
end;

function TJSHTMLInputElement.checkValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('checkValidity',[]);
end;

function TJSHTMLInputElement.reportValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('reportValidity',[]);
end;

procedure TJSHTMLInputElement.setCustomValidity(const aError: UnicodeString);
begin
  InvokeJSNoResult('setCustomValidity',[aError]);
end;

procedure TJSHTMLInputElement.select;
begin
  InvokeJSNoResult('select',[]);
end;

procedure TJSHTMLInputElement.setRangeText(const aReplacement: UnicodeString);
begin
  InvokeJSNoResult('setRangeText',[aReplacement]);
end;

procedure TJSHTMLInputElement.setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord; aSelectionMode: TSelectionMode); overload;
begin
  InvokeJSNoResult('setRangeText',[aReplacement,aStart,aEnd_,aSelectionMode]);
end;

procedure TJSHTMLInputElement.setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord); overload;
begin
  InvokeJSNoResult('setRangeText',[aReplacement,aStart,aEnd_]);
end;

procedure TJSHTMLInputElement.setSelectionRange(aStart: LongWord; aEnd_: LongWord; const aDirection: UnicodeString); overload;
begin
  InvokeJSNoResult('setSelectionRange',[aStart,aEnd_,aDirection]);
end;

procedure TJSHTMLInputElement.setSelectionRange(aStart: LongWord; aEnd_: LongWord); overload;
begin
  InvokeJSNoResult('setSelectionRange',[aStart,aEnd_]);
end;

procedure TJSHTMLInputElement.showPicker;
begin
  InvokeJSNoResult('showPicker',[]);
end;

function TJSHTMLInputElement.mozGetFileNameArray: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('mozGetFileNameArray',[],TJSArray) as TUnicodeStringDynArray;
end;

procedure TJSHTMLInputElement.mozSetDirectory(const aDirectoryPath: UnicodeString);
begin
  InvokeJSNoResult('mozSetDirectory',[aDirectoryPath]);
end;

function TJSHTMLInputElement.mozIsTextField(aExcludePassword: Boolean): Boolean;
begin
  Result:=InvokeJSBooleanResult('mozIsTextField',[aExcludePassword]);
end;

function TJSHTMLInputElement.getMinimum: Double;
begin
  Result:=InvokeJSDoubleResult('getMinimum',[]);
end;

function TJSHTMLInputElement.getMaximum: Double;
begin
  Result:=InvokeJSDoubleResult('getMaximum',[]);
end;

procedure TJSHTMLInputElement.openDateTimePicker(const aInitialValue: TJSDateTimeValue); overload;
begin
  InvokeJSNoResult('openDateTimePicker',[aInitialValue]);
end;

procedure TJSHTMLInputElement.openDateTimePicker; overload;
begin
  InvokeJSNoResult('openDateTimePicker',[]);
end;

procedure TJSHTMLInputElement.updateDateTimePicker(const aValue: TJSDateTimeValue); overload;
begin
  InvokeJSNoResult('updateDateTimePicker',[aValue]);
end;

procedure TJSHTMLInputElement.updateDateTimePicker; overload;
begin
  InvokeJSNoResult('updateDateTimePicker',[]);
end;

procedure TJSHTMLInputElement.closeDateTimePicker;
begin
  InvokeJSNoResult('closeDateTimePicker',[]);
end;

procedure TJSHTMLInputElement.setFocusState(aIsFocused: Boolean);
begin
  InvokeJSNoResult('setFocusState',[aIsFocused]);
end;

procedure TJSHTMLInputElement.updateValidityState;
begin
  InvokeJSNoResult('updateValidityState',[]);
end;

function TJSHTMLInputElement.getStep: Double;
begin
  Result:=InvokeJSDoubleResult('getStep',[]);
end;

function TJSHTMLInputElement.getStepBase: Double;
begin
  Result:=InvokeJSDoubleResult('getStepBase',[]);
end;

procedure TJSHTMLInputElement.setUserInput(const aInput: UnicodeString);
begin
  InvokeJSNoResult('setUserInput',[aInput]);
end;

procedure TJSHTMLInputElement.addObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('addObserver',[aObserver]);
end;

procedure TJSHTMLInputElement.removeObserver(aObserver: IJSimgINotificationObserver);
begin
  InvokeJSNoResult('removeObserver',[aObserver]);
end;

function TJSHTMLInputElement.getRequest(aRequestType: Integer): IJSimgIRequest;
begin
  Result:=InvokeJSObjectResult('getRequest',[aRequestType],TJSimgIRequest) as IJSimgIRequest;
end;

function TJSHTMLInputElement.getRequestType(aRequest: IJSimgIRequest): Integer;
begin
  Result:=InvokeJSLongIntResult('getRequestType',[aRequest]);
end;

procedure TJSHTMLInputElement.forceReload(aNotify: Boolean); overload;
begin
  InvokeJSNoResult('forceReload',[aNotify]);
end;

procedure TJSHTMLInputElement.forceReload; overload;
begin
  InvokeJSNoResult('forceReload',[]);
end;

class function TJSHTMLInputElement.Cast(Intf: IJSObject): IJSHTMLInputElement;
begin
  Result:=TJSHTMLInputElement.JOBCast(Intf);
end;

function TJSHTMLLabelElement._Getform: IJSHTMLFormElement;
begin
  Result:=ReadJSPropertyObject('form',TJSHTMLFormElement) as IJSHTMLFormElement;
end;

function TJSHTMLLabelElement._GethtmlFor: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('htmlFor');
end;

function TJSHTMLLabelElement._Getcontrol: IJSHTMLElement;
begin
  Result:=ReadJSPropertyObject('control',TJSHTMLElement) as IJSHTMLElement;
end;

procedure TJSHTMLLabelElement._SethtmlFor(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('htmlFor',aValue);
end;

class function TJSHTMLLabelElement.Cast(Intf: IJSObject): IJSHTMLLabelElement;
begin
  Result:=TJSHTMLLabelElement.JOBCast(Intf);
end;

function TJSHTMLLinkElement._Getdisabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('disabled');
end;

function TJSHTMLLinkElement._Gethref: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('href');
end;

function TJSHTMLLinkElement._GetcrossOrigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('crossOrigin');
end;

function TJSHTMLLinkElement._Getrel: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('rel');
end;

function TJSHTMLLinkElement._GetrelList: IJSDOMTokenList;
begin
  Result:=ReadJSPropertyObject('relList',TJSDOMTokenList) as IJSDOMTokenList;
end;

function TJSHTMLLinkElement._Getmedia: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('media');
end;

function TJSHTMLLinkElement._Gethreflang: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('hreflang');
end;

function TJSHTMLLinkElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLLinkElement._GetreferrerPolicy: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('referrerPolicy');
end;

function TJSHTMLLinkElement._Getsizes: IJSDOMTokenList;
begin
  Result:=ReadJSPropertyObject('sizes',TJSDOMTokenList) as IJSDOMTokenList;
end;

function TJSHTMLLinkElement._GetimageSrcset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('imageSrcset');
end;

function TJSHTMLLinkElement._GetimageSizes: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('imageSizes');
end;

function TJSHTMLLinkElement._Getcharset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('charset');
end;

function TJSHTMLLinkElement._Getrev: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('rev');
end;

function TJSHTMLLinkElement._Gettarget: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('target');
end;

function TJSHTMLLinkElement._Getintegrity: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('integrity');
end;

function TJSHTMLLinkElement._Getas_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('as');
end;

function TJSHTMLLinkElement._Getsheet: IJSStyleSheet;
begin
  Result:=ReadJSPropertyObject('sheet',TJSStyleSheet) as IJSStyleSheet;
end;

procedure TJSHTMLLinkElement._Setdisabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('disabled',aValue);
end;

procedure TJSHTMLLinkElement._Sethref(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('href',aValue);
end;

procedure TJSHTMLLinkElement._SetcrossOrigin(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('crossOrigin',aValue);
end;

procedure TJSHTMLLinkElement._Setrel(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('rel',aValue);
end;

procedure TJSHTMLLinkElement._Setmedia(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('media',aValue);
end;

procedure TJSHTMLLinkElement._Sethreflang(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('hreflang',aValue);
end;

procedure TJSHTMLLinkElement._Settype_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSHTMLLinkElement._SetreferrerPolicy(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('referrerPolicy',aValue);
end;

procedure TJSHTMLLinkElement._SetimageSrcset(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('imageSrcset',aValue);
end;

procedure TJSHTMLLinkElement._SetimageSizes(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('imageSizes',aValue);
end;

procedure TJSHTMLLinkElement._Setcharset(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('charset',aValue);
end;

procedure TJSHTMLLinkElement._Setrev(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('rev',aValue);
end;

procedure TJSHTMLLinkElement._Settarget(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('target',aValue);
end;

procedure TJSHTMLLinkElement._Setintegrity(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('integrity',aValue);
end;

procedure TJSHTMLLinkElement._Setas_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('as',aValue);
end;

class function TJSHTMLLinkElement.Cast(Intf: IJSObject): IJSHTMLLinkElement;
begin
  Result:=TJSHTMLLinkElement.JOBCast(Intf);
end;

function TJSHTMLOptionElement._Getdisabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('disabled');
end;

function TJSHTMLOptionElement._Getform: IJSHTMLFormElement;
begin
  Result:=ReadJSPropertyObject('form',TJSHTMLFormElement) as IJSHTMLFormElement;
end;

function TJSHTMLOptionElement._Getlabel_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('label');
end;

function TJSHTMLOptionElement._GetdefaultSelected: Boolean;
begin
  Result:=ReadJSPropertyBoolean('defaultSelected');
end;

function TJSHTMLOptionElement._Getselected: Boolean;
begin
  Result:=ReadJSPropertyBoolean('selected');
end;

function TJSHTMLOptionElement._Getvalue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('value');
end;

function TJSHTMLOptionElement._Gettext: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('text');
end;

function TJSHTMLOptionElement._Getindex: Integer;
begin
  Result:=ReadJSPropertyLongInt('index');
end;

procedure TJSHTMLOptionElement._Setdisabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('disabled',aValue);
end;

procedure TJSHTMLOptionElement._Setlabel_(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('label',aValue);
end;

procedure TJSHTMLOptionElement._SetdefaultSelected(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('defaultSelected',aValue);
end;

procedure TJSHTMLOptionElement._Setselected(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('selected',aValue);
end;

procedure TJSHTMLOptionElement._Setvalue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('value',aValue);
end;

procedure TJSHTMLOptionElement._Settext(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('text',aValue);
end;

class function TJSHTMLOptionElement.Cast(Intf: IJSObject): IJSHTMLOptionElement;
begin
  Result:=TJSHTMLOptionElement.JOBCast(Intf);
end;

function TJSHTMLProgressElement._Getvalue: Double;
begin
  Result:=ReadJSPropertyDouble('value');
end;

function TJSHTMLProgressElement._Getmax: Double;
begin
  Result:=ReadJSPropertyDouble('max');
end;

function TJSHTMLProgressElement._Getposition: Double;
begin
  Result:=ReadJSPropertyDouble('position');
end;

function TJSHTMLProgressElement._Getlabels: IJSNodeList;
begin
  Result:=ReadJSPropertyObject('labels',TJSNodeList) as IJSNodeList;
end;

procedure TJSHTMLProgressElement._Setvalue(const aValue: Double);
begin
  WriteJSPropertyDouble('value',aValue);
end;

procedure TJSHTMLProgressElement._Setmax(const aValue: Double);
begin
  WriteJSPropertyDouble('max',aValue);
end;

class function TJSHTMLProgressElement.Cast(Intf: IJSObject): IJSHTMLProgressElement;
begin
  Result:=TJSHTMLProgressElement.JOBCast(Intf);
end;

function TJSHTMLTextAreaElement._Getautocomplete: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('autocomplete');
end;

function TJSHTMLTextAreaElement._Getautofocus: Boolean;
begin
  Result:=ReadJSPropertyBoolean('autofocus');
end;

function TJSHTMLTextAreaElement._Getcols: LongWord;
begin
  Result:=ReadJSPropertyInt64('cols');
end;

function TJSHTMLTextAreaElement._Getdisabled: Boolean;
begin
  Result:=ReadJSPropertyBoolean('disabled');
end;

function TJSHTMLTextAreaElement._Getform: IJSHTMLFormElement;
begin
  Result:=ReadJSPropertyObject('form',TJSHTMLFormElement) as IJSHTMLFormElement;
end;

function TJSHTMLTextAreaElement._GetmaxLength: Integer;
begin
  Result:=ReadJSPropertyLongInt('maxLength');
end;

function TJSHTMLTextAreaElement._GetminLength: Integer;
begin
  Result:=ReadJSPropertyLongInt('minLength');
end;

function TJSHTMLTextAreaElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLTextAreaElement._Getplaceholder: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('placeholder');
end;

function TJSHTMLTextAreaElement._GetreadOnly: Boolean;
begin
  Result:=ReadJSPropertyBoolean('readOnly');
end;

function TJSHTMLTextAreaElement._Getrequired: Boolean;
begin
  Result:=ReadJSPropertyBoolean('required');
end;

function TJSHTMLTextAreaElement._Getrows: LongWord;
begin
  Result:=ReadJSPropertyInt64('rows');
end;

function TJSHTMLTextAreaElement._Getwrap: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('wrap');
end;

function TJSHTMLTextAreaElement._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSHTMLTextAreaElement._GetdefaultValue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('defaultValue');
end;

function TJSHTMLTextAreaElement._Getvalue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('value');
end;

function TJSHTMLTextAreaElement._GettextLength: LongWord;
begin
  Result:=ReadJSPropertyInt64('textLength');
end;

function TJSHTMLTextAreaElement._GetwillValidate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('willValidate');
end;

function TJSHTMLTextAreaElement._Getvalidity: IJSValidityState;
begin
  Result:=ReadJSPropertyObject('validity',TJSValidityState) as IJSValidityState;
end;

function TJSHTMLTextAreaElement._GetvalidationMessage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('validationMessage');
end;

function TJSHTMLTextAreaElement._Getlabels: IJSNodeList;
begin
  Result:=ReadJSPropertyObject('labels',TJSNodeList) as IJSNodeList;
end;

function TJSHTMLTextAreaElement._GetselectionStart: LongWord;
begin
  Result:=ReadJSPropertyInt64('selectionStart');
end;

function TJSHTMLTextAreaElement._GetselectionEnd: LongWord;
begin
  Result:=ReadJSPropertyInt64('selectionEnd');
end;

function TJSHTMLTextAreaElement._GetselectionDirection: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('selectionDirection');
end;

function TJSHTMLTextAreaElement._Getcontrollers: IJSXULControllers;
begin
  Result:=ReadJSPropertyObject('controllers',TJSXULControllers) as IJSXULControllers;
end;

function TJSHTMLTextAreaElement._GetpreviewValue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('previewValue');
end;

function TJSHTMLTextAreaElement._Geteditor: IJSnsIEditor;
begin
  Result:=ReadJSPropertyObject('editor',TJSnsIEditor) as IJSnsIEditor;
end;

function TJSHTMLTextAreaElement._GethasEditor: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hasEditor');
end;

function TJSHTMLTextAreaElement._GetisInputEventTarget: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isInputEventTarget');
end;

procedure TJSHTMLTextAreaElement._Setautocomplete(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('autocomplete',aValue);
end;

procedure TJSHTMLTextAreaElement._Setautofocus(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('autofocus',aValue);
end;

procedure TJSHTMLTextAreaElement._Setcols(const aValue: LongWord);
begin
  WriteJSPropertyDouble('cols',aValue);
end;

procedure TJSHTMLTextAreaElement._Setdisabled(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('disabled',aValue);
end;

procedure TJSHTMLTextAreaElement._SetmaxLength(const aValue: Integer);
begin
  WriteJSPropertyLongInt('maxLength',aValue);
end;

procedure TJSHTMLTextAreaElement._SetminLength(const aValue: Integer);
begin
  WriteJSPropertyLongInt('minLength',aValue);
end;

procedure TJSHTMLTextAreaElement._Setname(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLTextAreaElement._Setplaceholder(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('placeholder',aValue);
end;

procedure TJSHTMLTextAreaElement._SetreadOnly(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('readOnly',aValue);
end;

procedure TJSHTMLTextAreaElement._Setrequired(const aValue: Boolean);
begin
  WriteJSPropertyBoolean('required',aValue);
end;

procedure TJSHTMLTextAreaElement._Setrows(const aValue: LongWord);
begin
  WriteJSPropertyDouble('rows',aValue);
end;

procedure TJSHTMLTextAreaElement._Setwrap(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('wrap',aValue);
end;

procedure TJSHTMLTextAreaElement._SetdefaultValue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('defaultValue',aValue);
end;

procedure TJSHTMLTextAreaElement._Setvalue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('value',aValue);
end;

procedure TJSHTMLTextAreaElement._SetselectionStart(const aValue: LongWord);
begin
  WriteJSPropertyDouble('selectionStart',aValue);
end;

procedure TJSHTMLTextAreaElement._SetselectionEnd(const aValue: LongWord);
begin
  WriteJSPropertyDouble('selectionEnd',aValue);
end;

procedure TJSHTMLTextAreaElement._SetselectionDirection(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('selectionDirection',aValue);
end;

procedure TJSHTMLTextAreaElement._SetpreviewValue(const aValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('previewValue',aValue);
end;

function TJSHTMLTextAreaElement.checkValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('checkValidity',[]);
end;

function TJSHTMLTextAreaElement.reportValidity: Boolean;
begin
  Result:=InvokeJSBooleanResult('reportValidity',[]);
end;

procedure TJSHTMLTextAreaElement.setCustomValidity(const aError: UnicodeString);
begin
  InvokeJSNoResult('setCustomValidity',[aError]);
end;

procedure TJSHTMLTextAreaElement.select;
begin
  InvokeJSNoResult('select',[]);
end;

procedure TJSHTMLTextAreaElement.setRangeText(const aReplacement: UnicodeString);
begin
  InvokeJSNoResult('setRangeText',[aReplacement]);
end;

procedure TJSHTMLTextAreaElement.setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord; aSelectionMode: TSelectionMode); overload;
begin
  InvokeJSNoResult('setRangeText',[aReplacement,aStart,aEnd_,aSelectionMode]);
end;

procedure TJSHTMLTextAreaElement.setRangeText(const aReplacement: UnicodeString; aStart: LongWord; aEnd_: LongWord); overload;
begin
  InvokeJSNoResult('setRangeText',[aReplacement,aStart,aEnd_]);
end;

procedure TJSHTMLTextAreaElement.setSelectionRange(aStart: LongWord; aEnd_: LongWord; const aDirection: UnicodeString); overload;
begin
  InvokeJSNoResult('setSelectionRange',[aStart,aEnd_,aDirection]);
end;

procedure TJSHTMLTextAreaElement.setSelectionRange(aStart: LongWord; aEnd_: LongWord); overload;
begin
  InvokeJSNoResult('setSelectionRange',[aStart,aEnd_]);
end;

procedure TJSHTMLTextAreaElement.setUserInput(const aInput: UnicodeString);
begin
  InvokeJSNoResult('setUserInput',[aInput]);
end;

class function TJSHTMLTextAreaElement.Cast(Intf: IJSObject): IJSHTMLTextAreaElement;
begin
  Result:=TJSHTMLTextAreaElement.JOBCast(Intf);
end;

function TJSSVGSVGElement._Getx: IJSSVGAnimatedLength;
begin
  Result:=ReadJSPropertyObject('x',TJSSVGAnimatedLength) as IJSSVGAnimatedLength;
end;

function TJSSVGSVGElement._Gety: IJSSVGAnimatedLength;
begin
  Result:=ReadJSPropertyObject('y',TJSSVGAnimatedLength) as IJSSVGAnimatedLength;
end;

function TJSSVGSVGElement._Getwidth: IJSSVGAnimatedLength;
begin
  Result:=ReadJSPropertyObject('width',TJSSVGAnimatedLength) as IJSSVGAnimatedLength;
end;

function TJSSVGSVGElement._Getheight: IJSSVGAnimatedLength;
begin
  Result:=ReadJSPropertyObject('height',TJSSVGAnimatedLength) as IJSSVGAnimatedLength;
end;

function TJSSVGSVGElement._GetuseCurrentView: Boolean;
begin
  Result:=ReadJSPropertyBoolean('useCurrentView');
end;

function TJSSVGSVGElement._GetcurrentScale: Single;
begin
  Result:=ReadJSPropertyDouble('currentScale');
end;

function TJSSVGSVGElement._GetcurrentTranslate: IJSSVGPoint;
begin
  Result:=ReadJSPropertyObject('currentTranslate',TJSSVGPoint) as IJSSVGPoint;
end;

function TJSSVGSVGElement._GetviewBox: IJSSVGAnimatedRect;
begin
  Result:=ReadJSPropertyObject('viewBox',TJSSVGAnimatedRect) as IJSSVGAnimatedRect;
end;

function TJSSVGSVGElement._GetpreserveAspectRatio: IJSSVGAnimatedPreserveAspectRatio;
begin
  Result:=ReadJSPropertyObject('preserveAspectRatio',TJSSVGAnimatedPreserveAspectRatio) as IJSSVGAnimatedPreserveAspectRatio;
end;

function TJSSVGSVGElement._GetzoomAndPan: Word;
begin
  Result:=ReadJSPropertyLongInt('zoomAndPan');
end;

procedure TJSSVGSVGElement._SetcurrentScale(const aValue: Single);
begin
  WriteJSPropertyDouble('currentScale',aValue);
end;

procedure TJSSVGSVGElement._SetzoomAndPan(const aValue: Word);
begin
  WriteJSPropertyLongInt('zoomAndPan',aValue);
end;

function TJSSVGSVGElement.suspendRedraw(aMaxWaitMilliseconds: LongWord): LongWord;
begin
  Result:=InvokeJSMaxIntResult('suspendRedraw',[aMaxWaitMilliseconds]);
end;

procedure TJSSVGSVGElement.unsuspendRedraw(aSuspendHandleID: LongWord);
begin
  InvokeJSNoResult('unsuspendRedraw',[aSuspendHandleID]);
end;

procedure TJSSVGSVGElement.unsuspendRedrawAll;
begin
  InvokeJSNoResult('unsuspendRedrawAll',[]);
end;

procedure TJSSVGSVGElement.forceRedraw;
begin
  InvokeJSNoResult('forceRedraw',[]);
end;

procedure TJSSVGSVGElement.pauseAnimations;
begin
  InvokeJSNoResult('pauseAnimations',[]);
end;

procedure TJSSVGSVGElement.unpauseAnimations;
begin
  InvokeJSNoResult('unpauseAnimations',[]);
end;

function TJSSVGSVGElement.animationsPaused: Boolean;
begin
  Result:=InvokeJSBooleanResult('animationsPaused',[]);
end;

function TJSSVGSVGElement.getCurrentTime: Single;
begin
  Result:=InvokeJSDoubleResult('getCurrentTime',[]);
end;

procedure TJSSVGSVGElement.setCurrentTime(aSeconds: Single);
begin
  InvokeJSNoResult('setCurrentTime',[aSeconds]);
end;

procedure TJSSVGSVGElement.deselectAll;
begin
  InvokeJSNoResult('deselectAll',[]);
end;

function TJSSVGSVGElement.createSVGNumber: IJSSVGNumber;
begin
  Result:=InvokeJSObjectResult('createSVGNumber',[],TJSSVGNumber) as IJSSVGNumber;
end;

function TJSSVGSVGElement.createSVGLength: IJSSVGLength;
begin
  Result:=InvokeJSObjectResult('createSVGLength',[],TJSSVGLength) as IJSSVGLength;
end;

function TJSSVGSVGElement.createSVGAngle: IJSSVGAngle;
begin
  Result:=InvokeJSObjectResult('createSVGAngle',[],TJSSVGAngle) as IJSSVGAngle;
end;

function TJSSVGSVGElement.createSVGPoint: IJSSVGPoint;
begin
  Result:=InvokeJSObjectResult('createSVGPoint',[],TJSSVGPoint) as IJSSVGPoint;
end;

function TJSSVGSVGElement.createSVGMatrix: IJSSVGMatrix;
begin
  Result:=InvokeJSObjectResult('createSVGMatrix',[],TJSSVGMatrix) as IJSSVGMatrix;
end;

function TJSSVGSVGElement.createSVGRect: IJSSVGRect;
begin
  Result:=InvokeJSObjectResult('createSVGRect',[],TJSSVGRect) as IJSSVGRect;
end;

function TJSSVGSVGElement.createSVGTransform: IJSSVGTransform;
begin
  Result:=InvokeJSObjectResult('createSVGTransform',[],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGSVGElement.createSVGTransformFromMatrix(const aMatrix: TJSDOMMatrix2DInit): IJSSVGTransform; overload;
begin
  Result:=InvokeJSObjectResult('createSVGTransformFromMatrix',[aMatrix],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGSVGElement.createSVGTransformFromMatrix: IJSSVGTransform; overload;
begin
  Result:=InvokeJSObjectResult('createSVGTransformFromMatrix',[],TJSSVGTransform) as IJSSVGTransform;
end;

function TJSSVGSVGElement.getElementById(const aElementId: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aElementId],TJSElement) as IJSElement;
end;

class function TJSSVGSVGElement.Cast(Intf: IJSObject): IJSSVGSVGElement;
begin
  Result:=TJSSVGSVGElement.JOBCast(Intf);
end;


initialization
  JSDocument:=TJSDocument.JOBCreateGlobal('document');
  JSWindow:=TJSWindow.JOBCreateGlobal('window');
finalization
  JSDocument.Free;
  JSWindow.Free;
end.
