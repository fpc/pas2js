/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */


dictionary EventListenerOptions {
  boolean capture = false;
  /* Setting to true make the listener be added to the system group. */
  [Func="ThreadSafeIsChromeOrUAWidget"]
  boolean mozSystemGroup = false;
};

dictionary AddEventListenerOptions : EventListenerOptions {
  boolean passive;
  boolean once = false;
// Mattias:  AbortSignal signal;
  [ChromeOnly]
  boolean wantUntrusted;
};

[Exposed=(Window,Worker,WorkerDebugger,AudioWorklet)]
interface EventTarget {
  [Throws]
  constructor();

  /* Passing null for wantsUntrusted means "default behavior", which
     differs in content and chrome.  In content that default boolean
     value is true, while in chrome the default boolean value is
     false. */
  [Throws]
  void addEventListener(DOMString type,
                        EventListener? listener,
                        optional (AddEventListenerOptions or boolean) options = {},
                        optional boolean? wantsUntrusted = null);
  [Throws]
  void removeEventListener(DOMString type,
                           EventListener? listener,
                           optional (EventListenerOptions or boolean) options = {});
  [Throws, NeedsCallerType]
  boolean dispatchEvent(Event event);
};

// Mozilla extensions for use by JS-implemented event targets to
// implement on* properties.
partial interface EventTarget {
  // The use of [TreatNonCallableAsNull] here is a bit of a hack: it just makes
  // the codegen check whether the type involved is either
  // [TreatNonCallableAsNull] or [TreatNonObjectAsNull] and if it is handle it
  // accordingly.  In particular, it will NOT actually treat a non-null
  // non-callable object as null here.
  [ChromeOnly, Throws]
  void setEventHandler(DOMString type,
                       [TreatNonCallableAsNull] EventHandler handler);

// Mattias:   [ChromeOnly]
//  EventHandler getEventHandler(DOMString type);
};

// Mozilla extension to make firing events on event targets from
// chrome easier.  This returns the window which can be used to create
// events to fire at this EventTarget, or null if there isn't one.
partial interface EventTarget {
// Mattias:  [ChromeOnly, Exposed=Window, BinaryName="ownerGlobalForBindings"]
//  readonly attribute WindowProxy? ownerGlobal;
};
interface Node : EventTarget {
  const unsigned short ELEMENT_NODE = 1;
  const unsigned short ATTRIBUTE_NODE = 2;
  const unsigned short TEXT_NODE = 3;
  const unsigned short CDATA_SECTION_NODE = 4;
  const unsigned short ENTITY_REFERENCE_NODE = 5; // legacy
  const unsigned short ENTITY_NODE = 6; // legacy
  const unsigned short PROCESSING_INSTRUCTION_NODE = 7;
  const unsigned short COMMENT_NODE = 8;
  const unsigned short DOCUMENT_NODE = 9;
  const unsigned short DOCUMENT_TYPE_NODE = 10;
  const unsigned short DOCUMENT_FRAGMENT_NODE = 11;
  const unsigned short NOTATION_NODE = 12; // legacy
  readonly attribute unsigned short nodeType;
  readonly attribute DOMString nodeName;

  readonly attribute USVString baseURI;

  readonly attribute boolean isConnected;
  readonly attribute Document? ownerDocument;
  Node getRootNode(optional GetRootNodeOptions options = {});
  readonly attribute Node? parentNode;
  readonly attribute Element? parentElement;
  boolean hasChildNodes();
  [SameObject] readonly attribute NodeList childNodes;
  readonly attribute Node? firstChild;
  readonly attribute Node? lastChild;
  readonly attribute Node? previousSibling;
  readonly attribute Node? nextSibling;

  [CEReactions] attribute DOMString? nodeValue;
  [CEReactions] attribute DOMString? textContent;
  [CEReactions] undefined normalize();

  [CEReactions, NewObject] Node cloneNode(optional boolean deep = false);
  boolean isEqualNode(Node? otherNode);
  boolean isSameNode(Node? otherNode); // legacy alias of ===

  const unsigned short DOCUMENT_POSITION_DISCONNECTED = 0x01;
  const unsigned short DOCUMENT_POSITION_PRECEDING = 0x02;
  const unsigned short DOCUMENT_POSITION_FOLLOWING = 0x04;
  const unsigned short DOCUMENT_POSITION_CONTAINS = 0x08;
  const unsigned short DOCUMENT_POSITION_CONTAINED_BY = 0x10;
  const unsigned short DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 0x20;
  unsigned short compareDocumentPosition(Node other);
  boolean contains(Node? other);

  DOMString? lookupPrefix(DOMString? namespace);
  DOMString? lookupNamespaceURI(DOMString? prefix);
  boolean isDefaultNamespace(DOMString? namespace);

  [CEReactions] Node insertBefore(Node node, Node? child);
  [CEReactions] Node appendChild(Node node);
  [CEReactions] Node replaceChild(Node node, Node child);
  [CEReactions] Node removeChild(Node child);
};

dictionary GetRootNodeOptions {
  boolean composed = false;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
// Mattias:
//callback interface EventListener {
//  void handleEvent(Event event);
//};
callback EventListener = boolean (Event event);

interface Event {
// Mattias:   constructor(DOMString type, optional EventInit eventInitDict = {});

  readonly attribute DOMString type;
  readonly attribute EventTarget? target;
  readonly attribute EventTarget? srcElement; // legacy
  readonly attribute EventTarget? currentTarget;
  sequence<EventTarget> composedPath();

  const unsigned short NONE = 0;
  const unsigned short CAPTURING_PHASE = 1;
  const unsigned short AT_TARGET = 2;
  const unsigned short BUBBLING_PHASE = 3;
  readonly attribute unsigned short eventPhase;

  undefined stopPropagation();
           attribute boolean cancelBubble; // legacy alias of .stopPropagation()
  undefined stopImmediatePropagation();

  readonly attribute boolean bubbles;
  readonly attribute boolean cancelable;
           attribute boolean returnValue;  // legacy
  undefined preventDefault();
  readonly attribute boolean defaultPrevented;
  readonly attribute boolean composed;

// Mattias:   [LegacyUnforgeable] readonly attribute boolean isTrusted;
//  readonly attribute DOMHighResTimeStamp timeStamp;

  undefined initEvent(DOMString type, optional boolean bubbles = false, optional boolean cancelable = false); // legacy
};


/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#eventhandler
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */
[LegacyTreatNonObjectAsNull]
callback EventHandlerNonNull = any (Event event);
typedef EventHandlerNonNull? EventHandler;

[LegacyTreatNonObjectAsNull]
callback OnBeforeUnloadEventHandlerNonNull = DOMString? (Event event);
typedef OnBeforeUnloadEventHandlerNonNull? OnBeforeUnloadEventHandler;

[LegacyTreatNonObjectAsNull]
callback OnErrorEventHandlerNonNull = any ((Event or DOMString) event, optional DOMString source, optional unsigned long lineno, optional unsigned long column, optional any error);
typedef OnErrorEventHandlerNonNull? OnErrorEventHandler;

interface mixin GlobalEventHandlers {
           attribute EventHandler onabort;
           attribute EventHandler onblur;
// We think the spec is wrong here. See OnErrorEventHandlerForNodes/Window
// below.
//         attribute OnErrorEventHandler onerror;
           attribute EventHandler onfocus;
           //(Not implemented)attribute EventHandler oncancel;
           attribute EventHandler onauxclick;
           [Pref="dom.input_events.beforeinput.enabled"]
           attribute EventHandler onbeforeinput;
           attribute EventHandler oncanplay;
           attribute EventHandler oncanplaythrough;
           attribute EventHandler onchange;
           attribute EventHandler onclick;
           attribute EventHandler onclose;
           attribute EventHandler oncontextmenu;
           attribute EventHandler oncuechange;
           attribute EventHandler ondblclick;
           attribute EventHandler ondrag;
           attribute EventHandler ondragend;
           attribute EventHandler ondragenter;
           [Func="Event::IsDragExitEnabled"]
           attribute EventHandler ondragexit;
           attribute EventHandler ondragleave;
           attribute EventHandler ondragover;
           attribute EventHandler ondragstart;
           attribute EventHandler ondrop;
           attribute EventHandler ondurationchange;
           attribute EventHandler onemptied;
           attribute EventHandler onended;
           attribute EventHandler onformdata;
           attribute EventHandler oninput;
           attribute EventHandler oninvalid;
           attribute EventHandler onkeydown;
           attribute EventHandler onkeypress;
           attribute EventHandler onkeyup;
           attribute EventHandler onload;
           attribute EventHandler onloadeddata;
           attribute EventHandler onloadedmetadata;
           attribute EventHandler onloadend;
           attribute EventHandler onloadstart;
           attribute EventHandler onmousedown;
  [LegacyLenientThis] attribute EventHandler onmouseenter;
  [LegacyLenientThis] attribute EventHandler onmouseleave;
           attribute EventHandler onmousemove;
           attribute EventHandler onmouseout;
           attribute EventHandler onmouseover;
           attribute EventHandler onmouseup;
           attribute EventHandler onwheel;
           attribute EventHandler onpause;
           attribute EventHandler onplay;
           attribute EventHandler onplaying;
           attribute EventHandler onprogress;
           attribute EventHandler onratechange;
           attribute EventHandler onreset;
           attribute EventHandler onresize;
           attribute EventHandler onscroll;
           attribute EventHandler onsecuritypolicyviolation;
           attribute EventHandler onseeked;
           attribute EventHandler onseeking;
           attribute EventHandler onselect;
           attribute EventHandler onslotchange;
           //(Not implemented)attribute EventHandler onsort;
           attribute EventHandler onstalled;
           attribute EventHandler onsubmit;
           attribute EventHandler onsuspend;
           attribute EventHandler ontimeupdate;
           attribute EventHandler onvolumechange;
           attribute EventHandler onwaiting;

           attribute EventHandler onselectstart;
           attribute EventHandler onselectionchange;

           attribute EventHandler ontoggle;

           // Pointer events handlers
           attribute EventHandler onpointercancel;
           attribute EventHandler onpointerdown;
           attribute EventHandler onpointerup;
           attribute EventHandler onpointermove;
           attribute EventHandler onpointerout;
           attribute EventHandler onpointerover;
           attribute EventHandler onpointerenter;
           attribute EventHandler onpointerleave;
           attribute EventHandler ongotpointercapture;
           attribute EventHandler onlostpointercapture;

           // Mozilla-specific handlers. Unprefixed handlers live in
           // Document rather than here.
           [Deprecated="MozfullscreenchangeDeprecatedPrefix"]
           attribute EventHandler onmozfullscreenchange;
           [Deprecated="MozfullscreenerrorDeprecatedPrefix"]
           attribute EventHandler onmozfullscreenerror;

           // CSS-Animation and CSS-Transition handlers.
           attribute EventHandler onanimationcancel;
           attribute EventHandler onanimationend;
           attribute EventHandler onanimationiteration;
           attribute EventHandler onanimationstart;
           attribute EventHandler ontransitioncancel;
           attribute EventHandler ontransitionend;
           attribute EventHandler ontransitionrun;
           attribute EventHandler ontransitionstart;

           // CSS-Animation and CSS-Transition legacy handlers.
           // This handler isn't standard.
           [BinaryName="onwebkitAnimationEnd"]
           attribute EventHandler onwebkitanimationend;
           [BinaryName="onwebkitAnimationIteration"]
           attribute EventHandler onwebkitanimationiteration;
           [BinaryName="onwebkitAnimationStart"]
           attribute EventHandler onwebkitanimationstart;
           [BinaryName="onwebkitTransitionEnd"]
           attribute EventHandler onwebkittransitionend;
};

interface mixin WindowEventHandlers {
           attribute EventHandler onafterprint;
           attribute EventHandler onbeforeprint;
           attribute OnBeforeUnloadEventHandler onbeforeunload;
           attribute EventHandler onhashchange;
           attribute EventHandler onlanguagechange;
           attribute EventHandler onmessage;
           attribute EventHandler onmessageerror;
           attribute EventHandler onoffline;
           attribute EventHandler ononline;
           attribute EventHandler onpagehide;
           attribute EventHandler onpageshow;
           attribute EventHandler onpopstate;
           attribute EventHandler onrejectionhandled;
           attribute EventHandler onstorage;
           attribute EventHandler onunhandledrejection;
           attribute EventHandler onunload;
};

// https://w3c.github.io/gamepad/#extensions-to-the-windoweventhandlers-interface-mixin
partial interface mixin WindowEventHandlers {
  attribute EventHandler ongamepadconnected;
  attribute EventHandler ongamepaddisconnected;
};

interface mixin DocumentAndElementEventHandlers {
  attribute EventHandler oncopy;
  attribute EventHandler oncut;
  attribute EventHandler onpaste;
};

// The spec has |attribute OnErrorEventHandler onerror;| on
// GlobalEventHandlers, and calls the handler differently depending on
// whether an ErrorEvent was fired. We don't do that, and until we do we'll
// need to distinguish between onerror on Window or on nodes.

interface mixin OnErrorEventHandlerForNodes {
           attribute EventHandler onerror;
};

interface mixin OnErrorEventHandlerForWindow {
           attribute OnErrorEventHandler onerror;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://dom.spec.whatwg.org/#interface-document
 * https://html.spec.whatwg.org/multipage/dom.html#the-document-object
 * https://html.spec.whatwg.org/multipage/obsolete.html#other-elements%2C-attributes-and-apis
 * https://fullscreen.spec.whatwg.org/#api
 * https://w3c.github.io/pointerlock/#extensions-to-the-document-interface
 * https://w3c.github.io/pointerlock/#extensions-to-the-documentorshadowroot-mixin
 * https://w3c.github.io/page-visibility/#extensions-to-the-document-interface
 * https://drafts.csswg.org/cssom/#extensions-to-the-document-interface
 * https://drafts.csswg.org/cssom-view/#extensions-to-the-document-interface
 * https://wicg.github.io/feature-policy/#policy
 */

interface ContentSecurityPolicy;
interface Principal;
interface WindowProxy;
interface nsISupports;
interface URI;
interface nsIDocShell;
interface nsILoadGroup;
interface nsIReferrerInfo;
interface nsICookieJarSettings;
interface nsIPermissionDelegateHandler;
interface XULCommandDispatcher;

enum VisibilityState { "hidden", "visible" };

/* https://dom.spec.whatwg.org/#dictdef-elementcreationoptions */
dictionary ElementCreationOptions {
  DOMString is;

  [ChromeOnly]
  DOMString pseudo;
};

/* https://dom.spec.whatwg.org/#interface-document */
[Exposed=Window]
interface Document : Node {
  [Throws]
  constructor();

  [Throws]
  readonly attribute DOMImplementation implementation;
  [Pure, Throws, BinaryName="documentURIFromJS", NeedsCallerType]
  readonly attribute DOMString URL;
  [Pure, Throws, BinaryName="documentURIFromJS", NeedsCallerType]
  readonly attribute DOMString documentURI;
  [Pure]
  readonly attribute DOMString compatMode;
  [Pure]
  readonly attribute DOMString characterSet;
  [Pure,BinaryName="characterSet"]
  readonly attribute DOMString charset; // legacy alias of .characterSet
  [Pure,BinaryName="characterSet"]
  readonly attribute DOMString inputEncoding; // legacy alias of .characterSet
  [Pure]
  readonly attribute DOMString contentType;

  [Pure]
  readonly attribute DocumentType? doctype;
  [Pure]
  readonly attribute Element? documentElement;
  [Pure]
  HTMLCollection getElementsByTagName(DOMString localName);
  [Pure, Throws]
  HTMLCollection getElementsByTagNameNS(DOMString? namespace, DOMString localName);
  [Pure]
  HTMLCollection getElementsByClassName(DOMString classNames);
  [Pure]
  Element? getElementById(DOMString elementId);

  // These DOM methods cannot be accessed by UA Widget scripts
  // because the DOM element reflectors will be in the content scope,
  // instead of the desired UA Widget scope.
  [CEReactions, NewObject, Throws, Func="IsNotUAWidget"]
  Element createElement(DOMString localName, optional (ElementCreationOptions or DOMString) options = {});
  [CEReactions, NewObject, Throws, Func="IsNotUAWidget"]
  Element createElementNS(DOMString? namespace, DOMString qualifiedName, optional (ElementCreationOptions or DOMString) options = {});
  [NewObject]
  DocumentFragment createDocumentFragment();
  [NewObject, Func="IsNotUAWidget"]
  Text createTextNode(DOMString data);
  [NewObject, Func="IsNotUAWidget"]
  Comment createComment(DOMString data);
// Mattias:   [NewObject, Throws]
//  ProcessingInstruction createProcessingInstruction(DOMString target, DOMString data);

  [CEReactions, Throws, Func="IsNotUAWidget"]
  Node importNode(Node node, optional boolean deep = false);
  [CEReactions, Throws, Func="IsNotUAWidget"]
  Node adoptNode(Node node);

  [NewObject, Throws, NeedsCallerType]
  Event createEvent(DOMString interface);

  [NewObject, Throws]
  Range createRange();

  // NodeFilter.SHOW_ALL = 0xFFFFFFFF
// Mattias:  [NewObject, Throws]
//   NodeIterator createNodeIterator(Node root, optional unsigned long whatToShow = 0xFFFFFFFF, optional NodeFilter? filter = null);
// Mattias:  [NewObject, Throws]
//  TreeWalker createTreeWalker(Node root, optional unsigned long whatToShow = 0xFFFFFFFF, optional NodeFilter? filter = null);

  // NEW
  // No support for prepend/append yet
  // void prepend((Node or DOMString)... nodes);
  // void append((Node or DOMString)... nodes);

  // These are not in the spec, but leave them for now for backwards compat.
  // So sort of like Gecko extensions
  [NewObject, Throws]
  CDATASection createCDATASection(DOMString data);
  [NewObject, Throws]
  Attr createAttribute(DOMString name);
  [NewObject, Throws]
  Attr createAttributeNS(DOMString? namespace, DOMString name);
};

// https://html.spec.whatwg.org/multipage/dom.html#the-document-object
partial interface Document {
  [PutForwards=href, LegacyUnforgeable] readonly attribute Location? location;
  [SetterThrows]                           attribute DOMString domain;
  readonly attribute DOMString referrer;
  [Throws] attribute DOMString cookie;
  readonly attribute DOMString lastModified;
  readonly attribute DOMString readyState;

  // DOM tree accessors
  //(Not proxy yet)getter object (DOMString name);
  [CEReactions, SetterThrows, Pure]
           attribute DOMString title;
  [CEReactions, Pure]
           attribute DOMString dir;
   [CEReactions, Pure, SetterThrows]
           attribute HTMLElement? body;
  [Pure]
  readonly attribute HTMLHeadElement? head;
  [SameObject] readonly attribute HTMLCollection images;
  [SameObject] readonly attribute HTMLCollection embeds;
  [SameObject] readonly attribute HTMLCollection plugins;
  [SameObject] readonly attribute HTMLCollection links;
  [SameObject] readonly attribute HTMLCollection forms;
  [SameObject] readonly attribute HTMLCollection scripts;
  [Pure]
  NodeList getElementsByName(DOMString elementName);
  //(Not implemented)readonly attribute DOMElementMap cssElementMap;

  // dynamic markup insertion
  [CEReactions, Throws]
  Document open(optional DOMString unused1, optional DOMString unused2); // both arguments are ignored
  [CEReactions, Throws]
  WindowProxy? open(USVString url, DOMString name, DOMString features);
  [CEReactions, Throws]
  void close();
  [CEReactions, Throws]
  void write(DOMString... text);
  [CEReactions, Throws]
  void writeln(DOMString... text);

  // user interaction
  [Pure]
  readonly attribute WindowProxy? defaultView;
  [Throws]
  boolean hasFocus();
  [CEReactions, SetterThrows, SetterNeedsSubjectPrincipal]
           attribute DOMString designMode;
  [CEReactions, Throws, NeedsSubjectPrincipal]
  boolean execCommand(DOMString commandId, optional boolean showUI = false,
                      optional DOMString value = "");
  [Throws, NeedsSubjectPrincipal]
  boolean queryCommandEnabled(DOMString commandId);
  [Throws]
  boolean queryCommandIndeterm(DOMString commandId);
  [Throws]
  boolean queryCommandState(DOMString commandId);
  [Throws, NeedsCallerType]
  boolean queryCommandSupported(DOMString commandId);
  [Throws]
  DOMString queryCommandValue(DOMString commandId);
  //(Not implemented)readonly attribute HTMLCollection commands;

  // special event handler IDL attributes that only apply to Document objects
  [LegacyLenientThis] attribute EventHandler onreadystatechange;

  // Gecko extensions?
                attribute EventHandler onbeforescriptexecute;
                attribute EventHandler onafterscriptexecute;

  /**
   * True if this document is synthetic : stand alone image, video, audio file,
   * etc.
   */
  [Func="IsChromeOrUAWidget"] readonly attribute boolean mozSyntheticDocument;
  /**
   * Returns the script element whose script is currently being processed.
   *
   * @see <https://developer.mozilla.org/en/DOM/document.currentScript>
   */
  [Pure]
  readonly attribute Element? currentScript;
  /**
   * Release the current mouse capture if it is on an element within this
   * document.
   *
   * @see <https://developer.mozilla.org/en/DOM/document.releaseCapture>
   */
  [Deprecated=DocumentReleaseCapture, Pref="dom.mouse_capture.enabled"]
  void releaseCapture();
  /**
   * Use the given DOM element as the source image of target |-moz-element()|.
   *
   * This function introduces a new special ID (called "image element ID"),
   * which is only used by |-moz-element()|, and associates it with the given
   * DOM element.  Image elements ID's have the higher precedence than general
   * HTML id's, so if |document.mozSetImageElement(<id>, <element>)| is called,
   * |-moz-element(#<id>)| uses |<element>| as the source image even if there
   * is another element with id attribute = |<id>|.  To unregister an image
   * element ID |<id>|, call |document.mozSetImageElement(<id>, null)|.
   *
   * Example:
   * <script>
   *   canvas = document.createElement("canvas");
   *   canvas.setAttribute("width", 100);
   *   canvas.setAttribute("height", 100);
   *   // draw to canvas
   *   document.mozSetImageElement("canvasbg", canvas);
   * </script>
   * <div style="background-image: -moz-element(#canvasbg);"></div>
   *
   * @param aImageElementId an image element ID to associate with
   * |aImageElement|
   * @param aImageElement a DOM element to be used as the source image of
   * |-moz-element(#aImageElementId)|. If this is null, the function will
   * unregister the image element ID |aImageElementId|.
   *
   * @see <https://developer.mozilla.org/en/DOM/document.mozSetImageElement>
   */
  [UseCounter]
  void mozSetImageElement(DOMString aImageElementId,
                          Element? aImageElement);

  [ChromeOnly]
  readonly attribute URI? documentURIObject;

  /**
   * Current referrer policy - one of the referrer policy value from
   * ReferrerPolicy.webidl.
   */
  [ChromeOnly]
  readonly attribute ReferrerPolicy referrerPolicy;

    /**
   * Current referrer info, which holds all referrer related information
   * including referrer policy and raw referrer of document.
   */
  [ChromeOnly]
  readonly attribute nsIReferrerInfo referrerInfo;

};

// https://html.spec.whatwg.org/multipage/obsolete.html#other-elements%2C-attributes-and-apis
partial interface Document {
  [CEReactions] attribute [LegacyNullToEmptyString] DOMString fgColor;
  [CEReactions] attribute [LegacyNullToEmptyString] DOMString linkColor;
  [CEReactions] attribute [LegacyNullToEmptyString] DOMString vlinkColor;
  [CEReactions] attribute [LegacyNullToEmptyString] DOMString alinkColor;
  [CEReactions] attribute [LegacyNullToEmptyString] DOMString bgColor;

  [SameObject] readonly attribute HTMLCollection anchors;
  [SameObject] readonly attribute HTMLCollection applets;

  void clear();
  // @deprecated These are old Netscape 4 methods. Do not use,
  //             the implementation is no-op.
  // XXXbz do we actually need these anymore?
  void captureEvents();
  void releaseEvents();

  [SameObject] readonly attribute HTMLAllCollection all;
};

// https://fullscreen.spec.whatwg.org/#api
partial interface Document {
  // Note: Per spec the 'S' in these two is lowercase, but the "Moz"
  // versions have it uppercase.
  [LegacyLenientSetter, Unscopable]
  readonly attribute boolean fullscreen;
  [BinaryName="fullscreen"]
  readonly attribute boolean mozFullScreen;
  [LegacyLenientSetter, NeedsCallerType]
  readonly attribute boolean fullscreenEnabled;
  [BinaryName="fullscreenEnabled", NeedsCallerType]
  readonly attribute boolean mozFullScreenEnabled;

  [NewObject]
  Promise<void> exitFullscreen();
  [NewObject, BinaryName="exitFullscreen"]
  Promise<void> mozCancelFullScreen();

  // Events handlers
  attribute EventHandler onfullscreenchange;
  attribute EventHandler onfullscreenerror;
};

// https://w3c.github.io/pointerlock/#extensions-to-the-document-interface
// https://w3c.github.io/pointerlock/#extensions-to-the-documentorshadowroot-mixin
partial interface Document {
  void exitPointerLock();

  // Event handlers
  attribute EventHandler onpointerlockchange;
  attribute EventHandler onpointerlockerror;
};

// Mozilla-internal document extensions specific to error pages.
partial interface Document {
  [Func="Document::CallerIsTrustedAboutCertError", NewObject]
  Promise<any> addCertException(boolean isTemporary);

  [Func="Document::CallerIsTrustedAboutHttpsOnlyError"]
  void reloadWithHttpsOnlyException();

// Mattias:   [Func="Document::CallerIsTrustedAboutCertError", Throws]
//  FailedCertSecurityInfo getFailedCertSecurityInfo();

// Mattias:   [Func="Document::CallerIsTrustedAboutNetError", Throws]
//  NetErrorInfo getNetErrorInfo();

  [Func="Document::CallerIsTrustedAboutNetError"]
  attribute boolean allowDeprecatedTls;
};

// https://w3c.github.io/page-visibility/#extensions-to-the-document-interface
partial interface Document {
  readonly attribute boolean hidden;
  readonly attribute VisibilityState visibilityState;
           attribute EventHandler onvisibilitychange;
};

// https://drafts.csswg.org/cssom/#extensions-to-the-document-interface
partial interface Document {
    attribute DOMString? selectedStyleSheetSet;
    readonly attribute DOMString? lastStyleSheetSet;
    readonly attribute DOMString? preferredStyleSheetSet;
    [Constant]
    readonly attribute DOMStringList styleSheetSets;
    void enableStyleSheetsForSet (DOMString? name);
};

// https://drafts.csswg.org/cssom-view/#extensions-to-the-document-interface
partial interface Document {
    CaretPosition? caretPositionFromPoint (float x, float y);

    readonly attribute Element? scrollingElement;
};

// http://dev.w3.org/2006/webapi/selectors-api2/#interface-definitions
partial interface Document {
  [Throws, Pure]
  Element?  querySelector(UTF8String selectors);
  [Throws, Pure]
  NodeList  querySelectorAll(UTF8String selectors);

  //(Not implemented)Element?  find(DOMString selectors, optional (Element or sequence<Node>)? refNodes);
  //(Not implemented)NodeList  findAll(DOMString selectors, optional (Element or sequence<Node>)? refNodes);
};

// https://drafts.csswg.org/web-animations/#extensions-to-the-document-interface
partial interface Document {
// Mattias:   [Func="Document::AreWebAnimationsTimelinesEnabled"]
//  readonly attribute DocumentTimeline timeline;
};

// https://svgwg.org/svg2-draft/struct.html#InterfaceDocumentExtensions
partial interface Document {
  [BinaryName="SVGRootElement"]
  readonly attribute SVGSVGElement? rootElement;
};

//  Mozilla extensions of various sorts
partial interface Document {
  // Creates a new XUL element regardless of the document's default type.
  [ChromeOnly, CEReactions, NewObject, Throws]
  Element createXULElement(DOMString localName, optional (ElementCreationOptions or DOMString) options = {});
  // Wether the document was loaded using a nsXULPrototypeDocument.
  [ChromeOnly]
  readonly attribute boolean loadedFromPrototype;

  // The principal to use for the storage area of this document
  [ChromeOnly]
  readonly attribute Principal effectiveStoragePrincipal;

  // You should probably not be using this principal getter since it performs
  // no checks to ensure that the partitioned principal should really be used
  // here.  It is only designed to be used in very specific circumstances, such
  // as when inheriting the document/storage principal.
  [ChromeOnly]
  readonly attribute Principal partitionedPrincipal;

  // The cookieJarSettings of this document
  [ChromeOnly]
  readonly attribute nsICookieJarSettings cookieJarSettings;

  // Touch bits
  // XXXbz I can't find the sane spec for this stuff, so just cribbing
  // from our xpidl for now.
// Mattiastodo:   [NewObject, Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
  //Touch createTouch(optional Window? view = null,
  //                  optional EventTarget? target = null,
  //                  optional long identifier = 0,
  //                  optional long pageX = 0,
  //                  optional long pageY = 0,
  //                  optional long screenX = 0,
  //                  optional long screenY = 0,
  //                  optional long clientX = 0,
  //                  optional long clientY = 0,
  //                  optional long radiusX = 0,
  //                  optional long radiusY = 0,
  //                  optional float rotationAngle = 0,
  //                  optional float force = 0);
  // XXXbz a hack to get around the fact that we don't support variadics as
  // distinguishing arguments yet.  Once this hack is removed. we can also
  // remove the corresponding overload on Document, since Touch... and
  // sequence<Touch> look the same in the C++.
// Mattias:   [NewObject, Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
//  TouchList createTouchList(Touch touch, Touch... touches);
  // XXXbz and another hack for the fact that we can't usefully have optional
  // distinguishing arguments but need a working zero-arg form of
  // createTouchList().
// Mattias:   [NewObject, Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
//  TouchList createTouchList();
// Mattias:   [NewObject, Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
//  TouchList createTouchList(sequence<Touch> touches);

  [ChromeOnly]
  attribute boolean styleSheetChangeEventsEnabled;

  [ChromeOnly]
  attribute boolean shadowRootAttachedEventEnabled;

  [ChromeOnly] readonly attribute DOMString contentLanguage;

  [ChromeOnly] readonly attribute nsILoadGroup? documentLoadGroup;

  // Blocks the initial document parser until the given promise is settled.
  [ChromeOnly, NewObject]
  Promise<any> blockParsing(Promise<any> promise,
                            optional BlockParsingOptions options = {});

  [Func="nsContentUtils::IsPDFJS", BinaryName="blockUnblockOnloadForPDFJS"]
  void blockUnblockOnload(boolean block);

  // like documentURI, except that for error pages, it returns the URI we were
  // trying to load when we hit an error, rather than the error page's own URI.
  [ChromeOnly] readonly attribute URI? mozDocumentURIIfNotForErrorPages;

  // A promise that is resolved when we have both fired DOMContentLoaded and
  // are ready to start layout.
  // This is used for the  "document_idle" webextension script injection point.
  [ChromeOnly, Throws]
  readonly attribute Promise<void> documentReadyForIdle;

  // Lazily created command dispatcher, returns null if the document is not
  // chrome privileged.
  [ChromeOnly]
  readonly attribute XULCommandDispatcher? commandDispatcher;

  [ChromeOnly]
  attribute boolean devToolsWatchingDOMMutations;

  /**
   * Returns all the shadow roots connected to the document, in no particular
   * order, and without regard to open/closed-ness. Also returns UA widgets
   * (like <video> controls), which can be checked using
   * ShadowRoot.isUAWidget().
   */
// Mattias:   [ChromeOnly]
//  sequence<ShadowRoot> getConnectedShadowRoots();
};

dictionary BlockParsingOptions {
  /**
   * If true, blocks script-created parsers (created via document.open()) in
   * addition to network-created parsers.
   */
  boolean blockScriptCreated = true;
};

// Extension to give chrome JS the ability to determine when a document was
// created to satisfy an iframe with srcdoc attribute.
partial interface Document {
  [ChromeOnly] readonly attribute boolean isSrcdocDocument;
};


// Extension to give chrome JS the ability to get the underlying
// sandbox flag attribute
partial interface Document {
  [ChromeOnly] readonly attribute DOMString? sandboxFlagsAsString;
};


/**
 * Chrome document anonymous content management.
 * This is a Chrome-only API that allows inserting fixed positioned anonymous
 * content on top of the current page displayed in the document.
 * The supplied content is cloned and inserted into the document's CanvasFrame.
 * Note that this only works for HTML documents.
 */
partial interface Document {
  /**
   * Deep-clones the provided element and inserts it into the CanvasFrame.
   * Returns an AnonymousContent instance that can be used to manipulate the
   * inserted element.
   *
   * If aForce is true, tries to update layout to be able to insert the element
   * synchronously.
   */
// Mattias:   [ChromeOnly, NewObject, Throws]
//  AnonymousContent insertAnonymousContent(Element aElement, optional boolean aForce = false);

  /**
   * Removes the element inserted into the CanvasFrame given an AnonymousContent
   * instance.
   */
// Mattias:   [ChromeOnly, Throws]
//  void removeAnonymousContent(AnonymousContent aContent);
};

// http://w3c.github.io/selection-api/#extensions-to-document-interface
partial interface Document {
   [Throws]
  Selection? getSelection();
};

// https://github.com/whatwg/html/issues/3338
partial interface Document {
  [Pref="dom.storage_access.enabled", NewObject]
  Promise<boolean> hasStorageAccess();
  [Pref="dom.storage_access.enabled", NewObject]
  Promise<void> requestStorageAccess();
};

// A privileged API to give chrome privileged code and the content script of the
// webcompat extension the ability to request the storage access for a given
// third party.
partial interface Document {
  [Func="Document::CallerCanAccessPrivilegeSSA", NewObject]
  Promise<void> requestStorageAccessForOrigin(DOMString thirdPartyOrigin, optional boolean requireUserInteraction = true);
};

enum DocumentAutoplayPolicy {
  "allowed",       // autoplay is currently allowed
  "allowed-muted", // muted video autoplay is currently allowed
  "disallowed"     // autoplay is not current allowed
};

// https://github.com/WICG/autoplay/issues/1
partial interface Document {
  [Pref="dom.media.autoplay.autoplay-policy-api"]
  readonly attribute DocumentAutoplayPolicy autoplayPolicy;
};

// Extension to give chrome JS the ability to determine whether
// the user has interacted with the document or not.
partial interface Document {
  [ChromeOnly] readonly attribute boolean userHasInteracted;
};

// Extension to give chrome JS the ability to simulate activate the document
// by user gesture.
partial interface Document {
  [ChromeOnly]
  void notifyUserGestureActivation();
  // For testing only.
  [ChromeOnly]
  void clearUserGestureActivation();
  [ChromeOnly]
  readonly attribute boolean hasBeenUserGestureActivated;
  [ChromeOnly]
  readonly attribute boolean hasValidTransientUserGestureActivation;
// Mattias:   [ChromeOnly]
//  readonly attribute DOMHighResTimeStamp lastUserGestureTimeStamp;
  [ChromeOnly]
  boolean consumeTransientUserGestureActivation();
};

// Extension to give chrome JS the ability to set an event handler which is
// called with certain events that happened while events were suppressed in the
// document or one of its subdocuments.
partial interface Document {
  [ChromeOnly]
  void setSuppressedEventListener(EventListener? aListener);
};

// Allows frontend code to query a CSP which needs to be passed for a
// new load into docshell. Further, allows to query the CSP in JSON
// format for testing purposes.
partial interface Document {
  [ChromeOnly] readonly attribute ContentSecurityPolicy? csp;
  [ChromeOnly] readonly attribute DOMString cspJSON;
};

// For more information on Flash classification, see
// toolkit/components/url-classifier/flash-block-lists.rst
enum FlashClassification {
  "unknown",        // Site is not on the whitelist or blacklist
  "allowed",        // Site is on the Flash whitelist
  "denied"          // Site is on the Flash blacklist
};
partial interface Document {
  [ChromeOnly]
  readonly attribute FlashClassification documentFlashClassification;
};

partial interface Document {
// Mattias:   [Func="Document::DocumentSupportsL10n"] readonly attribute DocumentL10n? l10n;
};

// Mattias: Document includes XPathEvaluatorMixin;
Document includes GlobalEventHandlers;
Document includes DocumentAndElementEventHandlers;
// Mattias: Document includes TouchEventHandlers;
Document includes ParentNode;
Document includes OnErrorEventHandlerForNodes;
//Document includes GeometryUtils;
Document includes  FontFaceSource;
Document includes DocumentOrShadowRoot;

// https://w3c.github.io/webappsec-feature-policy/#idl-index
partial interface Document {
// Mattias:     [SameObject, Pref="dom.security.featurePolicy.webidl.enabled"]
//    readonly attribute FeaturePolicy featurePolicy;
};

// Extension to give chrome JS the ability to specify a non-default keypress
// event model.
partial interface Document {
  /**
   * setKeyPressEventModel() is called when we need to check whether the web
   * app requires specific keypress event model or not.
   *
   * @param aKeyPressEventModel  Proper keypress event model for the web app.
   *   KEYPRESS_EVENT_MODEL_DEFAULT:
   *     Use default keypress event model.  I.e., depending on
   *     "dom.keyboardevent.keypress.set_keycode_and_charcode_to_same_value"
   *     pref.
   *   KEYPRESS_EVENT_MODEL_SPLIT:
   *     Use split model.  I.e, if keypress event inputs a character,
   *     keyCode should be 0.  Otherwise, charCode should be 0.
   *   KEYPRESS_EVENT_MODEL_CONFLATED:
   *     Use conflated model.  I.e., keyCode and charCode values of each
   *     keypress event should be set to same value.
   */
  [ChromeOnly]
  const unsigned short KEYPRESS_EVENT_MODEL_DEFAULT = 0;
  [ChromeOnly]
  const unsigned short KEYPRESS_EVENT_MODEL_SPLIT = 1;
  [ChromeOnly]
  const unsigned short KEYPRESS_EVENT_MODEL_CONFLATED = 2;
  [ChromeOnly]
  void setKeyPressEventModel(unsigned short aKeyPressEventModel);
};

// Extensions to return information about about the nodes blocked by the
// Safebrowsing API inside a document.
partial interface Document {
  /*
   * Number of nodes that have been blocked by the Safebrowsing API to prevent
   * tracking, cryptomining and so on. This method is for testing only.
   */
  [ChromeOnly, Pure]
  readonly attribute long blockedNodeByClassifierCount;

  /*
   * List of nodes that have been blocked by the Safebrowsing API to prevent
   * tracking, fingerprinting, cryptomining and so on. This method is for
   * testing only.
   */
  [ChromeOnly, Pure]
  readonly attribute NodeList blockedNodesByClassifier;
};

// Extension to programmatically simulate a user interaction on a document,
// used for testing.
partial interface Document {
  [ChromeOnly, BinaryName="setUserHasInteracted"]
  void userInteractionForTesting();
};

// Extension for permission delegation.
partial interface Document {
  [ChromeOnly, Pure]
  readonly attribute nsIPermissionDelegateHandler permDelegateHandler;
};

// Extension used by the password manager to infer form submissions.
partial interface Document {
  /*
   * Set whether the document notifies an event when a fetch or
   * XHR completes successfully.
   */
  [ChromeOnly]
  void setNotifyFetchSuccess(boolean aShouldNotify);

  /*
   * Set whether a form and a password field notify an event when it is
   * removed from the DOM tree.
   */
  [ChromeOnly]
  void setNotifyFormOrPasswordRemoved(boolean aShouldNotify);
};

// Extension to allow chrome code to detect initial about:blank documents.
partial interface Document {
  [ChromeOnly]
  readonly attribute boolean isInitialDocument;
};

// Extension to allow chrome code to get some wireframe-like structure.
enum WireframeRectType {
  "image",
  "background",
  "text",
  "unknown",
};
dictionary WireframeTaggedRect {
  unrestricted double x = 0;
  unrestricted double y = 0;
  unrestricted double width = 0;
  unrestricted double height = 0;
  unsigned long color = 0; // in nscolor format
  WireframeRectType type;
  Node? node;
};
[GenerateInit]
dictionary Wireframe {
  unsigned long canvasBackground = 0; // in nscolor format
  sequence<WireframeTaggedRect> rects;
  unsigned long version = 1; // Increment when the wireframe structure changes in backwards-incompatible ways
};
partial interface Document {
// Mattias:   [ChromeOnly]
//  Wireframe? getWireframe(optional boolean aIncludeNodes = false);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=Window]
interface XPathExpression {
  // The result specifies a specific result object which may be reused and
  // returned by this method. If this is specified as null or it's not an
  // XPathResult object, a new result object will be constructed and returned.
// Mattias:   [Throws]
//  XPathResult evaluate(Node contextNode,
//                       optional unsigned short type = 0  /* XPathResult.ANY_TYPE */,
//                       optional object? result = null);

  // The result specifies a specific result object which may be reused and
  // returned by this method. If this is specified as null or it's not an
  // XPathResult object, a new result object will be constructed and returned.
// Mattias:   [Throws, ChromeOnly]
//  XPathResult evaluateWithContext(Node contextNode,
//                                  unsigned long contextPosition,
//                                  unsigned long contextSize,
//                                  optional unsigned short type = 0  /* XPathResult.ANY_TYPE */,
//                                  optional object? result = null);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#interface-parentnode
 */

interface mixin ParentNode {
  [Constant]
  readonly attribute HTMLCollection children;
  [Pure]
  readonly attribute Element? firstElementChild;
  [Pure]
  readonly attribute Element? lastElementChild;
  [Pure]
  readonly attribute unsigned long childElementCount;

  [ChromeOnly]
  HTMLCollection getElementsByAttribute(DOMString name,
                                        [LegacyNullToEmptyString] DOMString value);
  [ChromeOnly, Throws]
  HTMLCollection getElementsByAttributeNS(DOMString? namespaceURI, DOMString name,
                                          [LegacyNullToEmptyString] DOMString value);

  [CEReactions, Throws, Unscopable]
  void prepend((Node or DOMString)... nodes);
  [CEReactions, Throws, Unscopable]
  void append((Node or DOMString)... nodes);
  [CEReactions, Throws, Unscopable]
  void replaceChildren((Node or DOMString)... nodes);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/csswg/css-font-loading/#font-face-source
 *
 * Copyright © 2014 W3C® (MIT, ERCIM, Keio, Beihang), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface mixin FontFaceSource {

// Mattias:   [Pref="layout.css.font-loading-api.enabled"]
//  readonly attribute FontFaceSet fonts;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dom.spec.whatwg.org/#documentorshadowroot
 * http://w3c.github.io/webcomponents/spec/shadow/#extensions-to-the-documentorshadowroot-mixin
 * https://wicg.github.io/construct-stylesheets/#using-constructed-stylesheets
 */

interface mixin DocumentOrShadowRoot {
  // Not implemented yet: bug 1430308.
  // Selection? getSelection();
  Element? elementFromPoint(float x, float y);
  sequence<Element> elementsFromPoint(float x, float y);

  // TODO: Avoid making these ChromeOnly, see:
  // https://github.com/w3c/csswg-drafts/issues/556
  [ChromeOnly]
  Node? nodeFromPoint(float x, float y);
  [ChromeOnly]
  sequence<Node> nodesFromPoint(float x, float y);

  // Not implemented yet: bug 1430307.
  // CaretPosition? caretPositionFromPoint (float x, float y);

  readonly attribute Element? activeElement;
// Mattias:   readonly attribute StyleSheetList styleSheets;

  readonly attribute Element? pointerLockElement;
  [LegacyLenientSetter]
  readonly attribute Element? fullscreenElement;
  [BinaryName="fullscreenElement"]
  readonly attribute Element? mozFullScreenElement;
};

// https://drafts.csswg.org/web-animations-1/#extensions-to-the-documentorshadowroot-interface-mixin
partial interface mixin DocumentOrShadowRoot {
// Mattias:   [Func="Document::IsWebAnimationsGetAnimationsEnabled"]
//  sequence<Animation> getAnimations();
};

// https://wicg.github.io/construct-stylesheets/#using-constructed-stylesheets
partial interface mixin DocumentOrShadowRoot {
  // We are using [Pure, Cached, Frozen] sequence until `FrozenArray` is implemented.
  // See https://bugzilla.mozilla.org/show_bug.cgi?id=1236777 for more details.
// Mattias:  [Pref="layout.css.constructable-stylesheets.enabled"]
//  attribute ObservableArray<CSSStyleSheet> adoptedStyleSheets;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#interface-domimplementation
 *
 * Copyright:
 * To the extent possible under law, the editors have waived all copyright and
 * related or neighboring rights to this work.
 */

[Exposed=Window]
interface DOMImplementation {
  boolean hasFeature();

  [Throws]
  DocumentType createDocumentType(DOMString qualifiedName, DOMString publicId,
                                  DOMString systemId);
  [Throws]
  Document createDocument(DOMString? namespace,
                          [LegacyNullToEmptyString] DOMString qualifiedName,
                          optional DocumentType? doctype = null);
  [Throws]
  Document createHTMLDocument(optional DOMString title);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#documenttype
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface DocumentType : Node {
  readonly attribute DOMString name;
  readonly attribute DOMString publicId;
  readonly attribute DOMString systemId;
};

DocumentType includes ChildNode;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#interface-childnode
 */

interface mixin ChildNode {
  [CEReactions, Throws, Unscopable]
  void before((Node or DOMString)... nodes);
  [CEReactions, Throws, Unscopable]
  void after((Node or DOMString)... nodes);
  [CEReactions, Throws, Unscopable]
  void replaceWith((Node or DOMString)... nodes);
  [CEReactions, Unscopable]
  void remove();
};

interface mixin NonDocumentTypeChildNode {
  [Pure]
  readonly attribute Element? previousElementSibling;
  [Pure]
  readonly attribute Element? nextElementSibling;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#element and
 * http://domparsing.spec.whatwg.org/ and
 * http://dev.w3.org/csswg/cssom-view/ and
 * http://www.w3.org/TR/selectors-api/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface nsIScreen;

[Exposed=Window]
interface Element : Node {
  [Constant]
  readonly attribute DOMString? namespaceURI;
  [Constant]
  readonly attribute DOMString? prefix;
  [Constant]
  readonly attribute DOMString localName;

  // Not [Constant] because it depends on which document we're in
  [Pure]
  readonly attribute DOMString tagName;

  [CEReactions, Pure]
           attribute DOMString id;
  [CEReactions, Pure]
           attribute DOMString className;
  [Constant, PutForwards=value]
  readonly attribute DOMTokenList classList;

  // https://drafts.csswg.org/css-shadow-parts/#idl
  [SameObject, PutForwards=value]
  readonly attribute DOMTokenList part;

  [SameObject]
  readonly attribute NamedNodeMap attributes;
  [Pure]
  sequence<DOMString> getAttributeNames();
  [Pure]
  DOMString? getAttribute(DOMString name);
  [Pure]
  DOMString? getAttributeNS(DOMString? namespace, DOMString localName);
  [CEReactions, NeedsSubjectPrincipal=NonSystem, Throws]
  boolean toggleAttribute(DOMString name, optional boolean force);
  [CEReactions, NeedsSubjectPrincipal=NonSystem, Throws]
  void setAttribute(DOMString name, DOMString value);
  [CEReactions, NeedsSubjectPrincipal=NonSystem, Throws]
  void setAttributeNS(DOMString? namespace, DOMString name, DOMString value);
  [CEReactions, Throws]
  void removeAttribute(DOMString name);
  [CEReactions, Throws]
  void removeAttributeNS(DOMString? namespace, DOMString localName);
  [Pure]
  boolean hasAttribute(DOMString name);
  [Pure]
  boolean hasAttributeNS(DOMString? namespace, DOMString localName);
  [Pure]
  boolean hasAttributes();

  [Throws, Pure]
  Element? closest(UTF8String selector);

  [Throws, Pure]
  boolean matches(UTF8String selector);
  [Throws, Pure, BinaryName="matches"]
  boolean webkitMatchesSelector(UTF8String selector);

  [Pure]
  HTMLCollection getElementsByTagName(DOMString localName);
  [Throws, Pure]
  HTMLCollection getElementsByTagNameNS(DOMString? namespace, DOMString localName);
  [Pure]
  HTMLCollection getElementsByClassName(DOMString classNames);

  [CEReactions, Throws]
  Element? insertAdjacentElement(DOMString where, Element element); // historical

  [Throws]
  void insertAdjacentText(DOMString where, DOMString data); // historical

  /**
   * The ratio of font-size-inflated text font size to computed font
   * size for this element. This will query the element for its primary frame,
   * and then use this to get font size inflation information about the frame.
   * This will be 1.0 if font size inflation is not enabled, and -1.0 if an
   * error occurred during the retrieval of the font size inflation.
   *
   * @note The font size inflation ratio that is returned is actually the
   *       font size inflation data for the element's _primary frame_, not the
   *       element itself, but for most purposes, this should be sufficient.
   */
  [ChromeOnly]
  readonly attribute float fontSizeInflation;

  /**
   * Returns the pseudo-element string if this element represents a
   * pseudo-element, or null otherwise.
   */
  [ChromeOnly]
  readonly attribute DOMString? implementedPseudoElement;

  // Selectors API
  /**
   * Returns whether this element would be selected by the given selector
   * string.
   *
   * See <http://dev.w3.org/2006/webapi/selectors-api2/#matchesselector>
   */
  [Throws, Pure, BinaryName="matches"]
  boolean mozMatchesSelector(UTF8String selector);

  // Pointer events methods.
  [UseCounter, Throws]
  void setPointerCapture(long pointerId);
  [UseCounter, Throws]
  void releasePointerCapture(long pointerId);
  boolean hasPointerCapture(long pointerId);

  // Proprietary extensions
  /**
   * Set this during a mousedown event to grab and retarget all mouse events
   * to this element until the mouse button is released or releaseCapture is
   * called. If retargetToElement is true, then all events are targetted at
   * this element. If false, events can also fire at descendants of this
   * element.
   *
   */
  [Deprecated=ElementSetCapture, Pref="dom.mouse_capture.enabled"]
  void setCapture(optional boolean retargetToElement = false);

  /**
   * If this element has captured the mouse, release the capture. If another
   * element has captured the mouse, this method has no effect.
   */
  [Deprecated=ElementReleaseCapture, Pref="dom.mouse_capture.enabled"]
  void releaseCapture();

  /*
   * Chrome-only version of setCapture that works outside of a mousedown event.
   */
  [ChromeOnly]
  void setCaptureAlways(optional boolean retargetToElement = false);

  // Mozilla extensions

  // Obsolete methods.
  Attr? getAttributeNode(DOMString name);
  [CEReactions, Throws]
  Attr? setAttributeNode(Attr newAttr);
  [CEReactions, Throws]
  Attr? removeAttributeNode(Attr oldAttr);
  Attr? getAttributeNodeNS(DOMString? namespaceURI, DOMString localName);
  [CEReactions, Throws]
  Attr? setAttributeNodeNS(Attr newAttr);

  [Func="nsContentUtils::IsCallerChromeOrElementTransformGettersEnabled"]
  DOMMatrixReadOnly getTransformToAncestor(Element ancestor);
  [Func="nsContentUtils::IsCallerChromeOrElementTransformGettersEnabled"]
  DOMMatrixReadOnly getTransformToParent();
  [Func="nsContentUtils::IsCallerChromeOrElementTransformGettersEnabled"]
  DOMMatrixReadOnly getTransformToViewport();
};

// https://html.spec.whatwg.org/#focus-management-apis
dictionary FocusOptions {
  boolean preventScroll = false;
  // Prevents the focus ring if this is not a text control / editable element.
  [Func="nsContentUtils::IsCallerChromeOrErrorPage"]
  boolean preventFocusRing = false;
};

interface mixin HTMLOrForeignElement {
  [SameObject] readonly attribute DOMStringMap dataset;
  // See bug 1389421
  // attribute DOMString nonce; // intentionally no [CEReactions]

  // See bug 1575154
  // [CEReactions] attribute boolean autofocus;
  [CEReactions, SetterThrows, Pure] attribute long tabIndex;
  [Throws, NeedsCallerType] void focus(optional FocusOptions options = {});
  [Throws] void blur();
};

// https://drafts.csswg.org/cssom/#the-elementcssinlinestyle-mixin
interface mixin ElementCSSInlineStyle {
  [SameObject, PutForwards=cssText]
  readonly attribute CSSStyleDeclaration style;
};

// http://dev.w3.org/csswg/cssom-view/
enum ScrollLogicalPosition { "start", "center", "end", "nearest" };
dictionary ScrollIntoViewOptions : ScrollOptions {
  ScrollLogicalPosition block = "start";
  ScrollLogicalPosition inline = "nearest";
};

// http://dev.w3.org/csswg/cssom-view/#extensions-to-the-element-interface
partial interface Element {
  DOMRectList getClientRects();
  DOMRect getBoundingClientRect();

  // scrolling
  void scrollIntoView(optional (boolean or ScrollIntoViewOptions) arg = {});
  // None of the CSSOM attributes are [Pure], because they flush
           attribute long scrollTop;   // scroll on setting
           attribute long scrollLeft;  // scroll on setting
  readonly attribute long scrollWidth;
  readonly attribute long scrollHeight;

  void scroll(unrestricted double x, unrestricted double y);
  void scroll(optional ScrollToOptions options = {});
  void scrollTo(unrestricted double x, unrestricted double y);
  void scrollTo(optional ScrollToOptions options = {});
  void scrollBy(unrestricted double x, unrestricted double y);
  void scrollBy(optional ScrollToOptions options = {});
  // mozScrollSnap is used by chrome to perform scroll snapping after the
  // user performs actions that may affect scroll position
  // mozScrollSnap is deprecated, to be replaced by a web accessible API, such
  // as an extension to the ScrollOptions dictionary.  See bug 1137937.
  [ChromeOnly] void mozScrollSnap();

  readonly attribute long clientTop;
  readonly attribute long clientLeft;
  readonly attribute long clientWidth;
  readonly attribute long clientHeight;

  // Return the screen coordinates of the element, in CSS pixels relative to
  // the window's screen.
  [ChromeOnly] readonly attribute long screenX;
  [ChromeOnly] readonly attribute long screenY;
  [ChromeOnly] readonly attribute nsIScreen? screen;

  // Mozilla specific stuff
  /* The minimum/maximum offset that the element can be scrolled to
     (i.e., the value that scrollLeft/scrollTop would be clamped to if they were
     set to arbitrarily large values. */
  [ChromeOnly] readonly attribute long scrollTopMin;
               readonly attribute long scrollTopMax;
  [ChromeOnly] readonly attribute long scrollLeftMin;
               readonly attribute long scrollLeftMax;
};

// http://domparsing.spec.whatwg.org/#extensions-to-the-element-interface
partial interface Element {
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, Pure, SetterThrows, GetterCanOOM]
  attribute [LegacyNullToEmptyString] DOMString innerHTML;
  [CEReactions, Pure, SetterThrows]
  attribute [LegacyNullToEmptyString] DOMString outerHTML;
  [CEReactions, Throws]
  void insertAdjacentHTML(DOMString position, DOMString text);
};

// http://www.w3.org/TR/selectors-api/#interface-definitions
partial interface Element {
  [Throws, Pure]
  Element?  querySelector(UTF8String selectors);
  [Throws, Pure]
  NodeList  querySelectorAll(UTF8String selectors);
};

// https://dom.spec.whatwg.org/#dictdef-shadowrootinit
dictionary ShadowRootInit {
// Mattias:   required ShadowRootMode mode;
  [Pref="dom.shadowdom.delegatesFocus.enabled"]
  boolean delegatesFocus = false;
// Mattias:   [Pref="dom.shadowdom.slot.assign.enabled"]
//  SlotAssignmentMode slotAssignment = "named";
};

// https://dom.spec.whatwg.org/#element
partial interface Element {
  // Shadow DOM v1
  [Throws, UseCounter]
  ShadowRoot attachShadow(ShadowRootInit shadowRootInitDict);
  [BinaryName="shadowRootByMode"]
  readonly attribute ShadowRoot? shadowRoot;

  [Func="Document::IsCallerChromeOrAddon", BinaryName="shadowRoot"]
  readonly attribute ShadowRoot? openOrClosedShadowRoot;

// Mattias:   [BinaryName="assignedSlotByMode"]
//  readonly attribute HTMLSlotElement? assignedSlot;

// Mattias:   [ChromeOnly, BinaryName="assignedSlot"]
//  readonly attribute HTMLSlotElement? openOrClosedAssignedSlot;

  [CEReactions, Unscopable, SetterThrows]
           attribute DOMString slot;
};

Element includes ChildNode;
Element includes NonDocumentTypeChildNode;
Element includes ParentNode;
// Mattias: Element includes Animatable;
// Mattias: Element includes GeometryUtils;
// Mattias: Element includes AccessibilityRole;
// Mattias: Element includes AriaAttributes;

// https://fullscreen.spec.whatwg.org/#api
partial interface Element {
  [NewObject, NeedsCallerType]
  Promise<void> requestFullscreen();
  [NewObject, BinaryName="requestFullscreen", NeedsCallerType, Deprecated="MozRequestFullScreenDeprecatedPrefix"]
  Promise<void> mozRequestFullScreen();

  // Events handlers
  attribute EventHandler onfullscreenchange;
  attribute EventHandler onfullscreenerror;
};

// https://w3c.github.io/pointerlock/#extensions-to-the-element-interface
partial interface Element {
  [NeedsCallerType, Pref="dom.pointer-lock.enabled"]
  void requestPointerLock();
};

// Mozilla-specific additions to support devtools
partial interface Element {
  // Support reporting of Flexbox properties
  /**
   * If this element has a display:flex or display:inline-flex style,
   * this property returns an object with computed values for flex
   * properties, as well as a property that exposes the flex lines
   * in this container.
   */
// Mattias:   [ChromeOnly, Pure]
//  Flex? getAsFlexContainer();

  // Support reporting of Grid properties
  /**
   * If this element has a display:grid or display:inline-grid style,
   * this property returns an object with computed values for grid
   * tracks and lines.
   */
// Mattias:   [ChromeOnly, Pure]
//  sequence<Grid> getGridFragments();

  /**
   * Returns whether there are any grid fragments on this element.
   */
  [ChromeOnly, Pure]
  boolean hasGridFragments();

  /**
   * Returns a sequence of all the descendent elements of this element
   * that have display:grid or display:inline-grid style and generate
   * a frame.
   */
  [ChromeOnly, Pure]
  sequence<Element> getElementsWithGrid();

  /**
   * Set attribute on the Element with a customized Content-Security-Policy
   * appropriate to devtools, which includes:
   * style-src 'unsafe-inline'
   */
  [ChromeOnly, CEReactions, Throws]
  void setAttributeDevtools(DOMString name, DOMString value);
  [ChromeOnly, CEReactions, Throws]
  void setAttributeDevtoolsNS(DOMString? namespace, DOMString name, DOMString value);

  /**
   * Provide a direct way to determine if this Element has visible
   * scrollbars. Flushes layout.
   */
  [ChromeOnly]
  readonly attribute boolean hasVisibleScrollbars;
};

// These variables are used in vtt.js, they are used for positioning vtt cues.
partial interface Element {
  // These two attributes are a double version of the clientHeight and the
  // clientWidth.
  [ChromeOnly]
  readonly attribute double clientHeightDouble;
  [ChromeOnly]
  readonly attribute double clientWidthDouble;
  // This attribute returns the block size of the first line box under the different
  // writing directions. If the direction is horizontal, it represents box's
  // height. If the direction is vertical, it represents box's width.
  [ChromeOnly]
  readonly attribute double firstLineBoxBSize;
};


// Sanitizer API, https://wicg.github.io/sanitizer-api/
dictionary SetHTMLOptions {
// Mattias:   Sanitizer sanitizer;
};

partial interface Element {
  [UseCounter, Throws, Pref="dom.security.sanitizer.enabled"]
    void setHTML(DOMString aInnerHTML, optional SetHTMLOptions options = {});
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[LegacyUnenumerableNamedProperties,
 Exposed=Window]
interface HTMLCollection {
  readonly attribute unsigned long length;
  getter Element? item(unsigned long index);
  getter Element? namedItem(DOMString name);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120405/#interface-documentfragment
 * http://www.w3.org/TR/2012/WD-selectors-api-20120628/#interface-definitions
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface DocumentFragment : Node {
  [Throws]
  constructor();

  Element? getElementById(DOMString elementId);
};

// http://www.w3.org/TR/2012/WD-selectors-api-20120628/#interface-definitions
partial interface DocumentFragment {
  [Throws]
  Element?  querySelector(UTF8String selectors);
  [Throws]
  NodeList  querySelectorAll(UTF8String selectors);
};

DocumentFragment includes ParentNode;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window, ProbablyShortLivingWrapper]
interface Text : CharacterData {
  [Throws]
  constructor(optional DOMString data = "");

  [Throws]
  Text splitText(unsigned long offset);
  [Throws]
  readonly attribute DOMString wholeText;
};

partial interface Text {
// Mattias:   [BinaryName="assignedSlotByMode"]
//  readonly attribute HTMLSlotElement? assignedSlot;

// Mattias:   [ChromeOnly, BinaryName="assignedSlot"]
//  readonly attribute HTMLSlotElement? openOrClosedAssignedSlot;
};

// Mattias: Text includes GeometryUtils;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#comment
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface Comment : CharacterData {
  [Throws]
  constructor(optional DOMString data = "");
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=Window]
interface CDATASection : Text {
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface Attr : Node {
  readonly attribute DOMString localName;
           [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString value;

  [Constant]
  readonly attribute DOMString name;
  [Constant]
  readonly attribute DOMString? namespaceURI;
  [Constant]
  readonly attribute DOMString? prefix;

  readonly attribute boolean specified;
};

// Mozilla extensions

partial interface Attr {
           [GetterThrows]
  readonly attribute Element? ownerElement;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/history.html#the-location-interface
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[LegacyUnforgeable,
 Exposed=Window]
interface Location {
  [Throws, CrossOriginWritable, NeedsSubjectPrincipal]
  stringifier attribute USVString href;
  [Throws, NeedsSubjectPrincipal]
  readonly attribute USVString origin;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString protocol;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString host;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString hostname;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString port;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString pathname;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString search;
  [Throws, NeedsSubjectPrincipal]
           attribute USVString hash;

  [Throws, NeedsSubjectPrincipal]
  void assign(USVString url);

  [Throws, CrossOriginCallable, NeedsSubjectPrincipal]
  void replace(USVString url);

  // XXXbz there is no forceget argument in the spec!  See bug 1037721.
  [Throws, NeedsSubjectPrincipal]
  void reload(optional boolean forceget = false);

  // Bug 1085214 [SameObject] readonly attribute USVString[] ancestorOrigins;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[ProbablyShortLivingWrapper,
 Exposed=Window]
interface NodeList {
  getter Node? item(unsigned long index);
  readonly attribute unsigned long length;
  iterable<Node?>;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information please see
 * https://w3c.github.io/webappsec-referrer-policy#idl-index
 */

enum ReferrerPolicy {
  "",
  "no-referrer",
  "no-referrer-when-downgrade",
  "origin",
  "origin-when-cross-origin",
  "unsafe-url", "same-origin",
  "strict-origin",
  "strict-origin-when-cross-origin"
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker)]
interface DOMStringList {
  readonly attribute unsigned long length;
  getter DOMString? item(unsigned long index);
  boolean contains(DOMString string);
};
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

[Exposed=Window]
interface CaretPosition {

  /**
   * The offsetNode could potentially be null due to anonymous content.
   */
  readonly attribute Node? offsetNode;
  readonly attribute unsigned long offset;

};

/**
 * Gecko specific methods and properties for CaretPosition.
 */
partial interface CaretPosition {
   DOMRect? getClientRect();
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dom.spec.whatwg.org/#interface-domtokenlist
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface DOMTokenList {
  [Pure]
  readonly attribute unsigned long length;
  [Pure]
  getter DOMString? item(unsigned long index);
  [Pure]
  boolean contains(DOMString token);
  [CEReactions, Throws]
  void add(DOMString... tokens);
  [CEReactions, Throws]
  void remove(DOMString... tokens);
  [CEReactions, Throws]
  boolean replace(DOMString token, DOMString newToken);
  [CEReactions, Throws]
  boolean toggle(DOMString token, optional boolean force);
  [Throws]
  boolean supports(DOMString token);
  [CEReactions, SetterThrows, Pure]
  stringifier attribute DOMString value;
  iterable<DOMString?>;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

[LegacyUnenumerableNamedProperties,
 Exposed=Window]
interface NamedNodeMap {
  getter Attr? getNamedItem(DOMString name);
  [CEReactions, Throws, BinaryName="setNamedItemNS"]
  Attr? setNamedItem(Attr arg);
  [CEReactions, Throws]
  Attr removeNamedItem(DOMString name);

  getter Attr? item(unsigned long index);
  readonly attribute unsigned long length;

  Attr? getNamedItemNS(DOMString? namespaceURI, DOMString localName);
  [CEReactions, Throws]
  Attr? setNamedItemNS(Attr arg);
  [CEReactions, Throws]
  Attr removeNamedItemNS(DOMString? namespaceURI, DOMString localName);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/multipage/common-dom-interfaces.html#domstringmap-0
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[LegacyOverrideBuiltIns,
 Exposed=Window]
interface DOMStringMap {
  getter DOMString (DOMString name);
  [CEReactions, Throws]
  setter void (DOMString name, DOMString value);
  [CEReactions]
  deleter void (DOMString name);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/csswg/cssom/
 */

 // Because of getComputedStyle, many CSSStyleDeclaration objects can be
 // short-living.
[ProbablyShortLivingWrapper,
 Exposed=Window]
interface CSSStyleDeclaration {
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
  attribute UTF8String cssText;

  readonly attribute unsigned long length;
  getter UTF8String item(unsigned long index);

  [Throws, ChromeOnly]
  sequence<UTF8String> getCSSImageURLs(UTF8String property);

  [Throws]
  UTF8String getPropertyValue(UTF8String property);
  UTF8String getPropertyPriority(UTF8String property);
  [CEReactions, NeedsSubjectPrincipal=NonSystem, Throws]
  void setProperty(UTF8String property, [LegacyNullToEmptyString] UTF8String value, optional [LegacyNullToEmptyString] UTF8String priority = "");
  [CEReactions, Throws]
  UTF8String removeProperty(UTF8String property);

  readonly attribute CSSRule? parentRule;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#characterdata
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface CharacterData : Node {
  [Pure, SetterThrows]
  attribute [LegacyNullToEmptyString] DOMString data;
  [Pure]
  readonly attribute unsigned long length;
  [Throws]
  DOMString substringData(unsigned long offset, unsigned long count);
  [Throws]
  void appendData(DOMString data);
  [Throws]
  void insertData(unsigned long offset, DOMString data);
  [Throws]
  void deleteData(unsigned long offset, unsigned long count);
  [Throws]
  void replaceData(unsigned long offset, unsigned long count, DOMString data);
};

CharacterData includes ChildNode;
CharacterData includes NonDocumentTypeChildNode;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is:
 * http://www.whatwg.org/specs/web-apps/current-work/
 * https://dvcs.w3.org/hg/editing/raw-file/tip/editing.html
 * https://dvcs.w3.org/hg/IndexedDB/raw-file/tip/Overview.html
 * http://dev.w3.org/csswg/cssom/
 * http://dev.w3.org/csswg/cssom-view/
 * https://dvcs.w3.org/hg/webperf/raw-file/tip/specs/RequestAnimationFrame/Overview.html
 * https://dvcs.w3.org/hg/webperf/raw-file/tip/specs/NavigationTiming/Overview.html
 * https://dvcs.w3.org/hg/webcrypto-api/raw-file/tip/spec/Overview.html
 * http://dvcs.w3.org/hg/speech-api/raw-file/tip/speechapi.html
 * https://w3c.github.io/webappsec-secure-contexts/#monkey-patching-global-object
 * https://w3c.github.io/requestidlecallback/
 * https://drafts.css-houdini.org/css-paint-api-1/#dom-window-paintworklet
 * https://wicg.github.io/visual-viewport/#the-visualviewport-interface
 */

interface Principal;
interface nsIBrowserDOMWindow;
interface XULControllers;
interface nsIDOMWindowUtils;
interface nsIPrintSettings;

typedef OfflineResourceList ApplicationCache;

// http://www.whatwg.org/specs/web-apps/current-work/
[Global, LegacyUnenumerableNamedProperties, NeedResolve,
 Exposed=Window,
 InstrumentedProps=(AbsoluteOrientationSensor,
                    Accelerometer,
                    ApplicationCache,
                    ApplicationCacheErrorEvent,
                    Atomics,
                    AudioParamMap,
                    AudioWorklet,
                    AudioWorkletNode,
                    BackgroundFetchManager,
                    BackgroundFetchRecord,
                    BackgroundFetchRegistration,
                    BeforeInstallPromptEvent,
                    Bluetooth,
                    BluetoothCharacteristicProperties,
                    BluetoothDevice,
                    BluetoothRemoteGATTCharacteristic,
                    BluetoothRemoteGATTDescriptor,
                    BluetoothRemoteGATTServer,
                    BluetoothRemoteGATTService,
                    BluetoothUUID,
                    CanvasCaptureMediaStreamTrack,
                    chrome,
                    clientInformation,
                    ClipboardItem,
                    CSSImageValue,
                    CSSKeywordValue,
                    CSSMathInvert,
                    CSSMathMax,
                    CSSMathMin,
                    CSSMathNegate,
                    CSSMathProduct,
                    CSSMathSum,
                    CSSMathValue,
                    CSSMatrixComponent,
                    CSSNumericArray,
                    CSSNumericValue,
                    CSSPerspective,
                    CSSPositionValue,
                    CSSRotate,
                    CSSScale,
                    CSSSkew,
                    CSSSkewX,
                    CSSSkewY,
                    CSSStyleValue,
                    CSSTransformComponent,
                    CSSTransformValue,
                    CSSTranslate,
                    CSSUnitValue,
                    CSSUnparsedValue,
                    CSSVariableReferenceValue,
                    defaultStatus,
                    // Unfortunately, our telemetry histogram name generator
                    // (the one that generates TelemetryHistogramEnums.h) can't
                    // handle two DOM methods with names that only differ in
                    // case, because it forces everything to uppercase.
                    //defaultstatus,
                    DeviceMotionEventAcceleration,
                    DeviceMotionEventRotationRate,
                    DOMError,
                    EnterPictureInPictureEvent,
                    External,
                    FederatedCredential,
                    Gyroscope,
                    HTMLContentElement,
                    HTMLDialogElement,
                    HTMLShadowElement,
                    ImageCapture,
                    InputDeviceCapabilities,
                    InputDeviceInfo,
                    Keyboard,
                    KeyboardLayoutMap,
                    LinearAccelerationSensor,
                    Lock,
                    LockManager,
                    MediaMetadata,
                    MediaSession,
                    MediaSettingsRange,
                    MIDIAccess,
                    MIDIConnectionEvent,
                    MIDIInput,
                    MIDIInputMap,
                    MIDIMessageEvent,
                    MIDIOutput,
                    MIDIOutputMap,
                    MIDIPort,
                    NavigationPreloadManager,
                    NetworkInformation,
                    offscreenBuffering,
                    OffscreenCanvas,
                    OffscreenCanvasRenderingContext2D,
                    onbeforeinstallprompt,
                    oncancel,
                    ondeviceorientationabsolute,
                    onmousewheel,
                    onsearch,
                    onselectionchange,
                    openDatabase,
                    OrientationSensor,
                    OverconstrainedError,
                    PasswordCredential,
                    PaymentAddress,
                    PaymentInstruments,
                    PaymentManager,
                    PaymentMethodChangeEvent,
                    PaymentRequest,
                    PaymentRequestUpdateEvent,
                    PaymentResponse,
                    PerformanceEventTiming,
                    PerformanceLongTaskTiming,
                    PerformancePaintTiming,
                    PhotoCapabilities,
                    PictureInPictureWindow,
                    Presentation,
                    PresentationAvailability,
                    PresentationConnection,
                    PresentationConnectionAvailableEvent,
                    PresentationConnectionCloseEvent,
                    PresentationConnectionList,
                    PresentationReceiver,
                    PresentationRequest,
                    RelativeOrientationSensor,
                    RemotePlayback,
                    ReportingObserver,
                    RTCDtlsTransport,
                    RTCError,
                    RTCErrorEvent,
                    RTCIceTransport,
                    RTCSctpTransport,
                    Sensor,
                    SensorErrorEvent,
                    SharedArrayBuffer,
                    styleMedia,
                    StylePropertyMap,
                    StylePropertyMapReadOnly,
                    SVGDiscardElement,
                    SyncManager,
                    TaskAttributionTiming,
                    TextDecoderStream,
                    TextEncoderStream,
                    TextEvent,
                    Touch,
                    TouchEvent,
                    TouchList,
                    TransformStream,
                    USB,
                    USBAlternateInterface,
                    USBConfiguration,
                    USBConnectionEvent,
                    USBDevice,
                    USBEndpoint,
                    USBInterface,
                    USBInTransferResult,
                    USBIsochronousInTransferPacket,
                    USBIsochronousInTransferResult,
                    USBIsochronousOutTransferPacket,
                    USBIsochronousOutTransferResult,
                    USBOutTransferResult,
                    UserActivation,
                    visualViewport,
                    webkitCancelAnimationFrame,
                    webkitMediaStream,
                    WebKitMutationObserver,
                    webkitRequestAnimationFrame,
                    webkitRequestFileSystem,
                    webkitResolveLocalFileSystemURL,
                    webkitRTCPeerConnection,
                    webkitSpeechGrammar,
                    webkitSpeechGrammarList,
                    webkitSpeechRecognition,
                    webkitSpeechRecognitionError,
                    webkitSpeechRecognitionEvent,
                    webkitStorageInfo,
                    Worklet,
                    WritableStream)]
/*sealed*/ interface Window : EventTarget {
  // the current browsing context
  [LegacyUnforgeable, Constant, StoreInSlot,
   CrossOriginReadable] readonly attribute WindowProxy window;
  [Replaceable, Constant, StoreInSlot,
   CrossOriginReadable] readonly attribute WindowProxy self;
  [LegacyUnforgeable, StoreInSlot, Pure] readonly attribute Document? document;
  [Throws] attribute DOMString name;
  [PutForwards=href, LegacyUnforgeable, CrossOriginReadable,
   CrossOriginWritable] readonly attribute Location location;
  [Throws] readonly attribute History history;
  readonly attribute CustomElementRegistry customElements;
  [Replaceable, Throws] readonly attribute BarProp locationbar;
  [Replaceable, Throws] readonly attribute BarProp menubar;
  [Replaceable, Throws] readonly attribute BarProp personalbar;
  [Replaceable, Throws] readonly attribute BarProp scrollbars;
  [Replaceable, Throws] readonly attribute BarProp statusbar;
  [Replaceable, Throws] readonly attribute BarProp toolbar;
  [Throws] attribute DOMString status;
  [Throws, CrossOriginCallable, NeedsCallerType] void close();
  [Throws, CrossOriginReadable] readonly attribute boolean closed;
  [Throws] void stop();
  [Throws, CrossOriginCallable, NeedsCallerType] void focus();
  [Throws, CrossOriginCallable, NeedsCallerType] void blur();
  [Replaceable, Pref="dom.window.event.enabled"] readonly attribute any event;

  // other browsing contexts
  [Replaceable, Throws, CrossOriginReadable] readonly attribute WindowProxy frames;
  [Replaceable, CrossOriginReadable] readonly attribute unsigned long length;
  //[Unforgeable, Throws, CrossOriginReadable] readonly attribute WindowProxy top;
  [LegacyUnforgeable, Throws, CrossOriginReadable] readonly attribute WindowProxy? top;
  [Throws, CrossOriginReadable] attribute any opener;
  //[Throws] readonly attribute WindowProxy parent;
  [Replaceable, Throws, CrossOriginReadable] readonly attribute WindowProxy? parent;
  [Throws, NeedsSubjectPrincipal] readonly attribute Element? frameElement;
  //[Throws] WindowProxy? open(optional USVString url = "about:blank", optional DOMString target = "_blank", [TreatNullAs=EmptyString] optional DOMString features = "");
  [Throws] WindowProxy? open(optional USVString url = "", optional DOMString target = "", optional [LegacyNullToEmptyString] DOMString features = "");
  getter object (DOMString name);

  // the user agent
  readonly attribute Navigator navigator;
  [Pref="dom.window.clientinformation.enabled", BinaryName="Navigator"]
  readonly attribute Navigator clientInformation;

// Mattias:   [Replaceable, Throws] readonly attribute External external;
  [Throws, SecureContext, Pref="browser.cache.offline.enable"] readonly attribute ApplicationCache applicationCache;

  // user prompts
  [Throws, NeedsSubjectPrincipal] void alert();
  [Throws, NeedsSubjectPrincipal] void alert(DOMString message);
  [Throws, NeedsSubjectPrincipal] boolean confirm(optional DOMString message = "");
  [Throws, NeedsSubjectPrincipal] DOMString? prompt(optional DOMString message = "", optional DOMString default = "");
  [Throws, Pref="dom.enable_window_print"]
  void print();

  // Returns a window that you can use for a print preview.
  //
  // This may reuse an existing window if this window is already a print
  // preview document, or if you pass a docshell explicitly.
// Mattias:   [Throws, Func="nsContentUtils::IsCallerChromeOrFuzzingEnabled"]
//  WindowProxy? printPreview(optional nsIPrintSettings? settings = null,
//                            optional nsIWebProgressListener? listener = null,
//                            optional nsIDocShell? docShellToPreviewInto = null);

// Mattias:   [Throws, CrossOriginCallable, NeedsSubjectPrincipal,
//   BinaryName="postMessageMoz"]
//  void postMessage(any message, DOMString targetOrigin, optional sequence<object> transfer = []);
  [Throws, CrossOriginCallable, NeedsSubjectPrincipal,
   BinaryName="postMessageMoz"]
  void postMessage(any message, optional WindowPostMessageOptions options = {});

  // also has obsolete members
};
Window includes GlobalEventHandlers;
Window includes WindowEventHandlers;

// http://www.whatwg.org/specs/web-apps/current-work/
interface mixin WindowSessionStorage {
  //[Throws] readonly attribute Storage sessionStorage;
  [Throws] readonly attribute Storage? sessionStorage;
};
Window includes WindowSessionStorage;

// http://www.whatwg.org/specs/web-apps/current-work/
interface mixin WindowLocalStorage {
  [Throws] readonly attribute Storage? localStorage;
};
Window includes WindowLocalStorage;

// http://www.whatwg.org/specs/web-apps/current-work/
partial interface Window {
  void captureEvents();
  void releaseEvents();
};

// https://dvcs.w3.org/hg/editing/raw-file/tip/editing.html
partial interface Window {
  //[Throws] Selection getSelection();
  [Throws] Selection? getSelection();
};

// https://drafts.csswg.org/cssom/#extensions-to-the-window-interface
partial interface Window {
  //[NewObject, Throws] CSSStyleDeclaration getComputedStyle(Element elt, optional DOMString? pseudoElt = "");
  [NewObject, Throws] CSSStyleDeclaration? getComputedStyle(Element elt, optional DOMString? pseudoElt = "");
};

// http://dev.w3.org/csswg/cssom-view/
enum ScrollBehavior { "auto", "instant", "smooth" };

dictionary ScrollOptions {
  ScrollBehavior behavior = "auto";
};

dictionary ScrollToOptions : ScrollOptions {
  unrestricted double left;
  unrestricted double top;
};

partial interface Window {
  //[Throws, NewObject, NeedsCallerType] MediaQueryList matchMedia(DOMString query);
// Mattias:   [Throws, NewObject, NeedsCallerType] MediaQueryList? matchMedia(UTF8String query);
  // Per spec, screen is SameObject, but we don't actually guarantee that given
  // nsGlobalWindow::Cleanup.  :(
  //[SameObject, Replaceable, Throws] readonly attribute Screen screen;
  [Replaceable, Throws] readonly attribute Screen screen;

  // browsing context
  //[Throws] void moveTo(double x, double y);
  //[Throws] void moveBy(double x, double y);
  //[Throws] void resizeTo(double x, double y);
  //[Throws] void resizeBy(double x, double y);
  [Throws, NeedsCallerType] void moveTo(long x, long y);
  [Throws, NeedsCallerType] void moveBy(long x, long y);
  [Throws, NeedsCallerType] void resizeTo(long x, long y);
  [Throws, NeedsCallerType] void resizeBy(long x, long y);

  // viewport
  // These are writable because we allow chrome to write them.  And they need
  // to use 'any' as the type, because non-chrome writing them needs to act
  // like a [Replaceable] attribute would, which needs the original JS value.
  //[Replaceable, Throws] readonly attribute double innerWidth;
  //[Replaceable, Throws] readonly attribute double innerHeight;
  [Throws, NeedsCallerType] attribute any innerWidth;
  [Throws, NeedsCallerType] attribute any innerHeight;

  // viewport scrolling
  void scroll(unrestricted double x, unrestricted double y);
  void scroll(optional ScrollToOptions options = {});
  void scrollTo(unrestricted double x, unrestricted double y);
  void scrollTo(optional ScrollToOptions options = {});
  void scrollBy(unrestricted double x, unrestricted double y);
  void scrollBy(optional ScrollToOptions options = {});
  // mozScrollSnap is used by chrome to perform scroll snapping after the
  // user performs actions that may affect scroll position
  // mozScrollSnap is deprecated, to be replaced by a web accessible API, such
  // as an extension to the ScrollOptions dictionary.  See bug 1137937.
  [ChromeOnly] void mozScrollSnap();
  // The four properties below are double per spec at the moment, but whether
  // that will continue is unclear.
  [Replaceable, Throws] readonly attribute double scrollX;
  [Replaceable, Throws] readonly attribute double pageXOffset;
  [Replaceable, Throws] readonly attribute double scrollY;
  [Replaceable, Throws] readonly attribute double pageYOffset;

  // Aliases for screenX / screenY.
  [Replaceable, Throws, NeedsCallerType] readonly attribute double screenLeft;
  [Replaceable, Throws, NeedsCallerType] readonly attribute double screenTop;

  // client
  // These are writable because we allow chrome to write them.  And they need
  // to use 'any' as the type, because non-chrome writing them needs to act
  // like a [Replaceable] attribute would, which needs the original JS value.
  //[Replaceable, Throws] readonly attribute double screenX;
  //[Replaceable, Throws] readonly attribute double screenY;
  //[Replaceable, Throws] readonly attribute double outerWidth;
  //[Replaceable, Throws] readonly attribute double outerHeight;
  [Throws, NeedsCallerType] attribute any screenX;
  [Throws, NeedsCallerType] attribute any screenY;
  [Throws, NeedsCallerType] attribute any outerWidth;
  [Throws, NeedsCallerType] attribute any outerHeight;
};

// https://html.spec.whatwg.org/multipage/imagebitmap-and-animations.html#animation-frames
// Mattias: Window includes AnimationFrameProvider;

// https://dvcs.w3.org/hg/webperf/raw-file/tip/specs/NavigationTiming/Overview.html
partial interface Window {
// Mattias:   [Replaceable, Pure, StoreInSlot] readonly attribute Performance? performance;
};

// https://dvcs.w3.org/hg/webcrypto-api/raw-file/tip/spec/Overview.html
// Mattias: Window includes GlobalCrypto;

// https://fidoalliance.org/specifications/download/
// Mattias: Window includes GlobalU2F;

#ifdef MOZ_WEBSPEECH
// http://dvcs.w3.org/hg/speech-api/raw-file/tip/speechapi.html
interface mixin SpeechSynthesisGetter {
  [Throws, Pref="media.webspeech.synth.enabled"] readonly attribute SpeechSynthesis speechSynthesis;
};

Window includes SpeechSynthesisGetter;
#endif

// Mozilla-specific stuff
partial interface Window {
  //[NewObject, Throws] CSSStyleDeclaration getDefaultComputedStyle(Element elt, optional DOMString pseudoElt = "");
  [NewObject, Throws] CSSStyleDeclaration? getDefaultComputedStyle(Element elt, optional DOMString pseudoElt = "");

  // Mozilla extensions
  /**
   * Method for scrolling this window by a number of lines.
   */
  void                      scrollByLines(long numLines, optional ScrollOptions options = {});

  /**
   * Method for scrolling this window by a number of pages.
   */
  void                      scrollByPages(long numPages, optional ScrollOptions options = {});

  /**
   * Method for sizing this window to the content in the window.
   */
  [Throws, NeedsCallerType] void sizeToContent();

  // XXX Shouldn't this be in nsIDOMChromeWindow?
  [ChromeOnly, Replaceable, Throws] readonly attribute XULControllers controllers;

  [ChromeOnly, Throws] readonly attribute Element? realFrameElement;

  [ChromeOnly] readonly attribute nsIDocShell? docShell;

// Mattias:   [ChromeOnly, Constant, CrossOriginReadable, BinaryName="getBrowsingContext"]
//  readonly attribute BrowsingContext browsingContext;

  [Throws, NeedsCallerType]
  readonly attribute float mozInnerScreenX;
  [Throws, NeedsCallerType]
  readonly attribute float mozInnerScreenY;
  [Replaceable, Throws, NeedsCallerType]
  readonly attribute double devicePixelRatio;

  // Allows chrome code to convert desktop pixels to device pixels and vice
  // versa. Useful for interacting with the screen manager.
  [ChromeOnly, Throws]
  readonly attribute double desktopToDeviceScale;

  /* The maximum offset that the window can be scrolled to
     (i.e., the document width/height minus the scrollport width/height) */
  [ChromeOnly, Throws]  readonly attribute long   scrollMinX;
  [ChromeOnly, Throws]  readonly attribute long   scrollMinY;
  [Replaceable, Throws] readonly attribute long   scrollMaxX;
  [Replaceable, Throws] readonly attribute long   scrollMaxY;

  [Throws] attribute boolean fullScreen;

  // XXX Should this be in nsIDOMChromeWindow?
  void                      updateCommands(DOMString action,
                                           optional Selection? sel = null,
                                           optional short reason = 0);

  /* Find in page.
   * @param str: the search pattern
   * @param caseSensitive: is the search caseSensitive
   * @param backwards: should we search backwards
   * @param wrapAround: should we wrap the search
   * @param wholeWord: should we search only for whole words
   * @param searchInFrames: should we search through all frames
   * @param showDialog: should we show the Find dialog
   */
  [Throws] boolean    find(optional DOMString str = "",
                           optional boolean caseSensitive = false,
                           optional boolean backwards = false,
                           optional boolean wrapAround = false,
                           optional boolean wholeWord = false,
                           optional boolean searchInFrames = false,
                           optional boolean showDialog = false);

           attribute EventHandler ondevicemotion;
           attribute EventHandler ondeviceorientation;
           attribute EventHandler onabsolutedeviceorientation;
           [Pref="device.sensors.proximity.enabled"]
           attribute EventHandler onuserproximity;
           [Pref="device.sensors.ambientLight.enabled"]
           attribute EventHandler ondevicelight;

  void                      dump(DOMString str);

  /**
   * This method is here for backwards compatibility with 4.x only,
   * its implementation is a no-op
   */
  void                      setResizable(boolean resizable);

  /**
   * This is the scriptable version of
   * nsPIDOMWindow::OpenDialog() that takes 3 optional
   * arguments, plus any additional arguments are passed on as
   * arguments on the dialog's window object (window.arguments).
   */
  [Throws, ChromeOnly] WindowProxy? openDialog(optional DOMString url = "",
                                               optional DOMString name = "",
                                               optional DOMString options = "",
                                               any... extraArguments);

  [Func="nsGlobalWindowInner::ContentPropertyEnabled",
   NonEnumerable, Replaceable, Throws, NeedsCallerType]
  readonly attribute object? content;

  [Throws, ChromeOnly] any getInterface(any iid);

  /**
   * Same as nsIDOMWindow.windowRoot, useful for event listener targeting.
   */
// Mattias:   [ChromeOnly, Throws]
//  readonly attribute WindowRoot? windowRoot;

  /**
   * ChromeOnly method to determine if a particular window should see console
   * reports from service workers of the given scope.
   */
  [ChromeOnly]
  boolean shouldReportForServiceWorkerScope(USVString aScope);

  /**
   * InstallTrigger is used for extension installs.  Ideally it would
   * be something like a WebIDL namespace, but we don't support
   * JS-implemented static things yet.  See bug 863952.
   */
// Mattias:   [Replaceable, Deprecated="InstallTriggerDeprecated", Pref="extensions.InstallTrigger.enabled"]
//  readonly attribute InstallTriggerImpl? InstallTrigger;

  /**
   * Get the nsIDOMWindowUtils for this window.
   */
  [Constant, Throws, ChromeOnly]
  readonly attribute nsIDOMWindowUtils windowUtils;

// Mattias:   [Pure, ChromeOnly]
//  readonly attribute WindowGlobalChild? windowGlobalChild;

  /**
   * The principal of the client source of the window. This is supposed to be
   * used for the service worker.
   *
   * This is used for APIs like https://w3c.github.io/push-api/ that extend
   * ServiceWorkerRegistration and therefore need to operate consistently with
   * ServiceWorkers and its Clients API. The client principal is the appropriate
   * principal to pass to all nsIServiceWorkerManager APIs.
   *
   * Note that the client principal will be different from the node principal of
   * the window's document if the window is in a third-party context when dFPI
   * is enabled. In this case, the client principal will be the partitioned
   * principal to support the service worker partitioning.
   */
  [ChromeOnly]
  readonly attribute Principal? clientPrincipal;
};

// Mattias: Window includes TouchEventHandlers;

// Mattias: Window includes OnErrorEventHandlerForWindow;

#if defined(MOZ_WIDGET_ANDROID)
// https://compat.spec.whatwg.org/#windoworientation-interface
partial interface Window {
  [NeedsCallerType]
  readonly attribute short orientation;
           attribute EventHandler onorientationchange;
};
#endif

// Mozilla extension
// Sidebar is deprecated and it will be removed in the next cycles. See bug 1640138.
partial interface Window {
  [Replaceable, Throws, UseCounter, Pref="dom.window.sidebar.enabled"]
  readonly attribute (External or WindowProxy) sidebar;
};

[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback PromiseDocumentFlushedCallback = any ();

// Mozilla extensions for Chrome windows.
partial interface Window {
  // The STATE_* constants need to match the corresponding enum in nsGlobalWindow.cpp.
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  const unsigned short STATE_MAXIMIZED = 1;
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  const unsigned short STATE_MINIMIZED = 2;
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  const unsigned short STATE_NORMAL = 3;
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  const unsigned short STATE_FULLSCREEN = 4;

  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  readonly attribute unsigned short windowState;

  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  readonly attribute boolean isFullyOccluded;

  /**
   * browserDOMWindow provides access to yet another layer of
   * utility functions implemented by chrome script. It will be null
   * for DOMWindows not corresponding to browsers.
   */
  [Throws, Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
           attribute nsIBrowserDOMWindow? browserDOMWindow;

  [Throws, Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      getAttention();

  [Throws, Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      getAttentionWithCycleCount(long aCycleCount);

  [Throws, Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      setCursor(UTF8String cursor);

  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      maximize();
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      minimize();
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      restore();
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  DOMString                 getWorkspaceID();
  [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void                      moveToWorkspace(DOMString workspaceID);

  /**
   * Notify a default button is loaded on a dialog or a wizard.
   * defaultButton is the default button.
   */
  [Throws, Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  void notifyDefaultButtonLoaded(Element defaultButton);

// Mattias:   [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
//  readonly attribute ChromeMessageBroadcaster messageManager;

  /**
   * Returns the message manager identified by the given group name that
   * manages all frame loaders belonging to that group.
   */
// Mattias:   [Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
//  ChromeMessageBroadcaster getGroupMessageManager(DOMString aGroup);

  /**
   * Calls the given function as soon as a style or layout flush for the
   * top-level document is not necessary, and returns a Promise which
   * resolves to the callback's return value after it executes.
   *
   * In the event that the window goes away before a flush can occur, the
   * callback will still be called and the Promise resolved as the window
   * tears itself down.
   *
   * The callback _must not modify the DOM for any window in any way_. If it
   * does, after finishing executing, the Promise returned by
   * promiseDocumentFlushed will reject with
   * NS_ERROR_DOM_NO_MODIFICATION_ALLOWED_ERR.
   *
   * Note that the callback can be called either synchronously or asynchronously
   * depending on whether or not flushes are pending:
   *
   *   The callback will be called synchronously when calling
   *   promiseDocumentFlushed when NO flushes are already pending. This is
   *   to ensure that no script has a chance to dirty the DOM before the callback
   *   is called.
   *
   *   The callback will be called asynchronously if a flush is pending.
   *
   * The expected execution order is that all pending callbacks will
   * be fired first (and in the order that they were queued) and then the
   * Promise resolution handlers will all be invoked later on during the
   * next microtask checkpoint.
   *
   * Using window.top.promiseDocumentFlushed in combination with a callback
   * that is querying items in a window that might be swapped out via
   * nsFrameLoader::SwapWithOtherLoader is highly discouraged. For example:
   *
   *   let result = await window.top.promiseDocumentFlushed(() => {
   *     return window.document.body.getBoundingClientRect();
   *   });
   *
   *   If "window" might get swapped out via nsFrameLoader::SwapWithOtherLoader
   *   at any time, then the callback might get called when the new host window
   *   will still incur layout flushes, since it's only the original host window
   *   that's being monitored via window.top.promiseDocumentFlushed.
   *
   *   See bug 1519407 for further details.
   *
   * promiseDocumentFlushed does not support re-entrancy - so calling it from
   * within a promiseDocumentFlushed callback will result in the inner call
   * throwing an NS_ERROR_FAILURE exception, and the outer Promise rejecting
   * with that exception.
   *
   * The callback function *must not make any changes which would require
   * a style or layout flush*.
   *
   * Also throws NS_ERROR_FAILURE if the window is not in a state where flushes
   * can be waited for (for example, the PresShell has not yet been created).
   *
   * @param {function} callback
   * @returns {Promise}
   */
  [NewObject, Func="nsGlobalWindowInner::IsPrivilegedChromeWindow"]
  Promise<any> promiseDocumentFlushed(PromiseDocumentFlushedCallback callback);

  [ChromeOnly]
  readonly attribute boolean isChromeWindow;

// Mattias:   [ChromeOnly]
//  readonly attribute GleanImpl Glean;
// Mattias:   [ChromeOnly]
//  readonly attribute GleanPingsImpl GleanPings;
};

partial interface Window {
  [Pref="dom.vr.enabled"]
  attribute EventHandler onvrdisplayconnect;
  [Pref="dom.vr.enabled"]
  attribute EventHandler onvrdisplaydisconnect;
  [Pref="dom.vr.enabled"]
  attribute EventHandler onvrdisplayactivate;
  [Pref="dom.vr.enabled"]
  attribute EventHandler onvrdisplaydeactivate;
  [Pref="dom.vr.enabled"]
  attribute EventHandler onvrdisplaypresentchange;
};

#ifndef RELEASE_OR_BETA
// https://drafts.css-houdini.org/css-paint-api-1/#dom-window-paintworklet
partial interface Window {
// Mattias:     [Pref="dom.paintWorklet.enabled", Throws]
//    readonly attribute Worklet paintWorklet;
};
#endif

Window includes WindowOrWorkerGlobalScope;

partial interface Window {
// Mattias:   [Throws, Func="nsGlobalWindowInner::IsRequestIdleCallbackEnabled"]
//  unsigned long requestIdleCallback(IdleRequestCallback callback,
//                                    optional IdleRequestOptions options = {});
  [Func="nsGlobalWindowInner::IsRequestIdleCallbackEnabled"]
  void          cancelIdleCallback(unsigned long handle);
};

dictionary IdleRequestOptions {
  unsigned long timeout;
};

// Mattias: callback IdleRequestCallback = void (IdleDeadline deadline);

partial interface Window {
  /**
   * Returns a list of locales that the internationalization components
   * should be localized to.
   *
   * The function name refers to Regional Preferences which can be either
   * fetched from the internal internationalization database (CLDR), or
   * from the host environment.
   *
   * The result is a sorted list of valid locale IDs and it should be
   * used for all APIs that accept list of locales, like ECMA402 and L10n APIs.
   *
   * This API always returns at least one locale.
   *
   * Example: ["en-US", "de", "pl", "sr-Cyrl", "zh-Hans-HK"]
   */
  [Func="IsChromeOrUAWidget"]
  sequence<DOMString> getRegionalPrefsLocales();

  /**
   * Returns a list of locales that the web content would know from the user.
   *
   * One of the fingerprinting technique is to recognize users from their locales
   * exposed to web content. For those components that would be fingerprintable
   * from the locale should call this API instead of |getRegionalPrefsLocales()|.
   *
   * If the pref is set to spoof locale setting, this function will return the
   * spoofed locale, otherwise it returns what |getRegionalPrefsLocales()| returns.
   *
   * This API always returns at least one locale.
   *
   * Example: ["en-US"]
   */
  [Func="IsChromeOrUAWidget"]
  sequence<DOMString> getWebExposedLocales();

  /**
   * Getter funcion for IntlUtils, which provides helper functions for
   * localization.
   */
// Mattias:   [Throws, Func="IsChromeOrUAWidget"]
//  readonly attribute IntlUtils intlUtils;
};

partial interface Window {
// Mattias:   [SameObject, Pref="dom.visualviewport.enabled", Replaceable]
//  readonly attribute VisualViewport visualViewport;
};

// Used to assign marks to appear on the scrollbar when
// finding on a page.
partial interface Window {
  // The marks are values between 0 and scrollMax{X,Y} - scrollMin{X,Y}.
// Mattias:   [ChromeOnly]
//  void setScrollMarks(sequence<unsigned long> marks,
//                      optional boolean onHorizontalScrollbar = false);
};

dictionary WindowPostMessageOptions : StructuredSerializeOptions {
  USVString targetOrigin = "/";
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dvcs.w3.org/hg/webevents/raw-file/default/touchevents.html
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

dictionary TouchInit {
  required long identifier;
  required EventTarget target;
  long clientX = 0;
  long clientY = 0;
  long screenX = 0;
  long screenY = 0;
  long pageX = 0;
  long pageY = 0;
  float radiusX = 0;
  float radiusY = 0;
  float rotationAngle = 0;
  float force = 0;
};

[Func="mozilla::dom::Touch::PrefEnabled",
 Exposed=Window]
interface Touch {
  constructor(TouchInit touchInitDict);

  readonly    attribute long         identifier;
  readonly    attribute EventTarget? target;
  [NeedsCallerType]
  readonly    attribute long         screenX;
  [NeedsCallerType]
  readonly    attribute long         screenY;

  readonly    attribute long         clientX;
  readonly    attribute long         clientY;
  readonly    attribute long         pageX;
  readonly    attribute long         pageY;
  [NeedsCallerType]
  readonly    attribute long         radiusX;
  [NeedsCallerType]
  readonly    attribute long         radiusY;
  [NeedsCallerType]
  readonly    attribute float        rotationAngle;
  [NeedsCallerType]
  readonly    attribute float        force;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dvcs.w3.org/hg/webevents/raw-file/v1/touchevents.html
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Func="mozilla::dom::TouchList::PrefEnabled",
 Exposed=Window]
interface TouchList {
  [Pure]
  readonly attribute unsigned long length;
  getter Touch? item(unsigned long index);
};
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

[SecureContext, Pref="browser.cache.offline.enable",
Exposed=Window]
interface OfflineResourceList : EventTarget {
  /**
   * State of the application cache this object is associated with.
   */

  /* This object is not associated with an application cache. */
  const unsigned short UNCACHED = 0;

  /* The application cache is not being updated. */
  const unsigned short IDLE = 1;

  /* The manifest is being fetched and checked for updates */
  const unsigned short CHECKING = 2;

  /* Resources are being downloaded to be added to the cache */
  const unsigned short DOWNLOADING = 3;

  /* There is a new version of the application cache available */
  const unsigned short UPDATEREADY = 4;

  /* The application cache group is now obsolete. */
  const unsigned short OBSOLETE = 5;

  [Throws, UseCounter]
  readonly attribute unsigned short status;

  /**
   * Begin the application update process on the associated application cache.
   */
  [Throws, UseCounter]
  void update();

  /**
   * Swap in the newest version of the application cache, or disassociate
   * from the cache if the cache group is obsolete.
   */
  [Throws, UseCounter]
  void swapCache();

  /* Events */
  [UseCounter]
  attribute EventHandler onchecking;
  [UseCounter]
  attribute EventHandler onerror;
  [UseCounter]
  attribute EventHandler onnoupdate;
  [UseCounter]
  attribute EventHandler ondownloading;
  [UseCounter]
  attribute EventHandler onprogress;
  [UseCounter]
  attribute EventHandler onupdateready;
  [UseCounter]
  attribute EventHandler oncached;
  [UseCounter]
  attribute EventHandler onobsolete;
};

// Mozilla extensions.
partial interface OfflineResourceList {
  /**
   * Get the list of dynamically-managed entries.
   */
  [Throws]
  readonly attribute DOMStringList mozItems;

  /**
   * Check that an entry exists in the list of dynamically-managed entries.
   *
   * @param uri
   *        The resource to check.
   */
  [Throws]
  boolean mozHasItem(DOMString uri);

  /**
   * Get the number of dynamically-managed entries.
   * @status DEPRECATED
   *         Clients should use the "items" attribute.
   */
  [Throws]
  readonly attribute unsigned long mozLength;

  /**
   * Get the URI of a dynamically-managed entry.
   * @status DEPRECATED
   *         Clients should use the "items" attribute.
   */
  [Throws]
  getter DOMString mozItem(unsigned long index);

  /**
   * We need a "length" to actually be valid Web IDL, given that we have an
   * indexed getter.
   */
  readonly attribute unsigned long length;

  /**
   * Add an item to the list of dynamically-managed entries.  The resource
   * will be fetched into the application cache.
   *
   * @param uri
   *        The resource to add.
   */
  [Throws]
  void mozAdd(DOMString uri);

  /**
   * Remove an item from the list of dynamically-managed entries.  If this
   * was the last reference to a URI in the application cache, the cache
   * entry will be removed.
   *
   * @param uri
   *        The resource to remove.
   */
  [Throws]
  void mozRemove(DOMString uri);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-history-interface
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

enum ScrollRestoration { "auto", "manual" };

[Exposed=Window]
interface History {
  [Throws]
  readonly attribute unsigned long length;
  [Throws]
  attribute ScrollRestoration scrollRestoration;
  [Throws]
  readonly attribute any state;
  [Throws, NeedsSubjectPrincipal]
  void go(optional long delta = 0);
  [Throws, NeedsCallerType]
  void back();
  [Throws, NeedsCallerType]
  void forward();
  [Throws, NeedsCallerType]
  void pushState(any data, DOMString title, optional DOMString? url = null);
  [Throws, NeedsCallerType]
  void replaceState(any data, DOMString title, optional DOMString? url = null);
};
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/. */

// https://html.spec.whatwg.org/#dom-window-customelements
[Exposed=Window]
interface CustomElementRegistry {
// Mattias:   [CEReactions, Throws, UseCounter]
//  void define(DOMString name, CustomElementConstructor constructor,
//              optional ElementDefinitionOptions options = {});
  [ChromeOnly, Throws]
  void setElementCreationCallback(DOMString name, CustomElementCreationCallback callback);
  any get(DOMString name);
// Mattias:  [Throws]
//  Promise<CustomElementConstructor> whenDefined(DOMString name);
  [CEReactions] void upgrade(Node root);
};

dictionary ElementDefinitionOptions {
  DOMString extends;
};

// Mattias: callback constructor CustomElementConstructor = any ();

[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback CustomElementCreationCallback = void (DOMString name);

[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleConnectedCallback = void();
[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleDisconnectedCallback = void();
[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleAdoptedCallback = void(Document? oldDocument,
                                         Document? newDocment);
[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleAttributeChangedCallback = void(DOMString attrName,
                                                  DOMString? oldValue,
                                                  DOMString? newValue,
                                                  DOMString? namespaceURI);
[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleFormAssociatedCallback = void(HTMLFormElement? form);
[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleFormResetCallback = void();
[MOZ_CAN_RUN_SCRIPT_BOUNDARY]
callback LifecycleFormDisabledCallback = void(boolean disabled);
// Mattias: [MOZ_CAN_RUN_SCRIPT_BOUNDARY]
//callback LifecycleGetCustomInterfaceCallback = object?(any iid);

[GenerateInit]
dictionary LifecycleCallbacks {
  LifecycleConnectedCallback connectedCallback;
  LifecycleDisconnectedCallback disconnectedCallback;
  LifecycleAdoptedCallback adoptedCallback;
  LifecycleAttributeChangedCallback attributeChangedCallback;
// Mattias:   LifecycleFormAssociatedCallback formAssociatedCallback;
  LifecycleFormResetCallback formResetCallback;
  LifecycleFormDisabledCallback formDisabledCallback;
// Mattias:   [ChromeOnly] LifecycleGetCustomInterfaceCallback getCustomInterfaceCallback;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=Window]
interface BarProp
{
  [Throws, NeedsCallerType]
           attribute boolean visible;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-navigator-object
 * http://www.w3.org/TR/tracking-dnt/
 * http://www.w3.org/TR/geolocation-API/#geolocation_interface
 * http://www.w3.org/TR/battery-status/#navigatorbattery-interface
 * http://www.w3.org/TR/vibration/#vibration-interface
 * http://www.w3.org/2012/sysapps/runtime/#extension-to-the-navigator-interface-1
 * https://dvcs.w3.org/hg/gamepad/raw-file/default/gamepad.html#navigator-interface-extension
 * http://www.w3.org/TR/beacon/#sec-beacon-method
 * https://html.spec.whatwg.org/#navigatorconcurrenthardware
 * http://wicg.github.io/netinfo/#extensions-to-the-navigator-interface
 * https://w3c.github.io/webappsec-credential-management/#framework-credential-management
 * https://w3c.github.io/webdriver/webdriver-spec.html#interface
 * https://wicg.github.io/media-capabilities/#idl-index
 * https://w3c.github.io/mediasession/#idl-index
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface URI;

// http://www.whatwg.org/specs/web-apps/current-work/#the-navigator-object
[HeaderFile="Navigator.h",
 Exposed=Window]
interface Navigator {
  // objects implementing this interface also implement the interfaces given below
};
Navigator includes NavigatorID;
Navigator includes NavigatorLanguage;
Navigator includes NavigatorOnLine;
Navigator includes NavigatorContentUtils;
Navigator includes NavigatorStorageUtils;
Navigator includes NavigatorConcurrentHardware;
Navigator includes NavigatorStorage;
Navigator includes NavigatorAutomationInformation;
// Mattias: Navigator includes GPUProvider;

interface mixin NavigatorID {
  // WebKit/Blink/Trident/Presto support this (hardcoded "Mozilla").
  [Constant, Cached, Throws]
  readonly attribute DOMString appCodeName; // constant "Mozilla"
  [Constant, Cached, NeedsCallerType]
  readonly attribute DOMString appName;
  [Constant, Cached, Throws, NeedsCallerType]
  readonly attribute DOMString appVersion;
  [Pure, Cached, Throws, NeedsCallerType]
  readonly attribute DOMString platform;
  [Pure, Cached, Throws, NeedsCallerType]
  readonly attribute DOMString userAgent;
  [Constant, Cached]
  readonly attribute DOMString product; // constant "Gecko"

  // Everyone but WebKit/Blink supports this.  See bug 679971.
  [Exposed=Window]
  boolean taintEnabled(); // constant false
};

interface mixin NavigatorLanguage {

  // These two attributes are cached because this interface is also implemented
  // by Workernavigator and this way we don't have to go back to the
  // main-thread from the worker thread anytime we need to retrieve them. They
  // are updated when pref intl.accept_languages is changed.

  [Pure, Cached]
  readonly attribute DOMString? language;
  [Pure, Cached, Frozen]
  readonly attribute sequence<DOMString> languages;
};

interface mixin NavigatorOnLine {
  readonly attribute boolean onLine;
};

interface mixin NavigatorContentUtils {
  // content handler registration
  [Throws, ChromeOnly]
  void checkProtocolHandlerAllowed(DOMString scheme, URI handlerURI, URI documentURI);
  [Throws, SecureContext]
  void registerProtocolHandler(DOMString scheme, DOMString url);
  // NOT IMPLEMENTED
  //void unregisterProtocolHandler(DOMString scheme, DOMString url);
};

[SecureContext]
interface mixin NavigatorStorage {
// Mattias:   [Pref="dom.storageManager.enabled"]
//  readonly attribute StorageManager storage;
};

interface mixin NavigatorStorageUtils {
  // NOT IMPLEMENTED
  //void yieldForStorageUpdates();
};

partial interface Navigator {
// Mattias:   [Throws]
//  readonly attribute Permissions permissions;
};

partial interface Navigator {
// Mattias:   [Throws, SameObject]
//  readonly attribute MimeTypeArray mimeTypes;
// Mattias:   [Throws, SameObject]
//  readonly attribute PluginArray plugins;
  readonly attribute boolean pdfViewerEnabled;
};

// http://www.w3.org/TR/tracking-dnt/ sort of
partial interface Navigator {
  readonly attribute DOMString doNotTrack;
};

// https://globalprivacycontrol.github.io/gpc-spec/
partial interface Navigator {
  [Pref="privacy.globalprivacycontrol.functionality.enabled"]
  readonly attribute boolean globalPrivacyControl;
};

// http://www.w3.org/TR/geolocation-API/#geolocation_interface
interface mixin NavigatorGeolocation {
// Mattias:   [Throws, Pref="geo.enabled"]
//  readonly attribute Geolocation geolocation;
};
Navigator includes NavigatorGeolocation;

// http://www.w3.org/TR/battery-status/#navigatorbattery-interface
partial interface Navigator {
  // ChromeOnly to prevent web content from fingerprinting users' batteries.
// Mattias:   [Throws, ChromeOnly, Pref="dom.battery.enabled"]
//  Promise<BatteryManager> getBattery();
};

// http://www.w3.org/TR/vibration/#vibration-interface
partial interface Navigator {
    // We don't support sequences in unions yet
    //boolean vibrate ((unsigned long or sequence<unsigned long>) pattern);
    boolean vibrate(unsigned long duration);
// Mattias:     boolean vibrate(sequence<unsigned long> pattern);
};

// http://www.w3.org/TR/pointerevents/#extensions-to-the-navigator-interface
partial interface Navigator {
    [NeedsCallerType]
    readonly attribute long maxTouchPoints;
};

// https://wicg.github.io/media-capabilities/#idl-index
[Exposed=Window]
partial interface Navigator {
// Mattias:   [SameObject, Func="mozilla::dom::MediaCapabilities::Enabled"]
//  readonly attribute MediaCapabilities mediaCapabilities;
};

// Mozilla-specific extensions

// Chrome-only interface for Vibration API permission handling.
partial interface Navigator {
    /* Set permission state to device vibration.
     * @param permitted permission state (true for allowing vibration)
     * @param persistent make the permission session-persistent
     */
    [ChromeOnly]
    void setVibrationPermission(boolean permitted,
                                optional boolean persistent = true);
};

partial interface Navigator {
  [Throws, Constant, Cached, NeedsCallerType]
  readonly attribute DOMString oscpu;
  // WebKit/Blink support this; Trident/Presto do not.
  readonly attribute DOMString vendor;
  // WebKit/Blink supports this (hardcoded ""); Trident/Presto do not.
  readonly attribute DOMString vendorSub;
  // WebKit/Blink supports this (hardcoded "20030107"); Trident/Presto don't
  readonly attribute DOMString productSub;
  // WebKit/Blink/Trident/Presto support this.
  readonly attribute boolean cookieEnabled;
  [Throws, Constant, Cached, NeedsCallerType]
  readonly attribute DOMString buildID;

  // WebKit/Blink/Trident/Presto support this.
  [Affects=Nothing, DependsOn=Nothing]
  boolean javaEnabled();
};

// Addon manager bits
partial interface Navigator {
// Mattias:   [Throws, Func="mozilla::AddonManagerWebAPI::IsAPIEnabled"]
//  readonly attribute AddonManager mozAddonManager;
};

// NetworkInformation
partial interface Navigator {
// Mattias:   [Throws, Pref="dom.netinfo.enabled"]
//  readonly attribute NetworkInformation connection;
};

// https://dvcs.w3.org/hg/gamepad/raw-file/default/gamepad.html#navigator-interface-extension
partial interface Navigator {
// Mattias:   [Throws, Pref="dom.gamepad.enabled", SecureContext]
//  sequence<Gamepad?> getGamepads();
};
partial interface Navigator {
// Mattias:   [Pref="dom.gamepad.test.enabled"]
//  GamepadServiceTest requestGamepadServiceTest();
};

// https://immersive-web.github.io/webvr/spec/1.1/#interface-navigator
partial interface Navigator {
// Mattias:   [NewObject, SecureContext, Pref="dom.vr.enabled"]
//  Promise<sequence<VRDisplay>> getVRDisplays();
  // TODO: Use FrozenArray once available. (Bug 1236777)
// Mattias:   [SecureContext, Frozen, Cached, Pure, Pref="dom.vr.enabled"]
//  readonly attribute sequence<VRDisplay> activeVRDisplays;
  [ChromeOnly, Pref="dom.vr.enabled"]
  readonly attribute boolean isWebVRContentDetected;
  [ChromeOnly, Pref="dom.vr.enabled"]
  readonly attribute boolean isWebVRContentPresenting;
// Mattias:   [ChromeOnly, Pref="dom.vr.enabled"]
//  void requestVRPresentation(VRDisplay display);
};
partial interface Navigator {
// Mattias:   [Pref="dom.vr.puppet.enabled"]
//  VRServiceTest requestVRServiceTest();
};

// https://immersive-web.github.io/webxr/#dom-navigator-xr
partial interface Navigator {
// Mattias:   [SecureContext, SameObject, Throws, Pref="dom.vr.webxr.enabled"]
//  readonly attribute XRSystem xr;
};

// http://webaudio.github.io/web-midi-api/#requestmidiaccess
partial interface Navigator {
// Mattias:   [SecureContext, NewObject, Pref="dom.webmidi.enabled"]
//  Promise<MIDIAccess> requestMIDIAccess(optional MIDIOptions options = {});
};

// Mattias: callback NavigatorUserMediaSuccessCallback = void (MediaStream stream);
// Mattias: callback NavigatorUserMediaErrorCallback = void (MediaStreamError error);

partial interface Navigator {
// Mattias:   [Throws, Func="Navigator::HasUserMediaSupport"]
//  readonly attribute MediaDevices mediaDevices;

  // Deprecated. Use mediaDevices.getUserMedia instead.
// Mattias:   [Deprecated="NavigatorGetUserMedia", Throws,
//   Func="Navigator::HasUserMediaSupport",
//   NeedsCallerType,
//   UseCounter]
//    void mozGetUserMedia(MediaStreamConstraints constraints,
//                       NavigatorUserMediaSuccessCallback successCallback,
//                       NavigatorUserMediaErrorCallback errorCallback);
};

// Service Workers/Navigation Controllers
partial interface Navigator {
// Mattias:   [Func="ServiceWorkerContainer::IsEnabled", SameObject]
//  readonly attribute ServiceWorkerContainer serviceWorker;
};

partial interface Navigator {
// Mattias:   [Throws, Pref="beacon.enabled"]
//  boolean sendBeacon(DOMString url,
//                     optional BodyInit? data = null);
};

partial interface Navigator {
// Mattias:   [NewObject, Func="mozilla::dom::TCPSocket::ShouldTCPSocketExist"]
//  readonly attribute LegacyMozTCPSocket mozTCPSocket;
};

partial interface Navigator {
// Mattias:   [NewObject]
//  Promise<MediaKeySystemAccess>
//  requestMediaKeySystemAccess(DOMString keySystem,
//                              sequence<MediaKeySystemConfiguration> supportedConfigurations);
};

interface mixin NavigatorConcurrentHardware {
  readonly attribute unsigned long long hardwareConcurrency;
};

// https://w3c.github.io/webappsec-credential-management/#framework-credential-management
partial interface Navigator {
// Mattias:   [Pref="security.webauth.webauthn", SecureContext, SameObject]
//  readonly attribute CredentialsContainer credentials;
};

// https://w3c.github.io/webdriver/webdriver-spec.html#interface
interface mixin NavigatorAutomationInformation {
  [Constant, Cached]
  readonly attribute boolean webdriver;
};

// https://www.w3.org/TR/clipboard-apis/#navigator-interface
partial interface Navigator {
  [SecureContext, SameObject]
  readonly attribute Clipboard clipboard;
};

// Used for testing of origin trials.
partial interface Navigator {
  [Trial="TestTrial"]
  readonly attribute boolean testTrialGatedAttribute;
};

// https://wicg.github.io/web-share/#navigator-interface
partial interface Navigator {
  [SecureContext, NewObject, Func="Navigator::HasShareSupport"]
  Promise<void> share(optional ShareData data = {});
  [SecureContext, Func="Navigator::HasShareSupport"]
  boolean canShare(optional ShareData data = {});
};
// https://wicg.github.io/web-share/#sharedata-dictionary
dictionary ShareData {
  USVString title;
  USVString text;
  USVString url;
  // Note: we don't actually support files yet
  // we have it here for the .canShare() checks.
  sequence<File> files;
};

// https://w3c.github.io/mediasession/#idl-index
[Exposed=Window]
partial interface Navigator {
// Mattias:   [Pref="dom.media.mediasession.enabled", SameObject]
//  readonly attribute MediaSession mediaSession;
};

// https://wicg.github.io/web-locks/#navigator-mixins
[SecureContext]
interface mixin NavigatorLocks {
// Mattias:   [Pref="dom.weblocks.enabled"]
//  readonly attribute LockManager locks;
};
Navigator includes NavigatorLocks;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this file,
* You can obtain one at http://mozilla.org/MPL/2.0/.
*
* The origin of this IDL file is
* http://www.whatwg.org/html/#the-storage-interface
*
* © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
* Opera Software ASA. You are granted a license to use, reproduce
* and create derivative works of this document.
*/

[Exposed=Window]
interface Storage {
  [Throws, NeedsSubjectPrincipal]
  readonly attribute unsigned long length;

  [Throws, NeedsSubjectPrincipal]
  DOMString? key(unsigned long index);

  [Throws, NeedsSubjectPrincipal]
  getter DOMString? getItem(DOMString key);

  [Throws, NeedsSubjectPrincipal]
  setter void setItem(DOMString key, DOMString value);

  [Throws, NeedsSubjectPrincipal]
  deleter void removeItem(DOMString key);

  [Throws, NeedsSubjectPrincipal]
  void clear();

  [ChromeOnly]
  readonly attribute boolean isSessionOnly;
};

/**
 * Testing methods that exist only for the benefit of automated glass-box
 * testing.  Will never be exposed to content at large and unlikely to be useful
 * in a WebDriver context.
 */
partial interface Storage {
  /**
   * Does a security-check and ensures the underlying database has been opened
   * without actually calling any database methods.  (Read-only methods will
   * have a similar effect but also impact the state of the snapshot.)
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  void open();

  /**
   * Automatically ends any explicit snapshot and drops the reference to the
   * underlying database, but does not otherwise perturb the database.
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  void close();

  /**
   * Ensures the database has been opened and initiates an explicit snapshot.
   * Snapshots are normally automatically ended and checkpointed back to the
   * parent, but explicitly opened snapshots must be explicitly ended via
   * `endExplicitSnapshot` or `close`.
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  void beginExplicitSnapshot();

  /**
   * Checkpoints the explicitly begun snapshot. This is only useful for testing
   * of snapshot re-using when multiple checkpoints are involved. There's no
   * need to call this before `endExplicitSnapshot` because it checkpoints the
   * snapshot before it's ended.
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  void checkpointExplicitSnapshot();

  /**
   * Ends the explicitly begun snapshot and retains the underlying database.
   * Compare with `close` which also drops the reference to the database.
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  void endExplicitSnapshot();

  /**
   * Returns true if the underlying database has been opened, the database is
   * not being closed and it has a snapshot (initialized implicitly or
   * explicitly).
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  readonly attribute boolean hasSnapshot;

  /**
   * Returns snapshot usage.
   *
   * @throws NS_ERROR_NOT_AVAILABLE if the underlying database hasn't been
   *         opened or the database is being closed or it doesn't have a
   *         snapshot.
   */
  [Throws, NeedsSubjectPrincipal, Pref="dom.storage.testing"]
  readonly attribute long long snapshotUsage;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/selection-api/#selection-interface
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface Selection {
  [NeedsCallerType]
  readonly attribute Node?         anchorNode;
  [NeedsCallerType]
  readonly attribute unsigned long anchorOffset;
  [NeedsCallerType]
  readonly attribute Node?         focusNode;
  [NeedsCallerType]
  readonly attribute unsigned long focusOffset;
  readonly attribute boolean       isCollapsed;
  /**
   * Returns the number of ranges in the selection.
   */
  readonly attribute unsigned long rangeCount;
  readonly attribute DOMString     type;
  /**
   * Returns the range at the specified index.  Throws if the index is
   * out of range.
   */
  [Throws]
  Range     getRangeAt(unsigned long index);
  /**
   * Adds a range to the current selection.
   */
  [Throws, BinaryName="addRangeJS"]
  void      addRange(Range range);
  /**
   * Removes a range from the current selection.
   */
  [Throws, BinaryName="removeRangeAndUnselectFramesAndNotifyListeners"]
  void      removeRange(Range range);
  /**
   * Removes all ranges from the current selection.
   */
  [Throws]
  void      removeAllRanges();
  [Throws, BinaryName="RemoveAllRanges"]
  void      empty();
  [Throws, BinaryName="collapseJS"]
  void      collapse(Node? node, optional unsigned long offset = 0);
  [Throws, BinaryName="collapseJS"]
  void      setPosition(Node? node, optional unsigned long offset = 0);
  [Throws, BinaryName="collapseToStartJS"]
  void      collapseToStart();
  [Throws, BinaryName="collapseToEndJS"]
  void      collapseToEnd();
  [Throws, BinaryName="extendJS"]
  void      extend(Node node, optional unsigned long offset = 0);
  [Throws, BinaryName="setBaseAndExtentJS"]
  void      setBaseAndExtent(Node anchorNode,
                             unsigned long anchorOffset,
                             Node focusNode,
                             unsigned long focusOffset);
  [Throws, BinaryName="selectAllChildrenJS"]
  void      selectAllChildren(Node node);
  [CEReactions, Throws]
  void      deleteFromDocument();
  [Throws]
  boolean   containsNode(Node node,
                         optional boolean allowPartialContainment = false);
// Mattias: stringifier DOMString toString ();
};

// Additional methods not currently in the spec
partial interface Selection {
  [Throws]
  void modify(DOMString alter, DOMString direction,
              DOMString granularity);
};

// Additional chrome-only methods.
interface nsISelectionListener;
partial interface Selection {
  /**
   * A true value means "selection after newline"; false means "selection before
   * newline" when a selection is positioned "between lines".
   */
  [ChromeOnly,Throws, BinaryName=interlinePositionJS]
  attribute boolean interlinePosition;

  [Throws]
  attribute short? caretBidiLevel;

  [ChromeOnly,Throws]
  DOMString  toStringWithFormat(DOMString formatType, unsigned long flags, long wrapColumn);
  [ChromeOnly]
  void  addSelectionListener(nsISelectionListener newListener);
  [ChromeOnly]
  void  removeSelectionListener(nsISelectionListener listenerToRemove);

  [ChromeOnly,BinaryName="rawType"]
  readonly attribute short selectionType;

  /**
   * Return array of ranges intersecting with the given DOM interval.
   */
  [ChromeOnly,Throws,Pref="dom.testing.selection.GetRangesForInterval"]
  sequence<Range> GetRangesForInterval(Node beginNode, long beginOffset, Node endNode, long endOffset,
                                       boolean allowAdjacent);

  /**
   * Scrolls a region of the selection, so that it is visible in
   * the scrolled view.
   *
   * @param aRegion the region inside the selection to scroll into view
   *                (see selection region constants defined in
   *                nsISelectionController).
   * @param aIsSynchronous when true, scrolls the selection into view
   *                       before returning. If false, posts a request which
   *                       is processed at some point after the method returns.
   * @param aVPercent how to align the frame vertically.
   * @param aHPercent how to align the frame horizontally.
   */
  [ChromeOnly,Throws]
  void scrollIntoView(short aRegion, boolean aIsSynchronous, short aVPercent, short aHPercent);

  /**
   * setColors() sets custom colors for the selection.
   * Currently, this is supported only when the selection type is SELECTION_FIND.
   * Otherwise, throws an exception.
   *
   * @param aForegroundColor     The foreground color of the selection.
   *                             If this is "currentColor", foreground color
   *                             isn't changed by this selection.
   * @param aBackgroundColor     The background color of the selection.
   *                             If this is "transparent", background color is
   *                             never painted.
   * @param aAltForegroundColor  The alternative foreground color of the
   *                             selection.
   *                             If aBackgroundColor doesn't have sufficient
   *                             contrast with its around or foreground color
   *                             if "currentColor" is specified, alternative
   *                             colors are used if it have higher contrast.
   * @param aAltBackgroundColor  The alternative background color of the
   *                             selection.
   */
  [ChromeOnly,Throws]
  void setColors(DOMString aForegroundColor, DOMString aBackgroundColor,
                 DOMString aAltForegroundColor, DOMString aAltBackgroundColor);

  /**
   * resetColors() forget the customized colors which were set by setColors().
   */
  [ChromeOnly,Throws]
  void resetColors();
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

[Exposed=Window]
interface Screen : EventTarget {
  // CSSOM-View
  // http://dev.w3.org/csswg/cssom-view/#the-screen-interface
  [Throws]
  readonly attribute long availWidth;
  [Throws]
  readonly attribute long availHeight;
  [Throws]
  readonly attribute long width;
  [Throws]
  readonly attribute long height;
  [Throws]
  readonly attribute long colorDepth;
  [Throws]
  readonly attribute long pixelDepth;

  [Throws]
  readonly attribute long top;
  [Throws]
  readonly attribute long left;
  [Throws]
  readonly attribute long availTop;
  [Throws]
  readonly attribute long availLeft;

  /**
   * DEPRECATED, use ScreenOrientation API instead.
   * Returns the current screen orientation.
   * Can be: landscape-primary, landscape-secondary,
   *         portrait-primary or portrait-secondary.
   */
  [NeedsCallerType]
  readonly attribute DOMString mozOrientation;

  attribute EventHandler onmozorientationchange;

  /**
   * DEPRECATED, use ScreenOrientation API instead.
   * Lock screen orientation to the specified type.
   */
  [Throws]
  boolean mozLockOrientation(DOMString orientation);
// Mattias:   [Throws]
//  boolean mozLockOrientation(sequence<DOMString> orientation);

  /**
   * DEPRECATED, use ScreenOrientation API instead.
   * Unlock the screen orientation.
   */
  void mozUnlockOrientation();
};

// https://w3c.github.io/screen-orientation
partial interface Screen {
  readonly attribute ScreenOrientation orientation;
};

// https://wicg.github.io/media-capabilities/#idl-index
enum ScreenColorGamut {
  "srgb",
  "p3",
  "rec2020",
};

[Func="nsScreen::MediaCapabilitiesEnabled",
 Exposed=Window]
interface ScreenLuminance {
  readonly attribute double min;
  readonly attribute double max;
  readonly attribute double maxAverage;
};

partial interface Screen {
  [Func="nsScreen::MediaCapabilitiesEnabled"]
  readonly attribute ScreenColorGamut colorGamut;
  [Func="nsScreen::MediaCapabilitiesEnabled"]
  readonly attribute ScreenLuminance? luminance;

  [Func="nsScreen::MediaCapabilitiesEnabled"]
  attribute EventHandler onchange;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/clipboard-apis/
 *
 * Copyright © 2018 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

typedef sequence<ClipboardItem> ClipboardItems;

[SecureContext, Exposed=Window]
interface Clipboard : EventTarget {
  [Pref="dom.events.asyncClipboard.clipboardItem", NewObject, NeedsSubjectPrincipal]
  Promise<ClipboardItems> read();
  [Func="Clipboard::ReadTextEnabled", NewObject, NeedsSubjectPrincipal]
  Promise<DOMString> readText();

// Mattias:  [Pref="dom.events.asyncClipboard.clipboardItem", NewObject, NeedsSubjectPrincipal]
//  Promise<void> write(ClipboardItems data);

  [NewObject, NeedsSubjectPrincipal]
  Promise<void> writeText(DOMString data);
};

partial interface Clipboard {
  // @param allowed true, if the user allowed (e.g. clicked) the "Paste" menuitem.
  //                false, when the menupopup was dismissed.
  [ChromeOnly]
  void onUserReactedToPasteMenuPopup(boolean allowed);
};

typedef (DOMString or Blob) ClipboardItemDataType;
// typedef Promise<ClipboardItemDataType> ClipboardItemData;
// callback ClipboardItemDelayedCallback = ClipboardItemData ();

[SecureContext, Exposed=Window, Pref="dom.events.asyncClipboard.clipboardItem"]
interface ClipboardItem {
  // Note: The spec uses Promise<ClipboardItemDataType>.
  [Throws]
  constructor(record<DOMString, ClipboardItemDataType> items,
              optional ClipboardItemOptions options = {});

  // static ClipboardItem createDelayed(
  //     record<DOMString, ClipboardItemDelayedCallback> items,
  //     optional ClipboardItemOptions options = {});

  readonly attribute PresentationStyle presentationStyle;
  // readonly attribute long long lastModified;
  // readonly attribute boolean delayed;

  // TODO: Use FrozenArray once available. (Bug 1236777)
  // readonly attribute FrozenArray<DOMString> types;
  [Frozen, Cached, Pure]
  readonly attribute sequence<DOMString> types;

  [NewObject]
  Promise<Blob> getType(DOMString type);
};

enum PresentationStyle { "unspecified", "inline", "attachment" };

dictionary ClipboardItemOptions {
  PresentationStyle presentationStyle = "unspecified";
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dom.spec.whatwg.org/#range
 * http://domparsing.spec.whatwg.org/#dom-range-createcontextualfragment
 * http://dvcs.w3.org/hg/csswg/raw-file/tip/cssom-view/Overview.html#extensions-to-the-range-interface
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface Range : AbstractRange {
  [Throws]
  constructor();

  [Throws]
  readonly attribute Node commonAncestorContainer;

  [Throws, BinaryName="setStartJS"]
  void setStart(Node refNode, unsigned long offset);
  [Throws, BinaryName="setEndJS"]
  void setEnd(Node refNode, unsigned long offset);
  [Throws, BinaryName="setStartBeforeJS"]
  void setStartBefore(Node refNode);
  [Throws, BinaryName="setStartAfterJS"]
  void setStartAfter(Node refNode);
  [Throws, BinaryName="setEndBeforeJS"]
  void setEndBefore(Node refNode);
  [Throws, BinaryName="setEndAfterJS"]
  void setEndAfter(Node refNode);
  [BinaryName="collapseJS"]
  void collapse(optional boolean toStart = false);
  [Throws, BinaryName="selectNodeJS"]
  void selectNode(Node refNode);
  [Throws, BinaryName="selectNodeContentsJS"]
  void selectNodeContents(Node refNode);

  const unsigned short START_TO_START = 0;
  const unsigned short START_TO_END = 1;
  const unsigned short END_TO_END = 2;
  const unsigned short END_TO_START = 3;
  [Throws]
  short compareBoundaryPoints(unsigned short how, Range sourceRange);
  [CEReactions, Throws]
  void deleteContents();
  [CEReactions, Throws]
  DocumentFragment extractContents();
  [CEReactions, Throws]
  DocumentFragment cloneContents();
  [CEReactions, Throws]
  void insertNode(Node node);
  [CEReactions, Throws]
  void surroundContents(Node newParent);

  Range cloneRange();
  void detach();

  [Throws]
  boolean isPointInRange(Node node, unsigned long offset);
  [Throws]
  short comparePoint(Node node, unsigned long offset);

  [Throws]
  boolean intersectsNode(Node node);

// Mattias:  [Throws, BinaryName="ToString"]
//  stringifier;
};

// http://domparsing.spec.whatwg.org/#dom-range-createcontextualfragment
partial interface Range {
  [CEReactions, Throws]
  DocumentFragment createContextualFragment(DOMString fragment);
};

// http://dvcs.w3.org/hg/csswg/raw-file/tip/cssom-view/Overview.html#extensions-to-the-range-interface
partial interface Range {
  DOMRectList? getClientRects();
  DOMRect getBoundingClientRect();
};

dictionary ClientRectsAndTexts {
  required DOMRectList rectList;
  required sequence<DOMString> textList;
};

partial interface Range {
// Mattias:   [ChromeOnly, Throws]
//  ClientRectsAndTexts getClientRectsAndTexts();
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dom.spec.whatwg.org/#abstractrange
 *
 * Copyright  2012 W3C  (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface AbstractRange {
  [BinaryName="GetStartContainer"]
  readonly attribute Node startContainer;
  readonly attribute unsigned long startOffset;
  [BinaryName="GetEndContainer"]
  readonly attribute Node endContainer;
  readonly attribute unsigned long endOffset;
  readonly attribute boolean collapsed;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/screen-orientation
 *
 * Copyright © 2014 W3C® (MIT, ERCIM, Keio, Beihang), All Rights
 * Reserved. W3C liability, trademark and document use rules apply.
 */

enum OrientationType {
  "portrait-primary",
  "portrait-secondary",
  "landscape-primary",
  "landscape-secondary"
};

enum OrientationLockType {
  "any",
  "natural",
  "landscape",
  "portrait",
  "portrait-primary",
  "portrait-secondary",
  "landscape-primary",
  "landscape-secondary"
};

[Exposed=Window]
interface ScreenOrientation : EventTarget {
  [NewObject]
  Promise<void> lock(OrientationLockType orientation);
  [Throws]
  void unlock();
  [Throws, NeedsCallerType]
  readonly attribute OrientationType type;
  [Throws, NeedsCallerType]
  readonly attribute unsigned short angle;
  attribute EventHandler onchange;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=Window]
interface DOMRectList {
  readonly attribute unsigned long length;
  getter DOMRect? item(unsigned long index);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://drafts.fxtf.org/geometry/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker),
 Serializable]
interface DOMRect : DOMRectReadOnly {
    constructor(optional unrestricted double x = 0,
                optional unrestricted double y = 0,
                optional unrestricted double width = 0,
                optional unrestricted double height = 0);

    [NewObject] static DOMRect fromRect(optional DOMRectInit other = {});

    inherit attribute unrestricted double x;
    inherit attribute unrestricted double y;
    inherit attribute unrestricted double width;
    inherit attribute unrestricted double height;
};

[ProbablyShortLivingWrapper,
 Exposed=(Window,Worker),
 Serializable]
interface DOMRectReadOnly {
    constructor(optional unrestricted double x = 0,
                optional unrestricted double y = 0,
                optional unrestricted double width = 0,
                optional unrestricted double height = 0);

    [NewObject] static DOMRectReadOnly fromRect(optional DOMRectInit other = {});

    readonly attribute unrestricted double x;
    readonly attribute unrestricted double y;
    readonly attribute unrestricted double width;
    readonly attribute unrestricted double height;
    readonly attribute unrestricted double top;
    readonly attribute unrestricted double right;
    readonly attribute unrestricted double bottom;
    readonly attribute unrestricted double left;

    [Default] object toJSON();
};

dictionary DOMRectInit {
    unrestricted double x = 0;
    unrestricted double y = 0;
    unrestricted double width = 0;
    unrestricted double height = 0;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/ and
 * http://dev.w3.org/csswg/cssom-view/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window]
interface HTMLElement : Element {
  [HTMLConstructor] constructor();

  // metadata attributes
  [CEReactions]
           attribute DOMString title;
  [CEReactions]
           attribute DOMString lang;
  //         attribute boolean translate;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString dir;

  [CEReactions, GetterThrows, Pure]
           attribute [LegacyNullToEmptyString] DOMString innerText;
  [CEReactions, GetterThrows, SetterThrows, Pure]
           attribute [LegacyNullToEmptyString] DOMString outerText;

  // user interaction
  [CEReactions, SetterThrows, Pure]
           attribute boolean hidden;
  [CEReactions, SetterThrows, Pure, Pref="html5.inert.enabled"]
           attribute boolean inert;
  [NeedsCallerType]
  void click();
  [CEReactions, SetterThrows, Pure]
           attribute DOMString accessKey;
  [Pure]
  readonly attribute DOMString accessKeyLabel;
  [CEReactions, SetterThrows, Pure]
           attribute boolean draggable;
  //[PutForwards=value] readonly attribute DOMTokenList dropzone;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString contentEditable;
  [Pure]
  readonly attribute boolean isContentEditable;
  [Pure, Pref="dom.menuitem.enabled"]
  readonly attribute HTMLMenuElement? contextMenu;
  [CEReactions, SetterThrows, Pure]
           attribute boolean spellcheck;
  [CEReactions, Pure, SetterThrows, Pref="dom.forms.inputmode"]
           attribute DOMString inputMode;
  [CEReactions, Pure, SetterThrows, Pref="dom.forms.enterkeyhint"]
           attribute DOMString enterKeyHint;
  [CEReactions, Pure, SetterThrows, Pref="dom.forms.autocapitalize"]
           attribute DOMString autocapitalize;

  attribute DOMString nonce;

  // command API
  //readonly attribute DOMString? commandType;
  //readonly attribute DOMString? commandLabel;
  //readonly attribute DOMString? commandIcon;
  //readonly attribute boolean? commandHidden;
  //readonly attribute boolean? commandDisabled;
  //readonly attribute boolean? commandChecked;

  // https://html.spec.whatwg.org/multipage/custom-elements.html#dom-attachinternals
  [Throws]
  ElementInternals attachInternals();
};

// http://dev.w3.org/csswg/cssom-view/#extensions-to-the-htmlelement-interface
partial interface HTMLElement {
  // CSSOM things are not [Pure] because they can flush
  readonly attribute Element? offsetParent;
  readonly attribute long offsetTop;
  readonly attribute long offsetLeft;
  readonly attribute long offsetWidth;
  readonly attribute long offsetHeight;
};

partial interface HTMLElement {
  [ChromeOnly]
  readonly attribute ElementInternals? internals;

  [ChromeOnly]
  readonly attribute boolean isFormAssociatedCustomElements;
};

interface mixin TouchEventHandlers {
  [Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
           attribute EventHandler ontouchstart;
  [Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
           attribute EventHandler ontouchend;
  [Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
           attribute EventHandler ontouchmove;
  [Func="nsGenericHTMLElement::LegacyTouchAPIEnabled"]
           attribute EventHandler ontouchcancel;
};

HTMLElement includes GlobalEventHandlers;
HTMLElement includes HTMLOrForeignElement;
HTMLElement includes DocumentAndElementEventHandlers;
HTMLElement includes ElementCSSInlineStyle;
HTMLElement includes TouchEventHandlers;
HTMLElement includes OnErrorEventHandlerForNodes;

[Exposed=Window]
interface HTMLUnknownElement : HTMLElement {};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-head-element
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

// http://www.whatwg.org/specs/web-apps/current-work/#the-head-element
[Exposed=Window]
interface HTMLHeadElement : HTMLElement {
  [HTMLConstructor] constructor();
};

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* Emulates undefined through Codegen.py. */
[LegacyUnenumerableNamedProperties,
 Exposed=Window]
interface HTMLAllCollection {
  readonly attribute unsigned long length;
  getter Element (unsigned long index);
  getter (HTMLCollection or Element)? namedItem(DOMString name);
  (HTMLCollection or Element)? item(optional DOMString nameOrIndex);
// Mattias:  legacycaller (HTMLCollection or Element)? (optional DOMString nameOrIndex);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/grouping-content.html#the-menu-element
 * https://html.spec.whatwg.org/multipage/obsolete.html#HTMLMenuElement-partial
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface MenuBuilder;

// https://html.spec.whatwg.org/multipage/grouping-content.html#the-menu-element
[Exposed=Window]
interface HTMLMenuElement : HTMLElement {
  [HTMLConstructor] constructor();

           [CEReactions, SetterThrows, Pref="dom.menuitem.enabled"]
           attribute DOMString type;
           [CEReactions, SetterThrows, Pref="dom.menuitem.enabled"]
           attribute DOMString label;
};

// https://html.spec.whatwg.org/multipage/obsolete.html#HTMLMenuElement-partial
partial interface HTMLMenuElement {
           [CEReactions, SetterThrows]
           attribute boolean compact;
};

// Mozilla specific stuff
partial interface HTMLMenuElement {
  /**
   * Creates and dispatches a trusted event named "show".
   * The event is not cancelable and does not bubble.
   * See http://www.whatwg.org/specs/web-apps/current-work/multipage/interactive-elements.html#context-menus
   */
  [ChromeOnly]
  void sendShowEvent();

  /**
   * Creates a native menu builder. The builder type is dependent on menu type.
   * Currently, it returns the @mozilla.org/content/html-menu-builder;1
   * component. Toolbar menus are not yet supported (the method returns null).
   */
  [ChromeOnly]
  MenuBuilder? createBuilder();

  /*
   * Builds a menu by iterating over menu children.
   * See http://www.whatwg.org/specs/web-apps/current-work/multipage/interactive-elements.html#building-menus-and-toolbars
   * The caller can use a native builder by calling createBuilder() or provide
   * a custom builder that implements the nsIMenuBuilder interface.
   * A custom builder can be used for example to build native context menus
   * that are not defined using <menupopup>.
   */
  [ChromeOnly]
  void build(MenuBuilder aBuilder);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/#elementinternals
 */

[Exposed=Window]
interface ElementInternals {
  // Shadow root access
  readonly attribute ShadowRoot? shadowRoot;

  // Form-associated custom elements
  [Throws]
  void setFormValue((File or USVString or FormData)? value,
                    optional (File or USVString or FormData)? state);

  [Throws]
  readonly attribute HTMLFormElement? form;

  [Throws]
  void setValidity(optional ValidityStateFlags flags = {},
                   optional DOMString message,
                   optional HTMLElement anchor);
  [Throws]
  readonly attribute boolean willValidate;
  [Throws]
  readonly attribute ValidityState validity;
  [Throws]
  readonly attribute DOMString validationMessage;
  [Throws]
  boolean checkValidity();
  [Throws]
  boolean reportValidity();

  [Throws]
  readonly attribute NodeList labels;
};

partial interface ElementInternals {
  [ChromeOnly, Throws]
  readonly attribute HTMLElement? validationAnchor;
};

dictionary ValidityStateFlags {
  boolean valueMissing = false;
  boolean typeMismatch = false;
  boolean patternMismatch = false;
  boolean tooLong = false;
  boolean tooShort = false;
  boolean rangeUnderflow = false;
  boolean rangeOverflow = false;
  boolean stepMismatch = false;
  boolean badInput = false;
  boolean customError = false;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dvcs.w3.org/hg/webcomponents/raw-file/tip/spec/shadow/index.html
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

// https://dom.spec.whatwg.org/#enumdef-shadowrootmode
enum ShadowRootMode {
  "open",
  "closed"
};

enum SlotAssignmentMode { "manual", "named" };

// https://dom.spec.whatwg.org/#shadowroot
[Exposed=Window]
interface ShadowRoot : DocumentFragment
{
  // Shadow DOM v1
  readonly attribute ShadowRootMode mode;
  [Pref="dom.shadowdom.delegatesFocus.enabled"]
  readonly attribute boolean delegatesFocus;
  [Pref="dom.shadowdom.slot.assign.enabled"]
  readonly attribute SlotAssignmentMode slotAssignment;
  readonly attribute Element host;
  attribute EventHandler onslotchange;

  Element? getElementById(DOMString elementId);

  // https://w3c.github.io/DOM-Parsing/#the-innerhtml-mixin
  [CEReactions, SetterThrows]
  attribute [LegacyNullToEmptyString] DOMString innerHTML;

  // When JS invokes importNode or createElement, the binding code needs to
  // create a reflector, and so invoking those methods directly on the content
  // document would cause the reflector to be created in the content scope,
  // at which point it would be difficult to move into the UA Widget scope.
  // As such, these methods allow UA widget code to simultaneously create nodes
  // and associate them with the UA widget tree, so that the reflectors get
  // created in the right scope.
  [CEReactions, Throws, Func="IsChromeOrUAWidget"]
  Node importNodeAndAppendChildAt(Node parentNode, Node node, optional boolean deep = false);

  [CEReactions, Throws, Func="IsChromeOrUAWidget"]
  Node createElementAndAppendChildAt(Node parentNode, DOMString localName);

  // For triggering UA Widget scope in tests.
  [ChromeOnly]
  void setIsUAWidget();
  [ChromeOnly]
  boolean isUAWidget();
};

ShadowRoot includes DocumentOrShadowRoot;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#htmlformelement
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[LegacyOverrideBuiltIns, LegacyUnenumerableNamedProperties,
 Exposed=Window]
interface HTMLFormElement : HTMLElement {
  [HTMLConstructor] constructor();

           [CEReactions, Pure, SetterThrows]
           attribute DOMString acceptCharset;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString action;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString autocomplete;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString enctype;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString encoding;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString method;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString name;
           [CEReactions, Pure, SetterThrows]
           attribute boolean noValidate;
           [CEReactions, Pure, SetterThrows]
           attribute DOMString target;

  [Constant]
  readonly attribute HTMLCollection elements;
  [Pure]
  readonly attribute long length;

  getter Element (unsigned long index);
  // TODO this should be: getter (RadioNodeList or HTMLInputElement or HTMLImageElement) (DOMString name);
  getter nsISupports (DOMString name);

  [Throws]
  void submit();
  [Throws]
  void requestSubmit(optional HTMLElement? submitter = null);
  [CEReactions]
  void reset();
  boolean checkValidity();
  boolean reportValidity();
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#validitystate
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window]
interface ValidityState {
  readonly attribute boolean valueMissing;
  readonly attribute boolean typeMismatch;
  readonly attribute boolean patternMismatch;
  readonly attribute boolean tooLong;
  readonly attribute boolean tooShort;
  readonly attribute boolean rangeUnderflow;
  readonly attribute boolean rangeOverflow;
  readonly attribute boolean stepMismatch;
  readonly attribute boolean badInput;
  readonly attribute boolean customError;
  readonly attribute boolean valid;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface SVGViewSpec;

[Exposed=Window]
interface SVGSVGElement : SVGGraphicsElement {

  [Constant]
  readonly attribute SVGAnimatedLength x;
  [Constant]
  readonly attribute SVGAnimatedLength y;
  [Constant]
  readonly attribute SVGAnimatedLength width;
  [Constant]
  readonly attribute SVGAnimatedLength height;
  // readonly attribute SVGRect viewport;
  readonly attribute boolean useCurrentView;
  // readonly attribute SVGViewSpec currentView;
  [UseCounter]
           attribute float currentScale;
  readonly attribute SVGPoint currentTranslate;

  [DependsOn=Nothing, Affects=Nothing]
  unsigned long suspendRedraw(unsigned long maxWaitMilliseconds);
  [DependsOn=Nothing, Affects=Nothing]
  void unsuspendRedraw(unsigned long suspendHandleID);
  [DependsOn=Nothing, Affects=Nothing]
  void unsuspendRedrawAll();
  [DependsOn=Nothing, Affects=Nothing]
  void forceRedraw();
  void pauseAnimations();
  void unpauseAnimations();
  boolean animationsPaused();
  [BinaryName="getCurrentTimeAsFloat"]
  float getCurrentTime();
  void setCurrentTime(float seconds);
  // NodeList getIntersectionList(SVGRect rect, SVGElement referenceElement);
  // NodeList getEnclosureList(SVGRect rect, SVGElement referenceElement);
  // boolean checkIntersection(SVGElement element, SVGRect rect);
  // boolean checkEnclosure(SVGElement element, SVGRect rect);
  void deselectAll();
  [NewObject]
  SVGNumber createSVGNumber();
  [NewObject]
  SVGLength createSVGLength();
  [NewObject]
  SVGAngle createSVGAngle();
  [NewObject]
  SVGPoint createSVGPoint();
  [NewObject]
  SVGMatrix createSVGMatrix();
  [NewObject]
  SVGRect createSVGRect();
  [NewObject]
  SVGTransform createSVGTransform();
  [NewObject, Throws]
  SVGTransform createSVGTransformFromMatrix(optional DOMMatrix2DInit matrix = {});
  [UseCounter]
  Element? getElementById(DOMString elementId);
};

SVGSVGElement includes SVGFitToViewBox;
SVGSVGElement includes SVGZoomAndPan;

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface mixin SVGFitToViewBox {
  [Constant]
  readonly attribute SVGAnimatedRect viewBox;
  [Constant]
  readonly attribute SVGAnimatedPreserveAspectRatio preserveAspectRatio;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface mixin SVGZoomAndPan {
  // Zoom and Pan Types
  const unsigned short SVG_ZOOMANDPAN_UNKNOWN = 0;
  const unsigned short SVG_ZOOMANDPAN_DISABLE = 1;
  const unsigned short SVG_ZOOMANDPAN_MAGNIFY = 2;

  [SetterThrows]
  attribute unsigned short zoomAndPan;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://drafts.csswg.org/cssom/#the-cssrule-interface
 * https://drafts.csswg.org/css-animations/#interface-cssrule
 * https://drafts.csswg.org/css-counter-styles-3/#extentions-to-cssrule-interface
 * https://drafts.csswg.org/css-conditional-3/#extentions-to-cssrule-interface
 * https://drafts.csswg.org/css-fonts-3/#om-fontfeaturevalues
 */

// https://drafts.csswg.org/cssom/#the-cssrule-interface
[Exposed=Window]
interface CSSRule {

  const unsigned short STYLE_RULE = 1;
  const unsigned short CHARSET_RULE = 2; // historical
  const unsigned short IMPORT_RULE = 3;
  const unsigned short MEDIA_RULE = 4;
  const unsigned short FONT_FACE_RULE = 5;
  const unsigned short PAGE_RULE = 6;
  // FIXME: We don't support MARGIN_RULE yet.
  // XXXbz Should we expose the constant anyway?
  // const unsigned short MARGIN_RULE = 9;
  const unsigned short NAMESPACE_RULE = 10;
  [BinaryName="typeForBindings"] readonly attribute unsigned short type;
  attribute UTF8String cssText;
  readonly attribute CSSRule? parentRule;
  readonly attribute CSSStyleSheet? parentStyleSheet;
};

// https://drafts.csswg.org/css-animations/#interface-cssrule
partial interface CSSRule {
    const unsigned short KEYFRAMES_RULE = 7;
    const unsigned short KEYFRAME_RULE = 8;
};

// https://drafts.csswg.org/css-counter-styles-3/#extentions-to-cssrule-interface
partial interface CSSRule {
    const unsigned short COUNTER_STYLE_RULE = 11;
};

// https://drafts.csswg.org/css-conditional-3/#extentions-to-cssrule-interface
partial interface CSSRule {
    const unsigned short SUPPORTS_RULE = 12;
};

// Non-standard extension for @-moz-document rules.
partial interface CSSRule {
    [ChromeOnly]
    const unsigned short DOCUMENT_RULE = 13;
};

// https://drafts.csswg.org/css-fonts-3/#om-fontfeaturevalues
partial interface CSSRule {
  const unsigned short FONT_FEATURE_VALUES_RULE = 14;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGAnimatedLength {
  [Constant]
  readonly attribute SVGLength baseVal;
  [Constant]
  readonly attribute SVGLength animVal;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGPoint {

  [SetterThrows]
  attribute float x;
  [SetterThrows]
  attribute float y;

  [NewObject, Throws]
  SVGPoint matrixTransform(optional DOMMatrix2DInit matrix = {});
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGNumber {
  [SetterThrows]
  attribute float value;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGLength {

  // Length Unit Types
  const unsigned short SVG_LENGTHTYPE_UNKNOWN = 0;
  const unsigned short SVG_LENGTHTYPE_NUMBER = 1;
  const unsigned short SVG_LENGTHTYPE_PERCENTAGE = 2;
  const unsigned short SVG_LENGTHTYPE_EMS = 3;
  const unsigned short SVG_LENGTHTYPE_EXS = 4;
  const unsigned short SVG_LENGTHTYPE_PX = 5;
  const unsigned short SVG_LENGTHTYPE_CM = 6;
  const unsigned short SVG_LENGTHTYPE_MM = 7;
  const unsigned short SVG_LENGTHTYPE_IN = 8;
  const unsigned short SVG_LENGTHTYPE_PT = 9;
  const unsigned short SVG_LENGTHTYPE_PC = 10;

  readonly attribute unsigned short unitType;
  [Throws]
           attribute float value;
  [SetterThrows]
           attribute float valueInSpecifiedUnits;
  [SetterThrows]
           attribute DOMString valueAsString;

  [Throws]
  void newValueSpecifiedUnits(unsigned short unitType, float valueInSpecifiedUnits);
  [Throws]
  void convertToSpecifiedUnits(unsigned short unitType);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGAngle {

  // Angle Unit Types
  const unsigned short SVG_ANGLETYPE_UNKNOWN = 0;
  const unsigned short SVG_ANGLETYPE_UNSPECIFIED = 1;
  const unsigned short SVG_ANGLETYPE_DEG = 2;
  const unsigned short SVG_ANGLETYPE_RAD = 3;
  const unsigned short SVG_ANGLETYPE_GRAD = 4;

  readonly attribute unsigned short unitType;
           [SetterThrows]
           attribute float value;
           [SetterThrows]
           attribute float valueInSpecifiedUnits;
           [SetterThrows]
           attribute DOMString valueAsString;

  [Throws]
  void newValueSpecifiedUnits(unsigned short unitType, float valueInSpecifiedUnits);
  [Throws]
  void convertToSpecifiedUnits(unsigned short unitType);
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGMatrix {

  [SetterThrows]
  attribute float a;
  [SetterThrows]
  attribute float b;
  [SetterThrows]
  attribute float c;
  [SetterThrows]
  attribute float d;
  [SetterThrows]
  attribute float e;
  [SetterThrows]
  attribute float f;

  [NewObject]
  SVGMatrix multiply(SVGMatrix secondMatrix);
  [NewObject, Throws]
  SVGMatrix inverse();
  [NewObject]
  SVGMatrix translate(float x, float y);
  [NewObject]
  SVGMatrix scale(float scaleFactor);
  [NewObject]
  SVGMatrix scaleNonUniform(float scaleFactorX, float scaleFactorY);
  [NewObject]
  SVGMatrix rotate(float angle);
  [NewObject, Throws]
  SVGMatrix rotateFromVector(float x, float y);
  [NewObject]
  SVGMatrix flipX();
  [NewObject]
  SVGMatrix flipY();
  [NewObject, Throws]
  SVGMatrix skewX(float angle);
  [NewObject, Throws]
  SVGMatrix skewY(float angle);
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGRect {
  [SetterThrows]
  attribute float x;
  [SetterThrows]
  attribute float y;
  [SetterThrows]
  attribute float width;
  [SetterThrows]
  attribute float height;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGTransform {

  // Transform Types
  const unsigned short SVG_TRANSFORM_UNKNOWN = 0;
  const unsigned short SVG_TRANSFORM_MATRIX = 1;
  const unsigned short SVG_TRANSFORM_TRANSLATE = 2;
  const unsigned short SVG_TRANSFORM_SCALE = 3;
  const unsigned short SVG_TRANSFORM_ROTATE = 4;
  const unsigned short SVG_TRANSFORM_SKEWX = 5;
  const unsigned short SVG_TRANSFORM_SKEWY = 6;

  readonly attribute unsigned short type;
  [BinaryName="getMatrix"]
  readonly attribute SVGMatrix matrix;
  readonly attribute float angle;

  [Throws]
  void setMatrix(optional DOMMatrix2DInit matrix = {});
  [Throws]
  void setTranslate(float tx, float ty);
  [Throws]
  void setScale(float sx, float sy);
  [Throws]
  void setRotate(float angle, float cx, float cy);
  [Throws]
  void setSkewX(float angle);
  [Throws]
  void setSkewY(float angle);
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://drafts.fxtf.org/geometry/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker),
 Serializable]
interface DOMMatrixReadOnly {
    [Throws]
    constructor(optional (UTF8String or sequence<unrestricted double> or DOMMatrixReadOnly) init);

    [NewObject, Throws] static DOMMatrixReadOnly fromMatrix(optional DOMMatrixInit other = {});
// Mattias:    [NewObject, Throws] static DOMMatrixReadOnly fromFloat32Array(Float32Array array32);
// Mattias:    [NewObject, Throws] static DOMMatrixReadOnly fromFloat64Array(Float64Array array64);


    // These attributes are simple aliases for certain elements of the 4x4 matrix
    readonly attribute unrestricted double a;
    readonly attribute unrestricted double b;
    readonly attribute unrestricted double c;
    readonly attribute unrestricted double d;
    readonly attribute unrestricted double e;
    readonly attribute unrestricted double f;

    readonly attribute unrestricted double m11;
    readonly attribute unrestricted double m12;
    readonly attribute unrestricted double m13;
    readonly attribute unrestricted double m14;
    readonly attribute unrestricted double m21;
    readonly attribute unrestricted double m22;
    readonly attribute unrestricted double m23;
    readonly attribute unrestricted double m24;
    readonly attribute unrestricted double m31;
    readonly attribute unrestricted double m32;
    readonly attribute unrestricted double m33;
    readonly attribute unrestricted double m34;
    readonly attribute unrestricted double m41;
    readonly attribute unrestricted double m42;
    readonly attribute unrestricted double m43;
    readonly attribute unrestricted double m44;

    // Immutable transform methods
    DOMMatrix translate(optional unrestricted double tx = 0,
                        optional unrestricted double ty = 0,
                        optional unrestricted double tz = 0);
    [NewObject] DOMMatrix scale(optional unrestricted double scaleX = 1,
                                optional unrestricted double scaleY,
                                optional unrestricted double scaleZ = 1,
                                optional unrestricted double originX = 0,
                                optional unrestricted double originY = 0,
                                optional unrestricted double originZ = 0);
    [NewObject] DOMMatrix scaleNonUniform(optional unrestricted double scaleX = 1,
                                          optional unrestricted double scaleY = 1);
    DOMMatrix scale3d(optional unrestricted double scale = 1,
                      optional unrestricted double originX = 0,
                      optional unrestricted double originY = 0,
                      optional unrestricted double originZ = 0);
    [NewObject] DOMMatrix rotate(optional unrestricted double rotX = 0,
                                 optional unrestricted double rotY,
                                 optional unrestricted double rotZ);
    [NewObject] DOMMatrix rotateFromVector(optional unrestricted double x = 0,
                                           optional unrestricted double y = 0);
    [NewObject] DOMMatrix rotateAxisAngle(optional unrestricted double x = 0,
                                          optional unrestricted double y = 0,
                                          optional unrestricted double z = 0,
                                          optional unrestricted double angle = 0);
    DOMMatrix skewX(optional unrestricted double sx = 0);
    DOMMatrix skewY(optional unrestricted double sy = 0);
    [NewObject, Throws] DOMMatrix multiply(optional DOMMatrixInit other = {});
    DOMMatrix flipX();
    DOMMatrix flipY();
    DOMMatrix inverse();

    // Helper methods
    readonly attribute boolean is2D;
    readonly attribute boolean isIdentity;
    DOMPoint                   transformPoint(optional DOMPointInit point = {});
// Mattias:     [Throws] Float32Array      toFloat32Array();
// Mattias:     [Throws] Float64Array      toFloat64Array();
    [Exposed=Window, Throws]   stringifier;
    [Default] object           toJSON();
};

[Exposed=(Window,Worker),
 Serializable,
 LegacyWindowAlias=WebKitCSSMatrix]
interface DOMMatrix : DOMMatrixReadOnly {
    [Throws]
    constructor(optional (UTF8String or sequence<unrestricted double> or DOMMatrixReadOnly) init);

    [NewObject, Throws] static DOMMatrix fromMatrix(optional DOMMatrixInit other = {});
    [NewObject, Throws] static DOMMatrix fromFloat32Array(Float32Array array32);
    [NewObject, Throws] static DOMMatrix fromFloat64Array(Float64Array array64);


    // These attributes are simple aliases for certain elements of the 4x4 matrix
    inherit attribute unrestricted double a;
    inherit attribute unrestricted double b;
    inherit attribute unrestricted double c;
    inherit attribute unrestricted double d;
    inherit attribute unrestricted double e;
    inherit attribute unrestricted double f;

    inherit attribute unrestricted double m11;
    inherit attribute unrestricted double m12;
    inherit attribute unrestricted double m13;
    inherit attribute unrestricted double m14;
    inherit attribute unrestricted double m21;
    inherit attribute unrestricted double m22;
    inherit attribute unrestricted double m23;
    inherit attribute unrestricted double m24;
    inherit attribute unrestricted double m31;
    inherit attribute unrestricted double m32;
    inherit attribute unrestricted double m33;
    inherit attribute unrestricted double m34;
    inherit attribute unrestricted double m41;
    inherit attribute unrestricted double m42;
    inherit attribute unrestricted double m43;
    inherit attribute unrestricted double m44;

    // Mutable transform methods
    [Throws] DOMMatrix multiplySelf(optional DOMMatrixInit other = {});
    [Throws] DOMMatrix preMultiplySelf(optional DOMMatrixInit other = {});
    DOMMatrix translateSelf(optional unrestricted double tx = 0,
                            optional unrestricted double ty = 0,
                            optional unrestricted double tz = 0);
    DOMMatrix scaleSelf(optional unrestricted double scaleX = 1,
                        optional unrestricted double scaleY,
                        optional unrestricted double scaleZ = 1,
                        optional unrestricted double originX = 0,
                        optional unrestricted double originY = 0,
                        optional unrestricted double originZ = 0);
    DOMMatrix scale3dSelf(optional unrestricted double scale = 1,
                          optional unrestricted double originX = 0,
                          optional unrestricted double originY = 0,
                          optional unrestricted double originZ = 0);
    DOMMatrix rotateSelf(optional unrestricted double rotX = 0,
                         optional unrestricted double rotY,
                         optional unrestricted double rotZ);
    DOMMatrix rotateFromVectorSelf(optional unrestricted double x = 0,
                                   optional unrestricted double y = 0);
    DOMMatrix rotateAxisAngleSelf(optional unrestricted double x = 0,
                                  optional unrestricted double y = 0,
                                  optional unrestricted double z = 0,
                                  optional unrestricted double angle = 0);
    DOMMatrix skewXSelf(optional unrestricted double sx = 0);
    DOMMatrix skewYSelf(optional unrestricted double sy = 0);
    DOMMatrix invertSelf();
    [Exposed=Window, Throws] DOMMatrix setMatrixValue(UTF8String transformList);
};

dictionary DOMMatrix2DInit {
    unrestricted double a;
    unrestricted double b;
    unrestricted double c;
    unrestricted double d;
    unrestricted double e;
    unrestricted double f;
    unrestricted double m11;
    unrestricted double m12;
    unrestricted double m21;
    unrestricted double m22;
    unrestricted double m41;
    unrestricted double m42;
};

dictionary DOMMatrixInit : DOMMatrix2DInit {
    unrestricted double m13 = 0;
    unrestricted double m14 = 0;
    unrestricted double m23 = 0;
    unrestricted double m24 = 0;
    unrestricted double m31 = 0;
    unrestricted double m32 = 0;
    unrestricted double m33 = 1;
    unrestricted double m34 = 0;
    unrestricted double m43 = 0;
    unrestricted double m44 = 1;
    boolean is2D;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGAnimatedRect {
  readonly attribute SVGRect? baseVal;
  readonly attribute SVGRect? animVal;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGAnimatedPreserveAspectRatio {
  [Constant]
  readonly attribute SVGPreserveAspectRatio baseVal;
  [Constant]
  readonly attribute SVGPreserveAspectRatio animVal;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/csswg/cssom/
 * https://wicg.github.io/construct-stylesheets/
 */

enum CSSStyleSheetParsingMode {
  "author",
  "user",
  "agent"
};

dictionary CSSStyleSheetInit {
  (MediaList or UTF8String) media = "";
  boolean disabled = false;
  UTF8String baseURL;
};

[Exposed=Window]
interface CSSStyleSheet : StyleSheet {
  [Throws, Pref="layout.css.constructable-stylesheets.enabled"]
  constructor(optional CSSStyleSheetInit options = {});
  [Pure, BinaryName="DOMOwnerRule"]
  readonly attribute CSSRule? ownerRule;
  [Throws, NeedsSubjectPrincipal]
  readonly attribute CSSRuleList cssRules;
  [ChromeOnly, BinaryName="parsingModeDOM"]
  readonly attribute CSSStyleSheetParsingMode parsingMode;
  [Throws, NeedsSubjectPrincipal]
  unsigned long insertRule(UTF8String rule, optional unsigned long index = 0);
  [Throws, NeedsSubjectPrincipal]
  void deleteRule(unsigned long index);
  [NewObject, Pref="layout.css.constructable-stylesheets.enabled"]
  Promise<CSSStyleSheet> replace(UTF8String text);
  [Throws, Pref="layout.css.constructable-stylesheets.enabled"]
  void replaceSync(UTF8String text);

  // Non-standard WebKit things.
  [Throws, NeedsSubjectPrincipal, BinaryName="cssRules"]
  readonly attribute CSSRuleList rules;
  [Throws, NeedsSubjectPrincipal, BinaryName="deleteRule"]
  void removeRule(optional unsigned long index = 0);
  [Throws, NeedsSubjectPrincipal]
  long addRule(optional UTF8String selector = "undefined", optional UTF8String style = "undefined", optional unsigned long index);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://drafts.fxtf.org/geometry/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker),
 Serializable]
interface DOMPointReadOnly {
    constructor(optional unrestricted double x = 0,
                optional unrestricted double y = 0,
                optional unrestricted double z = 0,
                optional unrestricted double w = 1);

    [NewObject] static DOMPointReadOnly fromPoint(optional DOMPointInit other = {});

    readonly attribute unrestricted double x;
    readonly attribute unrestricted double y;
    readonly attribute unrestricted double z;
    readonly attribute unrestricted double w;

    [NewObject, Throws] DOMPoint matrixTransform(optional DOMMatrixInit matrix = {});

    [Default] object toJSON();
};

[Exposed=(Window,Worker),
 Serializable]
interface DOMPoint : DOMPointReadOnly {
    constructor(optional unrestricted double x = 0,
                optional unrestricted double y = 0,
                optional unrestricted double z = 0,
                optional unrestricted double w = 1);

    [NewObject] static DOMPoint fromPoint(optional DOMPointInit other = {});

    inherit attribute unrestricted double x;
    inherit attribute unrestricted double y;
    inherit attribute unrestricted double z;
    inherit attribute unrestricted double w;
};

dictionary DOMPointInit {
    unrestricted double x = 0;
    unrestricted double y = 0;
    unrestricted double z = 0;
    unrestricted double w = 1;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGPreserveAspectRatio {

  // Alignment Types
  const unsigned short SVG_PRESERVEASPECTRATIO_UNKNOWN = 0;
  const unsigned short SVG_PRESERVEASPECTRATIO_NONE = 1;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMINYMIN = 2;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMIDYMIN = 3;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMAXYMIN = 4;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMINYMID = 5;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMIDYMID = 6;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMAXYMID = 7;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMINYMAX = 8;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMIDYMAX = 9;
  const unsigned short SVG_PRESERVEASPECTRATIO_XMAXYMAX = 10;

  // Meet-or-slice Types
  const unsigned short SVG_MEETORSLICE_UNKNOWN = 0;
  const unsigned short SVG_MEETORSLICE_MEET = 1;
  const unsigned short SVG_MEETORSLICE_SLICE = 2;

  [SetterThrows]
  attribute unsigned short align;
  [SetterThrows]
  attribute unsigned short meetOrSlice;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=Window]
interface CSSRuleList {
  readonly attribute unsigned long length;
  getter CSSRule? item(unsigned long index);
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

dictionary SVGBoundingBoxOptions {
  boolean fill = true;
  boolean stroke = false;
  boolean markers = false;
  boolean clipped = false;
};

[Exposed=Window]
interface SVGGraphicsElement : SVGElement {
  [Pure]
  attribute boolean autofocus;

  readonly attribute SVGAnimatedTransformList transform;

  readonly attribute SVGElement? nearestViewportElement;
  readonly attribute SVGElement? farthestViewportElement;

  [NewObject]
  SVGRect getBBox(optional SVGBoundingBoxOptions aOptions = {});
  // Not implemented
  // SVGRect getStrokeBBox();
  SVGMatrix? getCTM();
  SVGMatrix? getScreenCTM();
  [Throws]
  SVGMatrix getTransformToElement(SVGGraphicsElement element);
};

SVGGraphicsElement includes SVGTests;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface mixin SVGTests {

  readonly attribute SVGStringList requiredExtensions;
  readonly attribute SVGStringList systemLanguage;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGAnimatedTransformList {
  [Constant]
  readonly attribute SVGTransformList baseVal;
  [Constant]
  readonly attribute SVGTransformList animVal;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGElement : Element {
           attribute DOMString id;

// Mattias:  [Constant]
//  readonly attribute SVGAnimatedString className;

  readonly attribute SVGSVGElement? ownerSVGElement;
  readonly attribute SVGElement? viewportElement;

  attribute DOMString nonce;
};

SVGElement includes GlobalEventHandlers;
SVGElement includes HTMLOrForeignElement;
SVGElement includes DocumentAndElementEventHandlers;
SVGElement includes ElementCSSInlineStyle;
SVGElement includes TouchEventHandlers;
SVGElement includes OnErrorEventHandlerForNodes;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://svgwg.org/svg2-draft/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGStringList {
  readonly attribute unsigned long length;
  readonly attribute unsigned long numberOfItems;

  void clear();
  [Throws]
  DOMString initialize(DOMString newItem);
  [Throws]
  DOMString getItem(unsigned long index);
  getter DOMString(unsigned long index);
  [Throws]
  DOMString insertItemBefore(DOMString newItem, unsigned long index);
  [Throws]
  DOMString replaceItem(DOMString newItem, unsigned long index);
  [Throws]
  DOMString removeItem(unsigned long index);
  [Throws]
  DOMString appendItem(DOMString newItem);
  //setter void (unsigned long index, DOMString newItem);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG11/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGTransformList {
  readonly attribute unsigned long numberOfItems;
  [Throws]
  void clear();
  [Throws]
  SVGTransform initialize(SVGTransform newItem);
  [Throws]
  getter SVGTransform getItem(unsigned long index);
  [Throws]
  SVGTransform insertItemBefore(SVGTransform newItem, unsigned long index);
  [Throws]
  SVGTransform replaceItem(SVGTransform newItem, unsigned long index);
  [Throws]
  SVGTransform removeItem(unsigned long index);
  [Throws]
  SVGTransform appendItem(SVGTransform newItem);
  [Throws]
  SVGTransform createSVGTransformFromMatrix(optional DOMMatrix2DInit matrix = {});
  [Throws]
  SVGTransform? consolidate();

  // Mozilla-specific stuff
  readonly attribute unsigned long length; // synonym for numberOfItems
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/SVG2/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface SVGAnimatedString {
           attribute DOMString baseVal;
  readonly attribute DOMString animVal;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/csswg/cssom/
 */

[Exposed=Window]
interface StyleSheet {
  [Constant]
  readonly attribute DOMString type;
  [Constant, Throws]
  readonly attribute DOMString? href;
  // Spec says "Node", but it can go null when the node gets a new
  // sheet.  That's also why it's not [Constant]
  [Pure]
  readonly attribute Node? ownerNode;
  [Pure]
  readonly attribute StyleSheet? parentStyleSheet;
  [Pure]
  readonly attribute DOMString? title;
// Mattias:  [Constant, PutForwards=mediaText]
//  readonly attribute MediaList media;
  [Pure]
  attribute boolean disabled;
  // The source map URL for this style sheet.  The source map URL can
  // be found in one of two ways.
  //
  // If a SourceMap or X-SourceMap response header is seen, this is
  // the value.  If both are seen, SourceMap is preferred.  Because
  // this relies on the HTTP response, it can change if checked before
  // the response is available -- which is why it is not [Constant].
  //
  // If the style sheet has the special "# sourceMappingURL=" comment,
  // then this is the URL specified there.
  //
  // If the source map URL is not found by either of these methods,
  // then this is an empty string.
  [ChromeOnly, Pure]
  readonly attribute DOMString sourceMapURL;
  // The source URL for this style sheet.  If the style sheet has the
  // special "# sourceURL=" comment, then this is the URL specified
  // there.  If no such comment is found, then this is the empty
  // string.
  [ChromeOnly, Pure]
  readonly attribute DOMString sourceURL;
  [ChromeOnly, Pure]
  readonly attribute Document? associatedDocument;
  [ChromeOnly, Pure, BinaryName="isConstructed"]
  readonly attribute boolean constructed;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-a-element
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

// http://www.whatwg.org/specs/web-apps/current-work/#the-a-element
[Exposed=Window]
interface HTMLAnchorElement : HTMLElement {
  [HTMLConstructor] constructor();

           [CEReactions, SetterThrows]
           attribute DOMString target;
           [CEReactions, SetterThrows]
           attribute DOMString download;
           [CEReactions, SetterThrows]
           attribute DOMString ping;
           [CEReactions, SetterThrows]
           attribute DOMString rel;
           [CEReactions, SetterThrows]
           attribute DOMString referrerPolicy;
           [PutForwards=value]
  readonly attribute DOMTokenList relList;
           [CEReactions, SetterThrows]
           attribute DOMString hreflang;
           [CEReactions, SetterThrows]
           attribute DOMString type;

           [CEReactions, Throws]
           attribute DOMString text;
};

HTMLAnchorElement includes HTMLHyperlinkElementUtils;

// http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
partial interface HTMLAnchorElement {
           [CEReactions, SetterThrows]
           attribute DOMString coords;
           [CEReactions, SetterThrows]
           attribute DOMString charset;
           [CEReactions, SetterThrows]
           attribute DOMString name;
           [CEReactions, SetterThrows]
           attribute DOMString rev;
           [CEReactions, SetterThrows]
           attribute DOMString shape;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-button-element
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

// http://www.whatwg.org/specs/web-apps/current-work/#the-button-element
[Exposed=Window]
interface HTMLButtonElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, SetterThrows, Pure]
           attribute boolean autofocus;
  [CEReactions, SetterThrows, Pure]
           attribute boolean disabled;
  [Pure]
  readonly attribute HTMLFormElement? form;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString formAction;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString formEnctype;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString formMethod;
  [CEReactions, SetterThrows, Pure]
           attribute boolean formNoValidate;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString formTarget;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString name;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString type;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString value;

  readonly attribute boolean willValidate;
  readonly attribute ValidityState validity;
  [Throws]
  readonly attribute DOMString validationMessage;
  boolean checkValidity();
  boolean reportValidity();
  void setCustomValidity(DOMString error);

  readonly attribute NodeList labels;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-canvas-element
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface nsISupports;

typedef (HTMLCanvasElement or OffscreenCanvas) CanvasSource;

[Exposed=Window]
interface HTMLCanvasElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, Pure, SetterThrows]
           attribute unsigned long width;
  [CEReactions, Pure, SetterThrows]
           attribute unsigned long height;

  [Throws]
  nsISupports? getContext(DOMString contextId, optional any contextOptions = null);

  [Throws, NeedsSubjectPrincipal]
  DOMString toDataURL(optional DOMString type = "",
                      optional any encoderOptions);
  [Throws, NeedsSubjectPrincipal]
  void toBlob(BlobCallback callback,
              optional DOMString type = "",
              optional any encoderOptions);
};

// Mozilla specific bits
partial interface HTMLCanvasElement {
  [Pure, SetterThrows]
           attribute boolean mozOpaque;
  // A Mozilla-only extension to get a canvas context backed by double-buffered
  // shared memory. Only privileged callers can call this.
  [ChromeOnly, Throws]
  nsISupports? MozGetIPCContext(DOMString contextId);

           attribute PrintCallback? mozPrintCallback;

// Mattias:   [Throws, Pref="canvas.capturestream.enabled", NeedsSubjectPrincipal]
//  CanvasCaptureMediaStream captureStream(optional double frameRate);
};

// For OffscreenCanvas
// Reference: https://wiki.whatwg.org/wiki/OffscreenCanvas
partial interface HTMLCanvasElement {
  [Func="CanvasUtils::IsOffscreenCanvasEnabled", Throws]
  OffscreenCanvas transferControlToOffscreen();
};

[ChromeOnly,
 Exposed=Window]
interface MozCanvasPrintState
{
  // A canvas rendering context.
  readonly attribute nsISupports context;

  // To be called when rendering to the context is done.
  void done();
};

callback PrintCallback = void(MozCanvasPrintState ctx);

callback BlobCallback = void(Blob? blob);
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window]
interface HTMLDivElement : HTMLElement {
  [HTMLConstructor] constructor();
};

partial interface HTMLDivElement {
  [CEReactions, SetterThrows]
           attribute DOMString align;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-embed-element
 * http://www.whatwg.org/specs/web-apps/current-work/#HTMLEmbedElement-partial
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

// http://www.whatwg.org/specs/web-apps/current-work/#the-embed-element
[NeedResolve,
 Exposed=Window]
interface HTMLEmbedElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, Pure, SetterThrows]
           attribute DOMString src;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString type;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString width;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString height;
};

// http://www.whatwg.org/specs/web-apps/current-work/#HTMLEmbedElement-partial
partial interface HTMLEmbedElement {
  [CEReactions, Pure, SetterThrows]
           attribute DOMString align;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString name;
};

partial interface HTMLEmbedElement {
  // GetSVGDocument
  [NeedsSubjectPrincipal]
  Document? getSVGDocument();
};

HTMLEmbedElement includes MozImageLoadingContent;
// Mattias: HTMLEmbedElement includes MozFrameLoaderOwner;
// Mattias: HTMLEmbedElement includes MozObjectLoadingContent;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-iframe-element
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * https://wicg.github.io/feature-policy/#policy
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window]
interface HTMLIFrameElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows, Pure]
           attribute DOMString src;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString srcdoc;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString name;
  [PutForwards=value] readonly attribute DOMTokenList sandbox;
           // attribute boolean seamless;
  [CEReactions, SetterThrows, Pure]
           attribute boolean allowFullscreen;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString width;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString height;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString referrerPolicy;
  [NeedsSubjectPrincipal]
  readonly attribute Document? contentDocument;
  readonly attribute WindowProxy? contentWindow;
};

// http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
partial interface HTMLIFrameElement {
  [CEReactions, SetterThrows, Pure]
           attribute DOMString align;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString scrolling;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString frameBorder;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString longDesc;

  [CEReactions, SetterThrows, Pure]
           attribute [LegacyNullToEmptyString] DOMString marginHeight;
  [CEReactions, SetterThrows, Pure]
           attribute [LegacyNullToEmptyString] DOMString marginWidth;
};

partial interface HTMLIFrameElement {
  // GetSVGDocument
  [NeedsSubjectPrincipal]
  Document? getSVGDocument();
};

// Mattias: HTMLIFrameElement includes MozFrameLoaderOwner;

// https://w3c.github.io/webappsec-feature-policy/#idl-index
partial interface HTMLIFrameElement {
// Mattias:   [SameObject, Pref="dom.security.featurePolicy.webidl.enabled"]
//  readonly attribute FeaturePolicy featurePolicy;

  [CEReactions, SetterThrows, Pure]
           attribute DOMString allow;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#htmlimageelement
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface imgINotificationObserver;
interface imgIRequest;
interface URI;
interface nsIStreamListener;

[LegacyFactoryFunction=Image(optional unsigned long width, optional unsigned long height),
 Exposed=Window]
interface HTMLImageElement : HTMLElement {
  [HTMLConstructor] constructor();

           [CEReactions, SetterThrows]
           attribute DOMString alt;
           [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString src;
           [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString srcset;
           [CEReactions, SetterThrows]
           attribute DOMString? crossOrigin;
           [CEReactions, SetterThrows]
           attribute DOMString useMap;
           [CEReactions, SetterThrows]
           attribute DOMString referrerPolicy;
           [CEReactions, SetterThrows]
           attribute boolean isMap;
           [CEReactions, SetterThrows]
           attribute unsigned long width;
           [CEReactions, SetterThrows]
           attribute unsigned long height;
           [CEReactions, SetterThrows]
           attribute DOMString decoding;
           [CEReactions, SetterThrows, Pref="dom.image-lazy-loading.enabled"]
           attribute DOMString loading;
  readonly attribute unsigned long naturalWidth;
  readonly attribute unsigned long naturalHeight;
  readonly attribute boolean complete;
           [NewObject]
           Promise<void> decode();
// Mattias:            [NewObject, ChromeOnly]
//           Promise<sequence<ImageText>> recognizeCurrentImageText();
};

// http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
partial interface HTMLImageElement {
           [CEReactions, SetterThrows]
           attribute DOMString name;
           [CEReactions, SetterThrows]
           attribute DOMString align;
           [CEReactions, SetterThrows]
           attribute unsigned long hspace;
           [CEReactions, SetterThrows]
           attribute unsigned long vspace;
           [CEReactions, SetterThrows]
           attribute DOMString longDesc;

  [CEReactions, SetterThrows] attribute [LegacyNullToEmptyString] DOMString border;
};

// [Update me: not in whatwg spec yet]
// http://picture.responsiveimages.org/#the-img-element
partial interface HTMLImageElement {
           [CEReactions, SetterThrows]
           attribute DOMString sizes;
  readonly attribute DOMString currentSrc;
};

// Mozilla extensions.
partial interface HTMLImageElement {
           [CEReactions, SetterThrows]
           attribute DOMString lowsrc;

  // These attributes are offsets from the closest view (to mimic
  // NS4's "offset-from-layer" behavior).
  readonly attribute long x;
  readonly attribute long y;
};

interface mixin MozImageLoadingContent {
  // Mirrored chrome-only nsIImageLoadingContent methods.  Please make sure
  // to update this list if nsIImageLoadingContent changes.
  [ChromeOnly]
  const long UNKNOWN_REQUEST = -1;
  [ChromeOnly]
  const long CURRENT_REQUEST = 0;
  [ChromeOnly]
  const long PENDING_REQUEST = 1;

  [ChromeOnly]
  attribute boolean loadingEnabled;
  /**
   * Same as addNativeObserver but intended for scripted observers or observers
   * from another or without a document.
   */
  [ChromeOnly]
  void addObserver(imgINotificationObserver aObserver);
  /**
   * Same as removeNativeObserver but intended for scripted observers or
   * observers from another or without a document.
   */
  [ChromeOnly]
  void removeObserver(imgINotificationObserver aObserver);
  [ChromeOnly,Throws]
  imgIRequest? getRequest(long aRequestType);
  [ChromeOnly,Throws]
  long getRequestType(imgIRequest aRequest);
  [ChromeOnly]
  readonly attribute URI? currentURI;
  // Gets the final URI of the current request, if available.
  // Otherwise, returns null.
  [ChromeOnly]
  readonly attribute URI? currentRequestFinalURI;
  /**
   * forceReload forces reloading of the image pointed to by currentURI
   *
   * @param aNotify request should notify
   * @throws NS_ERROR_NOT_AVAILABLE if there is no current URI to reload
   */
  [ChromeOnly,Throws]
  void forceReload(optional boolean aNotify = true);
};

HTMLImageElement includes MozImageLoadingContent;
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-input-element
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * https://wicg.github.io/entries-api/#idl-index
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

enum SelectionMode {
  "select",
  "start",
  "end",
  "preserve",
};

interface XULControllers;

[Exposed=Window]
interface HTMLInputElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, Pure, SetterThrows]
           attribute DOMString accept;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString alt;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString autocomplete;
  [CEReactions, Pure, SetterThrows]
           attribute boolean autofocus;
  [CEReactions, Pure, SetterThrows, Pref="dom.capture.enabled"]
           attribute DOMString capture;
  [CEReactions, Pure, SetterThrows]
           attribute boolean defaultChecked;
  [Pure]
           attribute boolean checked;
           // Bug 850337 - attribute DOMString dirName;
  [CEReactions, Pure, SetterThrows]
           attribute boolean disabled;
  readonly attribute HTMLFormElement? form;
  [Pure]
           attribute FileList? files;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString formAction;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString formEnctype;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString formMethod;
  [CEReactions, Pure, SetterThrows]
           attribute boolean formNoValidate;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString formTarget;
  [CEReactions, Pure, SetterThrows]
           attribute unsigned long height;
  [Pure]
           attribute boolean indeterminate;
  [Pure]
  readonly attribute HTMLElement? list;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString max;
  [CEReactions, Pure, SetterThrows]
           attribute long maxLength;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString min;
  [CEReactions, Pure, SetterThrows]
           attribute long minLength;
  [CEReactions, Pure, SetterThrows]
           attribute boolean multiple;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString name;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString pattern;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString placeholder;
  [CEReactions, Pure, SetterThrows]
           attribute boolean readOnly;
  [CEReactions, Pure, SetterThrows]
           attribute boolean required;
  [CEReactions, Pure, SetterThrows]
           attribute unsigned long size;
  [CEReactions, Pure, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString src;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString step;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString type;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString defaultValue;
  [CEReactions, Pure, SetterThrows, NeedsCallerType]
           attribute [LegacyNullToEmptyString] DOMString value;
  [Throws]
           attribute object? valueAsDate;
  [Pure, SetterThrows]
           attribute unrestricted double valueAsNumber;
  [CEReactions, SetterThrows]
           attribute unsigned long width;

  [Throws]
  void stepUp(optional long n = 1);
  [Throws]
  void stepDown(optional long n = 1);

  [Pure]
  readonly attribute boolean willValidate;
  [Pure]
  readonly attribute ValidityState validity;
  [Throws]
  readonly attribute DOMString validationMessage;
  boolean checkValidity();
  boolean reportValidity();
  void setCustomValidity(DOMString error);

  readonly attribute NodeList? labels;

  void select();

  [Throws]
           attribute unsigned long? selectionStart;
  [Throws]
           attribute unsigned long? selectionEnd;
  [Throws]
           attribute DOMString? selectionDirection;
  [Throws]
  void setRangeText(DOMString replacement);
  [Throws]
  void setRangeText(DOMString replacement, unsigned long start,
    unsigned long end, optional SelectionMode selectionMode = "preserve");
  [Throws]
  void setSelectionRange(unsigned long start, unsigned long end, optional DOMString direction);

  [Throws, Pref="dom.input.showPicker"]
  void showPicker();

  // also has obsolete members
};

partial interface HTMLInputElement {
  [CEReactions, Pure, SetterThrows]
           attribute DOMString align;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString useMap;
};

// Mozilla extensions

partial interface HTMLInputElement {
  [GetterThrows, ChromeOnly]
  readonly attribute XULControllers?       controllers;
  // Binaryname because we have a FragmentOrElement function named "TextLength()".
  [NeedsCallerType, BinaryName="inputTextLength"]
  readonly attribute long                  textLength;

  [Throws, ChromeOnly]
  sequence<DOMString> mozGetFileNameArray();

// Mattias:   [ChromeOnly, Throws]
//  void mozSetFileNameArray(sequence<DOMString> fileNames);

// Mattias:   [ChromeOnly]
//  void mozSetFileArray(sequence<File> files);

  // This method is meant to use for testing only.
  [ChromeOnly, Throws]
  void mozSetDirectory(DOMString directoryPath);

  // This method is meant to use for testing only.
// Mattias:   [ChromeOnly]
//  void mozSetDndFilesAndDirectories(sequence<(File or Directory)> list);

  // This method is meant to use for testing only.
// Mattias:   [ChromeOnly, NewObject]
//  Promise<sequence<(File or Directory)>> getFilesAndDirectories();

  boolean mozIsTextField(boolean aExcludePassword);

  [ChromeOnly]
  readonly attribute boolean hasBeenTypePassword;

  [ChromeOnly]
  attribute DOMString previewValue;

// Mattias:   [ChromeOnly]
  // This function will return null if @autocomplete is not defined for the
  // current @type
//  AutocompleteInfo? getAutocompleteInfo();

  [ChromeOnly]
  // The reveal password state for a type=password control.
  attribute boolean revealPassword;
};

interface mixin MozEditableElement {
  // Returns an nsIEditor instance which is associated with the element.
  // If the element can be associated with an editor but not yet created,
  // this creates new one automatically.
  [Pure, ChromeOnly, BinaryName="editorForBindings"]
  readonly attribute nsIEditor? editor;

  // Returns true if an nsIEditor instance has already been associated with
  // the element.
  [Pure, ChromeOnly]
  readonly attribute boolean hasEditor;

  // This is set to true if "input" event should be fired with InputEvent on
  // the element.  Otherwise, i.e., if "input" event should be fired with
  // Event, set to false.
  [ChromeOnly]
  readonly attribute boolean isInputEventTarget;

  // This is similar to set .value on nsIDOMInput/TextAreaElements, but handling
  // of the value change is closer to the normal user input, so 'change' event
  // for example will be dispatched when focusing out the element.
  [Func="IsChromeOrUAWidget", NeedsSubjectPrincipal]
  void setUserInput(DOMString input);
};

HTMLInputElement includes MozEditableElement;

HTMLInputElement includes MozImageLoadingContent;

// https://wicg.github.io/entries-api/#idl-index
partial interface HTMLInputElement {
  [Pref="dom.webkitBlink.filesystem.enabled", Frozen, Cached, Pure]
  readonly attribute sequence<FileSystemEntry> webkitEntries;

  [Pref="dom.webkitBlink.dirPicker.enabled", BinaryName="WebkitDirectoryAttr", SetterThrows]
          attribute boolean webkitdirectory;
};

dictionary DateTimeValue {
  long hour;
  long minute;
  long year;
  long month;
  long day;
};

partial interface HTMLInputElement {
// Mattias:   [ChromeOnly]
//  DateTimeValue getDateTimeInputBoxValue();

  [ChromeOnly]
  readonly attribute Element? dateTimeBoxElement;

  [ChromeOnly, BinaryName="getMinimumAsDouble"]
  double getMinimum();

  [ChromeOnly, BinaryName="getMaximumAsDouble"]
  double getMaximum();

  [Func="IsChromeOrUAWidget"]
  void openDateTimePicker(optional DateTimeValue initialValue = {});

  [Func="IsChromeOrUAWidget"]
  void updateDateTimePicker(optional DateTimeValue value = {});

  [Func="IsChromeOrUAWidget"]
  void closeDateTimePicker();

  [Func="IsChromeOrUAWidget"]
  void setFocusState(boolean aIsFocused);

  [Func="IsChromeOrUAWidget"]
  void updateValidityState();

  [Func="IsChromeOrUAWidget", BinaryName="getStepAsDouble"]
  double getStep();

  [Func="IsChromeOrUAWidget", BinaryName="getStepBaseAsDouble"]
  double getStepBase();
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window]
interface HTMLLabelElement : HTMLElement {
  [HTMLConstructor] constructor();

  readonly attribute HTMLFormElement? form;
  [CEReactions]
           attribute DOMString htmlFor;
  readonly attribute HTMLElement? control;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-link-element
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

// http://www.whatwg.org/specs/web-apps/current-work/#the-link-element
[Exposed=Window]
interface HTMLLinkElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, SetterThrows, Pure]
           attribute boolean disabled;
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows, Pure]
           attribute DOMString href;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString? crossOrigin;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString rel;
  [PutForwards=value]
  readonly attribute DOMTokenList relList;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString media;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString hreflang;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString type;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString referrerPolicy;
  [PutForwards=value] readonly attribute DOMTokenList sizes;
  [CEReactions, SetterThrows, Pure]
           attribute USVString imageSrcset;
  [CEReactions, SetterThrows, Pure]
           attribute USVString imageSizes;
};
HTMLLinkElement includes LinkStyle;

// http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
partial interface HTMLLinkElement {
  [CEReactions, SetterThrows, Pure]
           attribute DOMString charset;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString rev;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString target;
};

// https://w3c.github.io/webappsec/specs/subresourceintegrity/#htmllinkelement-1
partial interface HTMLLinkElement {
  [CEReactions, SetterThrows]
  attribute DOMString integrity;
};

//https://w3c.github.io/preload/
partial interface HTMLLinkElement {
  [SetterThrows, Pure]
           attribute DOMString as;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-option-element
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[LegacyFactoryFunction=Option(optional DOMString text = "", optional DOMString value, optional boolean defaultSelected = false, optional boolean selected = false),
 Exposed=Window]
interface HTMLOptionElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, SetterThrows]
  attribute boolean disabled;
  readonly attribute HTMLFormElement? form;
  [CEReactions, SetterThrows]
  attribute DOMString label;
  [CEReactions, SetterThrows]
  attribute boolean defaultSelected;
  attribute boolean selected;
  [CEReactions, SetterThrows]
  attribute DOMString value;

  [CEReactions, SetterThrows]
  attribute DOMString text;
  readonly attribute long index;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-progress-element
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window]
interface HTMLProgressElement : HTMLElement {
  [HTMLConstructor] constructor();

           [CEReactions, SetterThrows]
           attribute double value;
           [CEReactions, SetterThrows]
           attribute double max;
  readonly attribute double position;
  readonly attribute NodeList labels;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-textarea-element
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface nsIEditor;
interface XULControllers;

[Exposed=Window]
interface HTMLTextAreaElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, SetterThrows, Pure]
           attribute DOMString autocomplete;
  [CEReactions, SetterThrows, Pure]
           attribute boolean autofocus;
  [CEReactions, SetterThrows, Pure]
           attribute unsigned long cols;
           // attribute DOMString dirName;
  [CEReactions, SetterThrows, Pure]
           attribute boolean disabled;
  [Pure]
  readonly attribute HTMLFormElement? form;
           // attribute DOMString inputMode;
  [CEReactions, SetterThrows, Pure]
           attribute long maxLength;
  [CEReactions, SetterThrows, Pure]
           attribute long minLength;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString name;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString placeholder;
  [CEReactions, SetterThrows, Pure]
           attribute boolean readOnly;
  [CEReactions, SetterThrows, Pure]
           attribute boolean required;
  [CEReactions, SetterThrows, Pure]
           attribute unsigned long rows;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString wrap;

  [Constant]
  readonly attribute DOMString type;
  [CEReactions, Throws, Pure]
           attribute DOMString defaultValue;
  [CEReactions, SetterThrows] attribute [LegacyNullToEmptyString] DOMString value;
  [BinaryName="getTextLength"]
  readonly attribute unsigned long textLength;

  readonly attribute boolean willValidate;
  readonly attribute ValidityState validity;
  [Throws]
  readonly attribute DOMString validationMessage;
  boolean checkValidity();
  boolean reportValidity();
  void setCustomValidity(DOMString error);

  readonly attribute NodeList labels;

  void select();
  [Throws]
           attribute unsigned long? selectionStart;
  [Throws]
           attribute unsigned long? selectionEnd;
  [Throws]
           attribute DOMString? selectionDirection;
  [Throws]
  void setRangeText(DOMString replacement);
  [Throws]
  void setRangeText(DOMString replacement, unsigned long start,
    unsigned long end, optional SelectionMode selectionMode = "preserve");
  [Throws]
  void setSelectionRange(unsigned long start, unsigned long end, optional DOMString direction);
};

partial interface HTMLTextAreaElement {
  // Chrome-only Mozilla extensions

  [Throws, ChromeOnly]
  readonly attribute XULControllers controllers;
};

HTMLTextAreaElement includes MozEditableElement;

partial interface HTMLTextAreaElement {
  [ChromeOnly]
  attribute DOMString previewValue;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/semantics.html#htmlhyperlinkelementutils
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface mixin HTMLHyperlinkElementUtils {
  [CEReactions, SetterThrows]
  stringifier attribute USVString href;

  readonly attribute USVString origin;
  [CEReactions]
           attribute USVString protocol;
  [CEReactions]
           attribute USVString username;
  [CEReactions]
           attribute USVString password;
  [CEReactions]
           attribute USVString host;
  [CEReactions]
           attribute USVString hostname;
  [CEReactions]
           attribute USVString port;
  [CEReactions]
           attribute USVString pathname;
  [CEReactions]
           attribute USVString search;
  [CEReactions]
           attribute USVString hash;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/csswg/cssom/#the-linkstyle-interface
 */

interface mixin LinkStyle {
  [BinaryName="sheetForBindings"] readonly attribute StyleSheet? sheet;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information on this interface, please see
 * https://html.spec.whatwg.org/#the-offscreencanvas-interface
 */

typedef (OffscreenCanvasRenderingContext2D or ImageBitmapRenderingContext or WebGLRenderingContext or WebGL2RenderingContext or GPUCanvasContext) OffscreenRenderingContext;

dictionary ImageEncodeOptions {
  DOMString type = "image/png";
  unrestricted double quality;
};

enum OffscreenRenderingContextId { "2d", "bitmaprenderer", "webgl", "webgl2", "webgpu" };

[Exposed=(Window,Worker),
 Func="CanvasUtils::IsOffscreenCanvasEnabled"]
interface OffscreenCanvas : EventTarget {
  constructor([EnforceRange] unsigned long width, [EnforceRange] unsigned long height);

  [Pure, SetterThrows]
  attribute [EnforceRange] unsigned long width;
  [Pure, SetterThrows]
  attribute [EnforceRange] unsigned long height;

  [Throws]
  OffscreenRenderingContext? getContext(OffscreenRenderingContextId contextId,
                                        optional any contextOptions = null);

  [Throws]
  ImageBitmap transferToImageBitmap();
  [NewObject]
  Promise<Blob> convertToBlob(optional ImageEncodeOptions options = {});

  attribute EventHandler oncontextlost;
  attribute EventHandler oncontextrestored;

  // Deprecated by convertToBlob
// Mattias:   [Deprecated="OffscreenCanvasToBlob", NewObject]
//  Promise<Blob> toBlob(optional DOMString type = "",
//                       optional any encoderOptions);
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/FileAPI/#blob
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

typedef (BufferSource or Blob or USVString) BlobPart;

[Exposed=(Window,Worker)]
interface Blob {
  [Throws]
  constructor(optional sequence<BlobPart> blobParts,
              optional BlobPropertyBag options = {});

  [GetterThrows]
  readonly attribute unsigned long long size;

  readonly attribute DOMString type;

  //slice Blob into byte-ranged chunks

  [Throws]
  Blob slice(optional [Clamp] long long start,
             optional [Clamp] long long end,
             optional DOMString contentType);

  // read from the Blob.
// Mattias:   [NewObject, Throws] ReadableStream stream();
  [NewObject] Promise<USVString> text();
  [NewObject] Promise<ArrayBuffer> arrayBuffer();
};

enum EndingType { "transparent", "native" };

dictionary BlobPropertyBag {
  DOMString type = "";
  EndingType endings = "transparent";
};

partial interface Blob {
  // This returns the type of BlobImpl used for this Blob.
  [ChromeOnly]
  readonly attribute DOMString blobImplType;
};

/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/2006/webapi/FileAPI/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker)]
interface FileList {
  getter File? item(unsigned long index);
  readonly attribute unsigned long length;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/FileAPI/#file
 * https://wicg.github.io/entries-api
 */

interface nsIFile;

[Exposed=(Window,Worker)]
interface File : Blob {
  [Throws]
  constructor(sequence<BlobPart> fileBits,
              USVString fileName, optional FilePropertyBag options = {});

  readonly attribute DOMString name;

  [GetterThrows]
  readonly attribute long long lastModified;
};

dictionary FilePropertyBag : BlobPropertyBag {
  long long lastModified;
};

dictionary ChromeFilePropertyBag : FilePropertyBag {
  DOMString name = "";
  boolean existenceCheck = true;
};

// https://wicg.github.io/entries-api
partial interface File {
  [BinaryName="relativePath", Pref="dom.webkitBlink.dirPicker.enabled"]
  readonly attribute USVString webkitRelativePath;
};

// Mozilla extensions
partial interface File {
  [GetterThrows, ChromeOnly, NeedsCallerType]
  readonly attribute DOMString mozFullPath;
};

// Mozilla extensions
// These 2 methods can be used only in these conditions:
// - the main-thread
// - parent process OR file process OR, only for testing, with pref
//   `dom.file.createInChild' set to true.
[Exposed=(Window)]
partial interface File {
// Mattias:   [ChromeOnly, NewObject, NeedsCallerType]
//  static Promise<File> createFromNsIFile(nsIFile file,
//                                         optional ChromeFilePropertyBag options = {});

// Mattias:   [ChromeOnly, NewObject, NeedsCallerType]
//  static Promise<File> createFromFileName(USVString fileName,
//                                          optional ChromeFilePropertyBag options = {});
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/**
 * This dictionary is used for the input, textarea and select element's
 * getAutocompleteInfo method.
 */

dictionary AutocompleteInfo {
  DOMString section = "";
  DOMString addressType = "";
  DOMString contactType = "";
  DOMString fieldName = "";
  boolean canAutomaticallyPersist = true;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://wicg.github.io/entries-api/#idl-index
 */

dictionary FileSystemFlags {
    boolean create = false;
    boolean exclusive = false;
};

callback FileSystemEntryCallback = void (FileSystemEntry entry);

// Mattias: callback ErrorCallback = void (DOMException err);

[Exposed=Window]
interface FileSystem {
    readonly    attribute USVString name;
    readonly    attribute FileSystemDirectoryEntry root;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://wicg.github.io/entries-api/#idl-index
 */

[Exposed=Window]
interface FileSystemEntry {
    readonly attribute boolean isFile;
    readonly attribute boolean isDirectory;

    [GetterThrows]
    readonly attribute USVString name;

    [GetterThrows]
    readonly attribute USVString fullPath;

    readonly attribute FileSystem filesystem;

// Mattias:     void getParent(optional FileSystemEntryCallback successCallback,
//                   optional ErrorCallback errorCallback);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/webappapis.html#images
 *
 * The origin of the extended IDL file is
 * http://w3c.github.io/mediacapture-worker/#imagebitmap-extensions
 */

typedef (CanvasImageSource or
         Blob or
         CanvasRenderingContext2D or // This is out of spec.
         ImageData) ImageBitmapSource;

[Exposed=(Window,Worker)]
interface ImageBitmap {
  [Constant]
  readonly attribute unsigned long width;
  [Constant]
  readonly attribute unsigned long height;
};

// It's crucial that there be a way to explicitly dispose of ImageBitmaps
// since they refer to potentially large graphics resources. Some uses
// of this API proposal will result in repeated allocations of ImageBitmaps,
// and garbage collection will not reliably reclaim them quickly enough.
// Here we reuse close(), which also exists on another Transferable type,
// MessagePort. Potentially, all Transferable types should inherit from a
// new interface type "Closeable".
partial interface ImageBitmap {
  // Dispose of all graphical resources associated with this ImageBitmap.
  void close();
};

// ImageBitmap-extensions
// Bug 1141979 - [FoxEye] Extend ImageBitmap with interfaces to access its
// underlying image data

/*
 * An image or a video frame is conceptually a two-dimensional array of data and
 * each element in the array is called a pixel. The pixels are usually stored in
 * a one-dimensional array and could be arranged in a variety of image formats.
 * Developers need to know how the pixels are formatted so that they are able to
 * process them.
 *
 * The image format describes how pixels in an image are arranged. A single
 * pixel has at least one, but usually multiple pixel values. The range of a
 * pixel value varies, which means different image formats use different data
 * types to store a single pixel value.
 *
 * The most frequently used data type is 8-bit unsigned integer whose range is
 * from 0 to 255, others could be 16-bit integer or 32-bit floating points and
 * so forth. The number of pixel values of a single pixel is called the number
 * of channels of the image format. Multiple pixel values of a pixel are used
 * together to describe the captured property which could be color or depth
 * information. For example, if the data is a color image in RGB color space,
 * then it is a three-channel image format and a pixel is described by R, G and
 * B three pixel values with range from 0 to 255. As another example, if the
 * data is a gray image, then it is a single-channel image format with 8-bit
 * unsigned integer data type and the pixel value describes the gray scale. For
 * depth data, it is a single channel image format too, but the data type is
 * 16-bit unsigned integer and the pixel value is the depth level.
 *
 * For those image formats whose pixels contain multiple pixel values, the pixel
 * values might be arranged in one of the following ways:
 * 1) Planar pixel layout:
 *    each channel has its pixel values stored consecutively in separated
 *    buffers (a.k.a. planes) and then all channel buffers are stored
 *    consecutively in memory.
 *    (Ex: RRRRRR......GGGGGG......BBBBBB......)
 * 2) Interleaving pixel layout:
 *    each pixel has its pixel values from all channels stored together and
 *    interleaves all channels.
 *    (Ex: RGBRGBRGBRGBRGB......)
 */


/*
 * The ImageBitmap extensions use this enumeration to negotiate the image format
 * while 1) accessing the underlying data of an ImageBitmap and
 *       2) creating a new ImageBitmap.
 *
 * For each format in this enumeration, we use a 2x2 small image (4 pixels) as
 * example to illustrate the pixel layout.
 *
 * 2x2 image:   +--------+--------+
 *              | pixel1 | pixel2 |
 *              +--------+--------+
 *              | pixel3 | pixel4 |
 *              +--------+--------+
 *
 */
enum ImageBitmapFormat {
  /*
   * Channel order: R, G, B, A
   * Channel size: full rgba-chennels
   * Pixel layout: interleaving rgba-channels
   * Pixel layout illustration:
   *   [Plane1]: R1 G1 B1 A1 R2 G2 B2 A2 R3 G3 B3 A3 R4 G4 B4 A4
   * Data type: 8-bit unsigned integer
   */
  "RGBA32",

  /*
   * Channel order: B, G, R, A
   * Channel size: full bgra-channels
   * Pixel layout: interleaving bgra-channels
   * Pixel layout illustration:
   *   [Plane1]: B1 G1 R1 A1 B2 G2 R2 A2 B3 G3 R3 A3 B4 G4 R4 A4
   * Data type: 8-bit unsigned integer
   */
  "BGRA32",

  /*
   * Channel order: R, G, B
   * Channel size: full rgb-channels
   * Pixel layout: interleaving rgb-channels
   * Pixel layout illustration:
   *   [Plane1]: R1 G1 B1 R2 G2 B2 R3 G3 B3 R4 G4 B4
   * Data type: 8-bit unsigned integer
   */
  "RGB24",

  /*
   * Channel order: B, G, R
   * Channel size: full bgr-channels
   * Pixel layout: interleaving bgr-channels
   * Pixel layout illustration:
   *   [Plane1]: B1 G1 R1 B2 G2 R2 B3 G3 R3 B4 G4 R4
   * Data type: 8-bit unsigned integer
   */
  "BGR24",

  /*
   * Channel order: GRAY
   * Channel size: full gray-channel
   * Pixel layout: planar gray-channel
   * Pixel layout illustration:
   *   [Plane1]: GRAY1 GRAY2 GRAY3 GRAY4
   * Data type: 8-bit unsigned integer
   */
  "GRAY8",

  /*
   * Channel order: Y, U, V
   * Channel size: full yuv-channels
   * Pixel layout: planar yuv-channels
   * Pixel layout illustration:
   *   [Plane1]: Y1 Y2 Y3 Y4
   *   [Plane2]: U1 U2 U3 U4
   *   [Plane3]: V1 V2 V3 V4
   * Data type: 8-bit unsigned integer
   */
  "YUV444P",

  /*
   * Channel order: Y, U, V
   * Channel size: full y-channel, half uv-channels
   * Pixel layout: planar yuv-channels
   * Pixel layout illustration:
   *   [Plane1]: Y1 Y2 Y3 Y4
   *   [Plane2]: U1 U3
   *   [Plane3]: V1 V3
   * Data type: 8-bit unsigned integer
   */
  "YUV422P",

  /*
   * Channel order: Y, U, V
   * Channel size: full y-channel, quarter uv-channels
   * Pixel layout: planar yuv-channels
   * Pixel layout illustration:
   *   [Plane1]: Y1 Y2 Y3 Y4
   *   [Plane2]: U1
   *   [Plane3]: V1
   * Data type: 8-bit unsigned integer
   */
  "YUV420P",

  /*
   * Channel order: Y, U, V
   * Channel size: full y-channel, quarter uv-channels
   * Pixel layout: planar y-channel, interleaving uv-channels
   * Pixel layout illustration:
   *   [Plane1]: Y1 Y2 Y3 Y4
   *   [Plane2]: U1 V1
   * Data type: 8-bit unsigned integer
   */
  "YUV420SP_NV12",

  /*
   * Channel order: Y, V, U
   * Channel size: full y-channel, quarter vu-channels
   * Pixel layout: planar y-channel, interleaving vu-channels
   * Pixel layout illustration:
   *   [Plane1]: Y1 Y2 Y3 Y4
   *   [Plane2]: V1 U1
   * Data type: 8-bit unsigned integer
   */
  "YUV420SP_NV21",

  /*
   * Channel order: H, S, V
   * Channel size: full hsv-channels
   * Pixel layout: interleaving hsv-channels
   * Pixel layout illustration:
   *   [Plane1]: H1 S1 V1 H2 S2 V2 H3 S3 V3
   * Data type: 32-bit floating point value
   */
  "HSV",

  /*
   * Channel order: l, a, b
   * Channel size: full lab-channels
   * Pixel layout: interleaving lab-channels
   * Pixel layout illustration:
   *   [Plane1]: l1 a1 b1 l2 a2 b2 l3 a3 b3
   * Data type: 32-bit floating point value
   */
  "Lab",

  /*
   * Channel order: DEPTH
   * Channel size: full depth-channel
   * Pixel layout: planar depth-channel
   * Pixel layout illustration:
   *   [Plane1]: DEPTH1 DEPTH2 DEPTH3 DEPTH4
   * Data type: 16-bit unsigned integer
   */
  "DEPTH",
};

enum ChannelPixelLayoutDataType {
  "uint8",
  "int8",
  "uint16",
  "int16",
  "uint32",
  "int32",
  "float32",
  "float64"
};

/*
 * Two concepts, ImagePixelLayout and ChannelPixelLayout, together generalize
 * the variety of pixel layouts among image formats.
 *
 * The ChannelPixelLayout represents the pixel layout of a single channel in a
 * certain image format and the ImagePixelLayout is just the collection of
 * ChannelPixelLayouts. So, the ChannelPixelLayout is defined as a dictionary
 * type with properties to describe the layout and the ImagePixelLayout is just
 * an alias name to a sequence of ChannelPixelLayout objects.
 *
 * Since an image format is composed of at least one channel, an
 * ImagePixelLayout object contains at least one ChannelPixelLayout object.
 *
 * Although an image or a video frame is a two-dimensional structure, its data
 * is usually stored in a one-dimensional array in the row-major way and the
 * ChannelPixelLayout objects use the following properties to describe the
 * layout of pixel values in the buffer.
 *
 * 1) offset:
 *    denotes the beginning position of the channel's data relative to the
 *    beginning position of the one-dimensional array.
 * 2) width & height:
 *    denote the width and height of the channel respectively. Each channel in
 *    an image format may have different height and width.
 * 3) data type:
 *    denotes the format used to store one single pixel value.
 * 4) stride:
 *    the number of bytes between the beginning two consecutive rows in memory.
 *    (The total bytes of each row plus the padding bytes of each raw.)
 * 5) skip value:
 *    the value is zero for the planar pixel layout, and a positive integer for
 *    the interleaving pixel layout. (Describes how many bytes there are between
 *    two adjacent pixel values in this channel.)
 */

/*
 * Example1: RGBA image, width = 620, height = 480, stride = 2560
 *
 * chanel_r: offset = 0, width = 620, height = 480, data type = uint8, stride = 2560, skip = 3
 * chanel_g: offset = 1, width = 620, height = 480, data type = uint8, stride = 2560, skip = 3
 * chanel_b: offset = 2, width = 620, height = 480, data type = uint8, stride = 2560, skip = 3
 * chanel_a: offset = 3, width = 620, height = 480, data type = uint8, stride = 2560, skip = 3
 *
 *         <---------------------------- stride ---------------------------->
 *         <---------------------- width x 4 ---------------------->
 * [index] 01234   8   12  16  20  24  28                           2479    2559
 *         |||||---|---|---|---|---|---|----------------------------|-------|
 * [data]  RGBARGBARGBARGBARGBAR___R___R...                         A%%%%%%%%
 * [data]  RGBARGBARGBARGBARGBAR___R___R...                         A%%%%%%%%
 * [data]  RGBARGBARGBARGBARGBAR___R___R...                         A%%%%%%%%
 *              ^^^
 *              r-skip
 */

/*
 * Example2: YUV420P image, width = 620, height = 480, stride = 640
 *
 * chanel_y: offset = 0, width = 620, height = 480, stride = 640, skip = 0
 * chanel_u: offset = 307200, width = 310, height = 240, data type = uint8, stride = 320, skip = 0
 * chanel_v: offset = 384000, width = 310, height = 240, data type = uint8, stride = 320, skip = 0
 *
 *         <--------------------------- y-stride --------------------------->
 *         <----------------------- y-width ----------------------->
 * [index] 012345                                                  619      639
 *         ||||||--------------------------------------------------|--------|
 * [data]  YYYYYYYYYYYYYYYYYYYYYYYYYYYYY...                        Y%%%%%%%%%
 * [data]  YYYYYYYYYYYYYYYYYYYYYYYYYYYYY...                        Y%%%%%%%%%
 * [data]  YYYYYYYYYYYYYYYYYYYYYYYYYYYYY...                        Y%%%%%%%%%
 * [data]  ......
 *         <-------- u-stride ---------->
 *         <----- u-width ----->
 * [index] 307200              307509   307519
 *         |-------------------|--------|
 * [data]  UUUUUUUUUU...       U%%%%%%%%%
 * [data]  UUUUUUUUUU...       U%%%%%%%%%
 * [data]  UUUUUUUUUU...       U%%%%%%%%%
 * [data]  ......
 *         <-------- v-stride ---------->
 *         <- --- v-width ----->
 * [index] 384000              384309   384319
 *         |-------------------|--------|
 * [data]  VVVVVVVVVV...       V%%%%%%%%%
 * [data]  VVVVVVVVVV...       V%%%%%%%%%
 * [data]  VVVVVVVVVV...       V%%%%%%%%%
 * [data]  ......
 */

/*
 * Example3: YUV420SP_NV12 image, width = 620, height = 480, stride = 640
 *
 * chanel_y: offset = 0, width = 620, height = 480, stride = 640, skip = 0
 * chanel_u: offset = 307200, width = 310, height = 240, data type = uint8, stride = 640, skip = 1
 * chanel_v: offset = 307201, width = 310, height = 240, data type = uint8, stride = 640, skip = 1
 *
 *         <--------------------------- y-stride -------------------------->
 *         <----------------------- y-width ---------------------->
 * [index] 012345                                                 619      639
 *         ||||||-------------------------------------------------|--------|
 * [data]  YYYYYYYYYYYYYYYYYYYYYYYYYYYYY...                       Y%%%%%%%%%
 * [data]  YYYYYYYYYYYYYYYYYYYYYYYYYYYYY...                       Y%%%%%%%%%
 * [data]  YYYYYYYYYYYYYYYYYYYYYYYYYYYYY...                       Y%%%%%%%%%
 * [data]  ......
 *         <--------------------- u-stride / v-stride -------------------->
 *         <------------------ u-width + v-width ----------------->
 * [index] 307200(u-offset)                                       307819  307839
 *         |------------------------------------------------------|-------|
 * [index] |307201(v-offset)                                      |307820 |
 *         ||-----------------------------------------------------||------|
 * [data]  UVUVUVUVUVUVUVUVUVUVUVUVUVUVUV...                      UV%%%%%%%
 * [data]  UVUVUVUVUVUVUVUVUVUVUVUVUVUVUV...                      UV%%%%%%%
 * [data]  UVUVUVUVUVUVUVUVUVUVUVUVUVUVUV...                      UV%%%%%%%
 *          ^            ^
 *         u-skip        v-skip
 */

/*
 * Example4: DEPTH image, width = 640, height = 480, stride = 1280
 *
 * chanel_d: offset = 0, width = 640, height = 480, data type = uint16, stride = 1280, skip = 0
 *
 * note: each DEPTH value uses two bytes
 *
 *         <----------------------- d-stride ---------------------->
 *         <----------------------- d-width ----------------------->
 * [index] 02468                                                   1278
 *         |||||---------------------------------------------------|
 * [data]  DDDDDDDDDDDDDDDDDDDDDDDDDDDDD...                        D
 * [data]  DDDDDDDDDDDDDDDDDDDDDDDDDDDDD...                        D
 * [data]  DDDDDDDDDDDDDDDDDDDDDDDDDDDDD...                        D
 * [data]  ......
 */

dictionary ChannelPixelLayout {
    required unsigned long              offset;
    required unsigned long              width;
    required unsigned long              height;
    required ChannelPixelLayoutDataType dataType;
    required unsigned long              stride;
    required unsigned long              skip;
};

typedef sequence<ChannelPixelLayout> ImagePixelLayout;

enum ImageOrientation { "none", "flipY" };
enum PremultiplyAlpha { "none", "premultiply", "default" };
enum ColorSpaceConversion { "none", "default" };
//enum ResizeQuality { "pixelated", "low", "medium", "high" };

dictionary ImageBitmapOptions {
  ImageOrientation imageOrientation = "none";
  PremultiplyAlpha premultiplyAlpha = "default";
  // options to be added  bugs: 1363861
  ColorSpaceConversion colorSpaceConversion = "default";
  [EnforceRange] unsigned long resizeWidth;
  [EnforceRange] unsigned long resizeHeight;
  //ResizeQuality resizeQuality = "low";
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://wicg.github.io/entries-api/#idl-index
 */

[Exposed=Window]
interface FileSystemDirectoryEntry : FileSystemEntry {
    FileSystemDirectoryReader createReader();

// Mattias:     void getFile(optional USVString? path,
//                 optional FileSystemFlags options = {},
//                 optional FileSystemEntryCallback successCallback,
//                 optional ErrorCallback errorCallback);

// Mattias:     void getDirectory(optional USVString? path,
//                      optional FileSystemFlags options = {},
//                      optional FileSystemEntryCallback successCallback,
//                      optional ErrorCallback errorCallback);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://wicg.github.io/entries-api/#idl-index
 */

// Mattias: callback FileSystemEntriesCallback = void (sequence<FileSystemEntry> entries);

[Exposed=Window]
interface FileSystemDirectoryReader {

    // readEntries can be called just once. The second time it returns no data.

// Mattias:     [Throws]
//    void readEntries(FileSystemEntriesCallback successCallback,
//                     optional ErrorCallback errorCallback);
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://xhr.spec.whatwg.org
 */

typedef (Blob or Directory or USVString) FormDataEntryValue;

[Exposed=(Window,Worker)]
interface FormData {
  [Throws]
  constructor(optional HTMLFormElement form);

  [Throws]
  void append(USVString name, Blob value, optional USVString filename);
  [Throws]
  void append(USVString name, USVString value);
  void delete(USVString name);
  FormDataEntryValue? get(USVString name);
  sequence<FormDataEntryValue> getAll(USVString name);
  boolean has(USVString name);
  [Throws]
  void set(USVString name, Blob value, optional USVString filename);
  [Throws]
  void set(USVString name, USVString value);
  iterable<USVString, FormDataEntryValue>;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://slightlyoff.github.io/ServiceWorker/spec/service_worker/index.html#service-worker-obj
 *
 */

// Still unclear what should be subclassed.
// https://github.com/slightlyoff/ServiceWorker/issues/189
[Func="ServiceWorkerVisible",
 // FIXME(nsm): Bug 1113522. This is exposed to satisfy webidl constraints, but it won't actually work.
 Exposed=(Window,Worker)]
interface ServiceWorker : EventTarget {
  readonly attribute USVString scriptURL;
  readonly attribute ServiceWorkerState state;

  attribute EventHandler onstatechange;

// Mattias:  [Throws]
//  void postMessage(any message, sequence<object> transferable);
// Mattias:  [Throws]
//  void postMessage(any message, optional StructuredSerializeOptions options = {});
};

ServiceWorker includes AbstractWorker;

enum ServiceWorkerState {
  // https://github.com/w3c/ServiceWorker/issues/1162
  "parsed",

  "installing",
  "installed",
  "activating",
  "activated",
  "redundant"
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=(Window,Worker)]
interface mixin AbstractWorker {
    attribute EventHandler onerror;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is:
 * https://html.spec.whatwg.org/multipage/webappapis.html#windoworworkerglobalscope-mixin
 * https://fetch.spec.whatwg.org/#fetch-method
 * https://w3c.github.io/webappsec-secure-contexts/#monkey-patching-global-object
 * https://w3c.github.io/ServiceWorker/#self-caches
 */

// https://html.spec.whatwg.org/multipage/webappapis.html#windoworworkerglobalscope-mixin
[Exposed=(Window,Worker)]
interface mixin WindowOrWorkerGlobalScope {
  [Replaceable] readonly attribute USVString origin;
  readonly attribute boolean crossOriginIsolated;

  [Throws, NeedsCallerType]
  void reportError(any e);

  // base64 utility methods
  [Throws]
  DOMString btoa(DOMString btoa);
  [Throws]
  DOMString atob(DOMString atob);

  // timers
  // NOTE: We're using overloads where the spec uses a union.  Should
  // be black-box the same.
  [Throws]
  long setTimeout(Function handler, optional long timeout = 0, any... arguments);
  [Throws]
  long setTimeout(DOMString handler, optional long timeout = 0, any... unused);
  void clearTimeout(optional long handle = 0);
  [Throws]
  long setInterval(Function handler, optional long timeout = 0, any... arguments);
  [Throws]
  long setInterval(DOMString handler, optional long timeout = 0, any... unused);
  void clearInterval(optional long handle = 0);

// Mattias:  // microtask queuing
//  void queueMicrotask(VoidFunction callback);

  // ImageBitmap
// Mattias:  [Throws]
//  Promise<ImageBitmap> createImageBitmap(ImageBitmapSource aImage,
//                                         optional ImageBitmapOptions aOptions = {});
// Mattias:  [Throws]
//  Promise<ImageBitmap> createImageBitmap(ImageBitmapSource aImage,
//                                         long aSx, long aSy, long aSw, long aSh,
//                                         optional ImageBitmapOptions aOptions = {});

  // structured cloning
// Mattias:  [Throws]
//  any structuredClone(any value, optional StructuredSerializeOptions options = {});
};

// https://fetch.spec.whatwg.org/#fetch-method
partial interface mixin WindowOrWorkerGlobalScope {
  [NewObject, NeedsCallerType]
  Promise<Response> fetch(RequestInfo input, optional RequestInit init = {});
};

// https://w3c.github.io/webappsec-secure-contexts/#monkey-patching-global-object
partial interface mixin WindowOrWorkerGlobalScope {
  readonly attribute boolean isSecureContext;
};

// http://w3c.github.io/IndexedDB/#factory-interface
partial interface mixin WindowOrWorkerGlobalScope {
   // readonly attribute IDBFactory indexedDB;
// Mattias:   [Throws]
//   readonly attribute IDBFactory? indexedDB;
};

// https://w3c.github.io/ServiceWorker/#self-caches
partial interface mixin WindowOrWorkerGlobalScope {
  [Throws, Func="nsGlobalWindowInner::CachesEnabled", SameObject]
  readonly attribute CacheStorage caches;
};

// https://wicg.github.io/scheduling-apis/#ref-for-windoworworkerglobalscope-scheduler
partial interface mixin WindowOrWorkerGlobalScope {
// Mattias:  [Replaceable, Pref="dom.enable_web_task_scheduling", SameObject]
//  readonly attribute Scheduler scheduler;
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/ServiceWorker/#cachestorage-interface
 */

interface Principal;

[Exposed=(Window,Worker),
 Func="nsGlobalWindowInner::CachesEnabled"]
interface CacheStorage {
  [Throws, ChromeOnly]
  constructor(CacheStorageNamespace namespace, Principal principal);

// Mattias:  [NewObject]
//  Promise<Response> match(RequestInfo request, optional MultiCacheQueryOptions options = {});
  [NewObject]
  Promise<boolean> has(DOMString cacheName);
// Mattias:    [NewObject]
//  Promise<Cache> open(DOMString cacheName);
  [NewObject]
  Promise<boolean> delete(DOMString cacheName);
// Mattias:  [NewObject]
//  Promise<sequence<DOMString>> keys();
};

dictionary MultiCacheQueryOptions : CacheQueryOptions {
  DOMString cacheName;
};

// chrome-only, gecko specific extension
enum CacheStorageNamespace {
  "content", "chrome"
};
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://fetch.spec.whatwg.org/#response-class
 */

[Exposed=(Window,Worker)]
interface Response {
  // This should be constructor(optional BodyInit... but BodyInit doesn't
  // include ReadableStream yet because we don't want to expose Streams API to
  // Request.
  [Throws]
  constructor(optional (Blob or BufferSource or FormData or URLSearchParams or ReadableStream or USVString)? body = null,
              optional ResponseInit init = {});

  [NewObject] static Response error();
  [Throws,
  NewObject] static Response redirect(USVString url, optional unsigned short status = 302);

  readonly attribute ResponseType type;

  readonly attribute USVString url;
  readonly attribute boolean redirected;
  readonly attribute unsigned short status;
  readonly attribute boolean ok;
  readonly attribute ByteString statusText;
  [SameObject, BinaryName="headers_"] readonly attribute Headers headers;

  [Throws,
   NewObject] Response clone();

  [ChromeOnly, NewObject, Throws] Response cloneUnfiltered();

  // For testing only.
  [ChromeOnly] readonly attribute boolean hasCacheInfoChannel;

  Promise<JSON> json();
};
// Mattias: Response includes Body;

// This should be part of Body but we don't want to expose body to request yet.
// See bug 1387483.
partial interface Response {
// Mattias:  [GetterThrows]
//  readonly attribute ReadableStream? body;
};

dictionary ResponseInit {
  unsigned short status = 200;
  ByteString statusText = "";
  HeadersInit headers;
};

enum ResponseType { "basic", "cors", "default", "error", "opaque", "opaqueredirect" };
/* -*- Mode: IDL; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://fetch.spec.whatwg.org/#headers-class
 */

typedef (sequence<sequence<ByteString>> or record<ByteString, ByteString>) HeadersInit;

enum HeadersGuardEnum {
  "none",
  "request",
  "request-no-cors",
  "response",
  "immutable"
};

[Exposed=(Window,Worker)]
interface Headers {
  [Throws]
  constructor(optional HeadersInit init);

  [Throws] void append(ByteString name, ByteString value);
  [Throws] void delete(ByteString name);
  [Throws] ByteString? get(ByteString name);
  [Throws] boolean has(ByteString name);
  [Throws] void set(ByteString name, ByteString value);
  iterable<ByteString, ByteString>;

  // Used to test different guard states from mochitest.
  // Note: Must be set prior to populating headers or will throw.
  [ChromeOnly, SetterThrows] attribute HeadersGuardEnum guard;
};
/* -*- Mode: IDL; tab-width: 1; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://fetch.spec.whatwg.org/#request-class
 */

typedef (Request or USVString) RequestInfo;
typedef unsigned long nsContentPolicyType;

[Exposed=(Window,Worker)]
interface Request {
  [Throws]
  constructor(RequestInfo input, optional RequestInit init = {});

  readonly attribute ByteString method;
  readonly attribute USVString url;
  [SameObject, BinaryName="headers_"] readonly attribute Headers headers;

  readonly attribute RequestDestination destination;
  readonly attribute USVString referrer;
  [BinaryName="referrerPolicy_"]
  readonly attribute ReferrerPolicy referrerPolicy;
  readonly attribute RequestMode mode;
  readonly attribute RequestCredentials credentials;
  readonly attribute RequestCache cache;
  readonly attribute RequestRedirect redirect;
  readonly attribute DOMString integrity;

  // If a main-thread fetch() promise rejects, the error passed will be a
  // nsresult code.
  [ChromeOnly]
  readonly attribute boolean mozErrors;

// Mattias:  [BinaryName="getOrCreateSignal"]
//  readonly attribute AbortSignal signal;

  [Throws,
   NewObject] Request clone();

  // Bug 1124638 - Allow chrome callers to set the context.
  [ChromeOnly]
  void overrideContentPolicyType(nsContentPolicyType context);
};
// Mattias: Request includes Body;

dictionary RequestInit {
  ByteString method;
  HeadersInit headers;
// Mattias:  BodyInit? body;
  USVString referrer;
  ReferrerPolicy referrerPolicy;
  RequestMode mode;
  RequestCredentials credentials;
  RequestCache cache;
  RequestRedirect redirect;
  DOMString integrity;

  [ChromeOnly]
  boolean mozErrors;

// Mattias:  AbortSignal? signal;

// Mattias:  [Pref="dom.fetchObserver.enabled"]
//  ObserverCallback observe;
};

enum RequestDestination {
  "",
  "audio", "audioworklet", "document", "embed", "font", "frame", "iframe",
  "image", "manifest", "object", "paintworklet", "report", "script",
  "sharedworker", "style",  "track", "video", "worker", "xslt"
};

enum RequestMode { "same-origin", "no-cors", "cors", "navigate" };
enum RequestCredentials { "omit", "same-origin", "include" };
enum RequestCache { "default", "no-store", "reload", "no-cache", "force-cache", "only-if-cached" };
enum RequestRedirect { "follow", "error", "manual" };
