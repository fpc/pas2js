unit JOB_Web;

{$mode ObjFPC}{$H+}
{$ModeSwitch FunctionReferences}

interface

uses
  Classes, SysUtils, JOB_Shared, JOB_WAsm, JOB_JS;

type
  IJSEvent = interface;
  TJSEvent = class;

  IJSEventListenerEvent = IJSEvent;
  TJSEventListenerEvent = TJSEvent;

  TJSEventHandler = function(Event: IJSEventListenerEvent): boolean of object;

  IJSEventTarget = interface(IJSObject)
    ['{1883145B-C826-47D1-9C63-47546BA536BD}']
    procedure addEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
  end;

  { TJSEventTarget }

  TJSEventTarget = class(TJSObject,IJSEventTarget)
    class function Cast(Intf: IJSObject): IJSEventTarget; overload;
    procedure addEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
  end;

  { IJSNode }

  IJSNode = interface(IJSEventTarget)
    ['{D7A751A8-73AD-4620-B2EE-03165A9D65D7}']
    function GetInnerText: UnicodeString;
    procedure SetInnerText(const AValue: UnicodeString);
    property InnerText : UnicodeString read GetInnerText write SetInnerText;
  end;

  { TJSNode }

  TJSNode = class(TJSEventTarget,IJSNode)
    class function Cast(Intf: IJSObject): IJSNode; overload;
    function GetInnerText: UnicodeString;
    procedure SetInnerText(const AValue: UnicodeString);
  end;

  { IJSElement }

  IJSElement = interface(IJSNode)
    ['{A160069E-378F-4B76-BE64-1979A28B9EEA}']
    function childElementCount : Integer;
    function firstElementChild : IJSElement;
    function Getid: UnicodeString;
    function GetInnerHTML: UnicodeString;
    function GetName: UnicodeString;
    function GetOuterHTML: UnicodeString;
    function Get_ClassName: UnicodeString;
    procedure append(const aNode : IJSElement); overload;
    procedure append(const aText : UnicodeString); overload;
    procedure Setid(const AValue: UnicodeString);
    procedure SetInnerHTML(const AValue: UnicodeString);
    procedure SetName(const AValue: UnicodeString);
    procedure SetOuterHTML(const AValue: UnicodeString);
    procedure Set_ClassName(const AValue: UnicodeString);
    property Name: UnicodeString read GetName write SetName;
    property _ClassName: UnicodeString read Get_ClassName write Set_ClassName;
    property id : UnicodeString read Getid write Setid;
    property InnerHTML: UnicodeString read GetInnerHTML write SetInnerHTML;
    property OuterHTML : UnicodeString read GetOuterHTML write SetOuterHTML;
  end;

  { TJSElement }

  TJSElement = class(TJSNode,IJSElement)
  private
    function Getid: UnicodeString;
    function GetInnerHTML: UnicodeString;
    function GetName: UnicodeString;
    function GetOuterHTML: UnicodeString;
    function Get_ClassName: UnicodeString;
    procedure Setid(const AValue: UnicodeString);
    procedure SetInnerHTML(const AValue: UnicodeString);
    procedure SetName(const AValue: UnicodeString);
    procedure SetOuterHTML(const AValue: UnicodeString);
    procedure Set_ClassName(const AValue: UnicodeString);
  public
    class function Cast(Intf: IJSObject): IJSElement; overload;
    procedure append(const aText : UnicodeString); overload;
    procedure append(const aNode : IJSElement); overload;
    function childElementCount : Integer;
    function firstElementChild : IJSElement;
    property Name : UnicodeString read GetName write SetName;
    property _className : UnicodeString read Get_ClassName write Set_ClassName;
    property id : UnicodeString read Getid write Setid;
    property InnerHTML : UnicodeString read GetInnerHTML write SetInnerHTML;
    property OuterHTML : UnicodeString read GetOuterHTML write SetOuterHTML;
  end;

  { IJSEvent }

  IJSEvent = interface(IJSObject)
    ['{8B752F08-21F6-4F0D-B7A0-5A6616E752AD}']
    function CurrentTargetElement: IJSElement;
    function TargetElement: IJSElement;
  end;

  { TJSEvent }

  TJSEvent = class(TJSObject,IJSEvent)
    class function Cast(Intf: IJSObject): IJSEvent; overload;
    function CurrentTargetElement: IJSElement;
    function TargetElement: IJSElement;
  end;

  { IJSUIEvent }

  IJSUIEvent = interface(IJSEvent)
    ['{A1234998-5180-4905-B820-10FAB9B2DD12}']
  end;

  { TJSUIEvent }

  TJSUIEvent = class(TJSEvent,IJSUIEvent)
    class function Cast(Intf: IJSObject): IJSUIEvent; overload;
  end;

  { IJSMouseEvent }

  IJSMouseEvent = interface(IJSUIEvent)
    ['{B91DC727-1164-43AE-8481-55421D3148C4}']
  end;

  { TJSMouseEvent }

  TJSMouseEvent = class(TJSUIEvent,IJSMouseEvent)
    class function Cast(Intf: IJSObject): IJSMouseEvent; overload;
  end;

  TJSHTMLClickEventHandler = function(aEvent: IJSMouseEvent) : boolean of object;

  { IJSHTMLElement }

  IJSHTMLElement = interface(IJSElement)
    ['{D50E53E1-5B3B-4DA4-ACB0-1FD0DE32B711}']
    function Gettitle: UnicodeString;
    procedure Settitle(const AValue: UnicodeString);
    procedure set_onclick(const h: TJSHTMLClickEventHandler);
    property title: UnicodeString read Gettitle write Settitle;
  end;

  { TJSHTMLElement }

  TJSHTMLElement = class(TJSElement,IJSHTMLElement)
  private
    function Gettitle: UnicodeString;
    procedure Settitle(const AValue: UnicodeString);
  public
    class function Cast(Intf: IJSObject): IJSHTMLElement; overload;
    procedure set_onclick(const h: TJSHTMLClickEventHandler);
    property title: UnicodeString read Gettitle write Settitle;
  end;

  { IJSHTMLButtonElement }

  IJSHTMLButtonElement = interface(IJSHTMLElement)
    ['{81DC2F80-FEF4-4705-A6DC-A04B2E32B72D}']
  end;

  { TJSHTMLButtonElement }

  TJSHTMLButtonElement = class(TJSHTMLElement,IJSHTMLButtonElement)
    class function Cast(Intf: IJSObject): IJSHTMLButtonElement; overload;
  end;

  { IJSHTMLDivElement }

  IJSHTMLDivElement = interface(IJSHTMLElement)
    ['{A02A19B2-85B6-4C96-9281-AF90459E1CEC}']
  end;

  { TJSHTMLDivElement }

  TJSHTMLDivElement = class(TJSHTMLElement,IJSHTMLDivElement)
    class function Cast(Intf: IJSObject): IJSHTMLDivElement; overload;
  end;

  { IJSDocument }

  IJSDocument = interface(IJSNode)
    ['{CC3FB7C1-C4ED-4BBC-80AB-7B6C2989E026}']
    function createElement(const tagName : UnicodeString) : IJSElement; overload;
    function getElementById(const aID : UnicodeString) : IJSElement;
  end;

  { TJSDocument }

  TJSDocument = class(TJSNode,IJSDocument)
    class function Cast(Intf: IJSObject): IJSDocument; overload;
    function createElement(const tagName : UnicodeString) : IJSElement; overload;
    function getElementById(const aID : UnicodeString) : IJSElement; overload;
  end;

  { IJSWindow }

  IJSWindow = interface(IJSObject)
    ['{7DEBCDE5-2C6C-4758-9EE3-CF153AF2AFA0}']
    procedure addEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
    procedure Alert(Const Msg: UnicodeString);
  end;

  { TJSWindow }

  TJSWindow = class(TJSObject,IJSWindow)
    class function Cast(Intf: IJSObject): IJSWindow; overload;
    procedure addEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
    procedure Alert(Const Msg: UnicodeString);
  end;

var
  JSDocument: TJSDocument;
  JSWindow: TJSWindow;

function JOBCallTJSHTMLClickEventHandler(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
function JOBCallTJSEventHandler(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;

implementation

function JOBCallTJSHTMLClickEventHandler(const aMethod: TMethod;
  var H: TJOBCallbackHelper): PByte;
var
  Event: IJSMouseEvent;
begin
  Event:=H.GetObject(TJSMouseEvent) as IJSMouseEvent;
  Result:=H.AllocBool(TJSHTMLClickEventHandler(aMethod)(Event));
end;

function JOBCallTJSEventHandler(const aMethod: TMethod;
  var H: TJOBCallbackHelper): PByte;
var
  Event: IJSEventListenerEvent;
begin
  Event:=H.GetObject(TJSEventListenerEvent) as IJSEventListenerEvent;
  Result:=H.AllocBool(TJSEventHandler(aMethod)(Event));
end;

{ TJSHTMLDivElement }

class function TJSHTMLDivElement.Cast(Intf: IJSObject): IJSHTMLDivElement;
begin
  Result:=TJSHTMLDivElement.JOBCast(Intf);
end;

{ TJSHTMLButtonElement }

class function TJSHTMLButtonElement.Cast(Intf: IJSObject): IJSHTMLButtonElement;
begin
  Result:=TJSHTMLButtonElement.JOBCast(Intf);
end;

{ TJSMouseEvent }

class function TJSMouseEvent.Cast(Intf: IJSObject): IJSMouseEvent;
begin
  Result:=TJSMouseEvent.JOBCast(Intf);
end;

{ TJSUIEvent }

class function TJSUIEvent.Cast(Intf: IJSObject): IJSUIEvent;
begin
  Result:=TJSUIEvent.JOBCast(Intf);
end;

{ TJSEventTarget }

class function TJSEventTarget.Cast(Intf: IJSObject): IJSEventTarget;
begin
  Result:=TJSEventTarget.JOBCast(Intf);
end;

procedure TJSEventTarget.addEventListener(const aName: UnicodeString;
  const aListener: TJSEventHandler);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallTJSEventHandler);
  try
    InvokeJSNoResult('addEventListener',[aName,m]);
  finally
    m.Free;
  end;
end;

{ TJSHTMLElement }

function TJSHTMLElement.Gettitle: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('title');
end;

procedure TJSHTMLElement.Settitle(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('title',AValue);
end;

class function TJSHTMLElement.Cast(Intf: IJSObject): IJSHTMLElement;
begin
  Result:=TJSHTMLElement.JOBCast(Intf);
end;

procedure TJSHTMLElement.set_onclick(const h: TJSHTMLClickEventHandler);
var
  cb1: TJOB_Method;
begin
  cb1:=TJOB_Method.Create(TMethod(h),@JOBCallTJSHTMLClickEventHandler);
  try
    WriteJSPropertyValue('onclick',cb1);
  finally
    cb1.Free;
  end;
end;

{ TJSEvent }

class function TJSEvent.Cast(Intf: IJSObject): IJSEvent;
begin
  Result:=TJSEvent.JOBCast(Intf);
end;

function TJSEvent.CurrentTargetElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('currentTargetElement',TJSElement) as IJSElement;
end;

function TJSEvent.TargetElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('targetElement',TJSElement) as IJSElement;
end;

{ TJSNode }

class function TJSNode.Cast(Intf: IJSObject): IJSNode;
begin
  Result:=TJSNode.JOBCast(Intf);
end;

function TJSNode.GetInnerText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerText');
end;

procedure TJSNode.SetInnerText(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerText',AValue);
end;

{ TJSElement }

function TJSElement.Getid: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('id');
end;

function TJSElement.GetOuterHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('outerHTML');
end;

procedure TJSElement.Setid(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('id',AValue);
end;

procedure TJSElement.SetOuterHTML(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('outerHTML',AValue);
end;

class function TJSElement.Cast(Intf: IJSObject): IJSElement;
begin
  Result:=TJSElement.JOBCast(Intf);
end;

procedure TJSElement.append(const aText: UnicodeString);
begin
  InvokeJSNoResult('append',[aText]);
end;

procedure TJSElement.append(const aNode: IJSElement);
begin
  InvokeJSNoResult('append',[aNode]);
end;

function TJSElement.childElementCount: Integer;
begin
  Result:=ReadJSPropertyLongInt('childElementCount');
end;

function TJSElement.firstElementChild: IJSElement;
begin
  Result:=ReadJSPropertyObject('firstElementChild',TJSElement) as IJSElement;
end;

function TJSElement.GetInnerHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerHTML');
end;

function TJSElement.GetName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSElement.Get_ClassName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('className');
end;

procedure TJSElement.SetInnerHTML(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerHTML',AValue);
end;

procedure TJSElement.SetName(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',AValue);
end;

procedure TJSElement.Set_ClassName(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('className',AValue);
end;

{ TJSDocument }

class function TJSDocument.Cast(Intf: IJSObject): IJSDocument;
begin
  Result:=TJSDocument.JOBCast(Intf);
end;

function TJSDocument.createElement(const tagName: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('createElement',[tagName],TJSElement) as IJSElement;
end;

function TJSDocument.getElementById(const aID: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aID],TJSElement) as IJSElement;
end;

{ TJSWindow }

class function TJSWindow.Cast(Intf: IJSObject): IJSWindow;
begin
  Result:=TJSWindow.JOBCast(Intf);
end;

procedure TJSWindow.addEventListener(const aName: UnicodeString;
  const aListener: TJSEventHandler);
var
  cb1: TJOB_Method;
begin
  cb1:=TJOB_Method.Create(TMethod(aListener),@JOBCallTJSEventHandler);
  try
    InvokeJSNoResult('addEventListener',[aName,cb1]);
  finally
    cb1.Free;
  end;
end;

procedure TJSWindow.Alert(const Msg: UnicodeString);
begin
  InvokeJSNoResult('alert',[Msg]);
end;

initialization
  JSDocument:=TJSDocument.JOBCreateFromID(JOBObjIdDocument);
  JSWindow:=TJSWindow.JOBCreateFromID(JOBObjIdWindow);
finalization
  JSDocument.Free;
  JSWindow.Free;
end.

