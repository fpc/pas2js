unit JOB_Web;

{$mode ObjFPC}{$H+}
{$ModeSwitch FunctionReferences}

interface

uses
  Classes, SysUtils, JOB_Shared, JOB_WAsm;

type
  IJSEvent = interface;
  TJSEvent = class;

  IJSEventListenerEvent = IJSEvent;
  TJSEventListenerEvent = TJSEvent;

  TJSEventHandler = function(Event: IJSEventListenerEvent): boolean of object;

  IJSEventTarget = interface
    ['{1883145B-C826-47D1-9C63-47546BA536BD}']
    procedure addEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
  end;

  { TJSEventTarget }

  TJSEventTarget = class(TJSObject,IJSEventTarget)
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
    function GetInnerText: UnicodeString;
    procedure SetInnerText(const AValue: UnicodeString);
  end;

  { IJSElement }

  IJSElement = interface(IJSNode)
    ['{A160069E-378F-4B76-BE64-1979A28B9EEA}']
    function childElementCount : Integer;
    function firstElementChild : IJSElement;
    function GetInnerHTML: UnicodeString;
    function GetName: UnicodeString;
    function Get_ClassName: UnicodeString;
    procedure SetInnerHTML(const AValue: UnicodeString);
    procedure SetName(const AValue: UnicodeString);
    procedure Set_ClassName(const AValue: UnicodeString);
    property Name: UnicodeString read GetName write SetName;
    property _ClassName: UnicodeString read Get_ClassName write Set_ClassName;
    property InnerHTML: UnicodeString read GetInnerHTML write SetInnerHTML;
  end;

  { TJSElement }

  TJSElement = class(TJSNode,IJSElement)
    function childElementCount : Integer;
    function firstElementChild : IJSElement;
    function GetInnerHTML: UnicodeString;
    function GetName: UnicodeString;
    function Get_ClassName: UnicodeString;
    procedure SetInnerHTML(const AValue: UnicodeString);
    procedure SetName(const AValue: UnicodeString);
    procedure Set_ClassName(const AValue: UnicodeString);
  end;

  { IJSEvent }

  IJSEvent = interface(IJSObject)
    ['{8B752F08-21F6-4F0D-B7A0-5A6616E752AD}']
    function CurrentTargetElement: IJSElement;
    function TargetElement: IJSElement;
  end;

  { TJSEvent }

  TJSEvent = class(TJSObject,IJSEvent)
    function CurrentTargetElement: IJSElement;
    function TargetElement: IJSElement;
  end;

  IJSUIEvent = interface(IJSEvent)
    ['{A1234998-5180-4905-B820-10FAB9B2DD12}']
  end;
  TJSUIEvent = class(TJSEvent,IJSUIEvent)
  end;

  IJSMouseEvent = interface(IJSUIEvent)
    ['{B91DC727-1164-43AE-8481-55421D3148C4}']
  end;
  TJSMouseEvent = class(TJSUIEvent,IJSMouseEvent)
  end;

  TJSHTMLClickEventHandler = function(aEvent: IJSMouseEvent) : boolean of object;

  { IJSHTMLElement }

  IJSHTMLElement = interface(IJSElement)
    ['{D50E53E1-5B3B-4DA4-ACB0-1FD0DE32B711}']
    procedure set_onclick(const h: TJSHTMLClickEventHandler);
  end;

  { TJSHTMLElement }

  TJSHTMLElement = class(TJSElement,IJSHTMLElement)
    procedure set_onclick(const h: TJSHTMLClickEventHandler);
  end;

  IJSDocument = interface(IJSNode)
    ['{CC3FB7C1-C4ED-4BBC-80AB-7B6C2989E026}']
    function getElementById(const aID : UnicodeString) : IJSElement;
  end;

  { TJSDocument }

  TJSDocument = class(TJSNode,IJSDocument)
    function getElementById(const aID : UnicodeString) : IJSElement;
  end;

  { IJSWindow }

  IJSWindow = interface(IJSObject)
    ['{7DEBCDE5-2C6C-4758-9EE3-CF153AF2AFA0}']
    procedure addEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
    procedure Alert(Const Msg: UnicodeString);
  end;

  { TJSWindow }

  TJSWindow = class(TJSObject,IJSWindow)
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

{ TJSEventTarget }

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

function TJSEvent.CurrentTargetElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('currentTargetElement',TJSElement) as IJSElement;
end;

function TJSEvent.TargetElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('targetElement',TJSElement) as IJSElement;
end;

{ TJSNode }

function TJSNode.GetInnerText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerText');
end;

procedure TJSNode.SetInnerText(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerText',AValue);
end;

{ TJSElement }

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

function TJSDocument.getElementById(const aID: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aID],TJSElement) as IJSElement;
end;

{ TJSWindow }

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
  JSDocument:=TJSDocument.CreateFromID(JOBObjIdDocument);
  JSWindow:=TJSWindow.CreateFromID(JOBObjIdWindow);
finalization
  JSDocument.Free;
  JSWindow.Free;
end.

