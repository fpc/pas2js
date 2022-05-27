unit JOB_Web;

{$mode ObjFPC}{$H+}
{$ModeSwitch FunctionReferences}

interface

uses
  Classes, SysUtils, JOB_Shared, JOB_WAsm;

type

  { IJSNode }

  IJSNode = interface(IJSObject)
    ['{D7A751A8-73AD-4620-B2EE-03165A9D65D7}']
    function GetInnerText: UnicodeString;
    procedure SetInnerText(const AValue: UnicodeString);
    property InnerText : UnicodeString read GetInnerText write SetInnerText;
  end;

  { TJSNode }

  TJSNode = class(TJSObject,IJSNode)
  public
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

  IJSDocument = interface(IJSNode)
    ['{CC3FB7C1-C4ED-4BBC-80AB-7B6C2989E026}']
    function getElementById(const aID : UnicodeString) : IJSElement;
  end;

  { TJSDocument }

  TJSDocument = class(TJSNode,IJSDocument)
  public
    function getElementById(const aID : UnicodeString) : IJSElement;
  end;

  IJSEvent = interface(IJSObject)
    ['{8B752F08-21F6-4F0D-B7A0-5A6616E752AD}']
    function CurrentTargetElement: IJSElement;
    function TargetElement: IJSElement;
  end;

  { TJSEvent }

  TJSEvent = class(TJSObject,IJSEvent)
  public
    function CurrentTargetElement: IJSElement;
    function TargetElement: IJSElement;
  end;

  TEventListenerEvent = TJSEvent;

  TJSEventHandler = reference to function(Event: TEventListenerEvent): boolean;

  IJSWindow = interface(IJSObject)
    ['{7DEBCDE5-2C6C-4758-9EE3-CF153AF2AFA0}']
    procedure AddEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
    procedure Alert(Const Msg: UnicodeString);
  end;

  { TJSWindow }

  TJSWindow = class(TJSObject,IJSWindow)
  public
    procedure AddEventListener(const aName: UnicodeString; const aListener: TJSEventHandler);
    procedure Alert(Const Msg: UnicodeString);
  end;

var
  JSDocument: TJSDocument;
  JSWindow: TJSWindow;

implementation

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

procedure TJSWindow.AddEventListener(const aName: UnicodeString;
  const aListener: TJSEventHandler);
begin
  InvokeJSNoResult('addEventListener',[{Todo}]);
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

