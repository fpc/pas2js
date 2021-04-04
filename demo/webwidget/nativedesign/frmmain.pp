unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webideintf, Forms, Controls, Graphics, Dialogs, EditBtn,
  ExtCtrls, ComCtrls, StdCtrls, ActnList, GlobalCefApplication,
  {$IFDEF DARWIN}  uCEFLazarusCocoa,  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows, Messages,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces,
  uCEFWinControl, uCEFApplication, uCEFWorkScheduler, uCEFBrowserWindow, fpJSON, uCEFChromiumEvents;

type

  { TMainForm }

  TMainForm = class(TForm)
    AGoExternal: TAction;
    AGo: TAction;
    ALWidgets: TActionList;
    BrowserWindow1: TBrowserWindow;
    FEProject: TFileNameEdit;
    ILWidgets: TImageList;
    MLog: TMemo;
    PCDesigner: TPageControl;
    Project: TLabel;
    PBottom: TPanel;
    TBExternalGo: TToolButton;
    TSInspector: TTabSheet;
    TSBrowser: TTabSheet;
    TSLog: TTabSheet;
    TBWidgets: TToolBar;
    TBGo: TToolButton;
    ToolButton1: TToolButton;
    procedure AGoExecute(Sender: TObject);
    procedure AGoExternalExecute(Sender: TObject);
    procedure AGoUpdate(Sender: TObject);
    procedure BrowserWindow1BrowserClosed(Sender: TObject);
    procedure BrowserWindow1BrowserCreated(Sender: TObject);
    procedure cwOnBeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient;
      var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean;
      var Result: Boolean);
    procedure DEProjectEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FClientID : Int64; // Just one for now
    FDesignCaption : String;
    FWebIDEIntf : TIDEServer;
    FWidgetCount : Integer;
    FWidgets : Array of String;
    FURL : String;
    FURLCount : Integer;
    FAllowGo: Boolean;
{$IFDEF WINDOWS}
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
{$ENDIF}
    function GetProjectURL: String;
    procedure DoAddWidget(Sender: TObject);
    procedure DoAction(Sender: TObject; aExchange: TIDEExchange);
    procedure DoClientCame(Sender: TObject; aClient: TIDEClient);
    procedure DoClientLeft(Sender: TObject; aClient: TIDEClient);
    procedure DoLogRequest(Sender: TObject; aURL: String);
    procedure IsWidgetEnabled(Sender: TObject);
    procedure LogRequest;
    Procedure RegisterWidgets;
    Procedure RegisterWidget(aWidget: String; aImageIndex : Integer);
  public
    Procedure Log(Msg : String);
    Procedure Log(Fmt : String; Args : Array of const);
  end;

var
  MainForm: TMainForm;

implementation

uses lclintf, fpmimetypes;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DEProjectEditingDone(Sender: TObject);
begin
  FWebIDEIntf.ProjectDir:=ExtractFilePath(FEProject.FileName);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FWebIDEIntf.Active:=False;
  BrowserWindow1.CloseBrowser(True);
  CanClose:=BrowserWindow1.IsClosed;
  Visible  := False;
end;

Function TMainForm.GetProjectURL : String;

begin
  Result:=Format('http://localhost:%d/Project/%s',[FWebIDEIntf.Port,ExtractFileName(FEProject.FileName)]);
end;

procedure TMainForm.AGoExecute(Sender: TObject);
Var
  URL : String;

begin
  URL:=GetProjectURL;
  Log('Going to URL: %s',[URL]);
  BrowserWindow1.LoadURL(URL);
end;

procedure TMainForm.AGoExternalExecute(Sender: TObject);
Var
  URL : String;

begin
  URL:=GetProjectURL;
  Log('Going to URL: %s',[URL]);
  OpenURL(URL);
end;

procedure TMainForm.AGoUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=FAllowGo;
end;

procedure TMainForm.BrowserWindow1BrowserClosed(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BrowserWindow1BrowserCreated(Sender: TObject);
begin
  // Now the browser is fully initialized we can load the initial web page.
  FAllowGo:=True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FAllowGo:=False;
  FDesignCaption:=Caption;
  MimeTypes.LoadKnownTypes;
  FEProject.FileName:=ExtractFilePath(ExtractFilePath(Paramstr(0)))+'designdemo'+PathDelim+'designdemo.html';
  FWebIDEIntf:=TIDEServer.Create(Self);
  FWebIDEIntf.ProjectDir:=ExtractFilePath(FEProject.FileName);
  FWebIDEIntf.OnClientAdded:=@DoClientCame;
  FWebIDEIntf.OnClientRemoved:=@DoClientLeft;
  FWebIDEIntf.OnRequest:=@DoLogRequest;
  FWebIDEIntf.OnAction:=@DoAction;
  FWebIDEIntf.Active:=True;
  TSInspector.TabVisible:=False;
  RegisterWidgets;
end;

{$IFDEF WINDOWS}
procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);

begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);

begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;
{$ENDIF}

procedure TMainForm.cwOnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;


procedure TMainForm.DoAction(Sender: TObject; aExchange: TIDEExchange);

var
  PayJSON : TJSONObject;

begin
  payJSON:=Nil;
  if Not (aExchange.Payload is TJSONObject) then
    begin
    Log('Payload is not JSON Object');
    exit;
    end;
  payJSON:=aExchange.Payload as TJSONObject;
  with aExchange do
    case Name of
    'create':
      Log('Browser created widget of class %s, name %s',[PayJSON.Get('class',''),PayJSON.Get('widget','')]);
    'select':
      begin
      Log('Browser selected widget of class %s, name %s',[PayJSON.Get('class',''),PayJSON.Get('widget','')]);
      Log('Selected widget state: '+PayJSON.Get('state',''));
      end;
    end;
end;

procedure TMainForm.DoClientCame(Sender: TObject; aClient: TIDEClient);
begin
  if FClientID>0 then
    Log('Ignoring second client (id: %d) attachment.',[aClient.ID])
  else
    begin
    FClientID:=aClient.ID;
    Caption:=FDesignCaption+Format(' [Client: %d]',[FClientID]);
    end;
end;

procedure TMainForm.DoAddWidget(Sender: TObject);

Var
  Cmd : TIDECommand;
  aName : String;

begin
  aName:=FWidgets[(Sender as TAction).Tag];
  Cmd:=TIDECommand.Create;
  Cmd.NeedsConfirmation:=True;
  Cmd.ClientID:=FClientID;
  Cmd.name:='addWidget';
  Cmd.PayLoad:=TJSONObject.Create(['class','T'+aName+'Widget']);
  FWebIDEIntf.SendCommand(cmd);
end;

procedure TMainForm.DoClientLeft(Sender: TObject; aClient: TIDEClient);
begin
  if (aClient.ID=FClientID) then
    begin
    FClientID:=-1;
    Caption:=FDesignCaption;
    end;
end;

procedure TMainForm.LogRequest;

begin
  if (FURLCount=1) then // avoid excessive logging, command loop is on very short interval.
    Log('Internal server request received: '+FURL);
end;

procedure TMainForm.DoLogRequest(Sender: TObject; aURL: String);
begin
  if (aURL<>FURL) then
    begin
    FURLCount:=1;
    FURL:=aURL
    end
  else
    Inc(FURLCount);
  TThread.Synchronize(TThread.CurrentThread,@LogRequest);
end;

procedure TMainForm.IsWidgetEnabled(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FClientID<>-1);
end;

procedure TMainForm.RegisterWidgets;
begin
  SetLength(FWidgets,9);
  FWidgetCount:=0;
  RegisterWidget('Button',2);
  RegisterWidget('Checkbox',3);
  RegisterWidget('Radio',4);
  RegisterWidget('Edit',5);
  RegisterWidget('Image',6);
  RegisterWidget('TextArea',7);
  RegisterWidget('Select',8);
  RegisterWidget('Container',9);
  RegisterWidget('Jumbo',10);
end;

procedure TMainForm.RegisterWidget(aWidget: String; aImageIndex: Integer);

Var
  A : TAction;
  B : TToolButton;
  L,i : Integer;

begin
  FWidgets[FWidgetCount]:=aWidget;
  A:=TAction.Create(Self);
  A.ActionList:=ALWidgets;
  A.Name:='AAdd'+aWidget;
  A.Hint:='Add '+aWidget;
  A.Caption:='Add '+aWidget;
  A.ImageIndex:=aImageIndex;
  A.Tag:=FWidgetCount;
  A.OnExecute:=@DoAddWidget;
  A.OnUpdate:=@IsWidgetEnabled;
  L:=0;
  For I:=0 to TBWidgets.ControlCount-1 do
    if TBWidgets.Controls[i].BoundsRect.Right>L then
      L:=TBWidgets.Controls[i].BoundsRect.Right;
  B:=TToolButton.Create(Self);
  B.Parent:=TBWidgets;
  B.Left:=L;
  B.Height:=32;
  B.Action:=A;
  inc(FWidgetCount);
//  TBWidgets.AddControl;

end;

procedure TMainForm.Log(Msg: String);
begin
  MLog.Lines.Add(Msg);
end;

procedure TMainForm.Log(Fmt: String; Args: array of const);
begin
  Log(Format(Fmt,Args));
end;


initialization
  {$IFDEF DARWIN}
  AddCrDelegate;
  {$ENDIF}
  if GlobalCEFApp = nil then begin
    CreateGlobalCEFApp;
    if not GlobalCEFApp.StartMainProcess then begin
      DestroyGlobalCEFApp;
      DestroyGlobalCEFWorkScheduler;
      halt(0); // exit the subprocess
    end;
  end;

finalization
  (* Destroy from this unit, which is used after "Interfaces". So this happens before the Application object is destroyed *)
  if GlobalCEFWorkScheduler <> nil then
    GlobalCEFWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;

end.

