unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webideintf, Forms, Controls, Graphics, Dialogs, EditBtn, ExtCtrls, ComCtrls, StdCtrls, ActnList,
  {$IFDEF LINUX}
   WebBrowserCtrls, WebBrowserIntf,
  {$ELSE}
  {$IFDEF WINDOWS}
  Windows, Messages, uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces, uCEFWinControl, uCEFApplication,
  {$ELSE}
  {$ERROR Unsupported platform}
  {$ENDIF}
  {$ENDIF} fpJSON;

type

  { TMainForm }

  TMainForm = class(TForm)
    AGoExternal: TAction;
    AGo: TAction;
    ALWidgets: TActionList;
    FEProject: TFileNameEdit;
    ILWidgets: TImageList;
    MLog: TMemo;
    PCDesigner: TPageControl;
    Project: TLabel;
    PBottom: TPanel;
    TBExternalGo: TToolButton;
    tmrShowChromium: TTimer;
    TSInspector: TTabSheet;
    TSBrowser: TTabSheet;
    TSLog: TTabSheet;
    TBWidgets: TToolBar;
    TBGo: TToolButton;
    ToolButton1: TToolButton;
    procedure AGoExecute(Sender: TObject);
    procedure AGoExternalExecute(Sender: TObject);
    procedure AGoUpdate(Sender: TObject);
    procedure DEProjectEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrShowChromiumTimer(Sender: TObject);
  private
    FClientID : Int64; // Just one for now
    FDesignCaption : String;
    FWebIDEIntf : TIDEServer;
    FWidgetCount : Integer;
    FWidgets : Array of String;
    FURL : String;
    FURLCount : Integer;
    FCanClose : Boolean;
    FAllowGo: Boolean;
{$IFDEF LINUX}
    FWBDesign : TWebBrowser;
    FWIDesign : TWebInspector;
    FLastEmbeddedURI : String;
    procedure wbDesignConsoleMessage(Sender: TObject; const Message, Source: string; Line: Integer);
    procedure wbDesignError(Sender: TObject; const Uri: string; ErrorCode: LongWord; const ErrorMessage: string;   var Handled: Boolean);
    procedure wbDesignFavicon(Sender: TObject);
    procedure wbDesignHitTest(Sender: TObject; X, Y: Integer; HitTest: TWebHitTest; const Link, Media: string);
    procedure wbDesignLoadStatusChange(Sender: TObject);
    procedure wbDesignLocationChange(Sender: TObject);
    procedure wbDesignNavigate(Sender: TObject; const Uri: string; var aAction: TWebNavigateAction);
    procedure wbDesignProgress(Sender: TObject; Progress: Integer);
    procedure wbDesignRequest(Sender: TObject; var Uri: string);
    procedure wbDesignScriptDialog(Sender: TObject; Dialog: TWebScriptDialog; const Message: string; var Input: string;  var Accepted: Boolean; var Handled: Boolean);
{$ENDIF}
{$IFDEF WINDOWS}
    FClosing : Boolean;
    cwDesign : TChromiumWindow;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure cwBeforeClose(Sender: TObject);
    procedure cwClose(Sender: TObject);
    procedure cwAfterCreated(Sender: TObject);
    procedure cwOnBeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient;
      var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean;
      var Result: Boolean);
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
    procedure SetUpEmbeddedBrowser;
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
  CanClose:=FCanClose;
{$IFDEF WINDOWS}
  if not(FClosing) then
    begin
    FClosing := True;
    Visible  := False;
    cwDesign.CloseBrowser(True);
    end;
{$ENDIF}
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
{$IFDEF LINUX}
  FWBDesign.Location:=URL;
{$ENDIF}
{$IFDEF WINDOWS}
  cwDesign.LoadURL(URL);
{$ENDIF}
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

procedure TMainForm.FormCreate(Sender: TObject);



begin
  FAllowGo:=False;
  FDesignCaption:=Caption;
{$IFDEF Linux}
  MimeTypes.LoadFromFile('/etc/mime.types');
{$ENDIF}
{$IFDEF WINDOWS}
  MimeTypes.LoadFromFile(ExtractFilePath(Paramstr(0))+'mime.types');
{$ENDIF}
  FEProject.FileName:=ExtractFilePath(Paramstr(0))+'designdemo'+PathDelim+'designdemo.html';
  FWebIDEIntf:=TIDEServer.Create(Self);
  FWebIDEIntf.ProjectDir:=ExtractFilePath(FEProject.FileName);
  FWebIDEIntf.OnClientAdded:=@DoClientCame;
  FWebIDEIntf.OnClientRemoved:=@DoClientLeft;
  FWebIDEIntf.OnRequest:=@DoLogRequest;
  FWebIDEIntf.OnAction:=@DoAction;
  FWebIDEIntf.Active:=True;
  RegisterWidgets;
  SetUpEmbeddedBrowser;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
{$IFDEF WINDOWS}
  with cwDesign do
    begin
    ChromiumBrowser.OnBeforePopup := @cwOnBeforePopup;
    if not CreateBrowser then
       tmrShowChromium.Enabled := True;
    end;
{$ENDIF}
end;

procedure TMainForm.tmrShowChromiumTimer(Sender: TObject);
begin
  tmrShowChromium.Enabled := False;
{$IFDEF WINDOWS}
  With cwDesign do
    if not (CreateBrowser or Initialized) then
      tmrShowChromium.Enabled := True;
{$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TMainForm.SetUpEmbeddedBrowser;

begin
  FCanClose:=False;
  cwDesign:=TChromiumWindow.Create(Self);
  With cwDesign do
    begin
    Parent:=TSBrowser;
    Align:=alClient;
    OnClose:=@cwClose;
    OnBeforeClose:=@cwBeforeClose;
    OnAfterCreated:=@cwAfterCreated;
    end;
  TSInspector.TabVisible:=False;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);

begin
  inherited;
  if (cwDesign <> nil) then
    cwDesign.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);

begin
  inherited;
  if (cwDesign <> nil) then
    cwDesign.NotifyMoveOrResizeStarted;
end;

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

procedure TMainForm.cwBeforeClose(Sender: TObject);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;


procedure TMainForm.cwClose(Sender: TObject);
begin
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  if not(cwDesign.DestroyChildWindow) then
    begin
      FCanClose := True;
      Close;
    end;
end;

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

procedure TMainForm.cwAfterCreated(Sender: TObject);
begin
  // Now the browser is fully initialized we can load the initial web page.
  FAllowGo:=True;
end;

{$ENDIF}

{$IFDEF LINUX}
procedure TMainForm.SetUpEmbeddedBrowser;

begin
  FAllowGo:=True;
  FCanClose:=True;
  FWBDesign:=TWebBrowser.Create(Self);
  With FWBDesign do
    begin
    Parent:=TSBrowser;
    Align:=alClient;
    DesignMode := False;
    SourceView := False;
    ZoomContent := False;
    ZoomFactor := 1;
    { lots of logging }
    OnConsoleMessage:=@wbDesignConsoleMessage;
    OnScriptDialog:=@wbDesignScriptDialog;
    OnError:=@wbDesignError;
    OnFavicon:=@wbDesignFavicon;
    OnHitTest:=@wbDesignHitTest;
    OnLoadStatusChange:=@wbDesignLoadStatusChange;
    OnLocationChange:=@wbDesignLocationChange;
    OnNavigate:=@wbDesignNavigate;
    OnProgress:=@wbDesignProgress;
    OnRequest:=@wbDesignRequest;
    end;
  FWIDesign:=TWebInspector.Create(Self);
  With FWIDesign do
    begin
    Parent:=TSInspector;
    Align:=alClient;
    Active:=True;
    WebBrowser:=FWBDesign;
    end;
  TSInspector.TabVisible:=true;
  PCDesigner.ActivePage:=TSBrowser;
end;

procedure TMainForm.wbDesignConsoleMessage(Sender: TObject; const Message, Source: string; Line: Integer);
begin
  Log('Console message: %s  (%s: %d)',[Message,Source,Line]);
end;


procedure TMainForm.wbDesignError(Sender: TObject; const Uri: string; ErrorCode: LongWord; const ErrorMessage: string;
  var Handled: Boolean);
begin
  Log('Error: %s, code: %d, Message: %s',[URI,ErrorCode,ErrorMessage]);
  Handled:=True;
end;

procedure TMainForm.wbDesignFavicon(Sender: TObject);
begin
  Log('Favicon available/missed');
end;

procedure TMainForm.wbDesignHitTest(Sender: TObject; X, Y: Integer; HitTest: TWebHitTest; const Link, Media: string);
begin
//  Log('Hit test (%d,%d) link: %s, media: %s',[x,y,link,media]);
end;

procedure TMainForm.wbDesignLoadStatusChange(Sender: TObject);
begin
  Log('Load status change');
end;

procedure TMainForm.wbDesignLocationChange(Sender: TObject);
begin
  Log('Location change');
end;

procedure TMainForm.wbDesignNavigate(Sender: TObject; const Uri: string; var aAction: TWebNavigateAction);
begin
  Log('Navigation: %s',[URI]);
  aAction:=naAllow;
end;

procedure TMainForm.wbDesignProgress(Sender: TObject; Progress: Integer);
begin
  Log('Progress: %d',[Progress])
end;

procedure TMainForm.wbDesignRequest(Sender: TObject; var Uri: string);
begin
  if Uri<>FLastEmbeddedURI then
    Log('Embedded browser doing request : %s',[URI]);
  FLastEmbeddedURI:=URI;
end;

procedure TMainForm.wbDesignScriptDialog(Sender: TObject; Dialog: TWebScriptDialog; const Message: string; var Input: string;
  var Accepted: Boolean; var Handled: Boolean);
begin
  Log('Script dialog Message: %s; Input : %s',[message,Input]);
  Accepted:=true;
  Handled:=true;
end;
{$ENDIF}

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
//  TBWidgets.AddControl;;

end;

procedure TMainForm.Log(Msg: String);
begin
  MLog.Lines.Add(Msg);
end;

procedure TMainForm.Log(Fmt: String; Args: array of const);
begin
  Log(Format(Fmt,Args));
end;


end.

