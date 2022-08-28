unit BrowserApp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Types, JS, web, CustApp;

type
  TServiceWorkerRegisteredEvent = reference to procedure(Registration: TJSServiceWorkerRegistration);

  { TBrowserApplication }

  TBrowserApplication = class(TCustomApplication)
  private
    FShowExceptions: Boolean;
  protected
    function GetHTMLElement(aID : String) : TJSHTMLElement;
    function CreateHTMLElement(aTag : String; aID : String = '') : TJSHTMLElement;
    procedure DoRun; override;
    function GetConsoleApplication: boolean; override;
    Function LogGetElementErrors : Boolean; virtual;
    function GetLocation: String; override;
  public
    Constructor Create(aOwner: TComponent); override;
    Destructor destroy; override;
    procedure GetEnvironmentList(List: TStrings; NamesOnly: Boolean); override;
    procedure ShowException(E: Exception); override;
    procedure HandleException(Sender: TObject); override;
    function RegisterServiceWorker(aFile: String;
      const aOnRegistered: TServiceWorkerRegisteredEvent = Nil; DoLogging: Boolean = false) : Boolean;
    Property ShowExceptions : Boolean Read FShowExceptions Write FShowExceptions;
  end;

procedure ReloadEnvironmentStrings;

implementation

var
  EnvNames: TJSObject;
  Params : TStringDynArray;
  AppInstance : TBrowserApplication;

procedure ReloadEnvironmentStrings;

var
  I : Integer;
  S,N : String;
  A,P : TStringDynArray;
begin
  if Assigned(EnvNames) then
    FreeAndNil(EnvNames);
  EnvNames:=TJSObject.new;
  S:=Window.Location.search;
  S:=Copy(S,2,Length(S)-1);
  A:=TJSString(S).split('&');
  for I:=0 to Length(A)-1 do
    begin
    P:=TJSString(A[i]).split('=');
    N:=LowerCase(decodeURIComponent(P[0]));
    if Length(P)=2 then
      EnvNames[N]:=decodeURIComponent(P[1])
    else if Length(P)=1 then
      EnvNames[N]:=''
    end;
end;

procedure ReloadParamStrings;

begin
  SetLength(Params,1);
  Params[0]:=Window.location.pathname;
end;


function GetParamCount: longint;
begin
  Result:=Length(Params)-1;
end;

function GetParamStr(Index: longint): String;
begin
  Result:=Params[Index]
end;

function MyGetEnvironmentVariable(Const EnvVar: String): String;

Var
  aName : String;

begin
  aName:=Lowercase(EnvVar);
  if EnvNames.hasOwnProperty(aName) then
    Result:=String(EnvNames[aName])
  else
    Result:='';
end;

function MyGetEnvironmentVariableCount: Integer;
begin
  Result:=length(TJSOBject.getOwnPropertyNames(envNames));
end;

function MyGetEnvironmentString(Index: Integer): String;
begin
  Result:=String(EnvNames[TJSOBject.getOwnPropertyNames(envNames)[Index]]);
end;

{ TBrowserApplication }

function DoFindGlobalComponent(const aName: string): TComponent;
begin
  if Assigned(AppInstance) then
    Result:=AppInstance.FindComponent(aName)
  else
    Result:=Nil;
end;

function TBrowserApplication.GetHTMLElement(aID: String): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.getElementById(aID));
  if (Result=Nil) and LogGetElementErrors then
    Writeln('Could not find element with ID ',aID);
end;

function TBrowserApplication.CreateHTMLElement(aTag: String; aID: String): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.createElement(aTag));
  if aID<>'' then
    Result.ID:=aID;
end;

procedure TBrowserApplication.DoRun;
begin
  // Override in descendent classes.
end;

function TBrowserApplication.GetConsoleApplication: boolean;
begin
  Result:=true;
end;

function TBrowserApplication.LogGetElementErrors: Boolean;
begin
  Result:=True;
end;

function TBrowserApplication.GetLocation: String;
begin
  Result:=''; // ToDo ExtractFilePath(GetExeName);
end;

constructor TBrowserApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ShowExceptions:=True;
  if AppInstance=Nil then
     begin
     AppInstance:=Self;
     RegisterFindGlobalComponentProc(@DoFindGlobalComponent);
     end;

end;

destructor TBrowserApplication.destroy;
begin
  if AppInstance=Self then
    AppInstance:=Nil;
  inherited destroy;
end;

procedure TBrowserApplication.GetEnvironmentList(List: TStrings;
  NamesOnly: Boolean);
var
  Names: TStringDynArray;
  i: Integer;
begin
  Names:=TJSObject.getOwnPropertyNames(EnvNames);
  for i:=0 to length(Names)-1 do
  begin
    if NamesOnly then
      List.Add(Names[i])
    else
      List.Add(Names[i]+'='+String(EnvNames[Names[i]]));
  end;
end;

procedure TBrowserApplication.ShowException(E: Exception);

Var
  S : String;

begin
  if (E<>nil) then
    S:=E.ClassName+': '+E.Message
  else if ExceptObjectJS then
    s:=TJSObject(ExceptObjectJS).toString;
  S:='Unhandled exception caught: '+S;
  if ShowExceptions then
    window.alert(S);
  Writeln(S);
end;

procedure TBrowserApplication.HandleException(Sender: TObject);
begin
  if ExceptObject is Exception then
    ShowException(ExceptObject);
  inherited HandleException(Sender);
end;

function TBrowserApplication.RegisterServiceWorker(aFile: String;
  const aOnRegistered: TServiceWorkerRegisteredEvent; DoLogging: Boolean
  ): Boolean;
begin
  // register service worker if supported
  Result:=HasServiceWorker;
  if Result then
    Window.addEventListener('load',
      procedure()
      begin
        Window.navigator.serviceWorker
          .register(aFile)
          ._then(TJSPromiseResolver(procedure(Registration: TJSServiceWorkerRegistration)
            begin
              if DoLogging then
                console.log('service worker registered');
              if Assigned(aOnRegistered) then
                aOnRegistered(Registration);
            end))
          .catch(TJSPromiseResolver(procedure(err: JSValue)
            begin
              if DoLogging then
                console.log('service worker not registered: '+String(err));
            end));
      end);
end;

initialization
  IsConsole:=true;
  OnParamCount:=@GetParamCount;
  OnParamStr:=@GetParamStr;
  ReloadEnvironmentStrings;
  ReloadParamStrings;
  OnGetEnvironmentVariable:=@MyGetEnvironmentVariable;
  OnGetEnvironmentVariableCount:=@MyGetEnvironmentVariableCount;
  OnGetEnvironmentString:=@MyGetEnvironmentString;

end.

