{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017-2020 by the Pas2JS development team.

    Unit to send debug info (and console output) to /debugcapture API in simpleserver.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit debugcapture;

{$mode ObjFPC}

interface

uses
  Types, Classes, SysUtils;

Const
  DefaultURL = '/debugcapture';

Type

  { TDebugCaptureClient }

  TDebugCaptureClient = class(TComponent)
  private
    FBufferTimeout: Integer;
    FHookConsole: Boolean;
    FURL: String;
    FCurrent : String;
    FLines : TStringDynArray;
    FOldCallBack : TConsoleHandler;
    FTimeOutID : Integer;
    procedure SetBufferTimeout(AValue: Integer);
    procedure SetHookConsole(AValue: Boolean);
  Protected
    procedure PushLine(aLine: String); virtual;
    procedure DoPush; virtual;
    Procedure DoConsoleWrite(S : JSValue; NewLine : Boolean); virtual;
    Property Lines : TStringDynArray Read FLines;
    Property TimeoutID : Integer Read FTimeOutID;
  Public
    Constructor Create(aOwner : TComponent); override;
    Constructor CustomCreate(aOwner : TComponent; aURL : String; aBufferTimeOut : Integer); overload;
    Constructor CustomCreate(aURL : String; aBufferTimeOut : Integer); overload;
    Destructor Destroy; override;
    Procedure Capture(const aLine : String; NewLine : Boolean = True); virtual;
    Procedure SetConsoleHook;
    Procedure ClearConsoleHook;
    Procedure Flush;
  Published
    Property URL : String Read FURL Write FURL;
    Property BufferTimeout : Integer Read FBufferTimeout Write SetBufferTimeout;
    Property HookConsole : Boolean Read FHookConsole Write SetHookConsole;
  end;

implementation

uses web, js;

{ TDebugCaptureClient }

procedure TDebugCaptureClient.SetBufferTimeout(AValue: Integer);
begin
  if FBufferTimeout=AValue then Exit;
  FBufferTimeout:=AValue;
end;

procedure TDebugCaptureClient.SetHookConsole(AValue: Boolean);
begin
  if FHookConsole=AValue then Exit;
  if aValue then
    SetConsoleHook
  else
    ClearConsoleHook;
end;

procedure TDebugCaptureClient.DoConsoleWrite(S: JSValue; NewLine: Boolean);
begin
  Capture(String(S),NewLine);
  if Assigned(FOldCallBack) then
    FOldCallBack(S,NewLine);
end;

constructor TDebugCaptureClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FURL:=DefaultURL;
  FBufferTimeout:=0; // no buffer
end;

constructor TDebugCaptureClient.CustomCreate(aOwner: TComponent; aURL: String;
  aBufferTimeOut: Integer);
begin
  Create(aOwner);
  URL:=aURL;
  BufferTimeout:=aBufferTimeOut;
end;

constructor TDebugCaptureClient.CustomCreate(aURL: String; aBufferTimeOut: Integer);
begin
  CustomCreate(Nil,aUrl,aBufferTimeout);
end;

destructor TDebugCaptureClient.Destroy;
begin
  if HookConsole then
    ClearConsoleHook;
  Flush;
  inherited Destroy;
end;

procedure TDebugCaptureClient.DoPush;

Var
  aLines : TStringDynArray;
  aBody : String;

begin
  FTimeOutID:=0;
  if Length(FLines)=0 then
    exit;
  aLines:=FLines;
  FLines:=[];
  aBody:=TJSJSON.Stringify(new(['lines',aLines]));
  Window.Fetch(Url,new([
    'method','POST',
    'headers',new(['Content-Type','application.json']),
    'body',aBody
  ]));
end;

procedure TDebugCaptureClient.PushLine(aLine : String);

begin
  TJSArray(FLines).Push(aLine);
  if (FBufferTimeout>0) and (FTimeOutID=0) then
    FTimeOutID:=window.setTimeout(@DoPush,FBufferTimeout)
  else
    DoPush;
end;

procedure TDebugCaptureClient.Capture(const aLine: String; NewLine: Boolean);

Var
  aCurrent : String;

begin
  FCurrent:=FCurrent+aLine;
  if NewLine then
    begin
    aCurrent:=FCurrent;
    FCurrent:='';
    PushLine(aCurrent);
    end;
end;

procedure TDebugCaptureClient.SetConsoleHook;
begin
  FOldCallBack:=SetWriteCallBack(@DoConsoleWrite);
end;

procedure TDebugCaptureClient.ClearConsoleHook;
begin
  SetWriteCallBack(FOldCallBack);
  FOldCallBack:=Nil;
end;

procedure TDebugCaptureClient.Flush;
begin
  if (Length(Flines)>0) or (FCurrent<>'') then
    begin
    if FTimeOutID>0 then
      Window.ClearInterval(FTimeOutID);
    if (FCurrent<>'') then
      begin
      TJSArray(FLines).Push(FCurrent);
      FCurrent:='';
      end;
    DoPush;
    end;
end;

end.

