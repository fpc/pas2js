{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Michael Van Canneyt, member of the
    Free Pascal development team

    Simple SQLDBRESTBridge JSON dataset component and connection.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sqldbrestdataset;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, web, db, JSONDataset, restconnection;

Type

  { TSQLDBRestConnection }

  TSQLDBRestConnection = Class(TRestConnection)
  private
    FConnectionsResourceName: String;
    FCustomViewResourceName: String;
    FDataProperty: String;
    FmetaDataProperty: String;
    FMetaDataResourceName: String;
    FonGetResources: TNotifyEvent;
    FPassword: String;
    FResourceList: TStrings;
    FUserName: String;
    procedure DoResources(Sender: TObject);
    function DoStoreDataProp: Boolean;
    function DoStoreMetadata: Boolean;
    function DoStoreMetadataProp: Boolean;
  Protected
    Procedure SetupRequest(aXHR : TJSXMLHttpRequest); override;
    Function GetUpdateBaseURL(aRequest: TRecordUpdateDescriptor): String; override;
    Function GetReadBaseURL(aRequest: TDataRequest): String; Override;
  Public
    Constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure GetResources(OnResult : TNotifyEvent = Nil);
    Property ResourceList : TStrings Read FResourceList;
  Published
    Property OnGetResources : TNotifyEvent Read FonGetResources Write FOnGetResources;
    Property metaDataProperty : String read FmetaDataProperty Write FmetaDataProperty Stored DoStoreMetadataProp;
    Property DataProperty : String read FDataProperty Write FDataProperty Stored DoStoreDataProp;
    Property MetaDataResourceName : String Read FMetaDataResourceName Write FMetaDataResourceName Stored DoStoreMetadata;
    Property UserName : String Read FUserName Write FUserName;
    Property Password : String Read FPassword Write FPassword;
    Property ConnectionsResourceName : String Read FConnectionsResourceName Write FConnectionsResourceName;
    Property CustomViewResourceName : String Read FCustomViewResourceName Write FCustomViewResourceName;
    // Published from TRESTCOnnection
    Property BaseURL;
    Property PageParam;
    Property OnGetURL;
    Property OnUpdateURL;
  end;

  { TSQLDBRestDataset }

  { TQueryParam }

  TQueryParam = class(TParam)
  private
    FEnabled: Boolean;
  Public
    Procedure Assign(Source : TPersistent); override;
    function AsQuery : String;
  Published
    Property Enabled : Boolean Read FEnabled Write FEnabled;
  end;

  { TQueryParams }

  TQueryParams = Class(TParams)
  private
    function GetP(aIndex : Integer): TQueryParam;
    procedure SetP(aIndex : Integer; AValue: TQueryParam);
  Public
    function AddParam(aName : string; aEnabled : Boolean = False) : TQueryParam;  overload;
    Property Params[aIndex : Integer] : TQueryParam Read GetP Write SetP; default;
  end;

  TGetQueryParamsEvent = Procedure (Sender : TDataset; IsReadURL : Boolean; var QueryString : String) of object;

  TSQLDBRestDataset = Class(TJSONDataset)
  private
    FAutoApplyUpdates: Boolean;
    FConnection: TSQLDBRestConnection;
    FDatabaseConnection: String;
    FOnGetQueryParams: TGetQueryParamsEvent;
    FParams: TQueryParams;
    FResourceID: String;
    FResourceName: String;
    FSQL: TStrings;
    function CleanSQL: String;
    function CustomViewResourceName: String;
    procedure DoSQLChange(Sender: TObject);
    procedure SetConnection(AValue: TSQLDBRestConnection);
    procedure SetParams(AValue: TQueryParams);
    procedure SetResourceID(AValue: String);
    procedure SetResourceName(AValue: String);
    procedure SetSQL(AValue: TStrings);
  Protected
    Procedure DoAfterPost; override;
    Procedure DoAfterDelete; override;
    function MyURL(isRead : Boolean): String; virtual;
    Function CreateQueryParams : TQueryParams; virtual;
    function GetURLQueryParams(IsRead : Boolean): string; virtual;
    function DataPacketReceived(ARequest: TDataRequest): Boolean; override;
    function GetStringFieldLength(F: TJSObject; AName: String; AIndex: Integer): integer;virtual;
    function StringToFieldType(S: String): TFieldType; virtual;
    Function DoGetDataProxy: TDataProxy; override;
    Procedure MetaDataToFieldDefs; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function DefaultBlobDataToBytes(aValue : JSValue) : TBytes; override;
    Class Function DefaultBytesToBlobData(aValue : TBytes) : JSValue; override;
    Function ParamByName(const aName : String) : TQueryParam;
  Published
    // Connection to use to get data
    Property Connection: TSQLDBRestConnection Read FConnection Write SetConnection;
    // The resource to get/post/put/delete
    Property ResourceName : String Read FResourceName Write SetResourceName;
    // When set, the CustomView resource (as set in CustomViewResourceName) is used. Use with care!
    Property SQL : TStrings Read FSQL Write SetSQL;
    // Database connection to use for the resource. Will be appended to URL.
    property DatabaseConnection : String Read FDatabaseConnection Write FDatabaseConnection;
    // Parameters to send (use for filtering)
    Property Params : TQueryParams Read FParams Write SetParams;
    {
      If you want to get a single resource, set the ID of the resource here.
      This is equivalent to setting a parameter ID to the specified value.
    }
    Property ResourceID : String Read FResourceID Write SetResourceID;
    // Get additional parameters with this event.
    Property OnGetQueryParams : TGetQueryParamsEvent Read FOnGetQueryParams Write FOnGetQueryParams;
    // Always immediatly call ApplyUpdates after post and delete.
    Property AutoApplyUpdates : Boolean Read FAutoApplyUpdates Write FAutoApplyUpdates;
  end;

implementation

uses DateUtils;

Type

  { TServiceRequest }

  TServiceRequest = Class(TObject)
  Private
    FOnMyDone,
    FOnDone : TNotifyEvent;
    FXHR: TJSXMLHttpRequest;
    function GetResult: String;
    function GetResultJSON: TJSObject;
    function GetStatusCode: Integer;
    function onLoad(Event{%H-}: TEventListenerEvent): boolean;
  Public
    Constructor Create(Const aMethod,aURL,aUserName,aPassword : String; aOnDone1 : TNotifyEvent; aOnDone2 : TNotifyEvent = Nil);
    Procedure Execute;
    Property RequestResult : String read GetResult;
    Property ResultJSON : TJSObject read GetResultJSON;
    Property OnDone : TNotifyEvent Read FOnDone;
    Property StatusCode : Integer Read GetStatusCode;
  end;

{ TQueryParam }

procedure TQueryParam.Assign(Source: TPersistent);

Var
  P : TQueryParam absolute Source;

begin
  if Source is TQueryParam then
    begin
    FEnabled:=P.Enabled;
    end;
  inherited Assign(Source);
end;

function TQueryParam.AsQuery: String;

var
  B : TBytes;
  I : Integer;

begin
  Result:='';
  if Not Enabled then
    exit;
  Case DataType of
    ftInteger : Result:=IntToStr(AsInteger);
    ftAutoInc,
    ftLargeInt : Result:=IntToStr(AsLargeInt);
    ftBoolean : Result:=IntToStr(Ord(AsBoolean));
    ftFloat : Str(asFloat,Result);
    ftDate : Result:=DateToISO8601(asDateTime);
    ftTime : Result:=DateToISO8601(asDateTime);
    ftDateTime : Result:=DateToISO8601(asDateTime);
    ftBlob :
         begin
         B:=AsBlob;
         Result:='';
         For I:=0 to Length(B)-1 do
           Result:=TJSString(Result).Concat(TJSString.fromCharCode(B[I]));
         end;
    ftMemo : Result:=AsMemo;
  else
    Result:=AsString
  end;
  Result:=Name+'='+Result;
end;

{ TQueryParams }

function TQueryParams.GetP(aIndex : Integer): TQueryParam;
begin
  Result:=Items[aIndex] as TQueryParam
end;

procedure TQueryParams.SetP(aIndex : Integer; AValue: TQueryParam);
begin
  Items[aIndex]:=aValue;
end;

function TQueryParams.AddParam(aName: string; aEnabled: Boolean): TQueryParam;
begin
  Result:=add As TQueryParam;
  Result.Name:=aName;
  Result.Enabled:=aEnabled;
end;

{ TServiceRequest }

constructor TServiceRequest.Create(const aMethod,aURL, aUserName, aPassword: String; aOnDone1 : TNotifyEvent; aOnDone2 : TNotifyEvent = Nil);
begin
  FOnMyDone:=aOnDone1;
  FOnDone:=aOnDone2;
  FXHR:=TJSXMLHttpRequest.New;
  FXHR.AddEventListener('load',@onLoad);
  FXHR.open(aMethod,aURL,true);
(*  else
    begin
//    FXHR.withCredentials := true;
    FXHR.open(aMethod,aURL,true,aUserName,aPassword);
    end;*)
  FXHR.setRequestHeader('Content-Type', 'application/json');
  FXHR.setRequestHeader('Authorization', 'Basic '+window.btoa(aUserName+':'+aPassword));
end;

procedure TServiceRequest.Execute;
begin
  FXHR.send;
end;

function TServiceRequest.GetResult: String;
begin
  Result:=FXHR.responseText;
end;

function TServiceRequest.GetResultJSON: TJSObject;
begin
  if SameText(FXHR.getResponseHeader('Content-Type'),'application/json') then
    Result:=TJSJSON.parseObject(RequestResult)
  else
    Result:=nil;
end;

function TServiceRequest.GetStatusCode: Integer;
begin
  Result:=FXHR.Status;
end;

function TServiceRequest.onLoad(Event: TEventListenerEvent): boolean;
begin
  if Assigned(FOnMyDone) then
    FOnMyDone(Self);
  Result:=False;
end;




{ TSQLDBRestConnection }

function TSQLDBRestConnection.DoStoreMetadata: Boolean;
begin
  Result:=(FMetadataResourceName<>'metadata');
end;

function TSQLDBRestConnection.DoStoreMetadataProp: Boolean;
begin
  Result:=(FMetaDataProperty<>'metaData');
end;

procedure TSQLDBRestConnection.SetupRequest(aXHR: TJSXMLHttpRequest);
begin
  aXHR.setRequestHeader('Content-Type', 'application/json');
  aXHR.setRequestHeader('Accept', 'application/json');
  if (UserName<>'') then
    aXHR.setRequestHeader('Authorization', 'Basic '+window.btoa(UserName+':'+Password));
  // Will call the OnSetupHTTPRequest handler
  inherited SetupRequest(aXHR);
end;

function TSQLDBRestConnection.GetUpdateBaseURL(aRequest: TRecordUpdateDescriptor): String;

Var
  DS : TSQLDBRestDataset;
begin
  Result:=inherited GetUpdateBaseURL(aRequest);
  DS:=TSQLDBRestDataset(aRequest.Dataset);
  Result:=IncludeTrailingPathDelimiter(Result)+DS.MyURL(False);
end;

function TSQLDBRestConnection.GetReadBaseURL(aRequest: TDataRequest): String;

Var
  DS : TSQLDBRestDataset;

begin
  Result:=inherited GetReadBaseURL(aRequest);
  DS:=TSQLDBRestDataset(aRequest.Dataset);
  Result:=IncludeTrailingPathDelimiter(Result)+DS.MyURL(True);
end;

procedure TSQLDBRestConnection.DoResources(Sender: TObject);

Var
  R : TServiceRequest absolute Sender;
  J,Res : TJSObject;
  A : TJSArray;
  i : Integer;

begin
  FResourceList.Clear;
  if (R.StatusCode=200) then
    begin
    J:=R.ResultJSON;
    if J=Nil then
       exit;
    A:=TJSArray(J.Properties['data']);
    For I:=0 to A.Length-1 do
      begin
      Res:=TJSObject(A[i]);
      FResourceList.Add(String(Res.Properties['name']));
      end;
    end;
  If Assigned(R.OnDone) then
    R.OnDone(Self);
  If Assigned(OnGetResources) then
    OnGetResources(Self);
end;

function TSQLDBRestConnection.DoStoreDataProp: Boolean;
begin
  Result:=(FDataProperty<>'data');
end;

constructor TSQLDBRestConnection.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FResourceList:=TStringList.Create;
  FMetaDataResourceName:='metadata';
  FmetaDataProperty:='metaData';
  FDataProperty:='data';
  TStringList(FResourceList).Sorted:=true;
end;

destructor TSQLDBRestConnection.Destroy;
begin
  FreeAndNil(FResourceList);
  inherited Destroy;
end;

procedure TSQLDBRestConnection.GetResources(OnResult: TNotifyEvent);

Var
  aURL : String;
  R : TServiceRequest;

begin
  aURL:=IncludeTrailingPathDelimiter(BaseURL)+MetaDataResourceName+'?fmt=json';
  R:=TServiceRequest.Create('GET',aURL,Self.UserName,Self.Password,@DoResources,OnResult);
  R.Execute;
end;

{ TSQLDBRestDataset }

procedure TSQLDBRestDataset.SetConnection(AValue: TSQLDBRestConnection);
begin
  if FConnection=AValue then Exit;
  if Assigned(FConnection) then
    FConnection.RemoveFreeNotification(Self);
  FConnection:=AValue;
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
end;

procedure TSQLDBRestDataset.SetParams(AValue: TQueryParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

procedure TSQLDBRestDataset.SetResourceID(AValue: String);
begin
  if FResourceID=AValue then Exit;
  CheckInactive;
  FResourceID:=AValue;
end;

function TSQLDBRestDataset.GetURLQueryParams(IsRead :Boolean) : string;

  Procedure AddToResult(aQuery : string);

  begin
    if aQuery='' then
      exit;
    If Result<>'' then
      Result:=Result+'&';
    Result:=Result+aQuery;
  end;

Var
  I : Integer;

begin
  Result:='';
  if IsRead then
    begin
    if SameText(ResourceName,CustomViewResourceName) then
      AddToResult('SQL='+EncodeURIComponent(CleanSQL));
    For I:=0 to Params.Count-1 do
      AddToResult(Params[I].AsQuery);
    end;
  if Assigned(FOnGetQueryParams) then
    FOnGetQueryParams(Self,IsRead,Result);
end;

function TSQLDBRestDataset.MyURL(isRead: Boolean): String;

Var
  Qry : String;

begin
  Result:=DatabaseConnection;
  if (Result<>'') and (Result[Length(Result)]<>'/') then
    Result:=Result+'/';
  Result:=Result+ResourceName;
  if IsRead and (ResourceID<>'') then
    Result:=Result+'/'+EncodeURIComponent(ResourceID);
  Qry:=GetURLQueryParams(IsRead);
  if Qry<>'' then
    Result:=Result+'?'+Qry;
end;

procedure TSQLDBRestDataset.DoSQLChange(Sender: TObject);
begin
  if Trim(FSQL.Text)<>'' then
    FResourceName:=CustomViewResourceName;
end;

procedure TSQLDBRestDataset.SetResourceName(AValue: String);
begin
  if FResourceName=AValue then Exit;
  CheckInactive;
  if Not SameText(aValue,CustomViewResourceName) then
    FSQL.Clear;
  FResourceName:=AValue;
end;

function TSQLDBRestDataset.CustomViewResourceName : String;

begin
  if Assigned(Connection) then
    Result:=Connection.CustomViewResourceName
  else
    Result:='customView';
end;

function TSQLDBRestDataset.CleanSQL: String;

begin
  Result:=StringReplace(SQL.Text,#13#10,' ',[rfReplaceAll]);
  Result:=StringReplace(Result,#10,' ',[rfReplaceAll]);
  Result:=StringReplace(Result,#10,' ',[rfReplaceAll]);
end;

procedure TSQLDBRestDataset.SetSQL(AValue: TStrings);
begin
  if FSQL=AValue then Exit;
  FSQL.Assign(AValue);
end;

procedure TSQLDBRestDataset.DoAfterPost;
begin
  inherited DoAfterPost;
  if AutoApplyUpdates then
    ApplyUpdates;
end;

procedure TSQLDBRestDataset.DoAfterDelete;
begin
  inherited DoAfterDelete;
  if AutoApplyUpdates then
    ApplyUpdates;
end;

function TSQLDBRestDataset.CreateQueryParams: TQueryParams;
begin
  Result:=TQueryParams.Create(Self,TQueryParam);
end;



class function TSQLDBRestDataset.DefaultBlobDataToBytes(aValue: JSValue): TBytes;
begin
  Result:=BytesOf(Window.atob(String(aValue)));
end;

class function TSQLDBRestDataset.DefaultBytesToBlobData(aValue: TBytes
  ): JSValue;
begin
  Result:=Window.Btoa(StringOf(aValue));
end;

function TSQLDBRestDataset.ParamByName(const aName: String): TQueryParam;
begin
  Result:=TQueryParam(Params.ParamByName(aName));
end;

function TSQLDBRestDataset.DoGetDataProxy: TDataProxy;
begin
  Result:=Connection.DataProxy;
end;

function TSQLDBRestDataset.StringToFieldType(S: String): TFieldType;

begin
  if (s='int') then
    Result:=ftInteger
  else if (s='bigint') then
      Result:=ftLargeInt
  else if (s='float') then
    Result:=ftFloat
  else if (s='bool') then
    Result:=ftBoolean
  else if (s='date') then
    Result:=ftDate
  else if (s='datetime') then
    Result:=ftDateTime
  else if (s='time') then
    Result:=ftTime
  else if (s='blob') then
    Result:=ftBlob
  else if (s='string') then
    Result:=ftString
  else
    if MapUnknownToStringType then
      Result:=ftString
    else
      Raise EJSONDataset.CreateFmt('Unknown JSON data type : %s',[s]);
end;

function TSQLDBRestDataset.GetStringFieldLength(F: TJSObject; AName: String;
  AIndex: Integer): integer;

Var
  I,L : Integer;
  D : JSValue;

begin
  Result:=0;
  D:=F.Properties['maxLen'];
  if Not jsIsNan(toNumber(D)) then
    begin
    Result:=Trunc(toNumber(D));
    if (Result<0) then
      Raise EJSONDataset.CreateFmt('Invalid maximum length specifier for field %s',[AName])
    else if Result=0 then // memofield
      Result:=1000000
    end
  else
    begin
    For I:=0 to Rows.Length-1 do
      begin
      D:=FieldMapper.GetJSONDataForField(Aname,AIndex,Rows[i]);
      if isString(D) then
        begin
        l:=Length(String(D));
        if L>Result then
          Result:=L;
        end;
      end;
    end;
  if (Result=0) then
    Result:=20;
end;

procedure TSQLDBRestDataset.MetaDataToFieldDefs;
Var
  A : TJSArray;
  F : TJSObject;
  I,FS : Integer;
  N: String;
  ft: TFieldType;
  D : JSValue;

begin
  FieldDefs.Clear;
  D:=Metadata.Properties['fields'];
  if Not IsArray(D) then
    Raise EJSONDataset.Create('Invalid metadata object');
  A:=TJSArray(D);
  For I:=0 to A.Length-1 do
    begin
    If Not isObject(A[i]) then
      Raise EJSONDataset.CreateFmt('Field definition %d in metadata is not an object',[i]);
    F:=TJSObject(A[i]);
    D:=F.Properties['name'];
    If Not isString(D) then
      Raise EJSONDataset.CreateFmt('Field definition %d in has no or invalid name property',[i]);
    N:=String(D);
    D:=F.Properties['type'];
    If IsNull(D) or isUndefined(D) then
      ft:=ftstring
    else If Not isString(D) then
      begin
      Raise EJSONDataset.CreateFmt('Field definition %d in has invalid type property',[i])
      end
    else
      begin
      ft:=StringToFieldType(String(D));
      end;
    if (ft=ftString) then
      fs:=GetStringFieldLength(F,N,I)
    else
      fs:=0;
    FieldDefs.Add(N,ft,fs);
    end;
end;

constructor TSQLDBRestDataset.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSQL:=TStringList.Create;
  TStringList(FSQL).OnChange:=@DoSQLChange;
  FParams:=CreateQueryParams;
  BlobFormat:=bfBase64;
end;

destructor TSQLDBRestDataset.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndnil(FParams);
  inherited Destroy;
end;

function TSQLDBRestDataset.DataPacketReceived(ARequest: TDataRequest): Boolean;

Var
  O : TJSObject;
  A : TJSArray;
  smetadata,sroot : String;
begin
  Result:=False;
  If isNull(aRequest.Data) then
    exit;
  If isString(aRequest.Data) then
    O:=TJSOBject(TJSJSON.Parse(String(aRequest.Data)))
  else if isObject(aRequest.Data) then
    O:=TJSOBject(aRequest.Data)
  else
    DatabaseError('Cannot handle data packet');
  sRoot:=Connection.DataProperty;
  sMetaData:=Connection.metaDataProperty;
  if (sroot='') then
    sroot:='data';
  if (smetadata='') then
    smetadata:='metaData';
{  if (IDField='') then
    idField:='id';}
  if O.hasOwnProperty(sMetaData) and isObject(o[sMetaData]) then
    begin
    if not Active then // Load fields from metadata
      metaData:=TJSObject(o[SMetaData]);
{    if metaData.hasOwnProperty('idField') and isString(metaData['idField']) then
      IDField:=string(metaData['idField']);}
    end;
  if O.hasOwnProperty(sRoot) and isArray(o[sRoot]) then
    begin
    A:=TJSArray(o[sRoot]);
    Result:=A.Length>0;
    AddToRows(A);
    end;
end;


end.

