{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Michael Van Canneyt, member of the
    Free Pascal development team

    Dataset which talks to Remobjects Data Abstract server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dadataset;

interface

uses Types, Classes, DB, jsonDataset, JS, rosdk, da, dasdk;

Type
  EDADataset = Class(EDatabaseError);
  TDAConnection = Class;

  { TDADataset }

  TDADataset = class(TBaseJSONDataset)
  private
    FParams: TParams;
    FTableName: String;
    FDAConnection: TDAConnection;
    FWhereClause: String;
    function DataTypeToFieldType(s: String): TFieldType;
    procedure SetParams(AValue: TParams);
  Protected
    function ConvertToDateTime(aField : TField; aValue : JSValue; ARaiseException : Boolean) : TDateTime; override;
    function ConvertDateTimeToNative(aField : TField; aValue : TDateTime) : JSValue; override;
    Procedure MetaDataToFieldDefs; override;
  Public
    constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    function DoGetDataProxy: TDataProxy; override;
    // DA is index based. So create array field mapper.
    function CreateFieldMapper : TJSONFieldMapper; override;
    Procedure CreateFieldDefs(a : TJSArray);
    Property TableName : String Read FTableName Write FTableName;
    Property DAConnection : TDAConnection Read FDAConnection Write FDAConnection;
    Property Params : TParams Read FParams Write SetParams;
    Property WhereClause : String Read FWhereClause Write FWhereClause;
  end;

  TDADataRequest = Class(TDataRequest)
  Public
    Procedure doSuccess(res : JSValue) ;
    Procedure DoFail(response : TJSOBject; fail : String) ;
  End;

  { TDADataProxy }

  TDADataProxy = class(TDataProxy)
  private
    FConnection: TDAConnection;
    function ConvertParams(DADS: TDADataset): TDADataParameterDataArray;
  Protected
    Function GetDataRequestClass : TDataRequestClass; override;
  Public
    Function DoGetData(aRequest : TDataRequest) : Boolean; override;
    Function ProcessUpdateBatch(aBatch : TRecordUpdateBatch): Boolean; override;
    Property Connection : TDAConnection Read FConnection Write FConnection;
  end;

  TDAMessageType = (mtAuto, // autodetect from URL
                    mtBin,  // use BinMessage
                    mtJSON); // Use JSONMessage.
  TDAStreamerType = (stJSON,stBin);

  { TDAConnection }

  TDAConnection = class(TComponent)
  private
    FDataService: TDADataAbstractService;
    FDataserviceName: String;
    FLoginService: TDASimpleLoginService;
    FLoginServiceName: String;
    FMessageType: TDAMessageType;
    FMessage : TROmessage;
    FChannel : TROHTTPClientChannel;
    FOnLoginFailed: TDAFailedEvent;
    FOnLogin: TDALoginSuccessEvent;
    FOnLogout: TDASuccessEvent;
    FOnLogoutailed: TDAFailedEvent;
    FOnLogoutFailed: TDAFailedEvent;
    FStreamerType: TDAStreamerType;
    FURL: String;
    procedure ClearConnection;
    function GetChannel: TROHTTPClientChannel;
    function GetClientID: String;
    Function GetDataService : TDADataAbstractService;
    function GetLoginService: TDASimpleLoginService;
    function GetMessage: TROMessage;
    procedure SetDataserviceName(AValue: String);
    procedure SetLoginServiceName(AValue: String);
    procedure SetMessageType(AValue: TDAMessageType);
    procedure SetURL(AValue: String);
  Protected
    Procedure CreateChannelAndMessage; virtual;
    function DetectMessageType(Const aURL: String): TDAMessageType; virtual;
    Function CreateDataService : TDADataAbstractService; virtual;
    Function CreateLoginService : TDASimpleLoginService; virtual;
  Public
    Constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Returns a non-auto MessageType, but raises exception if it cannot be determined;
    Function EnsureMessageType : TDAMessageType;
    // Returns DataService, but raises exception if it is nil;
    Function EnsureDataservice : TDADataAbstractService;
    // Returns SimpleLoginService, but raises exception if it is nil;
    Function EnsureLoginservice : TDASimpleLoginService;
    // Call this to login. This is an asynchronous call, check the result using OnLoginOK and OnLoginFailed calls.
    Procedure Login(aUserName, aPassword : String);
    Procedure LoginEx(aLoginString : String);
    Procedure Logout;
    // You can set this. If you didn't set this, and URL is filled, an instance will be created.
    Property DataService : TDADataAbstractService Read GetDataService Write FDataService;
    //  You can set this. If you didn't set this, and URL is filled, an instance will be created.
    Property LoginService : TDASimpleLoginService Read GetLoginService Write FLoginService;
    // You can get this to use in other service constructors
    Property Channel : TROHTTPClientChannel Read GetChannel;
    Property Message : TROMessage Read GetMessage;
    // Get client ID
    Property ClientID : String Read GetClientID;
  Published
    // If set, this is the message type that will be used when auto-creating the service. Setting this while dataservice is Non-Nil will remove the reference
    Property MessageType : TDAMessageType Read FMessageType Write SetMessageType;
    // if set, URL is used to create a DataService. Setting this while dataservice is Non-Nil will remove the reference
    Property URL : String Read FURL Write SetURL;
    // DataServiceName is used to create a DataService. Setting this while dataservice is Non-Nil will remove the reference
    Property DataserviceName : String Read FDataserviceName Write SetDataserviceName;
    // LoginServiceName is used to create a login service. Setting this while loginservice is Non-Nil will remove the reference
    Property LoginServiceName : String read FLoginServiceName write SetLoginServiceName;
    // Called when login call is executed.
    Property OnLogin : TDALoginSuccessEvent Read FOnLogin Write FOnLogin;
    // Called when login call failed. When call was executed but user is wrong OnLogin is called !
    Property OnLoginCallFailed : TDAFailedEvent Read FOnLoginFailed Write FOnLoginFailed;
    // Called when logout call is executed.
    Property OnLogout : TDASuccessEvent Read FOnLogout Write FOnLogout;
    // Called when logout call failed.
    Property OnLogOutCallFailed : TDAFailedEvent Read FOnLogoutailed Write FOnLogoutFailed;
    // Streamertype : format of the data package in the message.
    Property StreamerType : TDAStreamerType Read FStreamerType Write FStreamerType;
  end;


implementation

uses strutils, sysutils;

resourcestring
  SErrInvalidDate = '%s is not a valid date value for %s';

{ TDAConnection }


function TDAConnection.GetDataService: TDADataAbstractService;
begin
  if (FDataservice=Nil) then
    FDataservice:=CreateDataService;
  Result:=FDataService;
end;

function TDAConnection.GetLoginService: TDASimpleLoginService;
begin
  if (FLoginService=Nil) then
    FLoginService:=CreateLoginService;
  Result:=FLoginService;
end;

function TDAConnection.GetMessage: TROMessage;
begin
  CreateChannelAndMessage;
  Result:=FMessage;
end;

procedure TDAConnection.SetDataserviceName(AValue: String);
begin
  if FDataserviceName=AValue then Exit;
  ClearConnection;
  FDataserviceName:=AValue;
end;

procedure TDAConnection.SetLoginServiceName(AValue: String);
begin
  if FLoginServiceName=AValue then Exit;
  FLoginServiceName:=AValue;
end;

procedure TDAConnection.SetMessageType(AValue: TDAMessageType);
begin
  if FMessageType=AValue then Exit;
  ClearConnection;
  FMessageType:=AValue;
end;

procedure TDAConnection.ClearConnection;

begin
  FDataservice:=Nil;
  FChannel:=Nil;
  FMessage:=Nil;
end;

function TDAConnection.GetChannel: TROHTTPClientChannel;
begin
  CreateChannelAndMessage;
  Result:=FChannel;
end;

function TDAConnection.GetClientID: String;
begin
  if Assigned(FMessage) then
    Result:=FMessage.ClientID
  else
    Result:='';
end;

procedure TDAConnection.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  ClearConnection;
  FURL:=AValue;
end;

procedure TDAConnection.CreateChannelAndMessage;


begin
  if (FChannel=Nil) then
    FChannel:=TROHTTPClientChannel.New(URL);
  if (FMessage=Nil) then
    Case EnsureMessageType of
      mtBin : fMessage:=TROBINMessage.New;
      mtJSON : fMessage:=TROJSONMessage.New;
    end;
end;

function TDAConnection.DetectMessageType(const aURL: String): TDAMessageType;

Var
  S : String;

begin
  S:=aURL;
  Delete(S,1,RPos('/',S));
  case lowercase(S) of
    'bin' : Result:=mtBin;
    'json' : Result:=mtJSON;
  else
    Raise EDADataset.Create(Name+': Could not determine message type from URL: '+aURL);
  end;
end;


function TDAConnection.CreateDataService: TDADataAbstractService;

begin
  Result:=Nil;
  if URL='' then exit;
  CreateChannelAndMessage;
  Result:=TDADataAbstractService.New(FChannel,FMessage,DataServiceName);
end;

function TDAConnection.CreateLoginService: TDASimpleLoginService;
begin
  Result:=Nil;
  if URL='' then exit;
  CreateChannelAndMessage;
  Result:=TDASimpleLoginService.New(FChannel,FMessage,LoginServiceName);
end;

constructor TDAConnection.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FDataServiceName:='DataService';
  FLoginServiceName:='LoginService';
end;

destructor TDAConnection.Destroy;
begin
  ClearConnection;
  inherited Destroy;
end;

function TDAConnection.EnsureMessageType: TDAMessageType;
begin
  Result:=MessageType;
  if Result=mtAuto then
    Result:=DetectMessageType(URL);
end;

function TDAConnection.EnsureDataservice: TDADataAbstractService;

begin
  Result:=Dataservice;
  if (Result=Nil) then
    Raise EDADataset.Create('No data service available. ');
end;

function TDAConnection.EnsureLoginservice: TDASimpleLoginService;

begin
  Result:=LoginService;
  if (Result=Nil) then
    Raise EDADataset.Create('No login service available. ');
end;

procedure TDAConnection.Login(aUserName, aPassword: String);

begin
  EnsureLoginService.Login(aUserName,aPassword,FOnLogin,FOnLoginFailed);
end;

procedure TDAConnection.LoginEx(aLoginString: String);
begin
  EnsureLoginService.LoginEx(aLoginString,FOnLogin,FOnLoginFailed);
end;

procedure TDAConnection.Logout;
begin
  EnsureLoginService.Logout(FOnLogout,FOnLogoutFailed);
end;

{ TDADataset }

function TDADataset.DataTypeToFieldType(s : String) : TFieldType;

Const
  FieldStrings : Array [TFieldType] of string = (
    '','String', 'Integer', 'LargeInt', 'Boolean', 'Float', 'Date',
    'Time', 'DateTime',  'AutoInc', 'Blob', 'Memo', 'FixedChar',
    'Variant','Dataset');


begin
  if (Copy(S,1,3)='dat') then
    system.Delete(S,1,3);
  Result:=High(TFieldType);
  While (Result>ftUnknown) and Not SameText(FieldStrings[Result],S) do
    Result:=Pred(Result);
  if Result=ftUnknown then
    case LowerCase(s) of
     'widestring' : result:=ftString;
     'currency' : result:=ftFloat;
     'smallint' : result:=ftInteger;
    else
      writeln('Unknown field type:',S)
    end;
end;

procedure TDADataset.SetParams(AValue: TParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

function TDADataset.ConvertToDateTime(aField: TField; aValue: JSValue; ARaiseException: Boolean): TDateTime;
begin
  Result:=0;
  if isDate(aValue) then
    Result:=JSDateToDateTime(TJSDate(aValue))
  else if isString(aValue) then
    Result:=Inherited  ConvertToDateTime(afield,aValue,ARaiseException)
  else
    if aRaiseException then
      DatabaseErrorFmt(SErrInvalidDate,[String(aValue),aField.FieldName],Self);
end;

function TDADataset.ConvertDateTimeToNative(aField: TField; aValue: TDateTime): JSValue;
begin
  Result:=DateTimeToJSDate(aValue);
end;

procedure TDADataset.MetaDataToFieldDefs;

begin
  if Not isArray(Metadata['fields']) then
    exit;
  CreateFieldDefs(TJSArray(Metadata['fields']));
end;

function TDADataset.DoGetDataProxy: TDataProxy;
begin
  Result:=TDADataProxy.Create(Self);
  TDADataProxy(Result).Connection:=DAConnection;
end;

constructor TDADataset.create(aOwner: TComponent);
begin
  inherited;
  DataProxy:=nil;
  FParams:=TParams.Create(Self);
end;

destructor TDADataset.Destroy;
begin
  FreeAndNil(FParams);
  Inherited;
end;

procedure TDADataset.CreateFieldDefs(a: TJSArray);

Var
  I : Integer;
  F : TDAField;
  FO : TJSObject absolute F;
  fn,dt : string;
  fs : Integer;
  FT : TFieldType;
  req : boolean;

begin
  FieldDefs.Clear;
  For I:=0 to A.length-1 do
    begin
    F:=TDAField(A.Elements[i]);
    fn:=F.Name;
    // The JSON streamer does not create all properties :(
    if FO.hasOwnProperty('size') then
      begin

      if isString(FO['size']) then
        fs:=StrToInt(String(FO['size']))
      else if isNumber(FO['size']) then
        fs:=F.Size
      else
        fs:=0;
      end
    else
      fs:=0;
    if FO.hasOwnProperty('type') then
      dt:=F.type_
    else
      dt:='string';
    if FO.hasOwnProperty('required') then
      req:=F.Required
    else
      Req:=false;
    Ft:=DataTypeToFieldType(dT);
    if (ft=ftBlob) and (fs=0) then
      fs:=1;
//    Writeln('FieldDef : ',fn,', ',ft,', ',fs);
    FieldDefs.Add(fn,ft,fs,Req);
    end;
end;

function TDADataset.CreateFieldMapper: TJSONFieldMapper;
begin
  Result := TJSONArrayFieldMapper.Create;
end;

{ TDADataProxy }

function TDADataProxy.ConvertParams(DADS : TDADataset) : TDADataParameterDataArray;

Var
  I : integer;
begin
  Result:=Nil;
  if DADS.Params.Count=0 then
     Exit;
  SetLength(Result,DADS.Params.Count);
  for I:=0 to DADS.Params.Count-1 do
    begin
    Result[i].Name:=DADS.Params[i].Name;
    Result[i].Value:=DADS.Params[i].Value;
//    Writeln('Set param ',Result[i].Name,' to ',Result[i].Value);
    end;
end;

function TDADataProxy.DoGetData(aRequest: TDataRequest): Boolean;

Var
  TN : TDAStringArray;
  TIA : TDATableRequestInfoArray;
  TID : TDATableRequestInfoV5Data;
  TI : TDATableRequestInfoV5;
  Srt : TDAColumnSortingData;
  R : TDADataRequest;
  DADS : TDADataset;
  PA : TDADataParameterDataArray;
  DS : TDADataAbstractService;
begin
  // DA does not support this option...
  if loAtEOF in aRequest.LoadOptions then
    exit(False);
  DADS:=aRequest.Dataset as TDADataset;
  R:=aRequest as TDADatarequest;
  if (Connection=Nil) then
    Raise EDADataset.Create(DADS.Name+': Cannot get data without connection');
  if (DADS.TableName='') then
    Raise EDADataset.Create(DADS.Name+': Cannot get data without tablename');
  DS:=Connection.EnsureDataservice;
  TN:=TDAStringArray.New;
  TN.fromObject([DADS.TableName]);
  TID.maxRecords:=-1;
  TID.IncludeSchema:=True;
  Srt.FieldName:='';
  Srt.SortDirection:='Ascending';
  TID.Sorting:=Srt;
  TID.UserFilter:='';
  if DADS.WhereClause<>'' then
    TID.WhereClause:=DADS.WhereClause;
  PA:=ConvertParams(DADS);
  if Length(PA)>0 then
    TID.Parameters:=Pa;
  TIA:=TDATableRequestInfoArray.new;
  // We need to manually fill the array
  TI:=TDATableRequestInfoV5.New;
  TI.FromObject(TID);
  TJSArray(TIA.items).push(TI);
  DS.GetData(TN,TIA,@R.doSuccess,@R.doFail);
  Result:=True;
end;

function TDADataProxy.GetDataRequestClass: TDataRequestClass;
begin
  Result:=TDADataRequest;
end;

function TDADataProxy.ProcessUpdateBatch(aBatch: TRecordUpdateBatch): Boolean;
begin
  Result:=False;
end;

{ TDADataRequest }

procedure TDADataRequest.DoFail(response: TJSOBject; fail: String);

Var
  O : TJSOBject;
  S : TStringDynArray;
  Msg : String;
  I : Integer;

begin
  if isObject(fail) then
    begin
    O:=TJSOBject(JSValue(fail));
    S:=TJSObject.getOwnPropertyNames(O);
    for I:=0 to Length(S)-1 do
      begin
      msg:=Msg+sLineBreak+S[i];
      Msg:=Msg+' : '+String(O[S[i]]);
      end;
    end
  else
    Msg:=Fail;
  Success:=rrFail;
  ErrorMsg:=Msg;
  DoAfterRequest;
end;

procedure TDADataRequest.doSuccess(res: JSValue);

Var
  S : String;
  Rows : TJSArray;
  DADS : TDADataset;
  DStr : TDADataStreamer;
  DT : TDADatatable;
  I : Integer;

begin
//  Writeln('Data loaded, dataset active: ',Dataset.Active);
  DADS:=Dataset as TDADataset;
  if not Assigned(DADS.DAConnection) then
    Raise EDADataset.Create(DADS.Name+': Cannot process response, connection not available');
  S:=String(Res);
  if (DADS.DAConnection.EnsureMessageType=mtJSON) then
    S:=TROUtil.Frombase64(S);
  Case DADS.DAConnection.StreamerType of
    stJSON : DStr:=TDAJSONDataStreamer.new;
    stBIN: DStr:=TDABIN2DataStreamer.new;
  end;
  DStr.Stream:=S;
  DStr.initializeRead;
  DT:=TDADataTable.New;
  DT.name:=DADS.TableName;
  DStr.ReadDataset(DT);
  Rows:=TJSArray.New;
  for I:=0 to length(DT.rows)-1 do
     Rows.Push(DT.Rows[i].__newValues);
  (Dataset as TDADataset).Metadata:=New(['fields',TJSArray(DT.Fields)]);
  // Data:=aJSON['data'];
  (Dataset as TDADataset).Rows:=Rows;
  Success:=rrOK;
  DoAfterRequest;
end;

end.
