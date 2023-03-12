unit Data.HTMLActions;

interface

uses sysutils, classes, web, Rtl.HTMLEventNames, Rtl.HTMLActions, db;

Type
  TDBCustomHTMLElementAction = class;
  TDBCustomHTMLInputElementAction = Class;
  TDBCustomHTMLButtonElementAction = Class;

  { THTMLActionDataLink }

  THTMLActionDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: string;
    FAction : TDBCustomHTMLElementAction;
    procedure SetFieldName(AValue: string);
  protected
    procedure DatasetChanged; override;
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(aField: TField); override;
    procedure UpdateData; override;
    procedure FocusControl(aField: JSValue); Override;
  public
    constructor Create(aAction : TDBCustomHTMLElementAction);
    function Edit: Boolean; override;
    Function CanModify : Boolean;
    Procedure Bind;
    Procedure UnBind;
    property Action: TDBCustomHTMLElementAction read FAction;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
  end;


  { TDBCustomHTMLInputElementAction }

  { TDBCustomHTMLElementAction }
  TFieldTextData = Record
    Field : TField;
    Value : String;
  end;

  TGetFieldTextEvent = procedure(Sender : TObject; var aData : TFieldTextData) of object;

  TDBCustomHTMLElementAction = class(THTMLCustomElementAction)
  Private
    FLink : THTMLActionDataLink;
    FOnEndEditing: TNotifyEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnStartEditing: TNotifyEvent;
    FOnGetFieldText: TGetFieldTextEvent;
    function GetDataSource: TDatasource;
    function GetField: TField;
    function GetFieldName: String;
    procedure SetDatasource(AValue: TDatasource);
    procedure SetFieldName(AValue: String);
    function TransformFieldText(F: TField; const Value: String): String;
  Protected
    procedure DoKeyDown(aEvent: TJSEvent); virtual;
    Procedure ActiveChanged; virtual;
    Procedure StartEditing; virtual;
    Procedure EndEditing; virtual;
    Procedure LayoutChanged; virtual;
    procedure CheckMaxLength; virtual;
    Property Link : THTMLActionDataLink Read FLink;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure ElementToDataset; virtual;
    Procedure DatasetToElement; virtual;
    Property Field : TField Read GetField;
    Property Datasource : TDatasource Read GetDataSource Write SetDatasource;
    Property FieldName : String Read GetFieldName Write SetFieldName;
    Property OnStartEditing : TNotifyEvent Read FOnStartEditing Write FOnStartEditing;
    Property OnEndEditing : TNotifyEvent Read FOnEndEditing Write FOnEndEditing;
    Property OnLayoutChanged : TNotifyEvent Read FOnLayoutChanged Write FOnLayoutChanged;
    Property OnGetFieldText : TGetFieldTextEvent Read FOnGetFieldText Write FOnGetFieldText;
  end;

  TDBHTMLElementAction = class(TDBCustomHTMLElementAction)
  Published
    Property Events;
    Property CustomEvents;
    Property ElementID;
    Property CSSSelector;
    Property PreventDefault;
    Property StopPropagation;
    Property OnExecute;
    Property BeforeBind;
    Property AfterBind;
    Property Datasource;
    Property FieldName;
    Property OnStartEditing;
    Property OnEndEditing;
    Property OnLayoutChanged;
    Property OnGetFieldText;
  end;

  TDBCustomHTMLInputElementAction = class(TDBCustomHTMLElementAction)
  Private
    procedure DoKeyDown(aEvent: TJSEvent); virtual;
    Procedure ActiveChanged; override;
    Procedure StartEditing; override;
    Procedure EndEditing; override;
    Procedure LayoutChanged; override;
    Property Link : THTMLActionDataLink Read FLink;
  Public
    procedure CheckMaxLength; override;
    Procedure ElementToDataset; override;
    Procedure DatasetToElement; override;
    Procedure BindEvents(aEl : TJSElement); override;
  end;
  
  TDBHTMLInputElementAction = class(TDBCustomHTMLInputElementAction)
  Published
    Property Events;
    Property CustomEvents;
    Property ElementID;
    Property CSSSelector;
    Property PreventDefault;
    Property StopPropagation;
    Property OnExecute;
    Property BeforeBind;
    Property AfterBind;
    Property Datasource;
    Property FieldName;
    Property OnStartEditing;
    Property OnEndEditing;
    Property OnLayoutChanged;
    Property OnGetFieldText;
  end;

  { TButtonActionDataLink }

  TButtonActionDataLink = class(TDataLink)
  private
    FAction: TDBCustomHTMLButtonElementAction;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    Property Action : TDBCustomHTMLButtonElementAction Read Faction;
  public
    constructor Create(aAction: TDBCustomHTMLButtonElementAction);
  end;


  { TDBCustomHTMLButtonElementAction }
  TDBButtonAction = (baFirst,baPrior,baNext,baLast,baEdit,baAppend,baInsert,baPost,baCancel,baApplyUpdates);
  TDBButtonActions = Set of TDBButtonAction;

  TDBCustomHTMLButtonElementAction = class(THTMLCustomElementAction)
  private
    FAction: TDBButtonAction;
    FLink: TButtonActionDataLink;
    FOnDisableControl: TNotifyEvent;
    FOnEnableControl: TNotifyEvent;
    procedure DoButtonClick(aEvent: TJSEvent);
    function GetDataSource: TDatasource;
    procedure SetDatasource(AValue: TDatasource);
  Protected
    procedure EditingChanged; virtual;
    procedure DataSetChanged; virtual;
    procedure ActiveChanged; virtual;
    procedure CheckButtonState; virtual;
    Procedure EnableControl; virtual;
    Procedure DisableControl; virtual;
    Property Link : TButtonActionDataLink Read FLink;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure ExecuteAction; virtual;
    Procedure BindEvents(aEl : TJSElement); override;
    Property ButtonAction : TDBButtonAction Read FAction Write FAction default baPost;
    Property Datasource : TDatasource Read GetDataSource Write SetDatasource;
    Property OnEnableControl : TNotifyEvent Read FOnEnableControl Write FOnEnableControl;
    Property OnDisableControl : TNotifyEvent Read FOnDisableControl Write FOnDisableControl;
  end;

  TDBHTMLButtonElementAction = class(TDBCustomHTMLButtonElementAction)
  Published
    Property Events;
    Property CustomEvents;
    Property ElementID;
    Property CSSSelector;
    Property PreventDefault;
    Property StopPropagation;
    Property OnExecute;
    Property BeforeBind;
    Property AfterBind;
    Property ButtonAction;
    Property Datasource;
    Property OnEnableControl;
    Property OnDisableControl;
  end;

Implementation

uses strutils, rtl.HTMLUtils;

{ TButtonActionDataLink }

procedure TButtonActionDataLink.EditingChanged;
begin
  inherited EditingChanged;
  Action.EditingChanged;
end;

procedure TButtonActionDataLink.DataSetChanged;
begin
  inherited DataSetChanged;
  Action.DatasetChanged;
end;

procedure TButtonActionDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  Action.ActiveChanged;
end;

constructor TButtonActionDataLink.Create(
  aAction: TDBCustomHTMLButtonElementAction);
begin
  Inherited Create;
  Faction:=aAction;
end;

{ TDBCustomHTMLButtonElementAction }

function TDBCustomHTMLButtonElementAction.GetDataSource: TDatasource;
begin
  Result:=Link.DataSource;
end;

procedure TDBCustomHTMLButtonElementAction.SetDatasource(AValue: TDatasource);
begin
  Link.DataSource:=aValue;
end;

procedure TDBCustomHTMLButtonElementAction.EditingChanged;
begin
  CheckButtonState;
end;

procedure TDBCustomHTMLButtonElementAction.DataSetChanged;
begin
  CheckButtonState;
end;

procedure TDBCustomHTMLButtonElementAction.ActiveChanged;

begin
  CheckButtonState;
end;

procedure TDBCustomHTMLButtonElementAction.CheckButtonState;

Var
  doEnable : Boolean;

begin
  DoEnable:=Link.Active;
  if DoEnable then
    With Link.Dataset do
      Case ButtonAction of
      baFirst,
      baPrior : DoEnable:=Not BOF;
      baNext,
      baLast : DoEnable:=Not EOF;
      baEdit,
      baAppend,
      baInsert : DoEnable:=Not (State in dsEditModes);
      baPost : DoEnable:= (State in dsEditModes);
      baCancel : DoEnable:= (State in dsEditModes);
      baApplyUpdates : DoEnable:=GetUpdateCount(True)>0;
    end;
  if DoEnable then
    EnableControl
  else
    DisableControl;
end;

procedure TDBCustomHTMLButtonElementAction.EnableControl;
begin
  if (Element is TJSHTMLButtonElement) then
    TJSHTMLButtonElement(Element).disabled:=False;
  if Assigned(FOnEnableControl) then
    FOnEnableControl(Self);
end;

procedure TDBCustomHTMLButtonElementAction.DisableControl;
begin
  if (Element is TJSHTMLButtonElement) then
    TJSHTMLButtonElement(Element).disabled:=True;
  if Assigned(FOnEnableControl) then
    FOnDisableControl(Self);
end;

constructor TDBCustomHTMLButtonElementAction.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLink:=TButtonActionDataLink.Create(Self);
  Faction:=baPost;
end;

destructor TDBCustomHTMLButtonElementAction.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TDBCustomHTMLButtonElementAction.ExecuteAction;
begin
  if Not Assigned(Link.DataSet) then
    Exit; // Maybe throw excption ?
  With Link.Dataset do
    Case ButtonAction of
      baFirst : First;
      baPrior : Prior;
      baNext : Next;
      baLast : Last;
      baEdit : Edit;
      baAppend : Append;
      baInsert : Insert;
      baPost : Post;
      baCancel : Cancel;
      baApplyUpdates : ApplyUpdates;
    end;
end;

procedure TDBCustomHTMLButtonElementAction.DoButtonClick(aEvent : TJSEvent);

begin
  ExecuteAction;
end;

procedure TDBCustomHTMLButtonElementAction.BindEvents(aEl: TJSElement);

Var
  doBindClick : Boolean;
  aType : String;

begin
  inherited BindEvents(aEl);
  doBindClick:=aEl is TJSHTMLButtonElement;
  if (not doBindClick) and (aEl is TJSHTMLInputElement) then
    begin
    aType:=LowerCase(TJSHTMLInputElement(aEl)._type);
    doBindClick:=(atype='submit') or (atype='reset');
    end;
  if DoBindClick then
    aEl.addEventListener('click',@DoButtonClick);
  CheckButtonState;
end;

{ TDBCustomHTMLElementAction }

function TDBCustomHTMLElementAction.GetDataSource: TDatasource;
begin
  Result:=Link.DataSource;
end;

function TDBCustomHTMLElementAction.GetField: TField;
begin
  Result:=Link.Field;
end;

function TDBCustomHTMLElementAction.GetFieldName: String;
begin
  Result:=Link.FieldName;
end;

procedure TDBCustomHTMLElementAction.SetDatasource(AValue: TDatasource);
begin
  if aValue=Link.DataSource then exit;
  Link.Datasource:=aValue;
end;

procedure TDBCustomHTMLElementAction.SetFieldName(AValue: String);
begin
  Link.FieldName:=aValue;
end;

procedure TDBCustomHTMLElementAction.DoKeyDown(aEvent: TJSEvent);
begin

end;

procedure TDBCustomHTMLElementAction.ActiveChanged;
begin
  if Link.Active then
    begin
    CheckMaxLength;
    DatasetToElement;
    end
  else
    ClearValue;
  if Link.Editing then
    StartEditing
  else
    EndEditing;
end;

procedure TDBCustomHTMLElementAction.StartEditing;
begin
  if Assigned(FOnStartEditing) then
    FOnStartEditing(Self);
end;


procedure TDBCustomHTMLElementAction.EndEditing;
begin
  if Assigned(FOnEndEditing) then
    FOnEndEditing(Self);
end;

procedure TDBCustomHTMLElementAction.LayoutChanged;
begin
  If Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(Self);
end;


constructor TDBCustomHTMLElementAction.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLink:=THTMLActionDataLink.Create(Self);
end;

destructor TDBCustomHTMLElementAction.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TDBCustomHTMLElementAction.ElementToDataset;

begin
  // Do nothing.
end;

procedure TDBCustomHTMLElementAction.CheckMaxLength;

begin
  // Do nothing
end;

Function TDBCustomHTMLElementAction.TransformFieldText(F : TField; const Value: String) : String;

Var
  D : TFieldTextData;

begin
  if not Assigned(FOnGetFieldText) then
    Result:=Value
  else
    begin
    D.Value:=Value;
    D.Field:=F;
    FOnGetFieldText(Self,D);
    Result:=D.Value;
    end;
end;

procedure TDBCustomHTMLElementAction.DatasetToElement;

Var
  F : TField;
  E : TJSHTMLElement;

begin
  F:=Field;
  E:=Element;
  if Not (Assigned(F) and assigned(E)) then
    Exit;
  Value:=TransFormFieldText(Field,F.AsString);
end;


procedure TDBCustomHTMLInputElementAction.BindEvents(aEl: TJSElement);
begin
  inherited BindEvents(aEl);
  aEl.addEventListener(sEventKeyDown,@DoKeyDown);
end;

procedure TDBCustomHTMLInputElementAction.DoKeyDown(aEvent : TJSEvent);

begin
  if aEvent is TJSKeyboardEvent then
    begin
    if not Link.Edit then
      begin
      aEvent.preventDefault;
      aEvent.stopPropagation;
      end;
    end;
end;

procedure TDBCustomHTMLInputElementAction.ActiveChanged;
begin
  inherited ActiveChanged;
end;

procedure TDBCustomHTMLInputElementAction.StartEditing;

begin
  if Element is TJSHTMLInputElement then
    TJSHTMLInputElement(Element).readOnly:=Link.ReadOnly;
  Inherited;
end;

procedure TDBCustomHTMLInputElementAction.EndEditing;
begin
  Inherited;
  if Element is TJSHTMLInputElement then
    TJSHTMLInputElement(Element).readOnly:=Link.ReadOnly;
end;

procedure TDBCustomHTMLInputElementAction.LayoutChanged;
begin
  inherited LayoutChanged;
end;


{ THTMLActionDataLink }

procedure THTMLActionDataLink.SetFieldName(AValue: string);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
end;

procedure THTMLActionDataLink.DatasetChanged;
begin
  inherited DatasetChanged;
  ActiveChanged;
end;

procedure THTMLActionDataLink.ActiveChanged;
begin
  if Active then
    Bind
  else
    UnBind;
  Action.ActiveChanged;
end;

procedure THTMLActionDataLink.EditingChanged;
begin
  if Editing then
    Action.StartEditing
  else
    Action.EndEditing
end;

procedure THTMLActionDataLink.LayoutChanged;
begin
  Action.LayoutChanged;
end;

procedure THTMLActionDataLink.RecordChanged(aField: TField);
begin
  inherited RecordChanged(aField);
  if (aField=FField) or (aField=Nil) then
    Action.DatasetToElement;
end;

procedure THTMLActionDataLink.UpdateData;
begin
  Action.ElementToDataset;
end;

procedure THTMLActionDataLink.FocusControl(aField: JSValue);
begin
  if (aField=Field) then
    Action.FocusControl;
end;

constructor THTMLActionDataLink.Create(aAction: TDBCustomHTMLElementAction);
begin
  Inherited Create;
  FAction:=aAction;
end;

function THTMLActionDataLink.Edit: Boolean;
begin
  if CanModify then
    Inherited Edit;
  Result:=Editing;
end;

function THTMLActionDataLink.CanModify: Boolean;
begin
  Result:=not ReadOnly;
  if Result then
    Result:=(Field<>nil) and Field.CanModify;
end;

procedure TDBCustomHTMLInputElementAction.CheckMaxLength;

Var
  El : TJSHTMLElement;
  iEl : TJSHTMLInputElement absolute El;

begin
  El:=Element;
  If Not (assigned(Field) and Assigned(El)) then exit;
  if Field.DataType<>ftString then
    Exit;
  if (El is TJSHTMLInputElement) and SameText(iEl._type,'text') then
      iel.maxLength:=Field.Size;
end;

procedure TDBCustomHTMLInputElementAction.ElementToDataset;

Var
  F : TField;
  E : TJSHTMLElement;
  EI : TJSHTMLInputElement absolute E;

begin
  F:=Field;
  E:=Element;
  if Not Assigned(F) then
    exit;
  if E is TJSHTMLInputElement then
    begin
    if (EI._type='checkbox') then
      F.AsBoolean:=EI.Checked
    else if SameText(EI._type,'date')then
      Field.AsDateTime:=ExtractDate(EI.value)
    else if SameText(EI._type,'time')then
      Field.AsDateTime:=ExtractTime(EI.value)
    else
      F.AsString:=Value;
    end
  else
    F.AsString:=Value;
end;

procedure TDBCustomHTMLInputElementAction.DatasetToElement;
Var
  F : TField;
  E : TJSHTMLElement;
  EI : TJSHTMLInputElement absolute E;

begin
  F:=Field;
  E:=Element;
  if Not (Assigned(F) and Assigned(E)) then
    Exit;
  Self.Value:=TransFormFieldText(Field,F.AsString);
  if E is TJSHTMLInputElement then
    begin
    if (EI._type='checkbox') then
      EI.Checked:=F.AsBoolean
    else if SameText(EI._type,'date') then
      EI.Value:=TransFormFieldText(F,FormatHTMLDate(F.AsDateTime))
    else if SameText(EI._type,'time') then
      EI.Value:=TransFormFieldText(F,FormatHTMLTime(F.AsDateTime))
    else
      Value:=TransFormFieldText(F,F.AsString);
    end
  else
    Inherited
end;


procedure THTMLActionDataLink.Bind;
begin
  If Assigned(Dataset) and (FieldName<>'') then
    FField:=Dataset.FieldByName(FieldName);
end;

procedure THTMLActionDataLink.UnBind;
begin
  FField:=nil;
end;

end.
