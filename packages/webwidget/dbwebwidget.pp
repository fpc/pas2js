{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set - DB-Aware widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dbwebwidget;


{$mode objfpc}
{$h+}

{$define NESTEDCLASSBUG}

interface

uses
  SysUtils, Classes, webwidget, db;

Type
  TCustomDBLoopTemplateWidget = class;

  TDBFieldValueData = Class(TLoopTemplateValue)
    Field : TField;
  end;
  TGetFieldValueEvent = Procedure (Sender : TObject; aData : TDBFieldValueData) of object;

  { TDBLoopTemplateGroup }

  TDBLoopTemplateGroup = Class(TLoopTemplateGroup)
  private
    FFieldList: String;
    FFields : TFPList;
  Public
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    procedure PopulateFields(aDataset: TDataset);
    Function CalcGroupValue(aDataset : TDataset) : String;
    Property Fields : TFPList Read FFields;
  Published
    Property FieldList : String Read FFieldList Write FFieldList;
  end;


  TDBLoopTemplateValueEvent = Procedure (Sender : TObject; aDataset : TDataset; aValue : TLoopTemplateValue) of object;

  { TLoopDatalink }

  TLoopDatalink = class(TDataLink)
  private
    FWidget: TCustomDBLoopTemplateWidget;
  Protected
    procedure ActiveChanged; override;
  Public
    Constructor Create(aWidget :TCustomDBLoopTemplateWidget);
    Property Widget :TCustomDBLoopTemplateWidget Read FWidget;
  end;

  { TCustomDBLoopTemplateWidget }

  TCustomDBLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  private
    FLink : TLoopDatalink;
    FOnFormatField: TGetFieldValueEvent;
    FOnGetGroupValue: TDBLoopTemplateValueEvent;
    function GetDataset: TDataset;
    procedure SetDatasource(AValue: TDatasource);
    Function GetDatasource: TDatasource;
  Protected
    Type
      TDBLoopEnumerator = Class(TLoopEnumerator)
      private
        FBof : Boolean;
        FDataset :  TDataset;
      public
        Procedure SetDataset(aDataset : TDataset);
        Function GetValue(Const aName : String): String; override;
        function MoveNext: Boolean; override;
        Property Dataset : TDataset Read FDataset;
      end;
  Protected
    procedure ActiveChanged; virtual;
    function FormatField(aEnum : TDBLoopEnumerator; aField: TField): String;
    Function CreateLoopEnumerator (aCurrValues : TLoopTemplateValue) : TLoopEnumerator; override;
    function GetGroupValue(aEnum: TLoopEnumerator; aGroupIndex: Integer; aGroup: TLoopTemplateGroup): String; override;
    Class Function CreateGroups(aOwner : TComponent) : TLoopTemplateGroupList; override;
    class Function CreateCurrValues: TLoopTemplateValue; override;
  Protected
    Property Datasource : TDatasource Read GetDatasource Write SetDatasource;
    Property OnFormatField : TGetFieldValueEvent Read FOnFormatField Write FOnFormatField;
    Property OnGetGroupValue : TDBLoopTemplateValueEvent Read FOnGetGroupValue Write FOnGetGroupValue;
  Public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    Property Dataset : TDataset Read GetDataset;
  end;

  { TDBLoopTemplateWidget }

  TDBLoopTemplateWidget = Class(TCustomDBLoopTemplateWidget)
  Published
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
    Property References;
    Property Datasource;
    Property OnFormatField;
    Property Groups;
    Property OnGetGroupValue;
  end;

implementation

{ TLoopDatalink }

procedure TLoopDatalink.ActiveChanged;
begin
  inherited ActiveChanged;
  Widget.ActiveChanged;
end;

constructor TLoopDatalink.Create(aWidget: TCustomDBLoopTemplateWidget);
begin
  Inherited;
  FWidget:=aWidget;
end;

{ TDBLoopTemplateGroup }

procedure TDBLoopTemplateGroup.PopulateFields(aDataset : TDataset);

begin
  if not assigned(aDataset) then
    exit;
  if not assigned(FFields) then
    FFields:=TFPList.Create;
  aDataset.GetFieldList(FFields,FieldList);
end;

function TDBLoopTemplateGroup.CalcGroupValue(aDataset : TDataset): String;

Var
  I : Integer;

begin
  Result:='';
  if Not Assigned(FFields) then
    PopulateFields(aDataset);
  If Assigned(FFields) then
    begin
    For I:=0 to FFields.Count-1 do
      Result:=Result+'|'+TField(FFields[i]).AsString;
    Result:=Result+'|';
    end;
end;

destructor TDBLoopTemplateGroup.Destroy;

begin
  FreeAndNil(FFields);
  inherited Destroy;
end;

procedure TDBLoopTemplateGroup.Assign(Source: TPersistent);

Var
  G : TDBLoopTemplateGroup absolute Source;

begin
  if Source is TDBLoopTemplateGroup then
    FieldList:=G.FieldList;
  inherited Assign(Source);
end;

{ TCustomDBLoopTemplateWidget.TDBLoopEnumerator }

procedure TCustomDBLoopTemplateWidget.TDBLoopEnumerator.SetDataset(aDataset: TDataset);
begin
  FDataset:=aDataset;
  FBOF:=True;
end;

function TCustomDBLoopTemplateWidget.TDBLoopEnumerator.GetValue(const aName: String): String;

Var
  F : TField;

begin
  F:=Dataset.Fields.FindField(aName);
  if Assigned(F) then
    Result:=TCustomDBLoopTemplateWidget(Widget).FormatField(Self,F)
  else
    Result:=inherited GetValue(aName);
end;

function TCustomDBLoopTemplateWidget.FormatField(aEnum : TDBLoopEnumerator; aField : TField) : String;

Var
  F : TDBFieldValueData;

begin
  if Assigned(FOnFormatField) then
    begin
    F:=TDBFieldValueData(aEnum.CurrValues);
    F.Field:=aField;
    F.Value:=aField.AsString;
    FOnFormatField(Self,F);
    Result:=F.Value;
    end
  else
    Result:=aField.AsString;
end;

function TCustomDBLoopTemplateWidget.TDBLoopEnumerator.MoveNext: Boolean;
begin
  If not Assigned(Dataset) then
    exit(False);
  if FBOF then
    FBof:=False
  else
    Dataset.Next;
  Result:=Not Dataset.EOF;
  if Result then
    begin
{$IFDEF NESTEDCLASSBUG}
    asm
    Result = pas.webwidget.TCustomLoopTemplateWidget.TLoopEnumerator.MoveNext.call(this);
    end;
{$ELSE}
    Result:=inherited MoveNext; // Update index;
{$ENDIF}
    end;
end;

{ TCustomDBLoopTemplateWidget }

function TCustomDBLoopTemplateWidget.GetDataset: TDataset;
begin
  if Assigned(Flink.Datasource) then
    Result:=Flink.Datasource.Dataset
  else
    Result:=Nil;
end;

procedure TCustomDBLoopTemplateWidget.SetDatasource(AValue: TDatasource);
begin
  Flink.Datasource:=aValue;
end;

function TCustomDBLoopTemplateWidget.CreateLoopEnumerator(aCurrValues: TLoopTemplateValue): TLoopEnumerator;

Var
  DBL : TDBLoopEnumerator;

begin
  DBL:=TDBLoopEnumerator.Create(Self,aCurrValues);
  DBL.SetDataset(Self.Dataset);
  Result:=DBL;
end;

function TCustomDBLoopTemplateWidget.GetGroupValue(aEnum: TLoopEnumerator;
  aGroupIndex: Integer; aGroup: TLoopTemplateGroup): String;
begin
  if aGroup is TDBLoopTemplateGroup then
    Result:=TDBLoopTemplateGroup(aGroup).CalcGroupValue(Dataset)
  else
    Result:=Inherited GetGroupValue(aEnum,aGroupIndex,aGroup);
  if Assigned(OnGetGroupValue) then
    begin
    aEnum.CurrValues.Name:=aGroup.Name;
    aEnum.CurrValues.Value:=Result;
    OnGetGroupValue(Self,Dataset,aEnum.CurrValues);
    Result:=aEnum.CurrValues.Value;
    end;
end;

class function TCustomDBLoopTemplateWidget.CreateGroups(aOwner: TComponent
  ): TLoopTemplateGroupList;
begin
  Result:=TLoopTemplateGroupList.Create(aOwner,TDBLoopTemplateGroup);
end;

class function TCustomDBLoopTemplateWidget.CreateCurrValues: TLoopTemplateValue;
begin
  Result:=TDBFieldValueData.Create;
end;

constructor TCustomDBLoopTemplateWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLink:=TLoopDatalink.Create(Self);
end;

destructor TCustomDBLoopTemplateWidget.Destroy;
begin
  FreeAndNil(FLink);
  inherited destroy;
end;

function TCustomDBLoopTemplateWidget.GetDatasource: TDatasource;
begin
  Result:=FLink.DataSource;
end;

procedure TCustomDBLoopTemplateWidget.ActiveChanged;
begin
  if FLink.Active then
    Refresh
  else
    UnRender;
end;

end.

