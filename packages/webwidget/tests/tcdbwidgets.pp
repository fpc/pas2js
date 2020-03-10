unit tcdbwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, testregistry, dbwebwidget, db, js, jsondataset, tcwidget, web;

Type
  { TBaseTestDBWidget }

  TBaseTestDBWidget = Class(TBaseTestWidget)
  private
    FFieldCount: Integer;
    FRecordCount: Integer;
    FDataset : TDataset;
    FDatasource : TDatasource;
    function GetDataset: TDataset;
    function GetDatasource: TDatasource;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
  Public
    Class Function CreateDataset(aRecordCount : Integer = 2; aFieldCount : Integer = 2) : TDataset;
    Property MyDataset : TDataset Read GetDataset;
    Property MyDatasource : TDatasource Read GetDatasource;
    Property RecordCount : Integer Read FRecordCount Write FRecordCount;
    Property FieldCount : Integer Read FFieldCount Write FFieldCount;
  end;

  TMyDBLoopTemplateWidget = Class(TDBLoopTemplateWidget);

  { TTestSimpleDBloopWidget }

  TTestSimpleDBloopWidget = Class(TBaseTestDBWidget)
  private
    FMy: TMyDBLoopTemplateWidget;
    procedure DoFormatField(Sender: TObject; aData: TDBFieldValueData);
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property My : TMyDBLoopTemplateWidget Read FMy;
  Published
    Procedure TestSetup;
    Procedure TestRender;
    Procedure TestFormatField;
  end;

implementation

Type

  { TMyJSONDataset }

  TMyJSONDataset = Class(TJSONDataset)
  Protected
    Procedure MetaDataToFieldDefs; override;
  Public
    Property Rows;
  end;

{ TTestSimpleDBloopWidget }

Const
  SSimpleHeader = '<ul>';
  SSimpleFooter = '</ul>';
  SSimpleItem = '<li id="{{FI1}}">{{FS2}}</li>';

procedure TTestSimpleDBloopWidget.DoFormatField(Sender: TObject; aData: TDBFieldValueData);
begin
  if aData.Field.Name='FI1' then
    aData.Value:='myf-'+IntToStr(aData.Field.AsInteger);
end;

procedure TTestSimpleDBloopWidget.SetUp;
begin
  inherited SetUp;
  RecordCount:=3;
  FMy:=TMyDBLoopTemplateWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
  FMy.Datasource:=MyDataSource;
  FMy.HeaderTemplate:=SSimpleHeader;
  FMy.FooterTemplate:=SSimpleFooter;
  FMy.ItemTemplate:=SSimpleItem;
end;

procedure TTestSimpleDBloopWidget.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestSimpleDBloopWidget.TestSetup;
begin
  AssertEquals('Recordcount',3,RecordCount);
  AssertNotNull('Have dataset',My);
  AssertNotNull('Have dataset datasource',My.Datasource);
end;

procedure TTestSimpleDBloopWidget.TestRender;

Var
  El : TJSHTMLElement;
  i : Integer;

begin
  My.Refresh;
  For I:=1 to 3 do
    begin
    El:=AssertTree('ul/li('+IntToStr(i)+')');
    AssertEquals('Inner text','FS2_'+IntToStr(I),EL.InnerText);
    end;
end;

procedure TTestSimpleDBloopWidget.TestFormatField;

Var
  El : TJSHTMLElement;
  i : Integer;

begin
  My.OnFormatField:=@DoFormatField;
  My.Refresh;
  For I:=1 to 3 do
    begin
    El:=AssertTree('ul/li('+IntToStr(i)+')');
    AssertEquals('Inner text','FS2_'+IntToStr(I),EL.InnerText);
    end;
end;


{ TMyJSONDataset }

procedure TMyJSONDataset.MetaDataToFieldDefs;
begin
  // Do nothing
end;

function TBaseTestDBWidget.GetDataset: TDataset;
begin
  if FDataset=Nil then
    FDataset:=CreateDataset(FRecordCount,FFieldCount);
  Result:=FDataset;
end;

function TBaseTestDBWidget.GetDatasource: TDatasource;
begin
  if FDatasource=Nil then
    begin
    FDatasource:=TDatasource.Create(Nil);
    FDatasource.Dataset:=MyDataset;
    end;
  Result:=FDatasource;
end;

procedure TBaseTestDBWidget.SetUp;
begin
  inherited SetUp;
  FRecordCount:=2;
  FFieldCount:=2;
end;

procedure TBaseTestDBWidget.TearDown;
begin
  FreeAndNil(FDatasource);
  FreeAndNil(FDataset);
  inherited TearDown;
end;

class function TBaseTestDBWidget.CreateDataset(aRecordCount : Integer = 2; aFieldCount : Integer = 2) : TDataset;

Var
  JD : TMyJSONDataset;
  I,J : integer;
  O : TJSObject;
  A : TJSArray;

begin
  JD:=TMyJSONDataSet.Create(Nil);
  Result:=JD;
  JD.FieldDefs.Add('FI1',ftInteger);
  For I:=2 to aFieldCount do
    JD.FieldDefs.Add('FS'+IntToStr(I),ftString,100);
  JD.RowType:=rtJSONObject;
  A:=TJSArray.New;
  For J:=1 to aRecordCount do
    begin
    O:=TJSObject.New;
    A.Push(O);
    O['FI1']:=J;
    For I:=2 to aFieldCount do
      O['FS'+IntToStr(I)]:='FS'+IntToStr(I)+'_'+IntToStr(J);
    end;
  JD.Rows:=A;
  JD.Open;
end;

begin
//  RegisterTests([TTestSimpleDBloopWidget]);
end.

