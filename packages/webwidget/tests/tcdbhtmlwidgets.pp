unit tcdbhtmlwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, tcwidget, htmlwidgets, testregistry, db, dbhtmlwidgets, jsondataset, tcdbwidgets;

Type


  TMyDBSelectWidget = class(TDBSelectWidget);

  { TTestDBSelectWidget }

  TTestDBSelectWidget= class(TBaseTestDBWidget)
  private
    FMy: TMyDBSelectWidget;
    procedure AssertOption(Idx: Integer; aText, aValue: String; Selected: Boolean=False);
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Procedure Hookup;
  Public
    Property My : TMyDBSelectWidget Read FMy;
  Published
    Procedure TestHookup;
    Procedure TestRender;
  end;


implementation

uses JS, web;

{ TBaseTestDBWidget }


{ TTestDBSelectWidget }

procedure TTestDBSelectWidget.AssertOption(Idx: Integer; aText, aValue: String; Selected: Boolean);

Var
  O : TJSHTMLOptionElement;

begin
  AssertTrue('Correct index',Idx<My.Element.childElementCount);
  O:=My.Element.children[Idx] as TJSHTMLOptionElement;
  AssertEquals('Text',aText,O.InnerText);
  if aValue='' then
    aValue:=aText;
  AssertEquals('Value',aValue,O.Value);
  AssertEquals('Selected',Selected,O.selected);
end;

procedure TTestDBSelectWidget.SetUp;
begin
  inherited SetUp;
  FMy:=TMyDBSelectWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
  FMy.ItemField:='FS2';
  FMy.ValueField:='FI1';
end;

procedure TTestDBSelectWidget.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestDBSelectWidget.Hookup;
begin
  FMy.Datasource:=MyDatasource;
end;

procedure TTestDBSelectWidget.TestHookup;
begin
  AssertNotNull('Have widget',My);
  AssertNull('Have no datasource',My.Datasource);
  AssertEquals('Have parentID',SBaseWindowID,My.ParentID);
  Hookup;
  AssertNotNull('Have datasource',MyDatasource);
  AssertNotNull('Have dataset',MyDataset);
  AssertSame('Have no datasource',MyDatasource,My.Datasource);
  AssertSame('Have dataset',MyDataset,My.Dataset);
end;

procedure TTestDBSelectWidget.TestRender;
begin
  Hookup;
  My.Refresh;
  AssertTree('select/option');
  AssertEquals('Multi',False,My.multiple);
  AssertEquals('SelectedIndex',-1,My.selectedIndex);
  AssertEquals('Amount of option values',2,My.Element.childElementCount);
  AssertOption(0,'FS2_1','1');
  AssertOption(1,'FS2_2','2');
end;


initialization
//  RegisterTests([TTestDBSelectWidget]);
end.

