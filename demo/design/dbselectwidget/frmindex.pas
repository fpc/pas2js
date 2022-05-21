unit frmindex;

{$mode ObjFPC}

interface

uses
  SysUtils, Classes, Rtl.HTMLActions, htmlfragment, dbwebwidget, webwidget,
  dbhtmlwidgets, DB, Web, jsondataset, localjsondataset;

type

  { TIndexFragment }

  TIndexFragment = class(THTMLFragment)
    actSelectCountry: THTMLElementAction;
    selCountry: TDBSelectWidget;
    DSCountries: TDataSource;
    alIndex: THTMLElementActionList;
    alShowHide: THTMLElementAction;
    jdsCountries: TLocalJSONDataset;
    jdsCountriesContinent: TStringField;
    jdsCountriesContinentFull: TStringField;
    jdsCountriesFlagCode: TStringField;
    jdsCountriesISO2: TStringField;
    jdsCountriesISO3: TStringField;
    jdsCountriesName: TStringField;
    jdsCountriesPrefix: TIntegerField;
    procedure actSelectCountryExecute(Sender: TObject; Event: TJSEvent);
    procedure alShowHideExecute(Sender: TObject; Event: TJSEvent);
    procedure DataModuleRendered(Sender: TObject);
    procedure ltwFlagsGetGroupValue(Sender: TObject;
      aDataset: TDataset; aValue: TLoopTemplateValue);
    procedure jdsCountriesCalcFields(DataSet: TDataSet);
  private

  public

  end;

var
  IndexFragment: TIndexFragment;

implementation

uses JS;

{$R *.lfm}

var
  // In countrycodes.js
  Countries : TJSArray; external name 'countries';


{ TIndexFragment }

procedure TIndexFragment.ltwFlagsGetGroupValue(Sender: TObject;
  aDataset: TDataset; aValue: TLoopTemplateValue);
begin
  aValue.Value:=IntToStr((aDataset.RecNo-1) div 4);
end;

procedure TIndexFragment.jdsCountriesCalcFields(DataSet: TDataSet);

Var
  S : String;

begin
  jdsCountriesFlagCode.AsString:=LowerCase(jdsCountriesISO2.AsString);
  Case jdsCountriesContinent.AsString of
  'AF' : S:='Africa';
  'AN' : S:='Antarctica';
  'AS' : S:='Asia';
  'EU' : S:='Europe';
  'OC' : S:='Oceania';
  'NA' : S:='North America';
  'SA' : S:='South America';
  end;
  jdsCountriesContinentFull.AsString:=S;
end;

procedure TIndexFragment.DataModuleRendered(Sender: TObject);
begin
  jdsCountries.Rows:=Countries;
end;

procedure TIndexFragment.alShowHideExecute(Sender: TObject; Event: TJSEvent);
begin
  if jdsCountries.Active then
    begin
    jdsCountries.Close;
    (sender as THTMLElementAction).Value:='Fill select';
    end
  else
    begin
    if not Assigned(jdsCountries.Rows) then
      jdsCountries.Rows:=Countries;
    jdsCountries.Open;
    (sender as THTMLElementAction).Value:='Clear select';
    end;
end;

procedure TIndexFragment.actSelectCountryExecute(Sender: TObject;  Event: TJSEvent);

Var
  ISO,Country : String;

begin
  ISO:=actSelectCountry.Value;
  if jdsCountries.Locate('ISO2',ISO,[loCaseInsensitive]) then
     Country:=jdsCountriesName.AsString
  else
     Country:='unknown ISO code: '+ISO;
  window.alert('You chose : '+Country+' ('+ISO+')');
end;

end.

