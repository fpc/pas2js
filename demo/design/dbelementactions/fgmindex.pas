unit fgmIndex;

{$mode ObjFPC}

interface

uses
  SysUtils, Classes, js, Rtl.HTMLActions, htmlfragment, Data.HTMLActions,
  jsondataset, localjsondataset, DB, Web;

type

  { ThfgmIndex }

  ThfgmIndex = class(THTMLFragment)
    actbtnCancel: TDBHTMLButtonElementAction;
    actbtnEdit: TDBHTMLButtonElementAction;
    actbtnFirst: TDBHTMLButtonElementAction;
    actbtnLast: TDBHTMLButtonElementAction;
    actbtnNext: TDBHTMLButtonElementAction;
    actbtnPost: TDBHTMLButtonElementAction;
    actbtnPrevious: TDBHTMLButtonElementAction;
    actedtCode: TDBHTMLInputElementAction;
    actedtName: TDBHTMLInputElementAction;
    alIndex: THTMLElementActionList;
    actBtnAppend: TDBHTMLButtonElementAction;
    dsCountries: TDataSource;
    HTMLElementAction1: THTMLElementAction;
    jdsCountries: TLocalJSONDataset;
    jdsCountriescode: TStringField;
    jdsCountriesName: TStringField;
    procedure DataModuleRendered(Sender: TObject);
    procedure DoAutoEdit(Sender: TObject; Event: TJSEvent);
  private

  public

  end;

var
  hfgmIndex: ThfgmIndex;
  countrycodes : TJSArray; external name 'countrycodes';

implementation

{$R *.lfm}

{ ThfgmIndex }

procedure ThfgmIndex.DataModuleRendered(Sender: TObject);
begin
  alIndex.Bind;
  jdsCountries.Rows:=countrycodes;
  jdsCountries.Open;

end;

procedure ThfgmIndex.DoAutoEdit(Sender: TObject; Event: TJSEvent
  );
begin
  dsCountries.AutoEdit:=TJSHTMLInputElement(HTMLElementAction1.Element).checked;
end;

end.

