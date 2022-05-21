unit hfIndex;

{$mode ObjFPC}

interface

uses
  JS, SysUtils, Classes, htmlfragment, bootstraptablewidget, jsondataset,
  localjsondataset, DB;

type

  { TfrmIndex }

  TfrmIndex = class(THTMLFragment)
    tbCountries: TDBBootstrapTableWidget;
    dsCountries: TDataSource;
    jdsCountries: TLocalJSONDataset;
    procedure DataModuleCreate(Sender: TObject);
  private
  public

  end;

var
  frmIndex: TfrmIndex;

implementation

Var
  Countries : TJSArray; external name 'countries';

{$R *.lfm}

{ TfrmIndex }

procedure TfrmIndex.DataModuleCreate(Sender: TObject);
begin
  jdsCountries.Rows:=Countries;
  jdsCountries.Open;
end;

end.

