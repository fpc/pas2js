unit JOB_JS;

{$mode ObjFPC}{$H+}

interface

uses
  JOB_Shared, JOB_WAsm;

type
  IJSDate = interface(IJSObject)
    ['{F12818EA-542E-488C-A3C5-279E05639E9E}']
    function toLocaleDateString: UnicodeString; overload; // date in locale timezone, no time
  end;

  { TJSDate }

  TJSDate = class(TJSObject,IJSDate)
  public
    class function Create(aYear: NativeInt; aMonth: NativeInt; aDayOfMonth: NativeInt = 1;
      TheHours: NativeInt = 0; TheMinutes: NativeInt = 0; TheSeconds: NativeInt = 0;
      TheMilliseconds: NativeInt = 0): IJSDate;
    function toLocaleDateString: UnicodeString; overload; // date in locale timezone, no time
  end;

var
  JSDate: TJSDate;

implementation

{ TJSDate }

class function TJSDate.Create(aYear: NativeInt; aMonth: NativeInt;
  aDayOfMonth: NativeInt; TheHours: NativeInt; TheMinutes: NativeInt;
  TheSeconds: NativeInt; TheMilliseconds: NativeInt): IJSDate;
begin
  Result:=JSDate.NewJSObject([aYear,aMonth,aDayOfMonth,TheHours,TheMinutes,TheSeconds,TheMilliseconds],TJSDate) as IJSDate;
end;

function TJSDate.toLocaleDateString: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toLocaleDateString',[]);
end;

initialization
  JSDate:=TJSDate.CreateFromID(JOBObjIdDate);

end.

