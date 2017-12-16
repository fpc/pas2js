program democlasstopas;

uses JS, Types, SysUtils;

function ClassToPas(Obj: TJSObject): string;
var
  Names: TStringDynArray;
  i, j: Integer;
  t: String;
  p: TJSArray;
  f: TJSFunction;
  Value: JSValue;
begin
  Result:='';
  p:=TJSArray.new;
  while Obj<>nil do
    begin
    Names:=TJSObject.getOwnPropertyNames(Obj);
    for i:=0 to length(Names)-1 do
      begin
      try
        Value:=Obj[Names[i]];
      except
        Result:=Result+'// not readable property "'+Names[i]+'"'+sLineBreak;
      end;
      if jsTypeOf(Value)='function' then
        begin
        f:=TJSFunction(Value);
        t:='function '+f.name+'(';
        for j:=1 to NativeInt(f['length']) do
          begin
          if j>1 then t:=t+';';
          t:=t+'arg'+IntToStr(j)+' : argtype'+IntToStr(j);
          end;
        t:=t+') : returntype;';
        end
      else
        t:=Names[i]+' : vartype;';
      if p.indexOf(t)<0 then
        begin
        p.push(t);
        Result:=Result+t+sLineBreak;
        end;
      end;
    Obj:=TJSObject.getPrototypeOf(Obj);
    if Obj<>nil then
      Result:=Result+'// next getPrototypeOf ...'+sLineBreak;
    end;
end;

procedure ShowRTLProps;
var
  o: TJSObject;
begin
  // get the new JavaScript object:
  asm
  o = window.localStorage; // rtl
  end;
  writeln(ClassToPas(o));
end;

begin
  ShowRTLProps;
end.

