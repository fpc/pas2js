program demoloadstreamfromurl;

{$mode objfpc}

uses
  browserconsole, Classes;

Var
  SS : TStringStream;

begin
  Writeln('Loading synchronously');
  SS:=TStringStream.Create('');
  try
    SS.LoadFromFile('bytes.txt');
    Writeln('Loaded : ',SS.DataString);
  finally
    SS.Free;
  end;
  Writeln('Loading asynchronously');
  SS:=TStringStream.Create('');
  try
    SS.LoadFromURL('bytes.txt',False,procedure(Sender: tobject)
    begin
    Writeln('Loaded 2: ',SS.DataString);
    end
    )
  finally
    SS.Free;
  end;
  Writeln('Loading non-existing file');
  SS:=TStringStream.Create('');
  try
    SS.LoadFromURL('bytesnonexist.txt',False,procedure(Sender: tobject)
    begin
    Writeln('Loaded 3: ',SS.DataString);
    end
    ,
    procedure(Sender: tobject; Const aError : string)
    begin
    Writeln('Load error: ',aError);
    end
    )
  finally
    SS.Free;
  end;
end.
