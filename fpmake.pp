{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_pas2js(const ADirectory: string);

Var
  P : TPackage;
  PT,T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('pas2js');

    P.Author := 'Free Pascal Team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Convert pascal sources to javascript.';
    P.Email := 'michael@freepascal.org';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='3.1.1';
    P.SourcePath.Add('compiler/utils/pas2js');
    P.UnitPath.Add('compiler/utils/pas2js');
    P.UnitPath.Add('compiler/packages/pastojs/src');
    P.UnitPath.Add('compiler/packages/fcl-passrc/src');
    P.UnitPath.Add('compiler/packages/fcl-js/src');
    P.UnitPath.Add('compiler/packages/fcl-json/src');
    P.IncludePath.Add('compiler/packages/pastojs/src');
    Defaults.Options.Add('-Sc');
    PT:=P.Targets.AddProgram('pas2js.pp');
    PT:=P.Targets.AddLibrary('pas2jslib.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_pas2js('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




