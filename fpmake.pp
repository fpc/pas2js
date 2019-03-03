{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} sysutils, fpmkunit;

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  {$IFDEF WINDOWS}
  // windows
  Result:=((length(TheFilename)>=2) and (TheFilename[1] in ['A'..'Z','a'..'z'])
           and (TheFilename[2]=':'))
     or ((length(TheFilename)>=2)
         and (TheFilename[1]='\') and (TheFilename[2]='\'));
  {$ELSE}
  // unix
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
  {$ENDIF}
end;

Procedure CreateConfigFile(CfgFile,BaseDir : String; rtl_js_dir: string = '');

Var
  F : Text;

  Procedure AddLn(S : String);

  begin
    Writeln(F,S);
  end;

begin
  if not ForceDirectories(ExtractFilePath(CfgFile)) then
    begin
    Writeln(StdErr,'Could not create destination directory ',ExtractFilePath(CfgFile));
    Halt(2);
    end;
  Assign(F,CfgFile);
  try
    Rewrite(F);
  except
    On E : exception do
      begin
      Writeln(StdErr,'Could not create config file ',CfgFile,' : ',E.Message);
      Halt(3);
      end;
  end;
  if (BaseDir<>'') then
    BaseDir:=IncludeTrailingPathDelimiter(BaseDir);
  Addln('#');
  Addln('# Minimal config file for pas2js compiler');
  Addln('#');
  Addln('# -d is the same as #DEFINE');
  Addln('# -u is the same as #UNDEF');
  Addln('#');
  Addln('# Write always a nice logo ;)');
  Addln('-l');
  Addln('');
  Addln('# Display Warnings, Notes and Hints');
  Addln('-vwnh');
  Addln('# If you don''t want so much verbosity use');
  Addln('#-vw');
  Addln('');
  if FilenameIsAbsolute(BaseDir) then
    Addln('-Fu'+BaseDir+'*')
  else
    Addln('-Fu$CfgDir'+PathDelim+BaseDir+'*');
  if rtl_js_dir<>'' then
    if FilenameIsAbsolute(rtl_js_dir) then
      AddLn('-Fu'+rtl_js_dir)
    else
      AddLn('-Fu$CfgDir'+PathDelim+rtl_js_dir);
  Addln('');
  Addln('#IFDEF nodejs');
  Addln('-Jirtl.js');
  Addln('#ENDIF');
  Addln('');
  Addln('# Put all generated JavaScript into one file');
  Addln('-Jc');
  Addln('');
  Addln('# end.');
  Close(F);
end;

Procedure AddInstallFiles(Files : TConditionalDestStrings; ADir,AllowedExt,APrefix : String);

Var
  Info : TSearchRec;
  ADestDir,E : String;
  P : Integer;

begin
  ADestDir:=ADir;
  P:=Pos(PathDelim,ADestDir);
  if (P>0) then
    Delete(ADestDir,1,P);
  ADir:=IncludeTrailingPathDelimiter(ADir);
  ADestDir:=IncludeTrailingPathDelimiter(ADestDir);
  if FindFirst(aDir+AllFilesMask,0,Info)=0 then
    try
      Repeat
      E:=LowerCase(ExtractFileExt(Info.Name));
      if pos(E,AllowedExt)>0 then
        Files.Add(ADir+Info.Name,aPrefix+ADestDir);
      until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
end;

Procedure AddPackageFiles(Files : TConditionalDestStrings; ADir,APrefix : String);

Const
  PackExt = '.pp.pas.inc.lpk';

begin
  AddInstallFiles(Files,'packages'+PathDelim+ADir,packExt,aPrefix);
end;


Procedure AddDemoFiles(Files : TConditionalDestStrings; ADir,APrefix : String);

Const
  DemoExt = '.pp.pas.inc.lpr.lpi.html.md';

begin
  AddInstallFiles(Files,'demo'+PathDelim+ADir,demoExt,APrefix);
end;

Var
  P : TPackage;
  UnitDir,DemoDir,BD: String;

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
    P.Version:='3.3.1';
    P.SourcePath.Add('compiler/utils/pas2js');
    P.UnitPath.Add('compiler/utils/pas2js');
    P.UnitPath.Add('compiler/packages/pastojs/src');
    P.UnitPath.Add('compiler/packages/fcl-passrc/src');
    P.UnitPath.Add('compiler/packages/fcl-js/src');
    P.UnitPath.Add('compiler/packages/fcl-json/src');
{$IFDEF VER3_0}    
    P.UnitPath.Add('compiler/packages/compat');
{$ENDIF}    
    P.UnitPath.Add('compiler/packages/webidl/src');
    P.IncludePath.Add('compiler/packages/pastojs/src');
    P.Dependencies.Clear;
    Defaults.Options.Add('-Sc');
    P.Targets.AddProgram('pas2js.pp');
    P.Targets.AddProgram('webidl2pas.pp');
    {$IF FPC_FULLVERSION>=30101}
    P.Targets.AddLibrary('pas2jslib.pp');
    {$ENDIF}
    P.Targets.AddImplicitUnit('fpjson',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('fppas2js',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('fppjssrcmap',False);
    P.Targets.AddImplicitUnit('jsbase',False);
    P.Targets.AddImplicitUnit('jsonparser',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('jsonreader',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('jsonscanner',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('jssrcmap',False);
    P.Targets.AddImplicitUnit('jstoken',False);
    P.Targets.AddImplicitUnit('jstree',False);
    P.Targets.AddImplicitUnit('jswriter',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('pas2jscompiler',False);
    P.Targets.AddImplicitUnit('pas2jslogger',False);
    P.Targets.AddImplicitUnit('pas2jspparser',False);
    P.Targets.AddImplicitUnit('pas2jsuseanalyzer',False);
    P.Targets.AddImplicitUnit('pas2jsfs',False);
    P.Targets.AddImplicitUnit('pas2jsfilecache',False);
    P.Targets.AddImplicitUnit('pas2jsfileutils',False);
    P.Targets.AddImplicitUnit('pas2jslibcompiler',False);
    P.Targets.AddImplicitUnit('pas2jsfscompiler',False);
    P.Targets.AddImplicitUnit('pas2jscompilercfg',False);
    P.Targets.AddImplicitUnit('pas2jscompilerpp',False);
    P.Targets.AddImplicitUnit('pas2jsutils',False);
    P.Targets.AddImplicitUnit('pasresolveeval',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('pasresolver',False);
    P.Targets.AddImplicitUnit('pastree',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('pasuseanalyzer',False);
    P.Targets.AddImplicitUnit('pparser',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('pscanner',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('pascodegen.o',False);
    P.Targets.AddImplicitUnit('webidldefs',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('webidlscanner',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('webidlparser',False).ResourceStrings:=True;
    P.Targets.AddImplicitUnit('webidltopas',False).ResourceStrings:=True;
    // Determine unit files location
    BD:=IncludeTrailingPathDelimiter(P.GetBinOutputDir(Defaults.BuildCPU,Defaults.BuildOS));
    Case Installer.RunMode of
    rmCompile,rmBuild:
      begin
      if not FileExists(BD+'pas2js.cfg') then
        CreateConfigFile(BD+'pas2js.cfg',SetDirSeparators('../../packages'),
                         SetDirSeparators('../../compiler/utils/pas2js/dist/'));
      end;
    rmInstall,rmArchive,rmZipInstall:
      begin
      // UnitDir = some\path\units\i386-win32\..\..\..\pas2js\
      UnitDir:=ExcludeTrailingPathDelimiter(Defaults.UnitInstallDir);
      UnitDir:=ExcludeTrailingPathDelimiter(ExtractFilePath(UnitDir));
      UnitDir:=ExcludeTrailingPathDelimiter(ExtractFilePath(UnitDir));
      UnitDir:=ExtractFilePath(UnitDir);
      UnitDir:=UnitDir+'pas2js'+PathDelim;
      // Config file
      // Create config file
      CreateConfigFile(BD+'pas2js.cfg',ExtractRelativePath(IncludeTrailingPathDelimiter(Defaults.BinInstallDir),IncludeTrailingPathDelimiter(UnitDir)));
      P.InstallFiles.Add(BD+'pas2js.cfg',Defaults.BinInstallDir);
      P.InstallFiles.Add('compiler/utils/pas2js/dist/rtl.js',IncludeTrailingPathDelimiter(UnitDir)+'rtl');
      AddPackageFiles(P.InstallFiles,'chartjs',UnitDir);
      AddPackageFiles(P.InstallFiles,'dataabstract',UnitDir);
      AddPackageFiles(P.InstallFiles,'fcl-base',UnitDir);
      AddPackageFiles(P.InstallFiles,'fcl-db',UnitDir);
      AddPackageFiles(P.InstallFiles,'fpcunit',UnitDir);
      AddPackageFiles(P.InstallFiles,'jspdf',UnitDir);
      AddPackageFiles(P.InstallFiles,'nodejs',UnitDir);
      AddPackageFiles(P.InstallFiles,'rtl',UnitDir);
      // Demo files
      DemoDir:=IncludeTrailingPathDelimiter(Defaults.ExamplesInstallDir);
      AddDemoFiles(P.InstallFiles,'fcldb',DemoDir);
      AddDemoFiles(P.InstallFiles,'fpcunit',DemoDir);
      AddDemoFiles(P.InstallFiles,'fpreport',DemoDir);
      AddDemoFiles(P.InstallFiles,'hotreload',DemoDir);
      AddDemoFiles(P.InstallFiles,'jquery',DemoDir);
      AddDemoFiles(P.InstallFiles,'rtl',DemoDir);
      end;
    rmDistClean:
      if FileExists(BD+'pas2js.cfg') then
        P.CleanFiles.Add(BD+'pas2js.cfg');
    end;
    Run;
    end;
end.
