(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ctd_launcher                                          *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ulauncher;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uJSON;

Const
  VersionInfoUrl = 'https://raw.githubusercontent.com/PascalCorpsman/ConfigTD/main/bin/ctd.version'; // URL zum DL der Versionsinfo .JSON
  (*
   * Historie : 0.01 = Initial version
   *            0.02 = first version that works under Linux
   *            0.03 = first version that works under Windows
   *                   ADD: Progressbar, checks and improoved flow control
   *            0.04 = Progressbar in byte not filecount
   *            0.05 = FIX: Download progress calculated wron
   *                   ADD: dummy image
   * -Released- 0.06 = DEL: uupdate.pas
   * -Released- 0.07 = FIX: correct placings of log dialog
   *                   FIX: logic for force was wrong
   * -Released- 0.08 = ADD: improoved UI and dialogs
   *                   Add: both ssL dll's for windows version
   * -Released- 0.09 = FIX: SSL Loader did not work on Windows 11
   * -Released- 0.10 = FIX: Close log on close "check for Updates
   * -Released- 0.11 = FIX: Autonextlevel wurde falsch geladen
   *                   FIX: "start next wave each" was not visible
   *                   ADD: Improve error message, if launcher needs update and no updater is present.
   * -Released- 0.12 = FIX: Übernehmen der Localen Version, wenn config_td binary runter geladen wurde.
   * -Released- 0.13 = ADD: Improve download progress view
   * -Released- 0.14 = FIX: High-DPI view
   *                   ADD: Auslesen Filelist.json anstatt .zip des repo's zu ziehen
   *                   ADD: Datensparsames Downloaden der Filelist
   *            0.15 =
   *
   * Known Bugs :
   *)
  LauncherVersion: integer = 15;

Type

  TFileKind = (fkFile, fkZip, fkExecutable, fkJSON);

  TFile = Record
    Kind: TFileKind;
    URL: String;
    Hash: String;
    Size: int64;
    Size2: int64; // Nur für executables
    InFileOffset: String; // Damit kann noch ein Teil des Ziel Dateinamens platt gemacht werden..
    Hash2: String; // Nur für executables
    Filename: String; // Nur für File / executable
    Description: String; // Nur für Zip
  End;

  TFiles = Array Of TFile;

  { TCTD_Version }

  TCTD_Version = Class
  private
    fFormat: TFormatSettings;
    fLauncherVersion: integer;
    fVersion: Single;
    fVersionText: String;
    fdownload_base: TFiles;
    fdownload: TFiles;
    Procedure Clear;
    Function getDownload(index: integer): TFile;
    Function getDownloadBase(index: integer): TFile;
    Function getDownloadBaseCount: integer;
    Function getDownloadCount: integer;
  public
    Property DownloadBaseCount: integer read getDownloadBaseCount;
    Property DownloadBase[index: integer]: TFile read getDownloadBase;
    Property DownloadCount: integer read getDownloadCount;
    Property Download[index: integer]: TFile read getDownload;

    Property Version: Single read fVersion;
    Property LauncherVersion: integer read fLauncherVersion;
    Property VersionText: String read fVersionText;

    Constructor Create();
    Destructor Destroy; override;
    Function LoadFromFile(Const Filename: String): Boolean;
  End;

Procedure ClearLog();
Procedure Log(Logtext: String);

Function FileSizeToString(Value: Int64): String;

Implementation

Uses unit1, unit2;

Procedure ClearLog();
Begin
  form2.Memo1.Clear;
  If FileExists('ctd_launcher.log') Then
    DeleteFile('ctd_launcher.log');
End;

Procedure Log(Logtext: String);
Var
  f: TextFile;
Begin
  form2.Memo1.Append(Logtext);
  assignfile(f, 'ctd_launcher.log');
  If FileExists('ctd_launcher.log') Then
    append(f)
  Else
    Rewrite(f);
  writeln(f, Logtext);
  CloseFile(f);
  If Not form2.Visible Then Begin
    form2.top := form1.top;
    form2.Left := form1.left + form1.Width + 10;
    Form2.Show;
  End;
End;

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
End;

{ TCTD_Version }

Procedure TCTD_Version.Clear;
Begin
  fVersion := 0;
  fLauncherVersion := 0;
  fVersionText := '';
  setlength(fdownload_base, 0);
  setlength(fdownload, 0);
End;

Function TCTD_Version.getDownload(index: integer): TFile;
Begin
  result := fdownload[index];
End;

Function TCTD_Version.getDownloadBase(index: integer): TFile;
Begin
  result := fdownload_base[index];
End;

Function TCTD_Version.getDownloadBaseCount: integer;
Begin
  result := length(fdownload_base);
End;

Function TCTD_Version.getDownloadCount: integer;
Begin
  result := length(fdownload);
End;

Constructor TCTD_Version.Create;
Begin
  Inherited create;
  fFormat := DefaultFormatSettings;
  fFormat.DecimalSeparator := '.';
  fdownload_base := Nil;
  fdownload := Nil;
  Clear;
End;

Destructor TCTD_Version.Destroy;
Begin

End;

Function TCTD_Version.LoadFromFile(Const Filename: String): Boolean;

  Function LoadFile(Const jn: TJSONNode): TFile;
  Var
    k: String;
    jno: TJSONNodeObj;
    jv: TJSONValue;
  Begin
    jno := jn.Obj[0] As TJSONNodeObj;
    k := lowercase(jno.Name);
    Case k Of
      'file': result.Kind := fkFile;
      'zip': result.Kind := fkZip;
      'json': result.Kind := fkJSON;
    End;
    result.Hash2 := '';
    result.Description := '';
    result.InFileOffset := '';
    result.Size := 0;
    result.Size2 := 0;
    jv := jno.FindPath('InFileOffset') As TJSONValue;
    If assigned(jv) Then result.InFileOffset := (jv).Value;
    jv := jno.FindPath('Size') As TJSONValue;
    If assigned(jv) Then result.Size := StrToInt64((jv).Value);
    If (result.Kind = fkFile) Then Begin
      result.Filename := (jno.FindPath('Filename') As TJSONValue).Value;
      If ExtractFileExt(lowercase(result.Filename)) = '.exe' Then Begin
        result.Kind := fkExecutable;
        result.Hash2 := (jno.FindPath('HASH2') As TJSONValue).Value;
        jv := jno.FindPath('Size2') As TJSONValue;
        If assigned(jv) Then result.Size2 := StrToInt64((jv).Value);
      End;
      result.Hash := (jno.FindPath('HASH') As TJSONValue).Value;
    End
    Else Begin
      result.Filename := '';
      result.Hash := '';
      jv := (jno.FindPath('Description') As TJSONValue);
      If assigned(jv) Then Begin
        result.Description := jv.Value;
      End
      Else Begin
        result.Description := '';
      End;
    End;
    result.URL := (jno.FindPath('URL') As TJSONValue).Value;
  End;

  Procedure LoadFiles(Const ja: TJSONArray; Var container: TFiles);
  Var
    i: Integer;
  Begin
    setlength(container, ja.ObjCount);
    For i := 0 To ja.ObjCount - 1 Do Begin
      container[i] := LoadFile(ja.Obj[i] As TJSONNode);
    End;
  End;

Var
  p: TJSONParser;
  sl: TStringList;
  jn: TJSONNode;
  jo: TJSONObj;
  ja: TJSONArray;
Begin
  log('Load: ' + FIlename);
  result := false;
  Clear;
  p := TJSONParser.Create;
  sl := TStringList.Create;
  sl.LoadFromFile(Filename);
  Try
    jo := p.Parse(sl.Text);
  Except
    On av: exception Do Begin
      log('Error: ' + av.Message);
      p.free;
      exit;
    End;
  End;
  sl.free;
  If Not assigned(jo) Then Begin
    jo.free;
    Log('Error, no data parsed.');
    p.free;
    exit;
  End;
  jn := jo.FindPath('config_td') As TJSONNode;
  If Not assigned(jn) Then Begin
    jo.free;
    Log('Error, no valid data parsed.');
    p.free;
    exit;
  End;
  Try
    fVersion := StrToFloat((jn.FindPath('Version') As TJSONValue).Value, fFormat);
    fLauncherVersion := StrToint((jn.FindPath('LauncherVersion') As TJSONValue).Value);
    fVersionText := (jn.FindPath('VersionText') As TJSONValue).Value;
    ja := jn.FindPath('Download_core') As TJSONArray;
    If assigned(ja) Then Begin
      LoadFiles(ja, fdownload_base);
    End;
    ja := jn.FindPath('Download') As TJSONArray;
    If assigned(ja) Then Begin
      LoadFiles(ja, fdownload);
    End;
  Except
    On av: exception Do Begin
      jo.free;
      Log('Error, no valid data parsed: ' + av.Message);
      p.free;
      exit;
    End;
  End;
  result := true;
  p.free;
End;

End.

