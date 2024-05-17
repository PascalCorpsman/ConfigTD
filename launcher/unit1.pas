(******************************************************************************)
(* ctd_launcher                                                    15.05.2024 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Starter for ctd, checks for updates, ..                      *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - first version that works under Linux                  *)
(*               0.03 - first version that works under Windows                *)
(*                      ADD: Progressbar, checks and improoved flow control   *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, ulauncher;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    ini: TIniFile;
    CTD_Version: TCTD_Version;
    LastCTDIntVersion: integer;
    LastCTDUpdaterVersion: Single;
    Procedure LoadSettings();
    Procedure StoreSettings();

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses unit2, Unit3, UTF8Process, LCLType, lclintf
{$IFDEF Windows}
  , LResources
{$ENDIF}
  ;


{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Idee: Suche nach Updates / einfaches Konfig / Installer
   *)
  CTD_Version := TCTD_Version.Create();
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  caption := format('Config TD, launcher ver. %0.2f', [LauncherVersion / 100]);
  ini := TIniFile.Create('settings.ini');
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  LoadSettings();
End;

Procedure TForm1.LoadSettings;
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  CheckBox2.Checked := ini.ReadBool('Global', 'ShowFPS', false);
  CheckBox1.Checked := ini.ReadBool('Global', 'DarkMode', false);
  CheckBox1.Checked := ini.ReadBool('Global', 'AutoNextWave', true);
  edit1.text := ini.ReadString('Global', 'Joinusername', 'Player 1');
  edit2.text := ini.ReadString('Global', 'Hostusername', 'Player 2');
  edit3.text := ini.ReadString('Global', 'Hostport', '1234');
  edit4.text := ini.ReadString('Global', 'AutoNextWaveDelay', '10');
  ComboBox1.text := ini.ReadString('Global', 'Menupos', 'Right');
  LastCTDIntVersion := ini.ReadInteger('Global', 'LastCTDIntVersion', -1);
  LastCTDUpdaterVersion := strtofloat(ini.ReadString('Global', 'LastCTDUpdaterVersion', '-1'), fm);
End;

Procedure TForm1.StoreSettings;
Begin
  ini.WriteBool('Global', 'ShowFPS', CheckBox2.Checked);
  ini.WriteBool('Global', 'DarkMode', CheckBox1.Checked);
  ini.WriteBool('Global', 'AutoNextWave', CheckBox3.Checked);
  ini.WriteString('Global', 'Joinusername', edit1.text);
  ini.WriteString('Global', 'Hostusername', edit2.text);
  ini.WriteString('Global', 'Hostport', edit3.text);
  ini.WriteString('Global', 'AutoNextWaveDelay', edit4.text);
  ini.WriteString('Global', 'Menupos', ComboBox1.text);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  P: TProcessUTF8;
Begin
  // Launch
  StoreSettings();
  ini.UpdateFile;
  // Short Prechecks
  If (Not FileExists('config_td'{$IFDEF Windows} + '.exe'{$ENDIF})) Or
    (Not FileExists('ctd_server'{$IFDEF Windows} + '.exe'{$ENDIF})) Then Begin
    showmessage('Error, installation not complete, please run "Check for updates"');
    exit;
  End;
  // Run the App ;)
  p := TProcessUTF8.Create(Nil);
  p.Executable := 'config_td'{$IFDEF Windows} + '.exe'{$ENDIF};
  p.Execute;
  p.free;
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  tmpFolder: String;
{$IFDEF Windows}
  st: TLazarusResourceStream;
{$ENDIF}
Begin
  ClearLog();
  // 1. Prüfen ob wir überhaupt die Fähigkeit haben https zu sprechen
{$IFDEF Windows}
  If Not FileExists('ssleay32.dll') Then Begin
    // https://wiki.freepascal.org/Lazarus_Resources
    st := TLazarusResourceStream.Create('SSL_DLL', Nil);
    Try
      st.SaveToFile(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll');
    Except
      On av: exception Do Begin
        log(av.Message);
        st.free;
        exit;
      End;
    End;
    st.free;
  End;
{$ENDIF}
  // 2. Download der Version Info
  tmpFolder := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update' + PathDelim;
  log('Tempfolder: ' + tmpFolder);
  If Not ForceDirectories(tmpFolder) Then Begin
    log('Error, could not create: ' + tmpFolder);
    exit;
  End;
  If Not DownloadFile(VersionInfoUrl, tmpFolder + 'ctd_version.json') Then Begin
{$IFDEF Linux}
    log('try installing ssl support: sudo aptitude install libssl-dev');
{$ENDIF}
    exit;
  End;
  If Not CTD_Version.LoadFromFile(tmpFolder + 'ctd_version.json') Then exit;
  log('Online version: ' + format('%0.5f', [CTD_Version.Version]));
  log('Local version: ' + format('%0.5f', [LastCTDUpdaterVersion]));
  log('Online launcher version: ' + format('%0.2f', [CTD_Version.LauncherVersion / 100]));
  log('Local launcher version: ' + format('%0.2f', [LauncherVersion / 100]));
  form3.InitWith(CTD_Version, trunc(CTD_Version.Version * 100000) <> trunc(LastCTDUpdaterVersion * 100000));
  If form3.GetFilesToDLCount() = 0 Then Begin
    showmessage('CTD is up to date, not "checked" are the .zip files.');
  End;
  form3.ShowModal;
  log('Finished');
End;

Procedure TForm1.Button4Click(Sender: TObject);

  Procedure Clear(SectionName: String);
  Begin
    ini.DeleteKey(SectionName, 'Top');
    ini.DeleteKey(SectionName, 'Left');
    ini.DeleteKey(SectionName, 'Width');
    ini.DeleteKey(SectionName, 'Height');
  End;

Begin
  // Löschen aller felder die Fenster Positionen platt machen
  Clear('MainForm');
  Clear('AskDifficultyForm');
  Clear('HostForm');
  Clear('LoadMapForm');
  Clear('MapEditorForm');
  Clear('JoinForm');
  Clear('ChatForm');
  Clear('QuestionMapDlgForm');
  Clear('OptionsForm');
  Clear('QuestionHeroDlgForm');
  Clear('QuestionOppDlgForm');
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  openurl('https://github.com/PascalCorpsman/ConfigTD');
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  CTD_Version.free;
  CTD_Version := Nil;
  ini.free;
  ini := Nil;
End;
{$IFDEF Windows}
Initialization
{$I ctd_launcher.lrs}
{$ENDIF}

End.

