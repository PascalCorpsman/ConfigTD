(******************************************************************************)
(* ctd_launcher                                                    15.05.2024 *)
(*                                                                            *)
(* Version     : see ulauncher.pas                                            *)
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
(* History     : see ulauncher.pas                                            *)
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
    ProtocollVersion: integer;
    Version: Single;
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
  , ssl_openssl_lib, ssl_openssl, blcksock
{$ENDIF}
  ;

{$IFDEF Windows}

Function CheckAndMaybeExtract(Const RessourceDLL: String): Boolean;
Var
  st: TLazarusResourceStream;
Begin
  result := FileExists(RessourceDLL + '.dll');
  If Not result Then Begin
    // https://wiki.freepascal.org/Lazarus_Resources
    st := TLazarusResourceStream.Create(RessourceDLL, Nil);
    Try
      st.SaveToFile(ExtractFilePath(ParamStr(0)) + RessourceDLL + '.dll');
      result := true;
    Except
      On av: exception Do Begin
        log(av.Message);
        st.free;
        exit;
      End;
    End;
    st.free;
  End;
End;
{$ENDIF}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
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
  CheckBox1.Checked := ini.ReadInteger('Global', 'DarkMode', 0) = 1;
  CheckBox3.Checked := ini.ReadBool('Global', 'AutoNextWave', true);
  edit1.text := ini.ReadString('Global', 'Joinusername', 'Player 1');
  edit2.text := ini.ReadString('Global', 'Hostusername', 'Player 2');
  edit3.text := ini.ReadString('Global', 'Hostport', '1234');
  edit4.text := ini.ReadString('Global', 'AutoNextWaveDelay', '10');
  ComboBox1.text := ini.ReadString('Global', 'Menupos', 'Right');
  ProtocollVersion := ini.ReadInteger('Global', 'ProtocollVersion', -1);
  Version := strtofloatdef(ini.ReadString('Global', 'Version', '-1'), -1, fm);
End;

Procedure TForm1.StoreSettings;
Begin
  ini.WriteBool('Global', 'ShowFPS', CheckBox2.Checked);
  ini.WriteInteger('Global', 'DarkMode', ord(CheckBox1.Checked));
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
Begin
  ClearLog();
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
  If Version = -1 Then Begin
    log('Local version: not available');
  End
  Else Begin
    log('Local version: ' + format('%0.5f', [Version]));
  End;
  log('Online launcher version: ' + format('%0.2f', [CTD_Version.LauncherVersion / 100]));
  log('Local launcher version: ' + format('%0.2f', [LauncherVersion / 100]));
  form3.InitWith(CTD_Version, Version = -1);
  If form3.GetFilesToDLCount() = 0 Then Begin
    showmessage(
      'Your version of Config TD is up to date.' + LineEnding + LineEnding +
      'But the additional *.zip files (like the Config TD Data repository) are not checked.' + LineEnding + LineEnding +
      'If you want, you can check these files now manually and update them.'
      );
  End;
  form2.top := form3.top;
  form2.Left := form3.left + form3.Width + 10;
  form3.ShowModal;
  form2.Hide;
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

  // 1. ggf. die Crypto libs entpacken und dann einrichten
{$IFDEF Windows}
  If Not CheckAndMaybeExtract('ssleay32') Then exit;
  If Not CheckAndMaybeExtract('libeay32') Then exit;
{$ENDIF}
  If SSLImplementation = TSSLNone Then Begin
    If InitSSLInterface Then
      SSLImplementation := TSSLOpenSSL;
  End;
{$ENDIF}

End.

