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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CheckLst, ComCtrls, ulauncher;

Type

  TitemObject = Class
    filecontainer: TFile;
  End;

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckListBox1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    SelfFile: TFile;
    fforce: Boolean;
    fNewVersion: Single;
    AdditionalJSONDownloads: TFiles;
    Procedure CheckAddFile(Const ListBox: TCheckListBox; Const aFile: TFile; Force: Boolean);
    Function dlFile(aFile: TFile): int64;
    Procedure TriggerUpdater(Executable: String);
    Procedure OnFileDownloadUpdateEvent(Sender: TObject; aSize, aTotalSize: Int64);
    Procedure UnpackZipFile(aFile: TFile);
    Procedure PrepareJSONFileForDownloading(aFile: TFile);
    Function ScanForAdditionalFiles(): TFiles;
  public
    Procedure InitWith(Const aVersion: TCTD_Version; Force: Boolean);

    Function GetFilesToDLCount(): Integer;
  End;

Var
  Form3: TForm3;
  StopDownloading: Boolean;

Implementation

Uses lazfileutils, md5, Zipper, FileUtil, process, UTF8Process, LCLType,
  unit1, usynapsedownloader, Unit4,
  uJSON;

{$R *.lfm}

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Update overview';
  Constraints.MinHeight := Height;
  //  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  AdditionalJSONDownloads := Nil;
End;

Procedure TForm3.CheckAddFile(Const ListBox: TCheckListBox; Const aFile: TFile;
  Force: Boolean);
Var
  io: TitemObject;
  fn, s, cfh, fh: String;
  NeedAdd: Boolean;
  i: integer;
Begin
  fforce := Force;
  NeedAdd := Force {Or (aFile.Kind = fkZip)};
  fn := aFile.Filename;
{$IFDEF Windows}
  fn := StringReplace(fn, '/', PathDelim, [rfReplaceAll]);
{$ENDIF}
  fh := aFile.Hash;
{$IFDEF Linux}
  If aFile.Kind = fkExecutable Then Begin
    fn := ExtractFileNameWithoutExt(fn);
    fh := aFile.Hash2;
  End;
{$ENDIF}
  If (Not NeedAdd) And (aFile.Kind In [fkExecutable, fkFile]) Then Begin
    If FileExists(fn) Then Begin
      cfh := MD5Print(MD5File(fn));
      If lowercase(fh) <> lowercase(cfh) Then Begin
        NeedAdd := true;
      End;
    End
    Else Begin
      NeedAdd := true;
    End;
  End;
  Case aFile.Kind Of
    fkFile: s := fn;
    fkExecutable: Begin
        s := fn;
        If fn = extractfilename(ParamStr(0)) Then Begin
          SelfFile := aFile;
          exit;
        End;
      End;
    fkZip: s := aFile.Description;
    fkJSON: s := aFile.Description;
  End;
  io := TitemObject.Create;
  io.filecontainer := aFile;
  i := ListBox.Items.AddObject(s, io);
  ListBox.Checked[i] := NeedAdd;
End;

Function TForm3.dlFile(aFile: TFile): int64;
Var
  dl: TSynapesDownloader;
{$IFDEF Linux}
  pr: TProcessUTF8;
{$ENDIF}
Begin
  result := 0;
{$IFDEF Linux}
  If aFile.Kind = fkExecutable Then Begin
    aFile.Filename := ExtractFileNameWithoutExt(aFile.Filename);
    aFile.URL := copy(aFile.URL, 1, length(aFile.URL) - length('.exe'));
  End;
{$ENDIF}
  If (aFile.Kind = fkZip) Or (aFile.Kind = fkJSON) Then Begin
    aFile.Filename := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update' + PathDelim + ExtractFileName(aFile.URL);
  End;
  dl := TSynapesDownloader.Create;
  dl.OnFileDownloadUpdateEvent := @OnFileDownloadUpdateEvent;
  Try
    label5.caption := ExtractFileName(aFile.Filename);
    If dl.DownloadFile(aFile.URL, aFile.Filename) Then Begin
      result := FileSize(aFile.Filename);
      If aFile.Kind = fkExecutable Then Begin
{$IFDEF LINUX}
        pr := TProcessUTF8.Create(Nil);
        pr.Options := [poWaitOnExit];
        pr.CurrentDirectory := GetCurrentDir;
        pr.Executable := 'chmod';
        pr.Parameters.Add('+x');
        pr.Parameters.Add(aFile.Filename);
        pr.Execute;
        pr.free;
{$ENDIF}
        // Die Locale Versionsanzeige "umbiegen", falls jemand 2 mal Check for update clickt ;)
        If lowercase(ExtractFileName(aFile.Filename)) = 'config_td'{$IFDEF Windows} + '.exe'{$ENDIF} Then Begin
          Form1.StoreVersion(fNewVersion);
        End;
      End;
      If aFile.Kind = fkZip Then Begin
        UnpackZipFile(aFile);
      End;
      If aFile.Kind = fkJSON Then Begin
        PrepareJSONFileForDownloading(aFile);
      End;
    End
    Else Begin
      // Es braucht keine Fehlermeldung, da das DownloadFile schon gemacht hat ..
    End;
  Except
    On av: exception Do Begin
      ShowMessage(av.Message);
    End;
  End;
  dl.free;
  Application.ProcessMessages;
End;

Procedure TForm3.TriggerUpdater(Executable: String);
Var
  p: TProcessUTF8;
Begin
  p := TProcessUTF8.Create(Nil);
{$IFDEF Windows}
  p.Executable := 'ctd_updater.exe';
{$ENDIF}
{$IFDEF LINUX}
  p.Executable := 'ctd_updater';
{$ENDIF}
  p.Parameters.add(Executable);
  p.Execute; // Ab jetzt heist es so schnell wie möglich raus aus der Anwendung
  p.free;
End;

Procedure TForm3.OnFileDownloadUpdateEvent(Sender: TObject; aSize,
  aTotalSize: Int64);
Begin
  ProgressBar1.Max := aTotalSize;
  ProgressBar1.Position := aSize;
  Application.ProcessMessages;
End;

Procedure TForm3.UnpackZipFile(aFile: TFile);
Var
  UnZipper: TUnZipper;
  newRoot, root, source, target, TargetDir: String;
  sl: TStringList;
  i: Integer;
Begin
  UnZipper := TUnZipper.Create;
  Try
    UnZipper.FileName := aFile.Filename;
    If aFile.InFileOffset = '' Then Begin
      UnZipper.OutputPath := ExtractFilePath(ParamStr(0));
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
    End
    Else Begin
      UnZipper.OutputPath := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update';
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
      root := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update' + PathDelim + aFile.InFileOffset;
      sl := FindAllFiles(root);
      root := root + PathDelim;
      newRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
      For i := 0 To sl.count - 1 Do Begin
        source := sl[i];
        target := StringReplace(sl[i], root, newRoot, []);
        TargetDir := ExtractFilePath(target);
        If TargetDir <> '' Then Begin
          ForceDirectories(TargetDir); // Braucht keine Fehlermeldung, weil die unten durch Copyfile auch gemacht wird ;)
        End;
        If Not CopyFile(source, target) Then Begin
          log('Error, could not create: ' + target);
        End;
        // Sollten die Temp daten wieder gelöscht werden oder ist uns dass egal ?
      End;
      sl.free;
    End;
  Except
    On av: exception Do Begin
      log(av.Message);
    End;
  End;
  UnZipper.Free;
End;

Procedure TForm3.PrepareJSONFileForDownloading(aFile: TFile);
Begin
  setlength(AdditionalJSONDownloads, high(AdditionalJSONDownloads) + 2);
  AdditionalJSONDownloads[high(AdditionalJSONDownloads)] := aFile;
End;

Function TForm3.ScanForAdditionalFiles(): TFiles;
Var
  i, j: Integer;
  sl: TStringList;
  jn, jan: TJSONNode;
  jp: TJSONParser;
  ja: TJSONArray;
  b: Boolean;
  hash, FN, filehash: String;
Begin
  result := Nil;
  jp := TJSONParser.Create;
  For i := 0 To high(AdditionalJSONDownloads) Do Begin
    sl := TStringList.Create;
    sl.LoadFromFile(AdditionalJSONDownloads[i].Filename);
    Try
      jn := jp.Parse(sl.Text) As TJSONNode;
    Except
      On av: Exception Do Begin
        showmessage(av.Message);
      End;
    End;
    sl.free;
    If Not assigned(jn) Then Continue;
    ja := jn.FindPath('Files') As TJSONArray;
    If Not assigned(ja) Then Begin
      jn.free;
      Continue;
    End;
    For j := 0 To ja.ObjCount - 1 Do Begin
      jan := ja.Obj[j] As TJSONNode;
      b := false;
      FN := (jan.FindPath('Name') As TJSONValue).Value;
{$IFDEF Windows}
      fn := StringReplace(fn, '/', PathDelim, [rfReplaceAll]);
{$ENDIF}
      hash := (jan.FindPath('HASH') As TJSONValue).Value;
      If FileExistsUTF8(fn) Then Begin
        filehash := MD5Print(MD5File(fn));
        If lowercase(hash) <> lowercase(filehash) Then Begin
          b := true;
        End;
      End
      Else Begin
        b := true;
      End;
      If b Then Begin
        setlength(result, high(result) + 2);
        result[high(Result)].Kind := fkFile;
        result[high(Result)].URL := (jan.FindPath('URL') As TJSONValue).Value;
        result[high(Result)].Hash := hash;
        result[high(Result)].Size := StrToInt64((jan.FindPath('Size') As TJSONValue).Value);
        result[high(Result)].Size2 := 0;
        result[high(Result)].InFileOffset := '';
        result[high(Result)].Hash2 := '';
        result[high(Result)].Filename := FN;
        result[high(Result)].Description := '';
      End;
    End;
    jn.free;
  End;
  jp.free;
End;

Function TForm3.GetFilesToDLCount: Integer;
Var
  i: integer;
Begin
  result := 0;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then inc(result);
  End;
  For i := 0 To CheckListBox2.Items.Count - 1 Do Begin
    If CheckListBox2.Checked[i] Then inc(result);
  End;
  If CheckBox1.Checked Then inc(result);
End;

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  StopDownloading := true;
  close;
End;

Procedure TForm3.Button3Click(Sender: TObject);
Var
  i: Integer;
Begin
  // None
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    CheckListBox1.Checked[i] := true;
  End;
  CheckListBox1Click(Nil);
End;

Procedure TForm3.Button4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // None
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    CheckListBox1.Checked[i] := false;
  End;
  CheckListBox1Click(Nil);
End;

Procedure TForm3.Button1Click(Sender: TObject);
Var
  FileCount, i: Integer;
  Fileinfo, OwnFile, updater: String;
  b: Boolean;
  omax, opos, total: int64;
  io: TitemObject;
  aFileList: TFiles;
Begin
  // Download and Update
  setlength(AdditionalJSONDownloads, 0);
  If GetFilesToDLCount() = 0 Then Begin
    showmessage('Nothing for download selected.');
    exit;
  End;
  b := false;
  For i := 0 To CheckListBox2.items.count - 1 Do Begin
    If CheckListBox2.Checked[i] Then Begin
      If ((CheckListBox2.Items.Objects[i] As TitemObject).filecontainer.Kind = fkZip) Then Begin
        b := true;
        break;
      End;
    End;
  End;
  If b And (Not fforce) Then Begin
    If id_no = Application.MessageBox('Zip files are merged without checks, do you really want to continue ?', 'Warning', MB_ICONWARNING Or MB_YESNO) Then Begin
      exit;
    End;
  End;
  // Wir sammeln wie "groß" das alles sein wird und Fragen den User ob das OK ist
  total := 0;
  StopDownloading := false;
  If CheckBox1.Checked Then Begin
{$IFDEF WINDOWS}
    total := total + SelfFile.Size;
{$ENDIF}
{$IFDEF LINUX}
    total := total + SelfFile.Size2;
{$ENDIF}
  End;
  For i := 0 To CheckListBox1.items.count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      io := (CheckListBox1.Items.Objects[i] As TitemObject);
      If io.filecontainer.Kind = fkExecutable Then Begin
{$IFDEF WINDOWS}
        total := total + io.filecontainer.Size;
{$ENDIF}
{$IFDEF LINUX}
        total := total + io.filecontainer.Size2;
{$ENDIF}
      End
      Else Begin
        total := total + io.filecontainer.Size;
      End;
    End;
  End;
  For i := 0 To CheckListBox2.items.count - 1 Do Begin
    If CheckListBox2.Checked[i] Then Begin
      io := (CheckListBox2.Items.Objects[i] As TitemObject);
      If io.filecontainer.Kind = fkExecutable Then Begin
{$IFDEF WINDOWS}
        total := total + io.filecontainer.Size;
{$ENDIF}
{$IFDEF LINUX}
        total := total + io.filecontainer.Size2;
{$ENDIF}
      End
      Else Begin
        total := total + io.filecontainer.Size;
      End;
    End;
  End;
  If total <> 0 Then Begin
    If ID_NO = Application.MessageBox(pchar('Need to download ' + FileSizeToString(total) + ' continue?'), 'Info', MB_ICONQUESTION Or MB_YESNO) Then Begin
      exit;
    End;
  End;
  ProgressBar2.Max := total;
  ProgressBar2.Position := 0;
  Fileinfo := Label3.Caption;
  FileCount := 0;
  total := 0;
  For i := 0 To CheckListBox1.items.count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      total := total + dlFile((CheckListBox1.Items.Objects[i] As TitemObject).filecontainer);
      inc(FileCount);
      label3.caption := Fileinfo + format(' (%d)', [FileCount]);
      ProgressBar2.Position := total;
      Application.ProcessMessages;
      If StopDownloading Then exit;
    End;
  End;
  For i := 0 To CheckListBox2.items.count - 1 Do Begin
    If CheckListBox2.Checked[i] Then Begin
      total := total + dlFile((CheckListBox2.Items.Objects[i] As TitemObject).filecontainer);
      inc(FileCount);
      label3.caption := Fileinfo + format(' (%d)', [FileCount]);
      ProgressBar2.Position := total;
      Application.ProcessMessages;
      If StopDownloading Then exit;
    End;
  End;
  If assigned(AdditionalJSONDownloads) Then Begin
    aFileList := ScanForAdditionalFiles;
    If assigned(aFileList) Then Begin
      omax := ProgressBar2.Max;
      opos := ProgressBar2.Position;
      form4.HandleFiles(aFileList, @dlFile);
      ProgressBar2.Max := omax;
      ProgressBar2.Position := opos;
      label3.caption := Fileinfo + format(' (%d)', [FileCount]);
      Application.ProcessMessages;
      If StopDownloading Then exit;
    End;
  End;
  If CheckBox1.Checked Then Begin
    If SelfFile.URL <> '' Then Begin
      updater := 'ctd_updater'{$IFDEF Windows} + '.exe'{$ENDIF};
      If Not FileExists(updater) Then Begin
        showmessage('Error, ' + updater + ' not found, without updater launcher can not be updated.');
        log('Error, ' + updater + ' not found, without updater launcher can not be updated.');
        exit;
      End;
      OwnFile := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update' + PathDelim + ExtractFileName(ParamStr(0));
      SelfFile.Filename := OwnFile;
      dlFile(SelfFile);
      total := total + dlFile(SelfFile);
      ProgressBar2.Position := total;
      inc(FileCount);
      label3.caption := Fileinfo + format(' (%d)', [FileCount]);
      Application.ProcessMessages;
      If StopDownloading Then exit;
      If FileExists(OwnFile) Then Begin
        // So schnell wie möglich beenden !
        TriggerUpdater(OwnFile);
        Application.Terminate;
        exit;
      End
      Else Begin
        // Der Fehler wurde ja schon ausgegeben
        close;
      End;
    End
    Else Begin
      log('Error, could not update launcher due to missing download informations.');
    End;
  End;
  log('Finished');
  ProgressBar1.Position := 0;
  ProgressBar2.Position := 0;
  Label5.Caption := '';
  Close;
End;

Procedure TForm3.CheckListBox1Click(Sender: TObject);
Begin
  label3.caption := format('%d files to download', [GetFilesToDLCount()]);
End;

Procedure TForm3.InitWith(Const aVersion: TCTD_Version; Force: Boolean);
Var
  i: Integer;
Begin
  label5.caption := '';
  fNewVersion := aVersion.Version;
  SelfFile.URL := '';
  SelfFile.Size := 0;
  SelfFile.Size2 := 0;
  memo1.Text := format('Config TD ver. %0.5f', [aVersion.Version]) + LineEnding + LineEnding + aVersion.VersionText;
  CheckListBox1.Clear;
  CheckListBox2.Clear;
  For i := 0 To aVersion.DownloadBaseCount - 1 Do Begin
    CheckAddFile(CheckListBox1, aVersion.DownloadBase[i], false);
  End;
  For i := 0 To aVersion.DownloadCount - 1 Do Begin
    CheckAddFile(CheckListBox2, aVersion.Download[i], true);
  End;
  CheckBox1.Checked := aVersion.LauncherVersion > LauncherVersion;
  CheckListBox1Click(Nil);
End;

End.

