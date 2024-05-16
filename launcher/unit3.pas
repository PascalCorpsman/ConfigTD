Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CheckLst, ulauncher;

Type

  TitemObject = Class
    filecontainer: TFile;
  End;

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckListBox1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    SelfFile: TFile;
    Procedure CheckAddFile(Const ListBox: TCheckListBox; Const aFile: TFile; Force: Boolean);
    Procedure dlFile(Const aFile: TFile);
    Procedure TriggerUpdater(Executable: String);
  public
    Procedure InitWith(Const aVersion: TCTD_Version; Force: Boolean);

  End;

Var
  Form3: TForm3;

Implementation

Uses lazfileutils, md5, Zipper, FileUtil, process, UTF8Process;

{$R *.lfm}

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Update overview';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
End;

Procedure TForm3.CheckAddFile(Const ListBox: TCheckListBox; Const aFile: TFile;
  Force: Boolean);
Var
  io: TitemObject;
  fn, s, cfh, fh: String;
  NeedAdd: Boolean;
  i: integer;
Begin
  NeedAdd := Force Or (aFile.Kind = fkZip);
  fn := aFile.Filename;
{$IFDEF Windows}
  fn := StringReplace(fn, '/', PathDelim, [rfReplaceAll]);
{$ENDIF}
{$IFDEF Linux}
  If aFile.Kind = fkExecutable Then Begin
    fn := ExtractFileNameWithoutExt(fn);
    fh := aFile.Hash2;
  End;
{$ENDIF}
  If (Not NeedAdd) And (aFile.Kind In [fkExecutable, fkFile]) Then Begin
    fh := aFile.Hash;
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
          CheckBox1.Checked := CheckBox1.Checked Or NeedAdd;
          SelfFile := aFile;
          exit;
        End;
      End;
    fkZip: s := aFile.Description;
  End;
  io := TitemObject.Create;
  io.filecontainer := aFile;
  i := ListBox.Items.AddObject(s, io);
  ListBox.Checked[i] := NeedAdd;
End;

Procedure TForm3.dlFile(Const aFile: TFile);
Var
  UnZipper: TUnZipper;
  newRoot, root, fn, source, target, TargetDir: String;
  sl: TStringList;
  i: Integer;
{$IFDEF Linux}
  pr: TProcessUTF8;
{$ENDIF}
Begin
  fn := aFile.Filename;
{$IFDEF Linux}
  If aFile.Kind = fkExecutable Then Begin
    fn := ExtractFileNameWithoutExt(fn);
  End;
{$ENDIF}
  If aFile.Kind = fkZip Then Begin
    fn := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update' + PathDelim + ExtractFileName(aFile.URL);
  End;
  If DownloadFile(aFile.URL, fn) Then Begin
    If aFile.Kind = fkExecutable Then Begin
{$IFDEF LINUX}
      pr := TProcessUTF8.Create(Nil);
      pr.Options := [poWaitOnExit];
      pr.CurrentDirectory := GetCurrentDir;
      pr.Executable := 'chmod';
      pr.Parameters.Add('+x');
      pr.Parameters.Add(fn);
      pr.Execute;
      pr.free;
{$ENDIF}
    End;
    If aFile.Kind = fkZip Then Begin
      UnZipper := TUnZipper.Create;
      Try
        UnZipper.FileName := fn;
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
  End
  Else Begin
    // Es braucht keine Fehlermeldung, da das DownloadFile schon gemacht hat ..
  End;
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

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Var
  i: Integer;
  OwnFile, updater: String;
Begin
  // Download and Update
  For i := 0 To CheckListBox1.items.count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      dlFile((CheckListBox1.Items.Objects[i] As TitemObject).filecontainer);
    End;
  End;
  For i := 0 To CheckListBox2.items.count - 1 Do Begin
    If CheckListBox2.Checked[i] Then Begin
      dlFile((CheckListBox2.Items.Objects[i] As TitemObject).filecontainer);
    End;
  End;
  If CheckBox1.Checked Then Begin
    If SelfFile.URL <> '' Then Begin
      updater := 'updater'{$IFDEF Windows} + '.exe'{$ENDIF};
      If Not FileExists(updater) Then Begin
        log('Error, ' + updater + ' not found.');
        close;
        exit;
      End;
      OwnFile := IncludeTrailingPathDelimiter(GetTempDir()) + 'ctd_update' + PathDelim + ExtractFileNameOnly(ParamStr(0));
      SelfFile.Filename := OwnFile;
      dlFile(SelfFile);
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
  Close;
End;

Procedure TForm3.CheckListBox1Click(Sender: TObject);
Var
  c, i: integer;
Begin
  c := 0;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then inc(c);
  End;
  For i := 0 To CheckListBox2.Items.Count - 1 Do Begin
    If CheckListBox2.Checked[i] Then inc(c);
  End;
  label3.caption := format('%d files to download', [c]);
End;

Procedure TForm3.InitWith(Const aVersion: TCTD_Version; Force: Boolean);
Var
  i: Integer;
Begin
  SelfFile.URL := '';
  memo1.Text := aVersion.VersionText;
  CheckBox1.Checked := aVersion.LauncherVersion <> LauncherVersion;
  CheckListBox1.Clear;
  CheckListBox2.Clear;
  For i := 0 To aVersion.DownloadBaseCount - 1 Do Begin
    CheckAddFile(CheckListBox1, aVersion.DownloadBase[i], false);
  End;
  For i := 0 To aVersion.DownloadCount - 1 Do Begin
    CheckAddFile(CheckListBox2, aVersion.Download[i], Force);
  End;
  CheckListBox1Click(Nil);
End;

End.

