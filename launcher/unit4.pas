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
Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst,
  StdCtrls, ulauncher, Types;

Type

  TDownloadFunction = Function(aFile: TFile): int64 Of Object;

  TDLContainer = Record
    Name: String;
    Size: Int64;
    Items: TFiles;
  End;

  TDLJob = Record
    aFile: TFile;
    copys: TStringArray;
  End;

  TDLJobs = Array Of TDLJob;

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckListBox1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    KnownFiles: TFiles;
    fdlfun: TDownloadFunction;
    DLContainer: Array Of TDLContainer;
    Procedure AddFile(Const aFile: TFile; Index: integer);
    Function CalcTotalSize: int64;
    Function CalcDownloadJobs(): TDLJobs;
    Function GetFilesToDLCount(): integer;
  public
    Procedure ResetKnownFiles;
    Procedure AddKnownFile(Const aFile: TFile);
    Procedure HandleFiles(Const Files: TFiles; dlfun: TDownloadFunction);
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses Unit3, LCLType, FileUtil;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Additionals downloader';
  KnownFiles := Nil;
End;

Procedure TForm4.Button4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // None
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    CheckListBox1.Checked[i] := false;
  End;
  CalcTotalSize;
End;

Procedure TForm4.Button9Click(Sender: TObject);
Var
  cat: String;
  i: Integer;
Begin
  // Select by Category
  cat := lowercase(TButton(sender).Caption) + PathDelim;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If pos(cat, lowercase(CheckListBox1.Items[i])) = 1 Then Begin
      CheckListBox1.Checked[i] := true;
    End;
  End;
  CalcTotalSize;
End;

Procedure TForm4.CheckListBox1Click(Sender: TObject);
Begin
  CalcTotalSize;
End;

Procedure TForm4.Button3Click(Sender: TObject);
Var
  i: Integer;
Begin
  // All
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    CheckListBox1.Checked[i] := true;
  End;
  CalcTotalSize;
End;

Procedure TForm4.AddFile(Const aFile: TFile; Index: integer);
Var
  fp: String;
Begin
  If ((index >= low(DLContainer)) And (index <= high(DLContainer))) Then Begin
    // In Bestehenden Container anfügen
    setlength(DLContainer[Index].Items, high(DLContainer[Index].Items) + 2);
    DLContainer[Index].Items[high(DLContainer[Index].Items)] := aFile;
    DLContainer[Index].Size := DLContainer[Index].Size + aFile.Size;
  End
  Else Begin
    // Neuer Eintrag
    setlength(DLContainer, high(DLContainer) + 2);
    setlength(DLContainer[high(DLContainer)].Items, 1);
    DLContainer[high(DLContainer)].Items[0] := aFile;
    fp := ExtractFilePath(aFile.Filename);
    If fp = '' Then Begin
      DLContainer[high(DLContainer)].Name := aFile.Filename;
    End
    Else Begin
      DLContainer[high(DLContainer)].Name := fp;
    End;
    DLContainer[high(DLContainer)].Size := aFile.Size;
    CheckListBox1.Items.Add(DLContainer[high(DLContainer)].Name);
  End;
End;

Function TForm4.CalcTotalSize: int64;
Var
  files, i: Integer;
  total: int64;
Begin
  total := 0;
  files := 0;
  For i := 0 To high(DLContainer) Do Begin
    If CheckListBox1.Checked[i] Then Begin
      total := total + DLContainer[i].Size;
      files := files + length(DLContainer[i].Items);
    End;
  End;
  label3.caption := format('%d files = %s', [files, FileSizeToString(total)]);
  result := total;
End;

Function TForm4.GetFilesToDLCount: integer;
Var
  files, i: Integer;
Begin
  files := 0;
  For i := 0 To high(DLContainer) Do Begin
    If CheckListBox1.Checked[i] Then Begin
      files := files + length(DLContainer[i].Items);
    End;
  End;
  result := files;
End;

Procedure TForm4.ResetKnownFiles;
Begin
  setlength(KnownFiles, 0);
End;

Procedure TForm4.AddKnownFile(Const aFile: TFile);
Var
  i: Integer;
Begin
  // Wir kennen die Datei schon ..
  For i := 0 To high(KnownFiles) Do Begin
    If LowerCase(KnownFiles[i].Hash) = LowerCase(aFile.Hash) Then exit;
  End;
  // Es ist eine neue Datei -> Hinzufügen
  setlength(KnownFiles, high(KnownFiles) + 2);
  KnownFiles[high(KnownFiles)] := aFile;
End;

Function TForm4.CalcDownloadJobs: TDLJobs;
  Procedure Add(Const aFile: TFile);
  Var
    i: Integer;
  Begin
    // Die Datei ist in der KnownListe -> Sie kann direkt Kopiert werden
    For i := 0 To high(KnownFiles) Do Begin
      If lowercase(KnownFiles[i].Hash) = lowercase(aFile.Hash) Then Begin
        ForceDirectories(extractfilepath(aFile.Filename));
        Copyfile(KnownFiles[i].Filename, aFile.Filename);
        exit;
      End;
    End;
    // Die Datei gibt es schon in der DL Liste
    For i := 0 To high(result) Do Begin
      If lowercase(result[i].aFile.Hash) = lowercase(aFile.Hash) Then Begin
        setlength(result[i].copys, high(result[i].copys) + 2);
        result[i].copys[high(result[i].copys)] := aFile.Filename;
        exit;
      End;
    End;
    // Die Datei gibt es als dl noch nicht
    setlength(result, high(result) + 2);
    result[High(result)].aFile := aFile;
    result[High(result)].copys := Nil;
  End;

Var
  i, j: Integer;
Begin
  result := Nil;
  For i := 0 To high(DLContainer) Do Begin
    If CheckListBox1.Checked[i] Then Begin
      For j := 0 To high(DLContainer[i].Items) Do Begin
        add(DLContainer[i].Items[j]);
      End;
    End;
  End;
End;

Procedure TForm4.HandleFiles(Const Files: TFiles; dlfun: TDownloadFunction);
Label
  Retry;
Var
  FileCount, i, j: Integer;
  s, fc, fp: String;
  found: Boolean;
  dltotal, total: int64;
  Jobs: TDLJobs;
Begin
  fdlfun := dlfun;
  CheckListBox1.Items.Clear;
  DLContainer := Nil;
  For i := 0 To high(Files) Do Begin
    fp := ExtractFilePath(Files[i].Filename);
    If fp = '' Then Begin
      AddFile(Files[i], -2);
    End
    Else Begin
      found := false;
      For j := 0 To high(DLContainer) Do Begin
        If DLContainer[j].Name = fp Then Begin
          found := true;
          AddFile(Files[i], j);
          break;
        End;
      End;
      If Not found Then Begin
        AddFile(Files[i], -2);
      End;
    End;
  End;
  For i := 0 To high(DLContainer) Do Begin
    CheckListBox1.Items[i] := format('%s (%d files = %s)', [
      CheckListBox1.Items[i],
        length(DLContainer[i].Items),
        FileSizeToString(DLContainer[i].Size)
        ]);
    CheckListBox1.Checked[i] := true;
  End;
  CalcTotalSize;
  Retry: // Oh mann, was hab ich getan ...
  If ShowModal = mrOK Then Begin
    StopDownloading := false;
    // Starten der DL's
    total := CalcTotalSize();
    Jobs := CalcDownloadJobs();
    dltotal := 0;
    For i := 0 To high(Jobs) Do Begin
      dltotal := dltotal + Jobs[i].aFile.Size;
    End;
    s := format(
      'Need to download %d files which will take %s on your harddisc.' + LineEnding + LineEnding +
      'Download heutistic can compress these files to %d distinct downloads, which will create %s traffic.' + LineEnding + LineEnding +
      'Continue?', [
      GetFilesToDLCount(),
        FileSizeToString(total),
        length(Jobs),
        FileSizeToString(dltotal)
        ]);
    If ID_NO = Application.MessageBox(pchar(s), 'Info', MB_ICONQUESTION Or MB_YESNO) Then Begin
      Goto Retry;
    End;
    fc := format('%d files to download', [length(Jobs)]);
    form3.label3.caption := fc;
    If total <> 0 Then Begin
      form3.ProgressBar2.Max := dltotal;
      form3.ProgressBar2.Position := 0;
      total := 0;
      FileCount := 0;
      For i := 0 To high(Jobs) Do Begin
        total := total + dlfun(Jobs[i].aFile);
        For j := 0 To high(Jobs[i].copys) Do Begin
          ForceDirectories(extractfilepath(Jobs[i].copys[j]));
          Copyfile(Jobs[i].aFile.Filename, Jobs[i].copys[j]);
        End;
        inc(FileCount);
        form3.ProgressBar2.Position := total;
        form3.label3.caption := fc + format(' (%d)', [FileCount]);
        Application.ProcessMessages;
        If StopDownloading Then Begin
          exit;
        End;
      End;
    End;
  End;
  setlength(DLContainer, 0);
End;

End.

