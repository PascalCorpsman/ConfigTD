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
    fdlfun: TDownloadFunction;
    DLContainer: Array Of TDLContainer;
    Procedure AddFile(Const aFile: TFile; Index: integer);
    Function CalcTotalSize: int64;
    Function GetFilesToDLCount(): integer;
  public

    Procedure HandleFiles(Const Files: TFiles; dlfun: TDownloadFunction);
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses Unit3;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Additionals downloader';
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

Function TForm4.GetFilesToDLCount(): integer;
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

Procedure TForm4.HandleFiles(Const Files: TFiles; dlfun: TDownloadFunction);
Var
  FileCount, i, j: Integer;
  fc, fp: String;
  found: Boolean;
  total: int64;
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
  If ShowModal = mrOK Then Begin
    StopDownloading := false;
    // Starten der DL's
    total := CalcTotalSize();
    fc := format('%d files to download', [GetFilesToDLCount()]);
    form3.label3.caption := fc;
    If total <> 0 Then Begin
      form3.ProgressBar2.Max := total;
      form3.ProgressBar2.Position := 0;
      total := 0;
      FileCount := 0;
      For i := 0 To high(DLContainer) Do Begin
        If CheckListBox1.Checked[i] Then Begin
          For j := 0 To high(DLContainer[i].Items) Do Begin
            total := total + dlfun(DLContainer[i].Items[j]);
            inc(FileCount);
            form3.ProgressBar2.Position := total;
            form3.label3.caption := fc + format(' (%d)', [FileCount]);
            Application.ProcessMessages;
            If StopDownloading Then Begin
              setlength(DLContainer, 0);
              exit;
            End;
          End;
        End;
      End;
    End;
  End;
  setlength(DLContainer, 0);
End;

End.

