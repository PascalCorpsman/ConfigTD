(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of config_td                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, types;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    ProgressBar1: TProgressBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
    fListboxData: Array Of TBitmap;
  public
    { public declarations }
  End;

Var
  Form1: TForm1;

Implementation

Uses Clipbrd, math, lazutf8, LazFileUtils, uopengl_animation;

{$R *.lfm}

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  sl: TStringList;
  p: TPortableNetworkGraphic;
  b: TBitmap;
  i, j, k: Integer;
  ani: TOpenGL_Animation;
Begin
  SelectDirectoryDialog1.FileName := ExtractFilePath(ParamStrUTF8(0));
  If SelectDirectoryDialog1.Execute Then Begin
    sl := FindAllFiles(SelectDirectoryDialog1.FileName, '*.bmp;*.png;*.ani', true);
    sl.Sorted := true;
    sl.Sort;
    For i := 0 To high(fListboxData) Do Begin
      fListboxData[i].Free;
    End;
    ProgressBar1.Visible := true;
    ProgressBar1.Max := sl.Count;
    setlength(fListboxData, sl.Count);
    ListBox1.Items.BeginUpdate;
    ListBox1.Items.Clear;
    For i := 0 To sl.Count - 1 Do Begin
      If i Mod max(1, sl.count Div 10) = 0 Then Begin
        ProgressBar1.Position := i;
        Application.ProcessMessages;
      End;
      Case lowercase(ExtractFileExt(sl[i])) Of
        '.png': Begin
            p := TPortableNetworkGraphic.Create;
            p.LoadFromFile(sl[i]);
            b := TBitmap.Create;
            b.Assign(p);
            p.free;
          End;
        '.bmp': Begin
            b := TBitmap.Create;
            b.LoadFromFile(sl[i]);
          End;
        '.ani': Begin
            ani := TOpenGL_Animation.Create;
            ani.LoadFromFile(sl[i], false);
            b := ani.GetFirstBitmap();
            ani.free;
          End;
      End;
      b.TransparentColor := clfuchsia;
      b.Transparent := true;
      fListboxData[i] := b;
      listbox1.Items.Add(sl[i]);
    End;
    ProgressBar1.Visible := false;
    ListBox1.Items.EndUpdate;
    listbox1.Invalidate;
    sl.free;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * 0.01 = Initialversion
   * 0.02 = Support Animations
   * 0.03 = Sort Elements
   * 0.04 = Speedup and Progressbar
   *)
  caption := 'Image shower ver. 0.04, by Corpsman, support : www.Corpsman.de';
  fListboxData := Nil;
  ListBox1.Clear;
  edit1.text := '';
  ListBox1.items.add('Scan folder: list all *.bmp, *.png, *.ani files.');
  ListBox1.items.add('Double click: add the filename to clibboard.');
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    edit1.text := ListBox1.Items[ListBox1.ItemIndex];
  End;
End;

Procedure TForm1.ListBox1DblClick(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    Clipboard.AsText := ListBox1.Items[ListBox1.ItemIndex];
  End;
End;

Procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var
  c: TColor;
  h, w: integer;
Begin
  listbox1.Canvas.Pen.Color := listbox1.Canvas.Brush.Color;
  listbox1.Canvas.Rectangle(aRect);
  If (index >= 0) And (Index <= high(fListboxData)) Then Begin
    listbox1.canvas.TextOut(arect.Left + 10 + arect.Bottom - arect.Top - 6, (arect.Bottom + ARect.Top - canvas.TextHeight(ListBox1.items[index])) Div 2, ExtractFileNameOnly(ListBox1.items[index]));
    w := fListboxData[Index].Width;
    h := arect.Bottom - arect.Top - 6;
    w := min(w, arect.Bottom - arect.Top - 6);
    h := min(h, fListboxData[Index].Height);
    listbox1.Canvas.StretchDraw(rect(arect.Left + 5, arect.Top + 3, arect.Left + 5 + w, ARect.Top + 3 + h), fListboxData[index]);
    If index > 0 Then Begin
      If ExtractFilePath(ListBox1.Items[index - 1]) <> ExtractFilePath(ListBox1.Items[index]) Then Begin
        c := listbox1.Canvas.Pen.Color;
        listbox1.Canvas.Pen.Color := clNavy;
        listbox1.Canvas.MoveTo(arect.Left + 1, arect.Top + 1);
        listbox1.Canvas.LineTo(arect.Right - 1, arect.Top + 1);
        listbox1.Canvas.Pen.Color := c;
      End;
    End;
  End
  Else Begin
    listbox1.canvas.TextOut(arect.Left + 10 + arect.Bottom - arect.Top - 6, (arect.Bottom + ARect.Top - canvas.TextHeight(ListBox1.items[index])) Div 2, (ListBox1.items[index]));
  End;
End;

End.

