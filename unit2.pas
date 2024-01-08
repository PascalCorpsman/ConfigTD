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
Unit unit2;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, types;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    Timer1: TTimer;
    Procedure Button3Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Const
  HostGame = 'Host a game';
  JoinGame = 'Join a game';
Var
  Form2: TForm2;
  Form2ShowOnce: Boolean = false;

Implementation

{$R *.lfm}

Uses uctd, uctd_common;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  //ComboBox1.Items.CommaText := GetHostIP('');
  //If ComboBox1.Items.Count = 0 Then Begin
  ComboBox1.Text := '0.0.0.0';
  //End
  //Else Begin
  //  ComboBox1.Text := ComboBox1.Items[0];
  //End;
End;

Procedure TForm2.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If Caption = HostGame Then Begin
    setValue('HostForm', 'Left', inttostr(left));
    setValue('HostForm', 'Top', inttostr(top));
    setValue('HostForm', 'Height', inttostr(Height));
  End
  Else Begin
    setValue('JoinForm', 'Left', inttostr(left));
    setValue('JoinForm', 'Top', inttostr(top));
    setValue('JoinForm', 'Height', inttostr(Height));
  End;
End;

Procedure TForm2.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button3.Click;
End;

Procedure TForm2.ListBox1Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    // Übernehmen der IP
    s := ListBox1.Items[ListBox1.ItemIndex];
    edit1.text := copy(s, pos('#', s) + 1, length(s));
    s := copy(s, 1, pos('#', s) - 1);
    ComboBox1.Text := '';
    For i := length(s) Downto 1 Do Begin
      If s[i] = ':' Then Begin
        ComboBox1.Text := copy(s, i + 1, length(s));
        break;
      End;
    End;
  End;
End;

Procedure TForm2.ListBox1DblClick(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    ModalResult := mrOK
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  ModalResult := mrOK;
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  If Not Form2ShowOnce Then Begin
    Form2ShowOnce := true;
    If Caption = HostGame Then Begin
      left := strtoint(GetValue('HostForm', 'Left', inttostr(left)));
      top := strtoint(GetValue('HostForm', 'Top', inttostr(top)));
      Height := strtoint(GetValue('HostForm', 'Height', inttostr(Height)));
    End
    Else Begin
      left := strtoint(GetValue('JoinForm', 'Left', inttostr(left)));
      top := strtoint(GetValue('JoinForm', 'Top', inttostr(top)));
      Height := strtoint(GetValue('JoinForm', 'Height', inttostr(Height)));
    End;
    FixFormPosition(form2);
  End;
End;

Procedure TForm2.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Begin
  ListBox1.canvas.Rectangle(ARect);
  ListBox1.canvas.TextRect(ARect, 5, (arect.Bottom - ARect.Top - ListBox1.Canvas.TextHeight('Pp')) Div 2, copy(ListBox1.Items[index], 1, pos(':', ListBox1.Items[index]) - 1));
End;

Procedure TForm2.Timer1Timer(Sender: TObject);
Var
  sl: TStringlist;
  s: String;
  i: Integer;
Begin
  s := '';
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ListBox1.Items[ListBox1.ItemIndex];
  End;
  ctd.PingForOpenGames;
  sl := ctd.AktualGameList;
  ListBox1.Items.CommaText := sl.CommaText;
  sl.free;
  If s <> '' Then Begin
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If ListBox1.Items[i] = s Then Begin
        ListBox1.ItemIndex := i;
        break;
      End;
    End;
  End;
End;

End.

