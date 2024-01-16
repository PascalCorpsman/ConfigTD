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
Unit unit13;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

Type

  { TForm13 }

  TForm13 = Class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PopupMenu1: TPopupMenu;
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure AddMessage(Const Text_: String);
  End;

Var
  Form13: TForm13;

Implementation

{$R *.lfm}

Uses uctd, LCLType, uctd_common;

Var
  Form13ShowOnce: Boolean = false;
  LastChat: String = '';

  { TForm13 }

Procedure TForm13.FormShow(Sender: TObject);
Begin
  If Not Form13ShowOnce Then Begin
    Form13ShowOnce := true;
    Form13.left := strtoint(GetValue('ChatForm', 'Left', inttostr(Form13.left)));
    Form13.top := strtoint(GetValue('ChatForm', 'Top', inttostr(Form13.top)));
    Form13.Width := strtoint(GetValue('ChatForm', 'Width', inttostr(Form13.Width)));
    Form13.Height := strtoint(GetValue('ChatForm', 'Height', inttostr(Form13.Height)));
    FixFormPosition(form13);
  End;
  edit1.SetFocus;
End;

Procedure TForm13.MenuItem1Click(Sender: TObject);
Var
  key: Char;
Begin
  // Clear Log
  edit1.text := '-clear';
  key := #13;
  Edit1KeyPress(edit1, key)
End;

Procedure TForm13.MenuItem2Click(Sender: TObject);
Var
  key: Char;
Begin
  // Show all bonus levels
  edit1.text := '-bonus';
  key := #13;
  Edit1KeyPress(edit1, key)
End;

Procedure TForm13.MenuItem4Click(Sender: TObject);
Var
  key: Char;
Begin
  // Show all boss levels
  edit1.text := '-boss';
  key := #13;
  Edit1KeyPress(edit1, key)
End;

Procedure TForm13.MenuItem5Click(Sender: TObject);
Var
  key: Char;
Begin
  // Show all air levels
  edit1.text := '-air';
  key := #13;
  Edit1KeyPress(edit1, key)
End;

Procedure TForm13.MenuItem6Click(Sender: TObject);
Var
  key: Char;
Begin
  // Show all ping times
  edit1.text := '-p';
  key := #13;
  Edit1KeyPress(edit1, key)
End;

Procedure TForm13.MenuItem8Click(Sender: TObject);
Var
  key: Char;
Begin
  // Show commands
  edit1.text := '-?';
  key := #13;
  Edit1KeyPress(edit1, key)
End;

Procedure TForm13.AddMessage(Const Text_: String);
Begin
  form13.Memo1.Lines.Add(Text_);
  form13.Memo1.SelStart := length(form13.Memo1.Lines.Text);
End;

Procedure TForm13.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  setValue('ChatForm', 'Left', inttostr(Form13.left));
  setValue('ChatForm', 'Top', inttostr(Form13.top));
  setValue('ChatForm', 'Width', inttostr(Form13.Width));
  setValue('ChatForm', 'Height', inttostr(Form13.Height));
End;

Procedure TForm13.Edit1KeyPress(Sender: TObject; Var Key: char);
Var
  s: String;
Begin
  If (key = #13) And (trim(edit1.text) <> '') Then Begin
    LastChat := Edit1.text;
    s := trim(lowercase(Edit1.text));
    Case s Of
      '-clear': memo1.clear;
      '-air': ctd.splashAirLevels;
      '-boss': ctd.splashBossLevels;
      '-bonus': ctd.splashBonusLevels;
      '-p': ctd.RequestPingTimes;
      '-?': Begin
          s :=
            'Valid commands:' + LineEnding +
            '-air               = show all levels containing air creeps' + LineEnding +
            '-boss              = show all levels containing boss creeps.' + LineEnding +
            '-bonus             = show all levels containing bonus creeps.' + LineEnding +
            LineEnding +
            '-p                 = show all ping times between server and players.' + LineEnding +
            '-clear             = clear messagelog' + LineEnding +
            '-kick <playername> = tells the server to kick player <playername> out of the game' + LineEnding +
            '-?                 = this help';
          AddMessage(s);
          ctd.Splashhint(s, DefaultSplashHintColor);
        End
    Else
      If pos('-kick ', s) = 1 Then Begin
        s := trim(copy(edit1.text, length('-kick') + 1, length(s)));
        ctd.SendKillCommand(s);
      End
      Else Begin
        ctd.SendChat(edit1.text);
      End;
    End;
    edit1.text := '';
  End;
  If key = #27 Then close;
End;

Procedure TForm13.Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = VK_UP Then Begin
    Edit1.Text := LastChat;
  End;
End;

Procedure TForm13.FormCreate(Sender: TObject);
Begin
  caption := 'Chat';
  memo1.Clear;
  edit1.text := '';
End;

End.

