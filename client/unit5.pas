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
Unit unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ExtCtrls, ucolorselectbox;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    ColorBox5: TColorBox;
    ColorBox6: TColorBox;
    ColorBox7: TColorBox;
    ColorBox8: TColorBox;
    ColorBox9: TColorBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ScrollBar1: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Function FindComp(AName: String): TComponent;
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses uctd, uctd_common, unit1;

Var
  Form5ShowOnce: Boolean = false;

  { TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Var
  i: integer;
  t, l, w, h: integer;
  n: String;
  cb: TColorSelectBox;
  o: TWinControl;
Begin
  caption := 'Options';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  // Die Colorboxen können nicht alle Farben, also fliegen sie hier Raus und
  // Werden durch TColorSelectBox ersetzt
  For i := form5.ComponentCount - 1 Downto 0 Do Begin
    If form5.Components[i] Is TColorBox Then Begin
      n := TColorBox(form5.Components[i]).Name;
      l := TColorBox(form5.Components[i]).Left;
      t := TColorBox(form5.Components[i]).Top;
      w := TColorBox(form5.Components[i]).Width;
      h := TColorBox(form5.Components[i]).Height;
      o := TColorBox(form5.Components[i]).Parent;
      form5.Components[i].free;
      cb := TColorSelectBox.Create(o);
      cb.Name := n;
      cb.Parent := o;
      cb.Left := l;
      cb.Top := t;
      cb.Width := w;
      cb.Height := h;
    End;
  End;
End;

Procedure TForm5.FormShow(Sender: TObject);

Begin
  If Not Form5ShowOnce Then Begin
    Form5ShowOnce := true;
    left := strtoint(GetValue('OptionsForm', 'Left', inttostr(left)));
    top := strtoint(GetValue('OptionsForm', 'Top', inttostr(top)));
    Width := strtoint(GetValue('OptionsForm', 'Width', inttostr(Width)));
    Height := strtoint(GetValue('OptionsForm', 'Height', inttostr(Height)));
    FixFormPosition(form5);
  End;
End;

Procedure TForm5.ScrollBar1Change(Sender: TObject);
Begin
  label13.caption := floattostr(ScrollBar1.Position / 10 + 1);
End;

Function TForm5.FindComp(AName: String): TComponent;
Begin
  result := FindComponent(aname);
  If Assigned(result) Then exit;
  result := GroupBox1.FindComponent(aname);
  If Assigned(result) Then exit;
  result := GroupBox2.FindComponent(aname);
  If Assigned(result) Then exit;
End;

Procedure TForm5.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  setValue('OptionsForm', 'Left', inttostr(left));
  setValue('OptionsForm', 'Top', inttostr(top));
  setValue('OptionsForm', 'Width', inttostr(Width));
  setValue('OptionsForm', 'Height', inttostr(Height));

  form1.RestoreForm4;
End;

Procedure TForm5.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  // OK
  SetValue('Global', 'InvertMouseScrolling', inttostr(ord(form5.CheckBox1.Checked)));
  SetValue('Global', 'AutoNextWave', inttostr(ord(form5.CheckBox2.Checked)));
  SetValue('Global', 'AlwaysShowLifepoints', inttostr(ord(form5.CheckBox3.Checked)));
  SetValue('Global', 'ShowBuildingRanges', inttostr(ord(form5.CheckBox4.Checked)));
  SetValue('Global', 'Building_Darkning', inttostr(ord(form5.CheckBox5.Checked)));
  SetValue('Global', 'ShowFPS', inttostr(ord(form5.CheckBox6.Checked)));
  SetValue('Global', 'DisableBackGroundTexturing', inttostr(ord(form5.CheckBox7.Checked)));
  SetValue('Global', 'Autozoom', inttostr(ord(form5.CheckBox8.Checked)));
  SetValue('Global', 'Middle_Mouse_Map_Scolling', inttostr(ord(form5.CheckBox9.Checked)));
  setValue('Global', 'ShowBuildableTilesDuringBuild', inttostr(ord(form5.CheckBox10.Checked)));
  SetValue('Global', 'ShowWaveOppHint', inttostr(ord(form5.CheckBox11.Checked)));
  SetValue('Global', 'ShowHeroRanges', inttostr(ord(form5.CheckBox12.Checked)));
  If GetValue('Global', 'DarkMode', '0') <> inttostr(ord(form5.CheckBox13.Checked)) Then Begin
    showmessage('Warning, changing dark mode needs a restart to apply.');
  End;
  SetValue('Global', 'DarkMode', inttostr(ord(form5.CheckBox13.Checked)));

  SetValue('Global', 'MapBlockSize', form5.Edit1.text);
  SetValue('Global', 'AutoNextWaveDelay', form5.Edit2.text);

  SetValue('Global', 'Good_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox1')).Selected));
  SetValue('Global', 'Damages_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox2')).Selected));
  SetValue('Global', 'Broken_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox3')).Selected));
  SetValue('Global', 'Nothing_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox4')).Selected));
  SetValue('Global', 'Walkable_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox5')).Selected));
  SetValue('Global', 'Buildable_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox6')).Selected));
  SetValue('Global', 'Both_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox7')).Selected));
  SetValue('Global', 'Grid_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox8')).Selected));
  SetValue('Global', 'BuildingRange_Color', ColorToString(TColorSelectBox(form5.FindComp('ColorBox9')).Selected));

  SetValue('Global', 'Menupos', form5.ComboBox1.Text);

  setvalue('Global', 'Fontscale', inttostr(form5.ScrollBar1.Position));

  form1.Load_CT_Settings();
  close;
End;

End.

