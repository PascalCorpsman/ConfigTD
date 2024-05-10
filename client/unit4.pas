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
Unit unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Spin, ExtDlgs, Buttons, uctd_mapobject, uctd_map,
  uwave_frame, Types;

Type

  TOnTransferFileDoneReason = (frUnknown, frBackTex, frDC1Tex, frDC2Tex, frDC3Tex, frDC4Tex);

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    SpinEdit1: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Timer1: TTimer;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Button14Click(Sender: TObject);
    Procedure Button15Click(Sender: TObject);
    Procedure Button16Click(Sender: TObject);
    Procedure Button17Click(Sender: TObject);
    Procedure Button18Click(Sender: TObject);
    Procedure Button19Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure CheckBox5Change(Sender: TObject);
    Procedure CheckBox6Change(Sender: TObject);
    Procedure CheckBox7Change(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure Memo1Change(Sender: TObject);
    Procedure PageControl2Change(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fTransferFilename: String;
    fOnTransferFileDoneReason: TOnTransferFileDoneReason;
    fOnFinishedLoadListDuringLoadObj: String;
    Procedure OnFinishedLoadListDuringLoad(Sender: TObject);
    Procedure OnGetComboBox2Content(Sender: TObject; Const Data: TStringlist);
    Procedure OnGetPageControl2Content(Sender: TObject; Const Data: TStringlist);
    Procedure OnTransferFileDone(Sender: TObject; Suceed: Boolean);
    Procedure AdjustMoveWaveButtons;
  public
    { public declarations }
    Procedure SetWaveCountTo(WaveCount: integer);
    Procedure AddWave(Reset: Boolean = true);
    Procedure ResetWave(Const Page: TTabSheet);
    Procedure DelLastWave;
    Procedure RefreshOpponentsClick(Sender: TObject);
    Procedure ChangeWaveIndexClick(WaveIndex: integer; Direction: Boolean);
    Procedure OnCTDWaveClone(Sender: TObject; SourceWaveNum, DestWaveNum: Integer);
  End;

Var
  Form4: TForm4;
  PlacementeObject: tctd_mapopbject = Nil;
  Form4updating: Boolean = false;
  JumpToLastWave: Boolean = false;

Function BuyableToString(Const data: TBuyAble): String;
Function StringToBuyAble(Data: String): TBuyAble;

Implementation

{$R *.lfm}

Uses
  math, LazFileUtils, LazUTF8,
  Unit1
  , unit3
  , unit6
  , unit7
  , unit14
  , unit19
  , uctd, uctd_messages, uctd_common, uctd_building, uctd_opp,
  LCLType;

Function BuyableToString(Const data: TBuyAble): String;
  Function KindToString(aKind: TBuyAbleKind): String;
  Begin
    Case aKind Of
      bkBuilding: result := 'building';
      bkHero: result := 'hero';
    Else Begin
        Raise exception.Create('BuyableToString: missing implementation');
      End;
    End;
  End;

Begin
  result := format('%s : %d : %d : %s', [ExtractFileNameOnly(data.Item), data.WaveNum + 1, data.Count, KindToString(data.Kind)]);
End;

Function StringToBuyAble(Data: String): TBuyAble;
Var
  pre: String;
Begin
  pre := trim(Copy(data, 1, pos(':', data) - 1));
  result.Item := pre;
  delete(data, 1, pos(':', data));

  pre := trim(Copy(data, 1, pos(':', data) - 1));
  result.WaveNum := strtointdef(pre, 1) - 1;
  delete(data, 1, pos(':', data));

  pre := trim(Copy(data, 1, pos(':', data) - 1));
  result.Count := strtointdef(pre, 1);
  delete(data, 1, pos(':', data));

  pre := trim(data);
  Case pre Of
    'building': result.Kind := bkBuilding;
    'hero': result.Kind := bkHero;
  Else Begin
      Raise exception.create('StringToBuyAble: missing implementation');
    End;
  End;

  Case result.Kind Of
    bkBuilding: result.item := result.item + '.geb';
    bkHero: result.item := result.item + '.hero';
  Else Begin
      Raise exception.Create('StringToBuyAble: missing implementation');
    End;
  End;
End;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  fOnTransferFileDoneReason := frUnknown;
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  PageControl1.ActivePageIndex := 0;
  ComboBox2.Clear;
  Edit5.text := '1';
  Edit6.text := '1';
  Edit7.text := '0';
End;

Procedure TForm4.FormDestroy(Sender: TObject);
Begin
  If assigned(PlacementeObject) Then
    PlacementeObject.free;
  PlacementeObject := Nil;
End;

Procedure TForm4.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  setValue('MapEditorForm', 'Left', inttostr(form4.left));
  setValue('MapEditorForm', 'Top', inttostr(form4.top));
  setValue('MapEditorForm', 'Width', inttostr(form4.Width));
  setValue('MapEditorForm', 'Height', inttostr(form4.Height));
  canclose := Not ctd.IsInEditMode;
End;

Procedure TForm4.ComboBox1Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  m := TMemoryStream.Create;
  i := ComboBox1.ItemIndex;
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpMapType, m);
End;

Procedure TForm4.ComboBox2Change(Sender: TObject);
Var
  s: String;
Begin
  s := MapFolder + MapName + PathDelim + ComboBox2.Text;
  If assigned(PlacementeObject) Then
    PlacementeObject.free;
  PlacementeObject := Nil;
  Case FilenameToType(s) Of
    moBuilding: PlacementeObject := TBuilding.create();
    moOpponent: PlacementeObject := TOpponent.create();
  End;
  If assigned(PlacementeObject) Then Begin
    PlacementeObject.LoadFromFile(s);
    PlacementeObject.Position.x := 0;
    PlacementeObject.Position.y := 0;
  End;
End;

Procedure TForm4.Button5Click(Sender: TObject);
Var
  sl: TStringList;
Begin
  // Check and Save Map
  log('TForm4.Button5Click', llTrace);
  If Form1.SaveAndCheckMap(true, true) Then Begin
    LogShow('Congratulations. Your maps has been approved to be playable and without warnings!', llInfo);
  End
  Else Begin
    sl := ctd.Map.GetListOfUnusedOpponents();
    If sl.count <> 0 Then Begin
      If ID_YES = Application.MessageBox('There are opponents which where spawned in no wave, do you want to remove them now ?', 'Question', mb_iconquestion Or MB_YESNO) Then Begin
        ctd.CleanupUnusedOpponets();
      End;
    End;
    sl.free;
  End;
  LogLeave;
End;

Procedure TForm4.Button6Click(Sender: TObject);
Begin
  // Open building Editor
  Form1.MenuItem15Click(Nil);
End;

Procedure TForm4.Button7Click(Sender: TObject);
Begin
  // Add new Wave
  SetWaveCountTo(length(ctd.Map.Waves) + 1);
End;

Procedure TForm4.SetWaveCountTo(WaveCount: integer);
Var
  waves: Integer;
  m: TMemoryStream;
  JumpToLast: Boolean;
Begin
  // Set Waves
  Form4updating := true;
  waves := WaveCount;
  JumpToLast := false;
  If length(ctd.Map.Waves) <> waves Then Begin
    JumpToLast := waves > length(ctd.Map.Waves);
    setlength(ctd.Map.Waves, waves);
  End;
  If Not ctd.BlockMapUpdateSending Then Begin
    m := TMemoryStream.Create;
    m.write(waves, sizeof(waves));
    ctd.UpdateMapProperty(mpWaveCount, m);
  End;
  While waves > PageControl2.PageCount Do Begin
    AddWave;
    ResetWave(PageControl2.pages[PageControl2.PageCount - 1]);
  End;
  While waves < PageControl2.PageCount Do
    DelLastWave;
  AdjustMoveWaveButtons;

  If JumpToLast Or JumpToLastWave Then Begin
    PageControl2.ActivePageIndex := PageControl2.PageCount - 1;
  End;
  JumpToLastWave := false;
  Timer1.Enabled := true; // Refresh der Comboboxen anstoßen
  Form4updating := false;
End;

Procedure TForm4.OnCTDWaveClone(Sender: TObject; SourceWaveNum,
  DestWaveNum: Integer);
Var
  fcloneWave: Twave;
  diff: integer;
  b: Boolean;
Begin
  fcloneWave := ctd.Map.Waves[SourceWaveNum];
  diff := DestWaveNum + 1 - length(ctd.Map.Waves);
  If diff <> 0 Then Begin
    While diff <> 0 Do Begin
      If diff > 0 Then Begin
        AddWave(false);
        diff := diff - 1;
      End
      Else Begin
        DelLastWave;
        diff := diff + 1;
      End;
    End;
  End;
  PageControl2.ActivePageIndex := DestWaveNum;
  // Die Wave wurde bereits vom Server behandelt, sonst käme
  // der Event ja nicht, alle Infos dazu brauchen also nicht nochmal
  // gesendet werden ..
  b := ctd.BlockMapUpdateSending;
  ctd.BlockMapUpdateSending := true;
  (PageControl2.Pages[PageControl2.ActivePageIndex].Components[0] As TWaveFrame).LoadWave(fcloneWave);
  ctd.BlockMapUpdateSending := b;
  AdjustMoveWaveButtons;
End;

Procedure TForm4.Button8Click(Sender: TObject);
Begin
  If (PageControl2.PageCount = 0) Then exit; // Es gibt keine Waves zum Clonen
  AddWave(false); // Die Neue Wave in der LCL schon mal anlegen
  ctd.CloneMapWave((PageControl2.Pages[PageControl2.ActivePageIndex].Components[0] As TWaveFrame).WaveNum, PageControl2.PageCount - 1);
End;

Procedure TForm4.Button9Click(Sender: TObject);
Begin
  // Start restart Game
  Form1.MenuItem18Click(Nil);
End;

Procedure TForm4.CheckBox1Change(Sender: TObject);
Begin
  If assigned(ctd.Map) Then
    ctd.Map.ShowBackTex := CheckBox1.Checked;
  If sender = CheckBox1 Then Begin
    CheckBox8.Checked := CheckBox1.Checked;
  End
  Else Begin
    CheckBox1.Checked := CheckBox8.Checked;
  End;
End;

Procedure TForm4.CheckBox2Change(Sender: TObject);
Begin
  If CheckBox2.Checked Then Begin
    If assigned(ctd.Map) Then Begin
      ctd.Map.ShowBackTex := false;
    End;
    CheckBox1.Checked := false;
    // Waypoints Deaktivieren
    CheckBox5.Checked := false;
    ctd.Map.ViewWaypoints := -1;
    // Placements Deaktivieren
    CheckBox6.Checked := false;
    // Wegpunkt Flächen Deaktivieren
    CheckBox7.Checked := false;
  End;
  If assigned(ctd.Map) Then Begin
    ctd.Map.EditTerrain := CheckBox2.Checked
  End;
End;

Procedure TForm4.CheckBox5Change(Sender: TObject);
Begin
  If CheckBox5.Checked Then Begin
    ctd.Map.ViewWaypoints := SpinEdit1.Value - 1;
    // Edit Terrain Deaktivieren
    CheckBox2.Checked := false;
    // Placements Deaktivieren
    CheckBox6.Checked := false;
    // Wegpunkt Flächen Deaktivieren
    CheckBox7.Checked := false;
  End
  Else Begin
    ctd.Map.ViewWaypoints := -1;
  End;
End;

Procedure TForm4.CheckBox6Change(Sender: TObject);
Begin
  If CheckBox6.Checked Then Begin
    // Edit Terrain Deaktivieren
    CheckBox2.Checked := false;
    // Waypoints Deaktivieren
    CheckBox5.Checked := false;
    // Wegpunkt Flächen Deaktivieren
    CheckBox7.Checked := false;
    ctd.Map.ViewWaypoints := -1;
  End;
End;

Procedure TForm4.CheckBox7Change(Sender: TObject);
Begin
  If CheckBox7.Checked Then Begin
    CheckBox1.Checked := false;
    CheckBox2.Checked := false;
    // Waypoints Deaktivieren
    CheckBox5.Checked := false;
    ctd.Map.ViewWaypoints := -1;
    // Placements Deaktivieren
    CheckBox6.Checked := false;
  End;
End;

Procedure TForm4.Button3Click(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  form3.caption := 'Resize map...';
  form3.Edit3.Visible := false;
  form3.Label3.Visible := false;
  Form3.Button3.Visible := false;
  form3.Edit1.Text := inttostr(ctd.Map.Width);
  form3.Edit2.Text := inttostr(ctd.Map.Height);
  form3.ModalResult := mrNone;
  If form3.ShowModal = mrOK Then Begin
    m := TMemoryStream.Create;
    i := max(1, strtointdef(form3.Edit1.Text, ctd.Map.Width));
    m.Write(i, sizeof(i));
    i := max(1, strtointdef(form3.Edit2.Text, ctd.Map.Height));
    m.Write(i, sizeof(i));
    ctd.UpdateMapProperty(mpResize, m);
  End;
End;

Procedure TForm4.Button10Click(Sender: TObject);
Var
  b: TBuyAble;
  m: TMemoryStream;
Begin
  // Set = Übernehmen der Einstellungen für das Gebäude
  If ListBox1.ItemIndex = -1 Then exit;
  log('TForm4.Button10Click', lltrace);
  b := StringToBuyAble(ListBox1.Items[ListBox1.ItemIndex]);
  b.WaveNum := strtointdef(edit6.text, 1) - 1;
  b.Count := strtointdef(edit7.text, 0);
  m := TMemoryStream.Create;
  m.WriteAnsiString(b.Item);
  m.Write(b.WaveNum, sizeof(b.WaveNum));
  m.Write(b.Count, sizeof(b.Count));
  m.Write(b.Kind, sizeof(b.Kind));
  //  ctd.Map.addBuyable(b.Item, b.WaveNum, b.Count); -- Das macht ctd schon
  ctd.UpdateMapProperty(mpUpdateBuyable, m);
  ListBox1.Items[ListBox1.ItemIndex] := BuyableToString(b);
  logleave;
End;

Procedure TForm4.Button11Click(Sender: TObject);
Begin
  // Anfrage Highscores der Karte
  ctd.OnGetHighscores := @form1.OnShowHighScores;
  ctd.RequestHighscores;
End;

Procedure TForm4.Button12Click(Sender: TObject);
Begin
  // Remove last wave
  If high(ctd.Map.Waves) > -1 Then Begin
    SetWaveCountTo(high(ctd.Map.Waves));
  End;
End;

Procedure TForm4.Button13Click(Sender: TObject);
Begin
  // Set Damageclass 1
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC1Tex;
    ctd.TransferFile(fTransferFilename, 'dc1.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button14Click(Sender: TObject);
Begin
  // Set Damageclass 2
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC2Tex;
    ctd.TransferFile(fTransferFilename, 'dc2.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button15Click(Sender: TObject);
Begin
  // Set Damageclass 3
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC3Tex;
    ctd.TransferFile(fTransferFilename, 'dc3.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button16Click(Sender: TObject);
Begin
  // Set Damageclass 4
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC4Tex;
    ctd.TransferFile(fTransferFilename, 'dc4.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button17Click(Sender: TObject);
Var
  i: integer;
Begin
  i := -1;
  Case QuestionDlg('Question', 'Please select difficulty to create wave with:', mtConfirmation, [mrOK, 'Hard', mrNo, 'Medium', mrYes, 'Easy', 'IsDefault'], '') Of
    mrYes: i := 0;
    mrNo: i := 1;
    mrOK: i := 2;
  End;
  If i In [0..2] Then Begin
    JumpToLastWave := true;
    ctd.AddNewRandomWave(i);
  End;
End;

Procedure TForm4.Button18Click(Sender: TObject);
Begin
  // Create Map Texture
  form1.MenuItem29Click(Nil);
End;

Procedure TForm4.Button19Click(Sender: TObject);
Begin
  // Open Hero Editor
  form1.MenuItem31Click(Nil);
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frBackTex;
    ctd.TransferFile(fTransferFilename, ExtractFileName(fTransferFilename), @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button2Click(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  // Clear Background Image
  m := TMemoryStream.Create;
  m.WriteAnsiString('');
  ctd.UpdateMapProperty(mpBackTex, m);
  CheckBox1.Checked := false;
End;

Procedure TForm4.Button4Click(Sender: TObject);
Begin
  // Refresh Placements List
  ctd.GetFileList('*.opp;*.geb', @OnGetComboBox2Content);
End;

Procedure TForm4.Edit1Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  m := TMemoryStream.Create;
  i := max(1, strtointdef(Edit1.text, 1));
  Edit1.text := inttostr(i);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpMaxPlayer, m);
End;

Procedure TForm4.Edit2Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  m := TMemoryStream.Create;
  i := StrToIntDef(edit2.text, 1);
  m.Write(i, sizeof(i));
  i := StrToIntDef(edit3.text, 1);
  m.Write(i, sizeof(i));
  i := StrToIntDef(edit4.text, 1);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpLives, m);
End;

Procedure TForm4.FormShow(Sender: TObject);
Begin
  form4.left := strtoint(GetValue('MapEditorForm', 'Left', inttostr(Form1.Left + Form1.Width)));
  form4.top := strtoint(GetValue('MapEditorForm', 'Top', inttostr(form1.Top)));
  form4.Width := strtoint(GetValue('MapEditorForm', 'Width', inttostr(form4.Width)));
  form4.Height := strtoint(GetValue('MapEditorForm', 'Height', inttostr(form4.Height)));
  FixFormPosition(form4);
End;

Procedure TForm4.ListBox1Click(Sender: TObject);
Var
  by: TBuyAble;
Begin
  // Preview Buyable Item
  If ListBox1.ItemIndex = -1 Then exit;
  by := StringToBuyAble(ListBox1.Items[ListBox1.ItemIndex]);
  edit6.text := inttostr(by.WaveNum + 1);
  edit7.text := inttostr(by.Count);
End;

Procedure TForm4.ListBox1DblClick(Sender: TObject);
Var
  by: TBuyAble;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    by := StringToBuyAble(ListBox1.Items[ListBox1.ItemIndex]);
    Case by.Kind Of
      bkBuilding: Begin
          // Öffnen eines Gebäudes im Editor
          fOnFinishedLoadListDuringLoadObj := ExtractFileNameOnly(by.Item);
          Form14.LoadBuildingSettings(@OnFinishedLoadListDuringLoad);
        End;
      bkHero: Begin
          fOnFinishedLoadListDuringLoadObj := ExtractFileNameOnly(by.Item);
          Form14.LoadHeroSettings(@OnFinishedLoadListDuringLoad);
        End;
    Else Begin
        Raise exception.Create('TForm4.ListBox1DblClick: implementieren.');
      End;
    End;
  End;
End;

Procedure TForm4.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var
  h: integer;
  lb: TListBox;
Begin
  lb := TListBox(Control);
  h := 0;
  If assigned(lb.Items.Objects[index]) Then Begin
    h := ARect.Bottom - ARect.top;
  End;
  // Erst mal Den "alten" Text wie gewohnt rendern
  lb.Canvas.TextRect(aRect, ARect.Left + h, (ARect.Bottom + ARect.top - lb.Canvas.TextHeight('8')) Div 2, ' ' + lb.Items[index]);
  // Gibt es Meta Infos -> Anzeigen
  If assigned(lb.Items.Objects[index]) Then Begin
    lb.Canvas.StretchDraw(rect(ARect.Left, arect.Top, ARect.Left + h, ARect.Bottom), TItemObject(lb.Items.Objects[index]).Image);
  End;
End;

Procedure TForm4.Memo1Change(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.WriteAnsiString(Serialize(memo1.Text));
  ctd.UpdateMapProperty(mpDescription, m);
End;

Procedure TForm4.PageControl2Change(Sender: TObject);
Begin
  // Braucht nicht, weil beim Wechsel zwischen den Opponent Tabs ebenfalls ein Refresh gesendet wird.
  //  RefreshOpponentsClick(Nil);
End;

Procedure TForm4.ScrollBar1Change(Sender: TObject);
Begin
  label7.caption := 'Cursor size: ' + inttostr((ScrollBar1.Position * 2) + 1);
End;

Procedure TForm4.ScrollBar2Change(Sender: TObject);
Begin
  label18.caption := 'Cursor size: ' + inttostr((ScrollBar2.Position * 2) + 1);
End;

Procedure TForm4.Timer1Timer(Sender: TObject);
Begin
  If ctd.BlockMapUpdateSending Or Form4updating Then exit;
  timer1.enabled := false; // Auf jeden Abschalten
  If Not assigned(ctd) Then Begin
    exit;
  End;
  Button4.Click; // refresh der Placements liste
  RefreshOpponentsClick(Nil); // Wave Opponents
End;

Procedure TForm4.OnFinishedLoadListDuringLoad(Sender: TObject);
Var
  obj: String;
  j: Integer;
Begin
  obj := fOnFinishedLoadListDuringLoadObj;
  fOnFinishedLoadListDuringLoadObj := '';
  If obj = '' Then exit;
  // Suchen des Eintrages in der Listbox2
  For j := 0 To form14.ListBox2.Items.Count - 1 Do
    If obj = form14.ListBox2.Items[j] Then Begin
      form1.HideForm4;
      Form14.Show;
      form14.ListBox2.ItemIndex := j;
      If form6.Visible Then form6.hide;
      If form7.Visible Then form7.hide;
      If form19.Visible Then form19.hide;
      // Öffnen des Gebäudes
      form14.Button5.Click;
      exit;
    End;
End;

Procedure TForm4.OnGetComboBox2Content(Sender: TObject; Const Data: TStringlist
  );
Var
  i: Integer;
Begin
  ComboBox2.Sorted := true;
  ComboBox2.Items.Clear;
  For i := 0 To data.Count - 1 Do Begin
    ComboBox2.Items.add(data[i]);
  End;
  //  ComboBox2.Items.Text := Data.Text;

  If ComboBox2.Items.Count <> 0 Then Begin
    ComboBox2.ItemIndex := 0;
  End
  Else Begin
    ComboBox2.Text := '';
  End;
  ComboBox2Change(Nil);
End;

Procedure TForm4.OnGetPageControl2Content(Sender: TObject;
  Const Data: TStringlist);
Var
  f: TWaveFrame;
  i: Integer;
Begin
  For i := 0 To PageControl2.PageCount - 1 Do Begin
    f := (PageControl2.Pages[i].Components[0]) As TWaveFrame;
    f.UpdateOpponents(Data.CommaText);
  End;
End;

Procedure TForm4.OnTransferFileDone(Sender: TObject; Suceed: Boolean);
Var
  m: TMemoryStream;
Begin
  If Suceed Then Begin
    Case fOnTransferFileDoneReason Of
      frBackTex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + ExtractFileName(fTransferFilename))) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          (*
           * KA warum, aber wenn man nicht ein "bisschen" wartet, dann wird der Client nicht richtig aktualisiert..
           * Empirsch gemessen gehts ab ca. 2ms
           *)
          sleep(250);
          m := TMemoryStream.Create;
          m.WriteAnsiString(ExtractFileName(fTransferFilename));
          ctd.UpdateMapProperty(mpBackTex, m);
          CheckBox1.Checked := true; // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC1Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc1.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc1.png');
          ctd.UpdateMapProperty(mpDC1Tex, m);
          Image1.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc1.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC2Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc2.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc2.png');
          ctd.UpdateMapProperty(mpDC2Tex, m);
          Image2.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc2.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC3Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc3.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc3.png');
          ctd.UpdateMapProperty(mpDC3Tex, m);
          Image3.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc3.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC4Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc4.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc4.png');
          ctd.UpdateMapProperty(mpDC4Tex, m);
          Image4.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc4.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End
    Else Begin
        logshow('TForm4.OnTransferFileDone: unknown reason.', llerror);
        exit;
      End;
    End;
  End
  Else Begin
    logshow(format('Could not send %s to map server', [fTransferFilename]), llerror);
  End;
  fOnTransferFileDoneReason := frUnknown;
End;

Procedure TForm4.AdjustMoveWaveButtons;
Var
  i: Integer;
Begin
  For i := 0 To PageControl2.PageCount - 1 Do Begin
    (PageControl2.Pages[i].Components[0] As TWaveFrame).Button3.Visible := i <> (PageControl2.PageCount - 1);
    (PageControl2.Pages[i].Components[0] As TWaveFrame).label1.Visible := (PageControl2.Pages[i].Components[0] As TWaveFrame).Button2.Visible Or (PageControl2.Pages[i].Components[0] As TWaveFrame).Button3.Visible;
  End;
End;

Procedure TForm4.RefreshOpponentsClick(Sender: TObject);
Begin
  If Form4updating Then exit;
  ctd.GetFileList('*.opp', @OnGetPageControl2Content);
End;

Procedure TForm4.ChangeWaveIndexClick(WaveIndex: integer; Direction: Boolean);
Var
  w1, w2: TWave;
Begin
  // Verschieben einer Kompletten Wave entlang der Richtung
  If Direction Then Begin
    w1 := (PageControl2.Pages[WaveIndex].Components[0] As TWaveFrame).GetWave();
    w2 := (PageControl2.Pages[WaveIndex + 1].Components[0] As TWaveFrame).GetWave();
    (PageControl2.Pages[WaveIndex].Components[0] As TWaveFrame).LoadWave(w2);
    (PageControl2.Pages[WaveIndex + 1].Components[0] As TWaveFrame).LoadWave(w1);
    PageControl2.PageIndex := WaveIndex + 1;
  End
  Else Begin
    w1 := (PageControl2.Pages[WaveIndex].Components[0] As TWaveFrame).GetWave();
    w2 := (PageControl2.Pages[WaveIndex - 1].Components[0] As TWaveFrame).GetWave();
    (PageControl2.Pages[WaveIndex].Components[0] As TWaveFrame).LoadWave(w2);
    (PageControl2.Pages[WaveIndex - 1].Components[0] As TWaveFrame).LoadWave(w1);
    PageControl2.PageIndex := WaveIndex - 1;
  End;
End;

Procedure TForm4.AddWave(Reset: Boolean);
Var
  t: TTabSheet;
  f: TWaveframe;
Begin
  t := PageControl2.AddTabSheet;
  t.caption := 'Wave' + inttostr(PageControl2.PageCount);
  t.name := t.caption;
  f := TWaveFrame.Create(t);
  f.name := 'WaveFrame' + inttostr(PageControl2.PageCount);
  f.Parent := t;
  f.RefreshOppentList := @RefreshOpponentsClick;
  f.ChangeWaveIndex := @ChangeWaveIndexClick;
  f.WaveNum := PageControl2.PageCount - 1;
  If reset Then Begin
    ResetWave(t);
  End;
End;

Procedure TForm4.ResetWave(Const Page: TTabSheet);
Var
  f: TWaveFrame;
Begin
  f := page.Components[0] As TWaveFrame;
  f.Reset;
  //  PageControl2.Height := f.Height;
  //  PageControl2.Width := f.Width;
End;

Procedure TForm4.DelLastWave;
Begin
  PageControl2.Pages[PageControl2.PageCount - 1].free;
End;

End.

