Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ExtDlgs, types, uopengl_animation;

Type

  TOpp = Record
    Image: TBitmap;
    Filename: String;
    Path: String;
    Name: String;
    Description: String;
    Lifepoints: Array[0..3] Of integer;
    LifeFactor: Array[0..2] Of Single;
    Speed: Single;
    Sizex: Single;
    SizeY: Single;
    CanFly: Boolean;
    boss: Boolean;
    Bonus: Boolean;
    infoString: String; // - Die Sortierinformationen
    ImageRotation: Boolean;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    RadioGroup1: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure RadioGroup1Click(Sender: TObject);
  private
    { private declarations }
    Function OppFileToTOpp(Filename: String): Topp;
    Function LoadImage(Filename: String): TBitmap;
    Procedure Sort(Index: integer; Direction: Boolean); // Sortiert Opplist entsprechend Index und hällt auch listbox1 Synchron.
    Procedure Clear();
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  OppList: Array Of TOpp;

Implementation

Uses IniFiles, strutils, math, LazUTF8, LazFileUtils, ugraphics;

{$R *.lfm}

Function Select(Index, item: integer): String;
Begin
  result := 'Undefiniert';
  Case item Of
    0: result := format('%0.1f', [OppList[index].Speed]);
    1: result := format('%d', [OppList[index].Lifepoints[0] + OppList[index].Lifepoints[1] + OppList[index].Lifepoints[2] + OppList[index].Lifepoints[3]]);
    2: result := format('%d', [OppList[index].Lifepoints[0]]);
    3: result := format('%d', [OppList[index].Lifepoints[1]]);
    4: result := format('%d', [OppList[index].Lifepoints[2]]);
    5: result := format('%d', [OppList[index].Lifepoints[3]]);
    6: result := format('%d', [ord(OppList[index].CanFly)]);
    7: result := format('%d', [ord(OppList[index].boss)]);
    8: result := format('%d', [ord(OppList[index].Bonus)]);
  End;
End;

Function DeSerialize(Const Value: String): String;
Var
  i, j: integer;
Begin
  setlength(result, length(value) * 2);
  j := 1;
  i := 1;
  While i <= length(value) Do Begin
    If value[i] = '#' Then Begin
      inc(i);
      If i > length(value) Then Begin
        Raise exception.create('Error invalid deserialize String "' + value + '"');
      End;
      Case Value[i] Of
        '#': Begin
            result[j] := '#';
            inc(j);
          End;
        '+': Begin
{$IFDEF Windows}
            result[j] := LineEnding[1];
            result[j + 1] := LineEnding[2];
            inc(j, 2);
{$ELSE}
            result[j] := LineEnding;
            inc(j);
{$ENDIF}
          End
      Else Begin
          Raise exception.Create('Error "' + value[i] + '" Not known as deserialize param.');
        End;
      End;
      inc(i);
    End
    Else Begin
      result[j] := value[i];
      inc(i);
      inc(j);
    End;
  End;
  setlength(result, j - 1);
End;

Function Serialize(Const Value: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 1 To length(value) Do Begin
    Case value[i] Of
{$IFDEF Windows}
      #10: Begin // Das Muss Geschluckt werden
        End;
      #13: Begin
{$ELSE}
      LineEnding: Begin
{$ENDIF}
          result := result + '#+';
        End;
      '#': Begin
          result := result + '##';
        End
    Else Begin
        result := result + value[i];
      End;
    End;
  End;
End;

{ TForm1 }

Procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Begin
  // Draw the List
  ListBox1.Canvas.Pen.Color := ListBox1.Canvas.Brush.Color; // Zumindest unter Windows braucht man das
  ListBox1.Canvas.Rectangle(ARect); // Zumindest unter Windows braucht man das
  ListBox1.Canvas.StretchDraw(rect(ARect.Left + 2, ARect.Top + 2, ARect.Left + ARect.Bottom - ARect.Top - 4, ARect.Bottom - 2), OppList[Index].Image);
  ListBox1.Canvas.TextOut(ARect.Right - ListBox1.Canvas.TextWidth(OppList[Index].infoString) - 2, ARect.Top + 2, OppList[Index].infoString);
  ListBox1.Canvas.TextOut(ARect.Left + ARect.Bottom - ARect.Top, ARect.Top + 2, OppList[Index].Filename);
End;

Procedure TForm1.RadioGroup1Click(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To high(OppList) Do Begin
    OppList[i].infoString := select(i, RadioGroup1.ItemIndex);
  End;
  ListBox1.Invalidate;
End;

Function TForm1.OppFileToTOpp(Filename: String): Topp;
Var
  ini: tinifile;
  s, p: String;
Begin
  FormatSettings.DecimalSeparator := '.';
  ini := TIniFile.Create(Filename);
  p := IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
  result.Name := ini.ReadString('Opponent', 'Name', '');
  result.filename := ExtractFileNameOnly(Filename);
  result.Path := p;
  result.Image := loadImage(p + ini.ReadString('Opponent', 'image', ''));
  result.Description := DeSerialize(ini.ReadString('Opponent', 'description', ''));
  result.Lifepoints[0] := ini.ReadInteger('Opponent', 'lifepoints0', 0);
  result.Lifepoints[1] := ini.ReadInteger('Opponent', 'lifepoints1', 0);
  result.Lifepoints[2] := ini.ReadInteger('Opponent', 'lifepoints2', 0);
  result.Lifepoints[3] := ini.ReadInteger('Opponent', 'lifepoints3', 0);
  result.LifeFactor[0] := ini.ReadFloat('Opponent', 'lifefactor0', 0);
  result.LifeFactor[1] := ini.ReadFloat('Opponent', 'lifefactor1', 0);
  result.LifeFactor[2] := ini.ReadFloat('Opponent', 'lifefactor2', 0);
  result.Speed := ini.ReadFloat('Opponent', 'speed', 0);
  s := ini.ReadString('Opponent', 'size', '0x0');
  result.Sizex := StrToFloat(copy(s, 1, pos('x', s) - 1));
  result.Sizey := StrToFloat(copy(s, pos('x', s) + 1, length(s)));
  result.CanFly := ini.ReadBool('Opponent', 'canfly', false);
  result.boss := ini.ReadBool('Opponent', 'boss', false);
  result.Bonus := ini.ReadBool('Opponent', 'bonus', false);
  result.ImageRotation := ini.ReadBool('Opponent', 'imagerotation', true);
  result.infoString := '';
  ini.free;
End;

Function TForm1.LoadImage(Filename: String): TBitmap;
Var
  png: TPortableNetworkGraphic;
  jpg: TJPEGImage;
  ani: TOpenGL_Animation;
  b: TBitmap;
Begin
  result := Tbitmap.create;
  Case lowercase(ExtractFileExt(Filename)) Of
    '.png': Begin
        png := TPortableNetworkGraphic.Create;
        png.LoadFromFile(Filename);
        result.Assign(png);
        png.free;
      End;
    '.bmp': Begin
        result.LoadFromFile(Filename);
      End;
    '.jpg': Begin
        jpg := TJPEGImage.Create;
        jpg.LoadFromFile(Filename);
        result.Assign(jpg);
        jpg.free;
      End;
    '.ani': Begin
        // Auspacken der Animationsgraphik
        ani := TOpenGL_Animation.Create;
        ani.LoadFromFile(Filename, False);
        b := ani.GetFirstBitmap();
        result.assign(b);
        ani.free;
        b.free;
      End;
  End;
  result.TransparentColor := clFuchsia;
  result.Transparent := true;
End;

Procedure TForm1.Sort(Index: integer; Direction: Boolean);
  Function Compare(i1, i2: integer): Boolean;
  Var
    v1, v2: String;
    m: integer;
  Begin
    v1 := Select(i1, Index);
    v2 := Select(i2, Index);
    m := max(length(v1), length(v2));
    v1 := AddChar('0', v1, m);
    v2 := AddChar('0', v2, m);
    result := CompareStr(v1, v2) >= 0;
    result := Direction Xor result;
  End;

Var
  i, j: integer;
  h: TOpp;
Begin
  // Aktualisieren der Infostrings
  For i := 0 To high(OppList) Do Begin
    OppList[i].infoString := Select(i, Index);
  End;
  // Sortieren -- Absichtlich kein Quicksort, da der nicht Ordnungserhaltend ist !!
  For i := high(OppList) Downto 1 Do Begin
    For j := 0 To i - 1 Do Begin
      // Aufsteigend
      If Compare(j, j + 1) Then Begin
        h := OppList[j];
        OppList[j] := OppList[j + 1];
        OppList[j + 1] := h;
      End;
    End;
  End;
  // Die Listbox Aktualisieren
  ListBox1.Items.BeginUpdate;
  For i := 0 To high(OppList) Do Begin
    ListBox1.Items[i] := OppList[i].Name;
  End;
  ListBox1.Items.EndUpdate;
End;

Procedure TForm1.Clear();
Var
  i: Integer;
Begin
  For i := 0 To high(OppList) Do Begin
    OppList[i].Image.free;
  End;
  setlength(OppList, 0);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  sl: Tstringlist;
  i: Integer;
Begin
  SelectDirectoryDialog1.FileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0)));
  If SelectDirectoryDialog1.Execute Then Begin
    sl := FindAllFiles(SelectDirectoryDialog1.FileName, '*.opp', true);
    Clear();
    setlength(OppList, sl.count);
    ListBox1.Items.Clear;
    For i := 0 To sl.count - 1 Do Begin
      OppList[i] := OppFileToTOpp(sl[i]);
      ListBox1.Items.add(OppList[i].Name);
    End;
    sl.free;
    button2.Click;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  s := '';
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ListBox1.Items[ListBox1.ItemIndex];
  End;
  Sort(RadioGroup1.ItemIndex, false);
  ListBox1.Invalidate;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If s = ListBox1.Items[i] Then Begin
      ListBox1.ItemIndex := i;
      ListBox1.OnClick(Nil);
      break;
    End;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  s := '';
  If ListBox1.ItemIndex <> -1 Then Begin
    s := OppList[ListBox1.ItemIndex].Name;
  End;
  Sort(RadioGroup1.ItemIndex, true);
  ListBox1.Invalidate;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If s = OppList[i].Name Then Begin
      ListBox1.ItemIndex := i;
      ListBox1.OnClick(Nil);
      break;
    End;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  nf, trootf, srootf, s: String;
  i: Integer;
  sl: TStringList;
  ini: TIniFile;
  b: TBitmap;
Begin
  // Create Opp with new Image
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, no existing opp selected.');
    exit;
  End;
  If OpenPictureDialog1.Execute Then Begin
    s := '';
    Repeat
      s := InputBox('Select new name', '', s);
      If s = '' Then Begin // Abbruch durch Nutzer
        exit;
      End;
      For i := 0 To high(OppList) Do Begin
        If lowercase(ExtractFileNameOnly(opplist[i].Filename)) = lowercase(s) Then Begin
          s := '';
          break;
        End;
      End;
      If s = '' Then Begin
        showmessage('Error, name already exists, please choose other.');
      End;
    Until s <> '';
    // Den neuen Gegner anlegen
    srootf := OppList[ListBox1.ItemIndex].Path;
    trootf := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFileDir(ExcludeTrailingPathDelimiter(srootf))) + s);
    If Not ForceDirectory(trootf) Then Begin
      showmessage('Error, could not create:' + trootf);
      exit;
    End;
    // Alle Dateien vom Quellverzeichnis ins Ziel Verzeichnis Kopieren
    sl := FindAllFiles(srootf, '', false);
    For i := 0 To sl.count - 1 Do Begin
      If Not CopyFile(sl[i], trootf + ExtractFileName(sl[i])) Then Begin
        showmessage('Error, could not copy: ' + sl[i]);
        exit;
      End;
    End;
    // Die Opp datei umbenennen
    nf := trootf + s + '.opp';
    If Not RenameFile(trootf + opplist[ListBox1.ItemIndex].Filename + '.opp', nf) Then Begin
      showmessage('Error, could not rename to: ' + nf);
      exit;
    End;
    // Das Alte Image Löschen
    ini := TIniFile.Create(nf);
    If Not DeleteFile(IncludeTrailingPathDelimiter(ExtractFileDir(nf)) + ini.ReadString('opponent', 'image', '')) Then Begin
      showmessage('Error, could not delete old image:' + ini.ReadString('opponent', 'image', ''));
      exit;
    End;
    // Das neue Image rein Kopieren
    If Not CopyFile(OpenPictureDialog1.FileName, IncludeTrailingPathDelimiter(ExtractFileDir(nf)) + ExtractFileName(OpenPictureDialog1.FileName)) Then Begin
      showmessage('Error, could not copy new image file ' + OpenPictureDialog1.FileName + ' -> ' + ExtractFileDir(nf));
      exit;
    End;
    ini.WriteString('opponent', 'image', ExtractFileName(OpenPictureDialog1.FileName));
    // Die Größe anpassen
    b := LoadImage(OpenPictureDialog1.Filename);
    ini.writestring('opponent', 'size', format('%0.2fx%0.2f', [b.Width / 10, b.height / 10]));
    b.free;
    ini.free;
    // In die Listbox rein laden
    setlength(OppList, high(OppList) + 2);
    OppList[high(OppList)] := OppFileToTOpp(nf);
    ListBox1.Items.add(OppList[high(OppList)].Name);
    // Sortieren
    Button2Click(Nil);
    RadioGroup1Click(Nil);
    // Das eingefügte anwählen
    nf := ExtractFileNameOnly(nf);
    For i := 0 To ListBox1.items.Count - 1 Do Begin
      If ListBox1.Items[i] = nf Then Begin
        ListBox1.ItemIndex := i;
        ListBox1Click(Nil);
        break;
      End;
    End;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  ini: TIniFile;
  i: integer;
Begin
  If ListBox1.ItemIndex = -1 Then exit;
  i := ListBox1.ItemIndex;
  ini := TIniFile.Create(OppList[i].Path + OppList[i].Filename + '.opp');
  ini.WriteString('Opponent', 'Name', Edit1.text);
  ini.WriteString('Opponent', 'description', Serialize(Memo1.Text));
  ini.WriteInteger('Opponent', 'lifepoints0', strtointdef(edit5.text, 0));
  ini.WriteInteger('Opponent', 'lifepoints1', strtointdef(edit6.text, 0));
  ini.WriteInteger('Opponent', 'lifepoints2', strtointdef(edit7.text, 0));
  ini.WriteInteger('Opponent', 'lifepoints3', strtointdef(edit8.text, 0));
  ini.WriteFloat('Opponent', 'lifefactor0', strtofloatdef(edit2.text, 0.0));
  ini.WriteFloat('Opponent', 'lifefactor1', strtofloatdef(edit9.text, 0.0));
  ini.WriteFloat('Opponent', 'lifefactor2', strtofloatdef(edit10.text, 0.0));
  ini.WriteFloat('Opponent', 'speed', strtofloatdef(edit3.text, 0.0));
  ini.WriteString('Opponent', 'size', Edit4.text);
  ini.WriteBool('Opponent', 'canfly', CheckBox1.Checked);
  ini.WriteBool('Opponent', 'boss', CheckBox2.Checked);
  ini.WriteBool('Opponent', 'bonus', CheckBox3.Checked);
  ini.WriteBool('Opponent', 'imagerotation', CheckBox4.Checked);
  ini.free;
  // Reload
  OppList[i] := OppFileToTOpp(OppList[i].Path + OppList[i].Filename + '.opp');
  OppList[i].infoString := select(i, RadioGroup1.ItemIndex);
  ListBox1.Invalidate;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Clear();
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *           0.02 = Support für Animationen
   *           0.03 = Crash wegen Readonly Checkboxen gefixt
   *                  Image changer, Edit erlaubt
   *)
  caption := 'Opponent viewer ver. 0.03';
  OppList := Nil;
  edit1.text := '';
  edit2.text := '';
  edit3.text := '';
  edit4.text := '';
  edit5.text := '';
  edit6.text := '';
  edit7.text := '';
  edit8.text := '';
  edit9.text := '';
  edit10.text := '';
  CheckBox4.Checked := true;
  memo1.clear;
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Var
  i: integer;
Begin
  // Laden des gerade gewählten Datensatzes in den Infobereich
  If ListBox1.ItemIndex <> -1 Then Begin
    i := ListBox1.ItemIndex;
    edit1.text := OppList[i].Name;
    Memo1.text := OppList[i].Description;
    Edit3.Text := format('%0.2f', [OppList[i].Speed]);
    Edit4.Text := format('%0.2fx%0.2f', [OppList[i].Sizex, OppList[i].Sizey]);
    Edit5.Text := format('%d', [OppList[i].Lifepoints[0]]);
    Edit6.Text := format('%d', [OppList[i].Lifepoints[1]]);
    Edit7.Text := format('%d', [OppList[i].Lifepoints[2]]);
    Edit8.Text := format('%d', [OppList[i].Lifepoints[3]]);
    Edit2.Text := format('%0.2f', [OppList[i].LifeFactor[0]]);
    Edit9.Text := format('%0.2f', [OppList[i].LifeFactor[1]]);
    Edit10.Text := format('%0.2f', [OppList[i].LifeFactor[2]]);
    CheckBox1.Checked := OppList[i].CanFly;
    CheckBox2.Checked := OppList[i].boss;
    CheckBox3.Checked := OppList[i].Bonus;
    CheckBox4.Checked := OppList[i].imagerotation;
    Image1.Picture.Bitmap := OppList[i].Image;
  End;
End;

End.

