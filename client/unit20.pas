Unit Unit20;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, IpHtml;

Type

  { TForm20 }

  TForm20 = Class(TForm)
    Button1: TButton;
    IpHtmlPanel1: TIpHtmlPanel;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure TreeView1Click(Sender: TObject);
  private
    Procedure LoadPage(Const Filename: String);

  public
    Procedure LoadHelp;

  End;

Var
  Form20: TForm20;

Implementation

{$R *.lfm}

{ TForm20 }

Procedure TForm20.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm20.FormCreate(Sender: TObject);
Begin
  caption := 'Online help';
End;

Procedure TForm20.TreeView1Click(Sender: TObject);
Var
  n: TTreeNode;
Begin
  n := TreeView1.Selected;
  If assigned(n) Then Begin
    LoadPage(n.Text);
  End;
End;

Procedure TForm20.LoadPage(Const Filename: String);
Var
  error_404, fn: String;
  NewHTML: TIpHtml;
  m: TMemoryStream;
Begin
  fn := 'help' + PathDelim + Filename + '.html';
  NewHTML := TIpHtml.Create;
  m := TMemoryStream.Create;
  If FileExists(fn) Then Begin
    m.LoadFromFile(fn);
  End
  Else Begin
    error_404 :=
      '<!DOCTYPE html>' + LineEnding +
      '<html>' + LineEnding +
      '<body>' + LineEnding +
      '<h1>Error:</h1>' + LineEnding +
      '<p>404 could not find: ' + Filename + '.html</p>' + LineEnding +
      '</body>' + LineEnding +
      '</html>';
    m.Write(error_404[1], length(error_404));
    m.Position := 0;
  End;
  NewHTML.LoadFromStream(m);
  IpHtmlPanel1.SetHtml(NewHTML);
End;

Procedure TForm20.LoadHelp;
Var
  sl: TStringList;
  i: Integer;
  s, pre, suf: String;
  par: TTreeNode;
Begin
  TreeView1.Items.Clear;
  If Not FileExists('help' + PathDelim + 'index.txt') Then Begin
    showmessage('Error, online help is not available, please use launcher to update.');
    LoadPage('General');
    exit;
  End;
  sl := TStringList.Create;
  sl.LoadFromFile('help' + PathDelim + 'index.txt');
  For i := 0 To sl.count - 1 Do Begin
    s := sl[i];
    If trim(s) = '' Then Continue;
    If s[1] = '#' Then Continue;
    pre := copy(s, 1, pos('-', s) - 1);
    suf := copy(s, pos('>', s) + 1, length(s));
    par := TreeView1.Items.FindNodeWithText(pre);
    TreeView1.Items.AddChild(par, suf);
  End;
  TreeView1.FullExpand;
  If TreeView1.Items.Count <> 0 Then TreeView1.Items.SelectOnlyThis(TreeView1.Items[0]);
  sl.free;
  LoadPage('General');
End;

End.

