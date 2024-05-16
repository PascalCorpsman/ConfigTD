Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses FileUtil, UTF8Process;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  fileToCopyAndStart, target: String;
  aTime: QWord;
  p: TProcessUTF8;
Begin
  caption := 'CTD_updater ver. 0.01';
  If ParamCount >= 1 Then Begin
    fileToCopyAndStart := ParamStr(1);
  End
  Else Begin
    showmessage('Error, invalid parameters, terminate now.');
    halt;
  End;
  If Not FileExists(fileToCopyAndStart) Then Begin
    showmessage('Error, could not find:' + fileToCopyAndStart);
    halt;
  End;
  target := ExtractFileName(fileToCopyAndStart);
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  // Wir Versuchen die Ziel Datei zu löschen
  aTime := GetTickCount64;
  While FileExists(target) And (aTime + 10000 > GetTickCount64) Do Begin
    sleep(100);
    DeleteFile(target);
  End;
  If FileExists(target) Then Begin
    Showmessage('Error, could not delete: ' + target);
    halt;
  End;
  If Not CopyFile(fileToCopyAndStart, target) Then Begin
    Showmessage('Error, could not copy: ' + fileToCopyAndStart + ' -> ' + target);
    halt;
  End;
  p := TProcessUTF8.Create(Nil);
  p.Executable := target;
  p.Execute;
  p.free;
  halt;
End;

End.

