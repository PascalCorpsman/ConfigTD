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
Unit uctd_splashhintmanager;

{$MODE objfpc}{$H+}

{$I globalsettings.inc}

Interface

Uses
  Classes, SysUtils,
  uvectormath,
  uctd_common
  ;

Type

  TSplashItem = Record
    Text: String;
    TimeStamp: int64;
    Delay: integer;
    Color: TVector3;
  End;

  { TSplashHintManager }

  TSplashHintManager = Class
  private
    fSplashItems: Array Of TSplashItem;
    FPausing: Boolean;
    fPauseTime: int64;
  public
    Position: TPoint;
    Constructor create; virtual;
    Destructor Destroy; override;
    Procedure Render();
    Procedure AddText(Const Text: String; Delay: integer = DefaultSplashHintDelay); overload;
    Procedure AddText(Const Text: String; Color: TVector3); overload;
    Procedure AddText(Const Text: String; Delay: integer; Color: TVector3); overload;
    Procedure Pause(Value: Boolean);
  End;

Implementation

Uses
  LCLIntf, dglOpenGL,
  uOpenGL_ASCII_Font;

{ TSplashHintManager }

Constructor TSplashHintManager.create;
Begin
  Inherited create;
  Position := point(0, 0);
  fSplashItems := Nil;
  FPausing := false;
End;

Destructor TSplashHintManager.Destroy;
Begin
  setlength(fSplashItems, 0);
End;

Procedure TSplashHintManager.Render;
Var
  fs, ah: Single;
  i, j: Integer;
  n: int64;
Begin
  fs := OpenGL_ASCII_Font.Size;
  OpenGL_ASCII_Font.Size := OpenGL_ASCII_Font.Size * 2;
  glBindTexture(GL_TEXTURE_2D, 0);
  n := GetTick;
  // Löschen Aller Einträge Älter als
  For i := high(fSplashItems) Downto 0 Do Begin
    If fSplashItems[i].TimeStamp + fSplashItems[i].Delay < n Then Begin
      For j := i To high(fSplashItems) - 1 Do
        fSplashItems[j] := fSplashItems[j + 1];
      setlength(fSplashItems, high(fSplashItems));
    End;
  End;
  // Rendert Alle Texte Oberhalb von Position(x,y), letzter = Jüngster
  ah := 0;
  glTranslatef(0, 0, ctd_Tipp_Layer + ctd_EPSILON);
  For i := high(fSplashItems) Downto 0 Do Begin
    ah := ah + OpenGL_ASCII_Font.TextHeight(fSplashItems[i].Text);
    OpenGL_ASCII_Font.Colorv3 := fSplashItems[i].Color;
    OpenGL_ASCII_Font.Textout(Position.x, position.y - round(ah), fSplashItems[i].Text);
  End;
  OpenGL_ASCII_Font.Size := fs;
End;

Procedure TSplashHintManager.AddText(Const Text: String; Delay: integer);
Begin
  AddText(text, delay, DefaultSplashHintColor);
End;

Procedure TSplashHintManager.AddText(Const Text: String; Color: TVector3);
Begin
  AddText(text, DefaultSplashHintDelay, Color);
End;

Procedure TSplashHintManager.AddText(Const Text: String; Delay: integer;
  Color: TVector3);
Begin
  //  log('TSplashHintManager.AddText : ' + Text, llTrace);
  setlength(fSplashItems, high(fSplashItems) + 2);
  fSplashItems[high(fSplashItems)].Text := text;
  fSplashItems[high(fSplashItems)].TimeStamp := GetTick;
  fSplashItems[high(fSplashItems)].Delay := Delay;
  fSplashItems[high(fSplashItems)].Color := Color;
  //  logleave;
End;

Procedure TSplashHintManager.Pause(Value: Boolean);
Var
  delta: Int64;
  i: integer;
Begin
  If FPausing Then Begin
    If Not value Then Begin
      delta := GetTick - fPauseTime;
      For i := 0 To high(fSplashItems) Do Begin // Entsprechend die Zeiten verschieben
        fSplashItems[i].TimeStamp := fSplashItems[i].TimeStamp + delta;
      End;
    End;
  End
  Else Begin
    If value Then Begin
      fPauseTime := GetTick;
    End;
  End;
  FPausing := value;
End;

End.

