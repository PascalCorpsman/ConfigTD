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
Unit usplashmarkmanager;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils;

Type

  TMark = Record
    CreationTime: int64;
    x, y: integer;
  End;

  { TSplashMarkManager }

  TSplashMarkManager = Class
  private
    fMarks: Array Of TMark;
    Procedure RenderAbstract(blockw, blockh: Single);
  public
    Texture: integer;
    Procedure AddMark(x, y: integer);
    Procedure Render(sx, sy, x, y: integer);
    Procedure RenderPreview(x, y, w, h, width, height: integer); // Gendert eine Vorschau der Karte
    Constructor Create;
    Destructor Destroy; override;
  End;

Implementation

Uses LCLIntf, uctd_common, dglOpenGL, uopengl_graphikengine;

{ TSplashMarkManager }

Constructor TSplashMarkManager.Create;
Begin
  Inherited create;
End;

Destructor TSplashMarkManager.Destroy;
Begin
  setlength(fMarks, 0);
End;

Procedure TSplashMarkManager.AddMark(x, y: integer);
Begin
  setlength(fMarks, high(fMarks) + 2);
  fMarks[high(fMarks)].CreationTime := gettick();
  fMarks[high(fMarks)].x := x;
  fMarks[high(fMarks)].y := y;
End;

Procedure TSplashMarkManager.RenderAbstract(blockw, blockh: Single);
Var
  s, i: integer;
  n: int64;
Begin
  n := GetTick();
  glPushMatrix;
  // So Skallieren, dass alles nachfolgende in MAP_BLOCK_WIDTH koordinaten gerendert werden kann.
  glScalef(blockw, blockh, 1);
  glColor4f(1, 0, 0, 1);
  For i := 0 To high(fMarks) Do Begin
    glPushMatrix;
    // Vom Erzeugungszeitpunkt bis zum "Sterben" wächst das Ausrufezeichen auf Mapsize * 7 an
    s := round(((n - fMarks[i].CreationTime) / MarkLifeTime) * 7 * MapBlockSize);
    glTranslatef(fMarks[i].x * MapBlockSize + MapBlockSize / 2, fMarks[i].y * MapBlockSize + MapBlockSize / 2, 0);
    RenderAlphaQuad(point(0, 0), s, s, 180, Texture);
    glpopmatrix;
  End;
  glpopmatrix;
End;

Procedure TSplashMarkManager.Render(sx, sy, x, y: integer);
Var
  i: Integer;
  n: int64;
  j: Integer;
Begin
  n := GetTick();
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glPushMatrix;
  glTranslatef(x - sx, y - sy, ctd_Map_Layer);
  glDisable(GL_DEPTH_TEST);
  RenderAbstract(1, 1);
  glEnable(GL_DEPTH_TEST);
  glpopmatrix;
  // Alte Marks Raus Schmeisen
  For i := high(fMarks) Downto 0 Do Begin
    If n - fMarks[i].CreationTime > MarkLifeTime Then Begin
      For j := i To high(fMarks) - 1 Do Begin
        fMarks[j] := fMarks[j + 1];
      End;
      SetLength(fMarks, high(fMarks));
    End;
  End;
End;

Procedure TSplashMarkManager.RenderPreview(x, y, w, h, width, height: integer);
Begin
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glPushMatrix;
  glTranslatef(x, y, ctd_Menu_Layer + ctd_Epsilon);
  glDisable(GL_DEPTH_TEST);
  RenderAbstract(w / (width * MapBlockSize), h / (height * MapBlockSize));
  glEnable(GL_DEPTH_TEST);
  glPopMatrix;
End;

End.

