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
  Classes, SysUtils, uopengl_graphikengine;

Type

  TMark = Record
    CreationTime: int64;
    x, y: integer;
  End;

  { TSplashMarkManager }

  TSplashMarkManager = Class
  private
    fMarks: Array Of TMark;
    Procedure RenderAbstract({$IFNDEF LEGACYMODE}l, t, d, {$ENDIF}blockw, blockh: Single);
  public
    Texture: TGraphikItem;
    Procedure AddMark(x, y: integer);
    Procedure Render(sx, sy, x, y: integer);
    Procedure RenderPreview(x, y, w, h, width, height: integer); // Gendert eine Vorschau der Karte
    Constructor Create;
    Destructor Destroy; override;
  End;

Implementation

Uses LCLIntf, uctd_common, dglOpenGL, uvectormath;

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

Procedure TSplashMarkManager.RenderAbstract({$IFNDEF LEGACYMODE}l, t, d, {$ENDIF}blockw, blockh: Single);
Var
{$IFDEF LEGACYMODE}
  s,
{$ENDIF}
  i: integer;
  n: int64;
{$IFNDEF LEGACYMODE}
  s,
{$ENDIF}
  sx, sy: Single;
Begin
  n := GetTick();
{$IFDEF LEGACYMODE}
  glPushMatrix;
  // So Skallieren, dass alles nachfolgende in MAP_BLOCK_WIDTH koordinaten gerendert werden kann.
  glScalef(blockw, blockh, 1);
  glColor4f(1, 0, 0, 1);
  For i := 0 To high(fMarks) Do Begin
    glPushMatrix;
    // Vom Erzeugungszeitpunkt bis zum "Sterben" wächst das Ausrufezeichen auf Mapsize * 7 an
    s := round(((n - fMarks[i].CreationTime) / MarkLifeTime) * 7 * MapBlockSize);
    glTranslatef(fMarks[i].x * MapBlockSize + MapBlockSize / 2, fMarks[i].y * MapBlockSize + MapBlockSize / 2, 0);
    RenderAlphaQuad(v2(0, 0), s, s, 180, Texture.Image);
    glpopmatrix;
  End;
  glpopmatrix;
{$ELSE}
  // So Skallieren, dass alles nachfolgende in MAP_BLOCK_WIDTH koordinaten gerendert werden kann.
  sx := blockw;
  sy := blockh;
  UseTextureShader(v4(1, 0, 0, 1));
  For i := 0 To high(fMarks) Do Begin
    // Vom Erzeugungszeitpunkt bis zum "Sterben" wächst das Ausrufezeichen auf Mapsize * 7 an
    s := (((n - fMarks[i].CreationTime) / MarkLifeTime) * 7) * MapBlockSize;
    RenderAlphaQuad(
      l + (fMarks[i].x * MapBlockSize + MapBlockSize / 2) * sx - s * sx / 2
      , t + (fMarks[i].y * MapBlockSize + MapBlockSize / 2) * sy - s * sy / 2
      , d
      , s * sx
      , s * sy
      , Texture
      );
  End;
  UseTextureShader();
{$ENDIF}
End;

Procedure TSplashMarkManager.Render(sx, sy, x, y: integer);
Var
  i: Integer;
  n: int64;
  j: Integer;
Begin
  n := GetTick();
{$IFDEF LEGACYMODE}
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, Texture.Image);
  glPushMatrix;
  glTranslatef(x - sx, y - sy, ctd_Map_Layer);
  glDisable(GL_DEPTH_TEST);
  RenderAbstract(1, 1);
  glEnable(GL_DEPTH_TEST);
  glpopmatrix;
{$ELSE}
  glDisable(GL_DEPTH_TEST);
  RenderAbstract(x - sx, y - sy, ctd_Map_Layer, 1, 1);
  glEnable(GL_DEPTH_TEST);
{$ENDIF}
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
{$IFDEF LEGACYMODE}
  glBindTexture(GL_TEXTURE_2D, Texture.Image);
  glColor4f(1, 1, 1, 1);
  glPushMatrix;
  glTranslatef(x, y, ctd_Menu_Layer + ctd_Epsilon);
  glDisable(GL_DEPTH_TEST);
  RenderAbstract(w / (width * MapBlockSize), h / (height * MapBlockSize));
  glEnable(GL_DEPTH_TEST);
  glPopMatrix;
{$ELSE}
  glDisable(GL_DEPTH_TEST);
  RenderAbstract(x, y, ctd_Menu_Layer + ctd_Epsilon, w / (width * MapBlockSize), h / (height * MapBlockSize));
  glEnable(GL_DEPTH_TEST);
{$ENDIF}
End;

End.

