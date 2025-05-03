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
Unit uctd_maptext;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, uctd_map, uctd_common, Graphics, ugraphics;

Type

  { TMapTex }

  TMapTex = Class
  private
    fmap: TMap;
    fHeightMap: TBitmap;
    Procedure CreateHeightmap;
    Function CreateScaledImage(UpScale, FilterRadius: integer): TBitmap;
  public
    NichtsImage: String;
    BegehbarImage: String;
    BebaubarImage: String;
    BeidesImage: String;

    Procedure LoadMap(MapName_: String);
    Function MapPreview(): TBitmap;

    Function CreateTexture(UpScale, FilterRadius: integer): TBitmap;
    Constructor Create;
    Destructor destroy; override;
  End;

Implementation

Uses math,
  LCLType, // HBitmap type
  IntfGraphics, // TLazIntfImage type
  fpImage // TFPColor type
  ;

{ TMapTex }

Procedure TMapTex.CreateHeightmap;
Var
  i, j: Integer;
  c: TColor;
Begin
  fHeightMap.Width := fmap.Width;
  fHeightMap.Height := fmap.Height;
  For i := 0 To fmap.Width - 1 Do Begin
    For j := 0 To fmap.Height - 1 Do Begin
      c := graphics.RGBToColor(0, 0, 0); // Farbe für Nichts
      Case fmap.fTerrain[i, j].data Of
        Walkable: Begin
            c := graphics.RGBToColor(64, 64, 64);
          End;
        Buildable: Begin
            c := graphics.RGBToColor(128, 128, 128);
          End;
        Buildable + Walkable: Begin
            c := graphics.RGBToColor(192, 192, 192);
          End;
      End;
      fHeightMap.Canvas.Pixels[i, j] := c;
    End;
  End;
End;

Procedure TMapTex.LoadMap(MapName_: String);
Begin
  fmap.Load(MapName_);
End;

Function TMapTex.MapPreview: TBitmap;
Begin
  // 1. Die Höhenmaske erstellen
  CreateHeightmap();
  // 2. Die Höhenmaske Skallieren
  result := CreateScaledImage(1, 0);
End;

Function TMapTex.CreateTexture(UpScale, FilterRadius: integer): TBitmap;

  Function LoadImage(Const Filename: String): TBitmap;
  Var
    jpg: TJPEGImage;
    png: TPortableNetworkGraphic;
  Begin
    result := Nil;
    If FileExists(Filename) Then Begin
      Case lowercase(ExtractFileExt(Filename)) Of
        '.bmp': Begin
            result := TBitmap.Create;
            result.LoadFromFile(Filename);
          End;
        '.jpg': Begin
            jpg := TJPEGImage.Create;
            jpg.LoadFromFile(Filename);
            result := TBitmap.Create;
            result.Assign(jpg);
            jpg.free;
          End;
        '.png': Begin
            png := TPortableNetworkGraphic.Create;
            png.LoadFromFile(Filename);
            result := TBitmap.Create;
            result.Assign(png);
            png.free;
          End;
      End;
    End;
  End;

  Function interPolate(c1, c2: TFPColor; f: Single): TFPColor;
  Var
    r1, r2, g1, g2, b1, b2: integer;
  Begin
    r1 := c1.red Shr 8;
    g1 := c1.green Shr 8;
    b1 := c1.blue Shr 8;
    r2 := c2.red Shr 8;
    g2 := c2.green Shr 8;
    b2 := c2.blue Shr 8;
    r1 := max(0, min(255, round(r1 * f + r2 * (1 - f))));
    g1 := max(0, min(255, round(g1 * f + g2 * (1 - f))));
    b1 := max(0, min(255, round(b1 * f + b2 * (1 - f))));
    result.red := r1 Shl 8;
    result.green := g1 Shl 8;
    result.blue := b1 Shl 8;
  End;

Var
  nichts, begehbar, bebaubar, beides: TBitmap;
  c, i, j: integer;

  nichtsIntfImg, begehbarIntfImg, bebaubarIntfImg, beidesIntfImg, DestIntfImg: TLazIntfImage;
  DestHandle, DestMaskHandle: HBitmap;
  CurColor: TFPColor;
Begin
  UpScale := max(1, UpScale);
  // 1. Laden der Skallierungstexturen
  nichts := LoadImage(NichtsImage);
  begehbar := LoadImage(BegehbarImage);
  bebaubar := LoadImage(BebaubarImage);
  beides := LoadImage(BeidesImage);
  nichtsIntfImg := TLazIntfImage.Create(0, 0);
  nichtsIntfImg.LoadFromBitmap(nichts.Handle, nichts.MaskHandle);

  begehbarIntfImg := TLazIntfImage.Create(0, 0);
  begehbarIntfImg.LoadFromBitmap(begehbar.Handle, begehbar.MaskHandle);

  bebaubarIntfImg := TLazIntfImage.Create(0, 0);
  bebaubarIntfImg.LoadFromBitmap(bebaubar.Handle, bebaubar.MaskHandle);

  beidesIntfImg := TLazIntfImage.Create(0, 0);
  beidesIntfImg.LoadFromBitmap(beides.Handle, beides.MaskHandle);

  // 1. Die Höhenmaske erstellen
  CreateHeightmap();
  // 2. Die Höhenmaske Skallieren
  result := CreateScaledImage(UpScale, FilterRadius);
  DestIntfImg := TLazIntfImage.Create(0, 0);
  DestIntfImg.LoadFromBitmap(result.Handle, result.MaskHandle);

  // 3. Umrechnen der Höhenmaske in das spätere Zielbild
  For i := 0 To result.Width - 1 Do Begin
    For j := 0 To result.Height - 1 Do Begin
      //      c := result.Canvas.Pixels[i, j] And $FF;
      CurColor := DestIntfImg.Colors[i, j];
      c := CurColor.red Shr 8;
      Case c Of
        0..63: Begin
            CurColor := interPolate(
              nichtsIntfImg.Colors[
              trunc(i * (nichts.Width - 1) / (Result.Width - 1)),
                trunc(j * (nichts.Height - 1) / (Result.Height - 1))],
                begehbarIntfImg.Colors[
              trunc(i * (begehbar.Width - 1) / (Result.Width - 1)),
                trunc(j * (begehbar.Height - 1) / (Result.Height - 1))],
                1 - c / 64);
          End;
        64..127: Begin
            CurColor := interPolate(
              begehbarIntfImg.Colors[
              trunc(i * (begehbar.Width - 1) / (Result.Width - 1)),
                trunc(j * (begehbar.Height - 1) / (Result.Height - 1))],
                bebaubarIntfImg.Colors[
              trunc(i * (bebaubar.Width - 1) / (Result.Width - 1)),
                trunc(j * (bebaubar.Height - 1) / (Result.Height - 1))],
                1 - (c - 64) / 64);
          End;
        128..192: Begin
            CurColor := interPolate(
              bebaubarIntfImg.Colors[
              trunc(i * (bebaubar.Width - 1) / (Result.Width - 1)),
                trunc(j * (bebaubar.Height - 1) / (Result.Height - 1))],
                beidesIntfImg.Colors[
              trunc(i * (beides.Width - 1) / (Result.Width - 1)),
                trunc(j * (beides.Height - 1) / (Result.Height - 1))],
                1 - (c - 128) / 64);
          End;
      End;
      DestIntfImg.Colors[i, j] := CurColor;
    End;
  End;
  DestIntfImg.CreateBitmaps(DestHandle, DestMaskHandle, false);
  result.Handle := DestHandle;
  result.MaskHandle := DestMaskHandle;
  DestIntfImg.free;

  nichtsIntfImg.free;
  begehbarIntfImg.free;
  bebaubarIntfImg.free;
  beidesIntfImg.free;

  nichts.free;
  begehbar.free;
  bebaubar.free;
  beides.free;
End;

Function TMapTex.CreateScaledImage(UpScale, FilterRadius: integer): TBitmap;
Begin
  // 1. Hoch skallieren
  result := TBitmap.Create;
  result.Width := round((UpScale) * fHeightMap.Width);
  result.Height := round((UpScale) * fHeightMap.Height);
  Stretchdraw(result, rect(0, 0, Result.Width, Result.Height), fHeightMap, imNone);
  // 2. Glätten via Mittelwertfilter
  If FilterRadius > 0 Then Begin
    Smooth(result, FilterRadius);
  End;
End;

Constructor TMapTex.Create;
Begin
  Inherited create;
  fmap := TMap.Create;
  fHeightMap := tbitmap.create;
End;

Destructor TMapTex.destroy;
Begin
  fHeightMap.free;
  fmap.free;
End;

End.

