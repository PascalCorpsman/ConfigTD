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
Unit uctd_mapobject;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils, uvectormath, uctd_common;

Type

  TMapObjectType = (moUnknown, moBuilding, moOpponent, moMap); // Spezifizierung einer Datei

  { tctd_mapopbject }

  tctd_mapopbject = Class
  protected
    fChanged: Boolean;
  public
    name: String;
    Image: String;
    Fimage: integer; // Image als OpenGL Version (eigentlich sollte das Protected sein, aber das Build Menü braucht die Texturen auch.
    Filename: String;
    Position: TVector2; // Gebäude nutzen nur den ganzzahlantei, alle anderen voll
    Width: Single; // Dass muss Single sein für Opponent, die Buildings nutzen nur den Integer teil
    Height: Single; // Dass muss Single sein für Opponent, die Buildings nutzen nur den Integer teil
    Owner: integer; // Damit das Objekt Weis, wem es gehört.

    Property Changed: Boolean read fChanged; // True, wenn irgendwas geändert wurde, was die Jeweilige Klasse in Savetostream Speichern müsste

    Constructor Create; virtual;
    Destructor Destroy; override;

    Function LoadFromFile(Const Filename_: String): Boolean; // virtual;
    Function LoadFromStream(Const Stream: TStream): Boolean; virtual;
    Function SaveToFile(Const Filename_: String): Boolean; // virtual;
    Function SaveToStream(Const Stream: TStream): Boolean; virtual;
{$IFDEF Client}
    Function GetBuyCost: integer; virtual; abstract; // Für Gebs und Heros
    Function GetHint(): THint; virtual; abstract; // Die Hint Informationen für Im Spiel
    Procedure Render(Grayed: Boolean); virtual; abstract; // Grayed ist optional und aktuell nur für gebäude implementiert
{$ENDIF}
    Procedure SetStage(value: integer); virtual; // Wird nur für TBuilding gebraucht, macht aber das Laden in Map einfacher
    Function ListOfImages(): TStringList; virtual; abstract; // Soll die Liste aller eingebundenen Bilder zurückgeben
  End;

Function FilenameToType(Const Filename: String): TMapObjectType;

Implementation

Uses IniFiles, LazFileUtils
{$IFDEF Client}
  , uctd
{$ENDIF}
  ;

Function FilenameToType(Const Filename: String): TMapObjectType;
Var
  ini: tinifile;
Begin
  result := moUnknown;
  If FileExistsUTF8(Filename) Then Begin
    ini := TIniFile.Create(Filename);
    If ini.ReadString('building', 'size', '') <> '' Then Begin
      result := moBuilding;
      ini.free;
      exit;
    End;
    If ini.ReadString('map', 'size', '') <> '' Then Begin
      result := moMap;
      ini.free;
      exit;
    End;
    If ini.ReadString('opponent', 'size', '') <> '' Then Begin
      result := moOpponent;
      ini.free;
      exit;
    End;
    ini.free;
  End;
End;

{ tctd_mapopbject }

Constructor tctd_mapopbject.Create;
Begin
  Inherited Create;
  fChanged := false;
  Fimage := 0;
  Filename := '';
  Position := point(0, 0);
  Width := 0;
  Height := 0;
  Owner := -1;
End;

Destructor tctd_mapopbject.Destroy;
Begin
{$IFDEF Client}
  ctd.NilSideObject(Self);
{$ENDIF}
  Inherited Destroy;
End;

Function tctd_mapopbject.LoadFromFile(Const Filename_: String): Boolean;
Var
  f: TFileStream;
  m: TMemoryStream;
Begin
  Filename := Filename_;
  result := false;
  f := TFileStream.Create(Filename_, fmOpenRead);
  m := TMemoryStream.Create;
  m.CopyFrom(f, f.Size);
  f.free;
  m.Position := 0;
  result := LoadFromStream(m);
  m.free;
End;

Function tctd_mapopbject.LoadFromStream(Const Stream: TStream): Boolean;
Begin
  fChanged := false;
  result := false;
End;

Function tctd_mapopbject.SaveToFile(Const Filename_: String): Boolean;
Var
  f: TFileStream;
  m: TMemoryStream;
Begin
  Filename := Filename_;
  m := TMemoryStream.Create;
  result := SaveToStream(m);
  If result Then Begin
    result := false;
    f := TFileStream.Create(Filename_, fmOpenReadWrite Or fmCreate);
    m.Position := 0;
    f.CopyFrom(m, m.Size);
    f.free;
    m.free;
    result := true;
  End;
End;

Function tctd_mapopbject.SaveToStream(Const Stream: TStream): Boolean;
Begin
  fChanged := false;
  result := false;
End;

Procedure tctd_mapopbject.SetStage(value: integer);
Begin

End;

End.

