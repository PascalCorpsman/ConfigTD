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
Program config_td;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads, // Für TUpdater
  cmem,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1, unit2, unit3, unit4, unit5, unit6, unit7, unit8, Unit9, unit10, unit11,
  unit12, unit13, unit14, unit15, unit16, unit17, Unit18, Unit19;

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm10, Form10);
  Application.CreateForm(TForm11, Form11);
  Application.CreateForm(TForm13, Form13);
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TForm14, Form14);
  Application.CreateForm(TForm7, Form7);
  Application.CreateForm(TForm15, Form15);
  Application.CreateForm(TForm16, Form16);
  Application.CreateForm(TForm12, Form12);
  Application.CreateForm(TForm17, Form17);
  Application.CreateForm(TForm8, Form8);
  Application.CreateForm(TForm9, Form9);
  Application.CreateForm(TForm18, Form18);
  Application.CreateForm(TForm19, Form19);
  Application.Run;
End.

