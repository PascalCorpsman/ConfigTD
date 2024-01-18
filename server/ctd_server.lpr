(******************************************************************************)
(* Config TD Server                                                31.12.2015 *)
(*                                                                            *)
(* Version     : See updater_settings.inc                                     *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is the Server module for Config TD                      *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : See uctd_common.pas                                          *)
(*                                                                            *)
(******************************************************************************)

Program ctd_server;

Uses Interfaces, sysutils, uctdserver, lazutf8, LazFileUtils,
  uctd_common, uctd_spawnmodule, uctd_bullet;

Procedure PrintHelp;
Begin
  writeln('');
  writeln('Online help for Config TD Server ver. ' + format('%0.2f', [Version / 100]));
  writeln('');
  writeln('-p <Port> = Spezifies the port number to listen to.');
  writeln('-pw <Password> = Spezifies the password default ""');
  writeln('-l <LogLevel> = Sets Loglevel (default = 2)');
  writeln('-t <Timeout> = Sets automatic close on no user connected, default ' + inttostr(ServerAutoTimeout) + 'ms');
  writeln('               0 = Disabled, typing "ESC" will terminate');
  writeln('-f <Filename> = Logs additional to a file');
  writeln('-m <Foldername> = Folder to load the maps from (default ./maps)');
  writeln('-h = This screen');
  writeln('');
End;

Var
  Server: TServer;
  Autotimeout: integer = ServerAutoTimeout;
  Port: integer = -1;
  i: integer;
  s: String;
  Password: String = '';
  Params: Array Of Boolean = Nil; // Zum Prüfen ob auch alle übergebenen Parameter verwendet wurden.

{$R *.res}

Begin
  DefaultFormatSettings.DecimalSeparator := '.';
  // Logger Konfigurieren
  InitLogger();
  // 1. Auslesen parameter
  MapFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'maps' + PathDelim;
  setlength(params, Paramcount + 1);
  For i := 1 To Paramcount Do Begin
    params[i] := false;
  End;
  For i := 1 To Paramcount - 1 Do Begin
    If lowercase(paramstr(i)) = '-m' Then Begin
      MapFolder := IncludeTrailingPathDelimiter(paramstr(i + 1));
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-p' Then Begin
      port := strtointdef(paramstr(i + 1), -1);
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-l' Then Begin
      SetLogLevel(strtointdef(paramstr(i + 1), 2));
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-t' Then Begin
      Autotimeout := strtointdef(paramstr(i + 1), ServerAutoTimeout);
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-f' Then Begin
      s := ExtractFilePath(ParamStrutf8(i + 1));
      Params[i] := true;
      Params[i + 1] := true;
      If Not DirectoryExistsutf8(s) Then Begin
        If Not CreateDirUTF8(s) Then Begin
          Log('Could not create : ' + s, llWarning);
          Continue;
        End;
      End;
      SetLoggerLogFile(paramstr(i + 1));
    End;
    If lowercase(paramstr(i)) = '-pw' Then Begin
      Password := paramstr(i + 1);
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If (lowercase(paramstr(i)) = '-h') Or (lowercase(paramstr(i)) = '-?') Then Begin
      PrintHelp;
      exit;
    End;
  End;
  Log('Config TD - Server ver. ' + format('%0.2f', [Version / 100]) + ' by Corpsman www.Corpsman.de', llInfo); // Eigentlich müsste dieser Log weiter oben sein, aber nur so ist er auch in der evtl. erstellten .log Datei
  s := '';
  For i := 1 To high(Params) Do Begin
    If Not params[i] Then Begin
      s := s + ' ' + ParamStrUTF8(i);
    End;
  End;
  If s <> '' Then Begin
    log('Unknown parameter :' + s, llWarning);
  End;
  setlength(params, 0);
  If Not DirectoryExistsUTF8(MapFolder) Then Begin
    If Not CreateDirUTF8(MapFolder) Then Begin
      Log('Could not create map folder : ' + MapFolder, llfatal);
      exit;
    End;
  End;
  log('Using map folder: ' + MapFolder, llInfo);
  // Es mus mindestens der Port gesetzt werden.
  If port = -1 Then Begin
    Log('No argument -p <portnumber> passed. Stopping now.', llCritical);
    Printhelp;
    exit;
  End
  Else Begin
    Log('Launching on Port : ' + inttostr(port), llinfo);
    If Autotimeout = 0 Then Begin
      Log('Autotimeout = 0, press "ESC" to terminate.', llinfo);
    End;
  End;
  // 2. Server Erstellen
  Randomize;
  Server := TServer.create(Port, Autotimeout, Password);
  // 3. Endlosschleife
  Try
    server.execute;
  Except
    On e: Exception Do Begin
      log(e.Message, llFatal);
      // Done : Speichere die Karte Dateiname = Zeitstempel ..
      server.SaveWhatPossible;
    End;
  End;
  log('Shutting down, thanks for playing.', llInfo);
  // 4. Fertig, aufräumen
  server.free;
End.

