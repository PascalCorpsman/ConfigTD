Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  OpenGLContext, uopengl_animation;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Image1: TImage;
    OpenGLControl1: TOpenGLControl;
    ScrollBar1: TScrollBar;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormHide(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    AniFilename: String;
    ani: TOpenGL_Animation;
    Procedure ResizeTo(aWidth, aHeight: Integer);
    Procedure Go2d();
    Procedure Exit2d();
  public
    Procedure LoadAndShow(Const Filename: String);

  End;

Var
  Form2: TForm2;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

{$R *.lfm}

Uses math, dglOpenGL, uopengl_graphikengine;

{ TForm2 }

Procedure Tform2.Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 0, 480, -1, 1); // Set Up An Ortho Screen
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure Tform2.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  caption := 'Preview';
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  ani := TOpenGL_Animation.Create;
End;

Procedure TForm2.FormDestroy(Sender: TObject);
Begin
  Initialized := false;
  timer1.Enabled := false;
  ani.free;
  ani := Nil;
End;

Procedure TForm2.FormHide(Sender: TObject);
Begin
  Timer1.Enabled := false;
End;

Var
  allowcnt: Integer = 0;

Procedure TForm2.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt >= 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    OpenGL_GraphikEngine.clear;
    glenable(GL_TEXTURE_2D); // Texturen
    glEnable(GL_DEPTH_TEST); // Tiefentest

    ani.LoadFromFile(AniFilename, true);
    ResizeTo(max(105, ani.Width(0)), max(105, ani.Height(0)));

    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
  End;
  Invalidate;
End;

Procedure TForm2.OpenGLControl1Paint(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();

  go2d;

  glTranslatef(8, 8, 0);
  glAlphaFunc(GL_LESS, 0.5);
  glEnable(GL_ALPHA_TEST);
  ani.Render(ScrollBar1.Position);
  gldisable(GL_ALPHA_TEST);

  exit2d;

  OpenGLControl1.SwapBuffers;
End;

Procedure TForm2.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
  End;
End;

Procedure TForm2.Timer1Timer(Sender: TObject);
Begin
  If Initialized Then Begin
    OpenGLControl1.Invalidate;
  End;
End;

Procedure TForm2.ResizeTo(aWidth, aHeight: Integer);
Begin
  Width := aWidth + Scale96ToForm(16);
  height := aHeight + Scale96ToForm(78);
  left := (Monitor.Width - Width) Div 2;
  top := (Monitor.Height - Height) Div 2;
End;

Procedure TForm2.LoadAndShow(Const Filename: String);
Begin
  If Not FileExists(Filename) Then exit;
  Case lowercase(ExtractFileExt(Filename)) Of
    '.png', '.bmp', '.jpg': Begin
        OpenGLControl1.Visible := false;
        Image1.Visible := True;
        ScrollBar1.Visible := false;
        image1.Picture.LoadFromFile(Filename);
        image1.Transparent := true;
        ResizeTo(max(105, image1.Picture.Width), max(105, Image1.Picture.Height));
      End;
    '.ani': Begin
        OpenGLControl1.Visible := true;
        Image1.Visible := false;
        ScrollBar1.Visible := true;
        Timer1.Enabled := true;
        AniFilename := Filename;
        allowcnt := min(allowcnt, 1);
        Invalidate;
      End;
  End;
  ShowModal;
End;

End.

