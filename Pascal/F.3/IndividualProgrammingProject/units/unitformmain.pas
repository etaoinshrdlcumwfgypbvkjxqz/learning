unit UnitFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  OpenGLContext, LCLType,
  UnitWorld;

type

  { TFormMain }

  TFormMain = class(TForm)
      OpenGLControlRender: TOpenGLControl;
      procedure OpenGLControlRenderResize(const Sender: TOpenGLControl);
      procedure OpenGLControlRenderDraw(const Sender: TOpenGLControl);
      procedure OpenGLControlRenderKeyDown(const Sender: TOpenGLControl; var Key: word; const Shift: TShiftState);
      procedure OpenGLControlRenderKeyUp(const Sender: TOpenGLControl; var Key: word; const Shift: TShiftState);
      procedure ApplicationOnIdle(Sender: TObject; var Done: boolean);
      procedure OpenGLControlRenderClick(const Sender: TOpenGLControl);
      procedure OpenGLControlRenderMouseMove(const Sender: TOpenGLControl; const Shift: TShiftState; const x, y: Integer);
      procedure OpenGLControlRenderMouseEnter(const Sender: TOpenGLControl);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ApplicationOnIdle(Sender: TObject; var Done: boolean);
begin
  OnIdle0(OpenGLControlRender, Done);
end;

procedure TFormMain.OpenGLControlRenderResize(const Sender: TOpenGLControl);
begin
  OnResize0(Sender);
end;


procedure TFormMain.OpenGLControlRenderDraw(const Sender: TOpenGLControl);
begin
  Draw0(Sender);
end;
procedure TFormMain.OpenGLControlRenderKeyDown(const Sender: TOpenGLControl; var Key: word; const Shift: TShiftState);
begin
  KeyDown0(Sender, Key, Shift);
end;
procedure TFormMain.OpenGLControlRenderKeyUp(const Sender: TOpenGLControl; var Key: word; const Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    else KeyUp0(Sender, Key, Shift);
  end;
end;
procedure TFormMain.OpenGLControlRenderClick(const Sender: TOpenGLControl);
begin
  OnClick0(Sender);
end;
procedure TFormMain.OpenGLControlRenderMouseMove(const Sender: TOpenGLControl; const Shift: TShiftState; const x, y: Integer);
begin
  OnMouseMove0(Sender, Shift, x, y);
end;
procedure TFormMain.OpenGLControlRenderMouseEnter(const Sender: TOpenGLControl);
begin
  OnMouseEnter0(Sender);
end;

end.

