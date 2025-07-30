unit debugline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm_Debug }

  TForm_Debug = class(TForm)
    Button_DebugLineClear: TButton;
    Memo_DebugLine: TMemo;
    procedure Button_DebugLineClearClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private

  public
    procedure AddMessage(msg:string);
  end;

var
  Form_Debug: TForm_Debug;

implementation

{$R *.lfm}

{ TForm_Debug }

procedure TForm_Debug.Button_DebugLineClearClick(Sender: TObject);
begin
  Memo_DebugLine.Lines.Clear;
end;

procedure TForm_Debug.FormDeactivate(Sender: TObject);
begin
  Hide;
end;

procedure TForm_Debug.AddMessage(msg:string);
begin
  Memo_DebugLine.Lines.Add(msg);
end;

end.

