unit form_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    Button_Apply: TButton;
    Button_Reset: TButton;
    Button_OK: TButton;
    Label_MultiDownloadWarning_unit: TLabel;
    Label_MaxDownloadThread: TLabel;
    Label_MultiDownloadFatal: TLabel;
    Label_MultiDownloadWarning: TLabel;
    Label_MultiDownloadFatal_unit: TLabel;
    SpinEdit_MaxDownloadTheard: TSpinEdit;
    SpinEdit_MultiDownloadFatal: TSpinEdit;
    SpinEdit_MultiDownloadWarning: TSpinEdit;
    procedure Button_ApplyClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_ResetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure DefaultOptions;
    procedure ResetOptions;
    procedure ApplyOptions;
  public
    TileMerger_Options : record
      MaxDownloadThread    : Integer;  // 最大线程数
      MultiDownloadWarning : Integer;  // 造成普通提示的单次下载瓦片数量
      MultiDownloadFatal   : Integer;  // 造成严重提示的单次下载瓦片数量（优先）
    end;
  public
    procedure LoadOptions;
    procedure SaveOptions;
    procedure Execute;
  end;

var
  FormOptions: TFormOptions;

implementation
uses fpjson, tile_merger_main;


{ TFormOptions }

procedure TFormOptions.Button_ResetClick(Sender: TObject);
begin
  ResetOptions;
end;

procedure TFormOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  SaveOptions;
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  LoadOptions;
end;

procedure TFormOptions.Button_ApplyClick(Sender: TObject);
begin
  ApplyOptions;
end;

procedure TFormOptions.Button_OKClick(Sender: TObject);
begin
  ApplyOptions;
end;

procedure TFormOptions.DefaultOptions;
begin
  with TileMerger_Options do begin
    MaxDownloadThread     := 100;
    MultiDownloadWarning  := 1000;
    MultiDownloadFatal    := 500;
  end;
end;

procedure TFormOptions.ResetOptions;
begin
  with TileMerger_Options do begin
    SpinEdit_MaxDownloadTheard.Value    := MaxDownloadThread;
    SpinEdit_MultiDownloadWarning.Value := MultiDownloadWarning;
    SpinEdit_MultiDownloadFatal.Value   := MultiDownloadFatal;
  end;
end;

procedure TFormOptions.ApplyOptions;
begin
  with TileMerger_Options do begin
    MaxDownloadThread    := SpinEdit_MaxDownloadTheard.Value;
    MultiDownloadWarning := SpinEdit_MultiDownloadWarning.Value;
    MultiDownloadFatal   := SpinEdit_MultiDownloadFatal.Value;
  end;
  FormTileMerger.TileViewer.TilePool.MaxDownloadThread:=TileMerger_Options.MaxDownloadThread;
  FormTileMerger.TileViewer.MultiDownloadWarning:=TileMerger_Options.MultiDownloadWarning;
  FormTileMerger.TileViewer.MultiDownloadFatal:=TileMerger_Options.MultiDownloadFatal;
end;

procedure TFormOptions.LoadOptions;
var filename:string;
    jData, jItem:TJSONData;
    lines:TMemoryStream;
begin
  filename:='TilesCache'+DirectorySeparator+'__server_manifest'+DirectorySeparator+'TileMergerOptions.json';
  if FileExists(filename) then begin
    lines:=TMemoryStream.Create;
    try
      lines.LoadFromFile(filename);
      jData:=GetJSON(lines);
      if (jData=nil) or (jData.JSONType<>jtObject) then begin
        DefaultOptions;
        exit;
      end;
      jItem:=TJSONObject(jData).Find('MaxDownloadThread');
      if jItem.JSONType=jtNumber then TileMerger_Options.MaxDownloadThread := jItem.AsInteger;
      jItem:=TJSONObject(jData).Find('MultiDownloadWarning');
      if jItem.JSONType=jtNumber then TileMerger_Options.MultiDownloadWarning  := jItem.AsInteger;
      jItem:=TJSONObject(jData).Find('MultiDownloadFatal');
      if jItem.JSONType=jtNumber then TileMerger_Options.MultiDownloadFatal  := jItem.AsInteger;
    finally
      lines.Free;
      jData.Free;
    end;
  end else DefaultOptions;
  FormTileMerger.TileViewer.TilePool.MaxDownloadThread:=TileMerger_Options.MaxDownloadThread;
  FormTileMerger.TileViewer.MultiDownloadWarning:=TileMerger_Options.MultiDownloadWarning;
  FormTileMerger.TileViewer.MultiDownloadFatal:=TileMerger_Options.MultiDownloadFatal;
end;

procedure TFormOptions.SaveOptions;
var filename:string;
    jObj:TJSONObject;
    lines:TStringList;
begin
  filename:='TilesCache'+DirectorySeparator+'__server_manifest'+DirectorySeparator+'TileMergerOptions.json';
  ForceDirectories('__server_manifest');
  jObj:=TJSONObject.Create;
  lines:=TStringList.Create;
  try
    jObj.Add('MaxDownloadThread',    TileMerger_Options.MaxDownloadThread);
    jObj.Add('MultiDownloadWarning', TileMerger_Options.MultiDownloadWarning);
    jObj.Add('MultiDownloadFatal',   TileMerger_Options.MultiDownloadFatal);
    lines.Text:=jObj.FormatJSON();
    lines.SaveToFile(filename);
  finally
    jObj.Free;
    lines.Free;
  end;
end;

procedure TFormOptions.Execute;
begin
  ResetOptions;
  Self.ShowModal;
end;


{$R *.lfm}

end.

