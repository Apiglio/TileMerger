unit tile_merger_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, tile_merger_core, tile_merger_view;

type

  { TFormTileMerger }

  TFormTileMerger = class(TForm)
    Button_export: TButton;
    Button_zoomtoworld: TButton;
    Button_wmts: TButton;
    Button_test: TButton;
    CheckBox_stopdrawing: TCheckBox;
    CheckBox_ShowGrid: TCheckBox;
    CheckBox_ShowInfo: TCheckBox;
    ComboBox_imagetype: TComboBox;
    Edit_export: TEdit;
    Edit_folder: TEdit;
    FloatSpinEditEx_x: TFloatSpinEditEx;
    FloatSpinEditEx_y: TFloatSpinEditEx;
    Label_export: TLabel;
    Label_imagetype: TLabel;
    Label_load_level: TLabel;
    Label_lng: TLabel;
    Label_lat: TLabel;
    Label_lvl: TLabel;
    Label_folder: TLabel;
    Memo_test: TMemo;
    Panel_viewer: TPanel;
    SpinEdit_load_level: TSpinEdit;
    SpinEditEx_level: TSpinEditEx;
    procedure Button_exportClick(Sender: TObject);
    procedure Button_testClick(Sender: TObject);
    procedure Button_wmtsClick(Sender: TObject);
    procedure Button_zoomtoworldClick(Sender: TObject);
    procedure CheckBox_stopdrawingChange(Sender: TObject);
    procedure CheckBox_ShowGridChange(Sender: TObject);
    procedure CheckBox_ShowInfoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTileViewer:TTileViewer;
  public

  end;

var
  FormTileMerger: TFormTileMerger;

implementation

{$R *.lfm}

{ TFormTileMerger }

procedure TFormTileMerger.Button_testClick(Sender: TObject);
const sp = '';
var tmpLL:TLatLong;
    tmpLv:Integer;
    tmpWM:TWebMercator;
    tmpWMXY:TDoublePoint;
    tmpLL_calc:TLatLong;
begin
  tmpLL.x:=FloatSpinEditEx_x.Value;
  tmpLL.y:=FloatSpinEditEx_y.Value;
  tmpLv:=SpinEditEx_level.Value;
  tmpWM:=LatlongToWebmercator(tmpLL,tmpLv);
  tmpLL_calc:=WebmercatorToLatlong(tmpWM);
  tmpWMXY:=WebmercatorToXY(tmpWM);
  //tmpWMXY:=LatlongToWebmercatorXY(tmpLL);
  Memo_test.Lines.Add(
    Format(
      '[%03.5f, '+sp+'%02.5f] '+sp+sp+'  (%d, '+sp+'%d, '+sp+'%d)'+sp+sp
      +'  [%03.5f, '+sp+'%02.5f]'+sp+sp+'  (%f, '+sp+'%f)',
      [tmpLL.x,tmpLL.y,tmpWM.coord.x,tmpWM.coord.y,tmpWM.level,
      tmpLL_calc.x,tmpLL_calc.y,tmpWMXY.x,tmpWMXY.y])
  );
end;

procedure TFormTileMerger.Button_exportClick(Sender: TObject);
begin
  ForceDirectories(ExtractFileDir(Edit_export.Text));
  FTileViewer.SaveToGeoTiff(Edit_export.Text);
end;

procedure TFormTileMerger.Button_wmtsClick(Sender: TObject);
var image_format:string;
    tile_format:TTileFormat;
begin
  image_format:=ComboBox_imagetype.Items[ComboBox_imagetype.ItemIndex];
  FTileViewer.Clear;
  case lowercase(image_format) of
    'png':tile_format:=tfPNG;
    'bmp':tile_format:=tfBMP;
    else tile_format:=tfJPG;
  end;
  FTileViewer.LoadFromWMTS(Edit_folder.Text,SpinEdit_load_level.Value,tile_format);
  FTileViewer.ZoomToWorld;
end;

procedure TFormTileMerger.Button_zoomtoworldClick(Sender: TObject);
begin
  FTileViewer.ZoomToWorld;
end;

procedure TFormTileMerger.CheckBox_stopdrawingChange(Sender: TObject);
begin
  FTileViewer.StopDrawing:=CheckBox_stopdrawing.Checked;
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.CheckBox_ShowGridChange(Sender: TObject);
begin
  FTileViewer.ShowGrid:=CheckBox_ShowGrid.Checked;
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.CheckBox_ShowInfoChange(Sender: TObject);
begin
  FTileViewer.ShowInfo:=CheckBox_ShowInfo.Checked;
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.FormCreate(Sender: TObject);
begin
  FTileViewer:=TTileViewer.Create(Self);
  FTileViewer.Parent:=Panel_viewer;
  FTileViewer.Align:=alClient;
end;

end.

