unit tile_merger_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls,
  tile_merger_core, tile_merger_view, tile_merger_wmts_client;

type

  { TFormTileMerger }

  TFormTileMerger = class(TForm)
    Button_showtiles: TButton;
    Button_export: TButton;
    Button_zoomtoworld: TButton;
    Button_wmts: TButton;
    Button_test: TButton;
    CheckBox_stopdrawing: TCheckBox;
    CheckBox_ShowGrid: TCheckBox;
    CheckBox_ShowInfo: TCheckBox;
    ComboBox_imagetype: TComboBox;
    Edit_currentstatus: TEdit;
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
    TreeView_wmts_list: TTreeView;
    procedure Button_exportClick(Sender: TObject);
    procedure Button_showtilesClick(Sender: TObject);
    procedure Button_testClick(Sender: TObject);
    procedure Button_wmtsClick(Sender: TObject);
    procedure Button_zoomtoworldClick(Sender: TObject);
    procedure CheckBox_stopdrawingChange(Sender: TObject);
    procedure CheckBox_ShowGridChange(Sender: TObject);
    procedure CheckBox_ShowInfoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView_wmts_listSelectionChanged(Sender: TObject);
  private
    FTileViewer:TTileViewer;
  public

  end;

var
  FormTileMerger: TFormTileMerger;
  WMTS_Client:TWMTS_Client;

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

procedure TFormTileMerger.Button_showtilesClick(Sender: TObject);
var tmpTMS:TWMTS_TileMatrixSet;
    tmpTM:TWMTS_TileMatrix;
begin
  if (FTileViewer.CurrentLayer=nil) or (FTileViewer.CurrentTileMatrixSet=nil) then begin
    Memo_test.Lines.Add('请先选择Layer和TileMatrix！！');
    exit;
  end;

  Memo_test.Lines.Add('');
  Memo_test.Lines.Add(Format('FScaleX = %3.1f',[FTileViewer.ScaleX]));

  tmpTMS:=FTileViewer.CurrentTileMatrixSet;
  tmpTM:=tmpTMS.TileMatrixs[SpinEditEx_level.Value];
  if tmpTM=nil then
    FTileViewer.ShowTiles
  else
    FTileViewer.ShowTiles(tmpTM.Scale);
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
var root,node,lyr,tms:TTreeNode;
    server:TWMTS_Service;
    tmplayer:TWMTS_Layer;
    tmpTMS:TWMTS_TileMatrixSet;
    len,idx:integer;
begin
  FTileViewer:=TTileViewer.Create(Self);
  FTileViewer.Parent:=Panel_viewer;
  FTileViewer.Align:=alClient;
  root:=TreeView_wmts_list.Items.Add(nil,'WMTS Servers');
  root.Data:=nil;
  server:=WMTS_Client.Services[0];
  node:=TreeView_wmts_list.Items.AddChild(root,server.Title);
  node.Data:=server;
  lyr:=TreeView_wmts_list.Items.AddChild(node,'Layers');
  tms:=TreeView_wmts_list.Items.AddChild(node,'TileMatrixSets');
  len:=server.LayerCount;
  for idx:=0 to len-1 do begin
    tmplayer:=server.Layers[idx];
    TreeView_wmts_list.Items.AddChild(lyr,tmplayer.Title).Data:=tmplayer;
  end;
  len:=server.TileMatrixSetCount;
  for idx:=0 to len-1 do begin
    tmpTMS:=server.TileMatrixSets[idx];
    TreeView_wmts_list.Items.AddChild(tms,tmpTMS.Identifier).Data:=tmpTMS;
  end;

end;

procedure TFormTileMerger.TreeView_wmts_listSelectionChanged(Sender: TObject);
var DataObject:TObject;
    idx,len:integer;
begin
  DataObject:=TObject(TreeView_wmts_list.Selected.Data);
  if DataObject=nil then exit;
  if DataObject is TWMTS_Service then FTileViewer.CurrentService:=DataObject as TWMTS_Service;
  if DataObject is TWMTS_Layer then FTileViewer.CurrentLayer:=DataObject as TWMTS_Layer;
  if DataObject is TWMTS_TileMatrixSet then FTileViewer.CurrentTileMatrixSet:=DataObject as TWMTS_TileMatrixSet;
  if FTileViewer.CurrentTileMatrixSet=nil then exit;
  if FTileViewer.CurrentLayer=nil then exit;
  Edit_currentstatus.Caption:='lyr='+FTileViewer.CurrentLayer.Title+'  tms='+FTileViewer.CurrentTileMatrixSet.Identifier;
  len:=FTileViewer.CurrentTileMatrixSet.TileMatrixCount;
  Memo_test.Lines.Add('[CurrentTileMatrixSet]'+FTileViewer.CurrentTileMatrixSet.Identifier);
  for idx:=0 to len-1 do
    with FTileViewer.CurrentTileMatrixSet.TileMatrixs[idx] do begin
      Memo_test.Lines.Add(Format('  Index  = %d',[idx]));
      Memo_test.Lines.Add(Format('    Scale  = %3.1f',[Scale]));
      Memo_test.Lines.Add(Format('    ColCnt = %d',[ColumnCount]));
      Memo_test.Lines.Add(Format('    RowCnt = %d',[RowCount]));
    end;
end;

initialization
  WMTS_Client:=TWMTS_Client.Create;

finalization
  WMTS_Client.Free;

end.

