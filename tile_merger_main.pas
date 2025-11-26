unit tile_merger_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, Menus,
  {$ifdef windows}
  Windows,
  {$endif}
  tile_merger_view, tile_merger_wmts_client, CalendarFlow;

const
  _appname_ = 'Apiglio TileMerger';
  _version_ = '0.5';
  _authors_ = 'Apiglio';
  _newline_ = {$ifdef windows}#13#10{$else}#10{$endif};

type

  { TFormTileMerger }

  TFormTileMerger = class(TForm)
    CalendarFlow_TimeOption: TCalendarFlow;
    Label_export: TLabel;
    MainMenu_TileMerger: TMainMenu;
    MenuItem_ViewShowScale: TMenuItem;
    MenuItem_TV_ZoomToResolution: TMenuItem;
    MenuItem_TV_Redownload: TMenuItem;
    MenuItem_DownloadDiv01: TMenuItem;
    MenuItem_DownloadExport: TMenuItem;
    MenuItem_ViewDiv01: TMenuItem;
    MenuItem_ViewAutoFetch: TMenuItem;
    MenuItem_DownloadModeForce: TMenuItem;
    MenuItem_DownloadModeAuto: TMenuItem;
    MenuItem_DownloadModeManual: TMenuItem;
    MenuItem_ViewShowInfo: TMenuItem;
    MenuItem_ViewShowGrid: TMenuItem;
    MenuItem_OptionDiv_01: TMenuItem;
    MenuItem_OptionLog: TMenuItem;
    MenuItem_OptionSetting: TMenuItem;
    MenuItem_OptionAbout: TMenuItem;
    MenuItem_Option: TMenuItem;
    MenuItem_DownloadMode: TMenuItem;
    MenuItem_DownloadCachePath: TMenuItem;
    MenuItem_Download: TMenuItem;
    MenuItem_View: TMenuItem;
    MenuItem_ServerAdd: TMenuItem;
    MenuItem_Server: TMenuItem;
    Panel_viewer: TPanel;
    PopupMenu_TileViewer: TPopupMenu;
    Splitter_PanelH: TSplitter;
    Splitter_MainV: TSplitter;
    StatusBar_TileMerger: TStatusBar;
    TreeView_wmts_list: TTreeView;
    procedure CalendarFlow_TimeOptionDateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem_DownloadCachePathClick(Sender: TObject);
    procedure MenuItem_DownloadExportClick(Sender: TObject);
    procedure MenuItem_DownloadModeSwitchClick(Sender: TObject); //所有的MenuItem_DownloadMode*的OnClick都执行这个
    procedure MenuItem_OptionAboutClick(Sender: TObject);
    procedure MenuItem_OptionLogClick(Sender: TObject);
    procedure MenuItem_TV_RedownloadClick(Sender: TObject);
    procedure MenuItem_TV_ZoomToResolutionClick(Sender: TObject);
    procedure MenuItem_ViewAutoFetchClick(Sender: TObject);
    procedure MenuItem_ViewShowGridClick(Sender: TObject);
    procedure MenuItem_ViewShowInfoClick(Sender: TObject);
    procedure MenuItem_ViewShowScaleClick(Sender: TObject);
    procedure TreeView_wmts_listSelectionChanged(Sender: TObject);
  private
    FTileViewer:TTileViewer;
  public
    procedure UpdateStatusBar(Sender: TObject);
  end;

var
  FormTileMerger: TFormTileMerger;
  WMTS_Client:TWMTS_Client;

implementation
uses debugline, exporttiff, tile_merger_projection;

{$R *.lfm}

{ TFormTileMerger }

procedure TFormTileMerger.FormCreate(Sender: TObject);
var root,node,lyrs,tmss,layer:TTreeNode;
    server:TWMTS_Service;
    tmplayer:TWMTS_Layer;
    tmpTMS:TWMTS_TileMatrixSet;
    len,idx,len_server,idx_server:integer;
    option_key,option_value:TCollectionItem;
begin
  FTileViewer:=TTileViewer.Create(Self);
  FTileViewer.Parent:=Panel_viewer;
  FTileViewer.Align:=alClient;
  root:=TreeView_wmts_list.Items.Add(nil,'地图服务器');
  root.Data:=nil;
  len_server:=WMTS_Client.ServiceCount;
  for idx_server:=0 to len_server-1 do begin
    server:=WMTS_Client.Services[idx_server];
    node:=TreeView_wmts_list.Items.AddChild(root,server.DisplayName);
    node.Data:=server;

    lyrs:=node;//展开LYRs列表
    //lyrs:=TreeView_wmts_list.Items.AddChild(node,'数据图层');
    //tmss:=TreeView_wmts_list.Items.AddChild(node,'层级方案');
    len:=server.LayerCount;
    for idx:=0 to len-1 do begin
      tmplayer:=server.Layers[idx];
      layer:=TreeView_wmts_list.Items.AddChild(lyrs,tmplayer.Title);
      layer.Data:=tmplayer;

      for option_key in tmplayer.ParameterList do begin
        for option_value in tmplayer.ParameterList[TWMTS_Parameter(option_key).Title].ValueList do begin
          TreeView_wmts_list.Items.AddChild(layer,TWMTS_ParameterValue(option_value).Value).Data:=option_value;
        end;
      end;
    end;
    //不再单独呈现TMSs列表，使用TMSLink
    {
    len:=server.TileMatrixSetCount;
    for idx:=0 to len-1 do begin
      tmpTMS:=server.TileMatrixSets[idx];
      TreeView_wmts_list.Items.AddChild(tmss,tmpTMS.Identifier).Data:=tmpTMS;
    end;
    }
  end;
  server:=WMTS_Client.Services[0];
  FTileViewer.InitializeLayerAndTileMatrixSet(server.Layers[0],server.TileMatrixSets[0]);
  FTileViewer.AutoFetchTile:=true;
  FTileViewer.Refresh;

  Caption:=_appname_;
  FTileViewer.OnLayerChange:=@UpdateStatusBar;
  FTileViewer.OnTileMatrixSetChange:=@UpdateStatusBar;
  CalendarFlow_TimeOption.CellSize.BeltWidth:=48;//神奇，beltwidth在properties里头怎么设置不了
  UpdateStatusBar(Self);

end;

procedure TFormTileMerger.CalendarFlow_TimeOptionDateChange(Sender: TObject);
var tmpCF:TCalendarFlow;
    tmpLayer:TWMTS_Layer;
begin
  tmpCF:=Sender as TCalendarFlow;
  tmpLayer:=FTileViewer.CurrentLayer;
  tmpLayer.TimeTagSelected:=tmpCF.CurrentDate;
  if tmpLayer.TimeTagCount>1 then FTileViewer.Refresh;
end;

procedure TFormTileMerger.MenuItem_DownloadCachePathClick(Sender: TObject);
begin
  ShellExecute(0,'open','explorer',pchar(FTileViewer.CachePath),nil,SW_NORMAL);
end;

procedure TFormTileMerger.MenuItem_DownloadExportClick(Sender: TObject);
begin
  Form_ExportTiff.Execute(FTileViewer, FTileViewer.CurrentLayer, FTileViewer.CurrentTileMatrixSet);
end;

procedure TFormTileMerger.MenuItem_DownloadModeSwitchClick(Sender: TObject);
begin
  if Sender=MenuItem_DownloadModeManual then begin
    FTileViewer.AutoFetchTile:=false;
    FTileViewer.ForceFetchTile:=false;
  end else if Sender=MenuItem_DownloadModeAuto then begin
    FTileViewer.AutoFetchTile:=true;
    FTileViewer.ForceFetchTile:=false;
  end else if Sender=MenuItem_DownloadModeForce then begin
    FTileViewer.AutoFetchTile:=true;
    FTileViewer.ForceFetchTile:=true;
  end else begin
    assert(false,'无效的菜单选项');
  end;
end;

procedure TFormTileMerger.MenuItem_OptionAboutClick(Sender: TObject);
begin
  ShowMessage(_appname_+_newline_+'version '+_version_+_newline_+'by '+_authors_);
end;

procedure TFormTileMerger.MenuItem_OptionLogClick(Sender: TObject);
begin
  Form_Debug.Show;
end;

//{$define MonoTile}
procedure TFormTileMerger.MenuItem_TV_RedownloadClick(Sender: TObject);
{$ifdef MonoTile}
//暂时不实现单一瓦片操作
var bestTM:TWMTS_TileMatrix;
    tmpIndex:TTileIndex;
    tmpRect:TGeoRectangle;
begin
  with FTileViewer.CurrentTileMatrixSet do begin
    bestTM:=BestFitTileMatrix(FTileViewer.ScaleX);
    with bestTM do begin
      tmpIndex:=Projection.GetWMTSTileIndex(
        LeftTop, Scale, Width, Height,
        FTileViewer.CursorToLocation(PopupMenu_TileViewer.PopupPoint.X, PopupMenu_TileViewer.PopupPoint.Y)
      );
      tmpRect:=Projection.GetWMTSTileRect(
        LeftTop, Scale, Width, Height,
        tmpIndex.col, tmpIndex.row
      );
    end;
  end;
  FTileViewer.ShowTilesRange(tmpRect);
end;
{$else}
var fft_stored:boolean;
begin
  fft_stored:=FTileViewer.ForceFetchTile;
  FTileViewer.ForceFetchTile:=true;
  FTileViewer.ShowTiles;
  FTileViewer.ForceFetchTile:=fft_stored;
end;
{$endif}
{$undef MonoTile}

procedure TFormTileMerger.MenuItem_TV_ZoomToResolutionClick(Sender: TObject);
var bestTM:TWMTS_TileMatrix;
begin
  bestTM:=FTileViewer.CurrentTileMatrixSet.BestFitTileMatrix(FTileViewer.ScaleX);
  FTileViewer.ZoomTo(bestTM.Scale);
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.MenuItem_ViewAutoFetchClick(Sender: TObject);
begin
  FTileViewer.AutoFetchTile:=not MenuItem_ViewAutoFetch.Checked;
  MenuItem_ViewAutoFetch.Checked:=FTileViewer.AutoFetchTile;
end;

procedure TFormTileMerger.MenuItem_ViewShowGridClick(Sender: TObject);
begin
  FTileViewer.ShowGrid:=not MenuItem_ViewShowGrid.Checked;
  MenuItem_ViewShowGrid.Checked:=FTileViewer.ShowGrid;
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.MenuItem_ViewShowInfoClick(Sender: TObject);
begin
  FTileViewer.ShowInfo:=not MenuItem_ViewShowInfo.Checked;
  MenuItem_ViewShowInfo.Checked:=FTileViewer.ShowInfo;
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.MenuItem_ViewShowScaleClick(Sender: TObject);
begin
  FTileViewer.ShowScale:=not MenuItem_ViewShowScale.Checked;
  MenuItem_ViewShowScale.Checked:=FTileViewer.ShowScale;
  FTileViewer.Refresh;
end;

procedure TFormTileMerger.TreeView_wmts_listSelectionChanged(Sender: TObject);
var DataObject:TObject;
    tmpLayer:TWMTS_Layer;
begin
  DataObject:=TObject(TreeView_wmts_list.Selected.Data);
  if DataObject=nil then exit;
  if DataObject is TWMTS_Layer then FTileViewer.CurrentLayer:=DataObject as TWMTS_Layer;
  if DataObject is TWMTS_TileMatrixSet then FTileViewer.CurrentTileMatrixSet:=DataObject as TWMTS_TileMatrixSet;
  if DataObject is TWMTS_ParameterValue then begin
    TWMTS_ParameterValue(DataObject).Owner.Selected:=TWMTS_ParameterValue(DataObject);
    tmpLayer:=TWMTS_ParameterValue(DataObject).Owner.Owner.Owner.Owner; //so weird
    if FTileViewer.CurrentLayer<>tmpLayer then FTileViewer.CurrentLayer:=tmpLayer;
  end;
  //if FTileViewer.CurrentTileMatrixSet=nil then exit;
  //if FTileViewer.CurrentLayer=nil then exit;
end;

procedure TFormTileMerger.UpdateStatusBar(Sender: TObject);
var idx,len:integer;
begin
  with StatusBar_TileMerger.Panels[0] do begin
    Text:=FTileViewer.CurrentLayer.Title;
    Width:=Canvas.TextWidth(Text+'##');
  end;
  with StatusBar_TileMerger.Panels[1] do begin
    Text:=FTileViewer.CurrentTileMatrixSet.Identifier;
    Width:=Canvas.TextWidth(Text+'##');
  end;
  CalendarFlow_TimeOption.ClearCountDate;
  with FTileViewer.CurrentLayer do begin
    len:=TimeTagCount;
    CalendarFlow_TimeOption.BeginUpdate;
    for idx:=len-1 downto 0 do CalendarFlow_TimeOption.CountInDate(TimeTag[idx]);
    CalendarFlow_TimeOption.CurrentDate:=TimeTag[len-1];
    CalendarFlow_TimeOption.EndUpdate;
    if TimeTagSelected<>0.0 then CalendarFlow_TimeOption.CurrentDate:=TimeTagSelected;
  end;
  CalendarFlow_TimeOption.Refresh; //没有时间参数时，时间轴界面会回到1899/12/31，目前看问题不大
end;

initialization
  WMTS_Client:=TWMTS_Client.Create;

finalization
  WMTS_Client.Free;

end.

