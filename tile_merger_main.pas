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
  _version_ = '0.7.1';
  _authors_ = 'Apiglio';
  _newline_ = {$ifdef windows}#13#10{$else}#10{$endif};

type

  { TFormTileMerger }

  TFormTileMerger = class(TForm)
    CalendarFlow_TimeOption: TCalendarFlow;
    Label_export: TLabel;
    MainMenu_TileMerger: TMainMenu;
    MenuItem_ViewSaveRect: TMenuItem;
    MenuItem_ViewLocation: TMenuItem;
    MenuItem_FeatureExport: TMenuItem;
    MenuItem_PoiServer: TMenuItem;
    MenuItem_SL_Reload: TMenuItem;
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
    MenuItem_WmtsServer: TMenuItem;
    MenuItem_Server: TMenuItem;
    Panel_viewer: TPanel;
    PopupMenu_FeatureList: TPopupMenu;
    PopupMenu_ServerList: TPopupMenu;
    PopupMenu_TileViewer: TPopupMenu;
    SaveDialog_FeatureExport: TSaveDialog;
    Splitter_PanelH: TSplitter;
    Splitter_MainV: TSplitter;
    StatusBar_TileMerger: TStatusBar;
    TreeView_wmts_list: TTreeView;
    procedure CalendarFlow_TimeOptionDateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem_DownloadCachePathClick(Sender: TObject);
    procedure MenuItem_DownloadExportClick(Sender: TObject);
    procedure MenuItem_DownloadModeSwitchClick(Sender: TObject); //所有的MenuItem_DownloadMode*的OnClick都执行这个
    procedure MenuItem_FeatureExportClick(Sender: TObject);
    procedure MenuItem_OptionAboutClick(Sender: TObject);
    procedure MenuItem_OptionLogClick(Sender: TObject);
    procedure MenuItem_OptionSettingClick(Sender: TObject);
    procedure MenuItem_PoiServerClick(Sender: TObject);
    procedure MenuItem_SL_ReloadClick(Sender: TObject);
    procedure MenuItem_TV_RedownloadClick(Sender: TObject);
    procedure MenuItem_TV_ZoomToResolutionClick(Sender: TObject);
    procedure MenuItem_ViewAutoFetchClick(Sender: TObject);
    procedure MenuItem_ViewLocationClick(Sender: TObject);
    procedure MenuItem_ViewSaveRectClick(Sender: TObject);
    procedure MenuItem_ViewShowGridClick(Sender: TObject);
    procedure MenuItem_ViewShowInfoClick(Sender: TObject);
    procedure MenuItem_ViewShowScaleClick(Sender: TObject);
    procedure TreeView_wmts_listMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeView_wmts_listMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView_wmts_listSelectionChanged(Sender: TObject);
  private
    FTileViewer:TTileViewer;
  public
    property TileViewer:TTileViewer read FTileViewer;
  public
    procedure UpdateStatusBar(Sender: TObject);
    procedure UpdateServerListEntry(Server:TWMTS_Service; ServerNode:TTreeNode=nil);
    procedure UpdateFeatureListEntry;
  end;

var
  FormTileMerger: TFormTileMerger;
  WMTS_Client:TWMTS_Client;

implementation
uses debugline, exporttiff, form_search_poi, form_options, form_view_location,
     tile_merger_feature, tile_merger_projection;

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
  TreeView_wmts_list.Items.Add(nil,'要素图层');
  root:=TreeView_wmts_list.Items.Add(nil,'地图服务器');
  root.Data:=nil;
  len_server:=WMTS_Client.ServiceCount;
  for idx_server:=0 to len_server-1 do begin
    server:=WMTS_Client.Services[idx_server];
    node:=TreeView_wmts_list.Items.AddChild(root,server.DisplayName);
    node.Data:=server;
    UpdateServerListEntry(server, node);
  end;

  UpdateFeatureListEntry;
  len:=WMTS_Client.FeatureLayerCount;
  for idx:=len-1 downto 0 do begin
    FTileViewer.AddFeatureLayer(WMTS_Client.FeatureLayers[idx]);
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

procedure TFormTileMerger.MenuItem_FeatureExportClick(Sender: TObject);
var filename:string;
    SelectedFeatureLayer:TWMTS_FeatureLayer;
begin
  if not (TObject(TreeView_wmts_list.Selected.Data) is TWMTS_FeatureLayer) then exit;
  SelectedFeatureLayer:=TWMTS_FeatureLayer(TreeView_wmts_list.Selected.Data);
  if SaveDialog_FeatureExport.Execute then begin
    filename:=SaveDialog_FeatureExport.FileName;
    case SaveDialog_FeatureExport.FilterIndex of
      1 {csv}      : SelectedFeatureLayer.Features.SaveToCSV(filename);
      2 {esrijson} : SelectedFeatureLayer.Features.SaveToEsriJSON(filename);
      3 {geojson}  : SelectedFeatureLayer.Features.SaveToGeoJSON(filename);
      //4 {shp}      : ;
      //5 {kml}      : ;
      else ShowMessage('暂不支持到处此格式要素文件。');
    end;
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

procedure TFormTileMerger.MenuItem_OptionSettingClick(Sender: TObject);
begin
  FormOptions.Execute;
end;

procedure TFormTileMerger.MenuItem_PoiServerClick(Sender: TObject);
begin
  //临时用于查找POI，该菜单按键后续用于POI服务器的设置
  Form_PoiServer.Show;
end;

procedure TFormTileMerger.MenuItem_SL_ReloadClick(Sender: TObject);
var DataObject:TObject;
    tmpServer:TWMTS_Service;
begin
  DataObject:=TObject(TreeView_wmts_list.Selected.Data);
  if DataObject is TWMTS_Service then begin


    tmpServer:=TWMTS_Service(DataObject);
    tmpServer.Clear;//只清除layer和tms
    tmpServer.LoadFromManifestXml(tmpServer.XmlURL,tmpServer.Config,true);
    UpdateServerListEntry(tmpServer);
    FTileViewer.InitializeLayerAndTileMatrixSet(tmpServer.Layers[0],tmpServer.TileMatrixSets[0]);
  end;
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

procedure TFormTileMerger.MenuItem_ViewLocationClick(Sender: TObject);
begin
  FormViewLocation.ShowModal;
end;

procedure TFormTileMerger.MenuItem_ViewSaveRectClick(Sender: TObject);
var view_name:string;
    tmpFT:TAGeoPolyline;
    view_rect:TGeoRectangle;
    ltLL, rbLL:TGeoPoint;
begin
  view_name:=InputBox('保存视图','保存视图名称：','');
  if view_name<>'' then begin
    view_rect:=TileViewer.ViewRect;
    ltLL:=TileViewer.CurrentTileMatrixSet.Projection.XYToLatlong(view_rect.LeftTop);
    rbLL:=TileViewer.CurrentTileMatrixSet.Projection.XYToLatlong(view_rect.RightBottom);
    tmpFT:=TAGeoPolyline.Create(2);
    tmpFT.AppendVertex(5);
    tmpFT.SeekVertes(0);
    tmpFT.X:=ltLL.lng;
    tmpFT.Y:=ltLL.lat;
    tmpFT.SeekVertes(1);
    tmpFT.X:=rbLL.lng;
    tmpFT.Y:=ltLL.lat;
    tmpFT.SeekVertes(2);
    tmpFT.X:=rbLL.lng;
    tmpFT.Y:=rbLL.lat;
    tmpFT.SeekVertes(3);
    tmpFT.X:=ltLL.lng;
    tmpFT.Y:=rbLL.lat;
    tmpFT.SeekVertes(4);
    tmpFT.X:=ltLL.lng;
    tmpFT.Y:=ltLL.lat;
    tmpFT.LabelText:=view_name;
    WMTS_Client.FeatureLayerByName['视图框'].Features.AddFeature(tmpFT);
    UpdateFeatureListEntry;
  end;
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

procedure TFormTileMerger.TreeView_wmts_listMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmpNode:TTreeNode;
begin
  if Button<>mbRight then exit;
  tmpNode:=TreeView_wmts_list.GetNodeAt(X,Y);
  TreeView_wmts_list.BeginUpdate;
  if tmpNode<>nil then try
    TreeView_wmts_list.ClearSelection;
    TreeView_wmts_list.Select(tmpNode);
  finally
    TreeView_wmts_list.EndUpdate;
  end;
end;

procedure TFormTileMerger.TreeView_wmts_listMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmpNode:TTreeNode;
begin
  if Button<>mbRight then exit;
  tmpNode:=TreeView_wmts_list.GetNodeAt(X,Y);
  TreeView_wmts_list.BeginUpdate;
  if tmpNode<>nil then try
    TreeView_wmts_list.ClearSelection;
    TreeView_wmts_list.Select(tmpNode);
  finally
    TreeView_wmts_list.EndUpdate;
  end;
end;

procedure TFormTileMerger.TreeView_wmts_listSelectionChanged(Sender: TObject);
var DataObject:TObject;
    tmpLayer:TWMTS_Layer;
begin
  if TreeView_wmts_list.Selected=nil then exit;
  DataObject:=TObject(TreeView_wmts_list.Selected.Data);

  //根据所选项修改右键弹出菜单，并提前退出无有效选择的情况
  if DataObject=nil then begin
    TreeView_wmts_list.PopupMenu:=nil;
    exit;
  end;
  if DataObject is TWMTS_FeatureLayer then TreeView_wmts_list.PopupMenu:=PopupMenu_FeatureList
  else if DataObject is TWMTS_Service then TreeView_wmts_list.PopupMenu:=PopupMenu_ServerList
  else TreeView_wmts_list.PopupMenu:=nil;

  //根据所选项修改状态变量
  if DataObject is TWMTS_Layer then FTileViewer.CurrentLayer:=DataObject as TWMTS_Layer;
  if DataObject is TWMTS_Service then
    if TWMTS_Service(DataObject).LayerCount=1 then begin
      FTileViewer.CurrentLayer:=TWMTS_Service(DataObject).Layers[0];
      //此处会导致直接右键服务器时同时导致图层更新，在右键菜单占用的情况下造成瓦片下载内存地址错误
    end else begin
      TreeView_wmts_list.Selected.Expanded:=true;
    end;
  if DataObject is TWMTS_TileMatrixSet then FTileViewer.CurrentTileMatrixSet:=DataObject as TWMTS_TileMatrixSet;
  if DataObject is TWMTS_ParameterValue then begin
    TWMTS_ParameterValue(DataObject).Owner.Selected:=TWMTS_ParameterValue(DataObject);
    tmpLayer:=TWMTS_ParameterValue(DataObject).Owner.Owner.Owner.Owner; //so weird
    if FTileViewer.CurrentLayer<>tmpLayer then FTileViewer.CurrentLayer:=tmpLayer;
  end;
  if DataObject is TWMTS_FeatureLayer then begin
    //点选要素图层后的动作
  end;
  if DataObject is TAGeoFeature then begin
    TileViewer.ZoomRect(TileViewer.CurrentTileMatrixSet.Projection.LatlongToXY(TAGeoFeature(DataObject).Boundary),1.0);
    TileViewer.Refresh;
  end;
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
  if FTileViewer.CurrentLayer.TimeTagCount=0 then CalendarFlow_TimeOption.CurrentDate:=Now;
  CalendarFlow_TimeOption.Refresh;
end;

procedure TFormTileMerger.UpdateServerListEntry(Server:TWMTS_Service; ServerNode:TTreeNode=nil);
//ServerNode为nil的时候通过Server查找已有项，清空后重新生成自项目
var root,node,layer:TTreeNode;
    tmplayer:TWMTS_Layer;
    idx,len:integer;
    option_key,option_value:TCollectionItem;
begin
  TreeView_wmts_list.BeginUpdate;
  try
    node:=nil;
    if ServerNode<>nil then
      node:=ServerNode
    else begin
      root:=TreeView_wmts_list.Items.FindNodeWithText('地图服务器');
      len:=root.Count;
      for idx:=0 to len-1 do begin
        if TObject(root.Items[idx].Data) = TObject(Server) then begin
          node:=TTreeNode(root.Items[idx]);
          break;
        end;
      end;
    end;
    if node=nil then begin
      node:=TreeView_wmts_list.Items.AddChild(root,Server.DisplayName);
    end else begin;
      len:=node.count;
      for idx:=len-1 downto 0 do TreeView_wmts_list.Items.Delete(node.Items[idx]);//node.Items[idx].Delete;
    end;
    len:=Server.LayerCount;
    for idx:=0 to len-1 do begin
      tmplayer:=Server.Layers[idx];
      layer:=TreeView_wmts_list.Items.AddChild(node,tmplayer.Title);
      layer.Data:=tmplayer;

      for option_key in tmplayer.ParameterList do begin
        for option_value in tmplayer.ParameterList[TWMTS_Parameter(option_key).Title].ValueList do begin
          TreeView_wmts_list.Items.AddChild(layer,TWMTS_ParameterValue(option_value).Value).Data:=option_value;
        end;
      end;
    end;
  finally
    TreeView_wmts_list.EndUpdate;
  end;
end;

procedure TFormTileMerger.UpdateFeatureListEntry;
var root, feature_layer, feature_node:TTreeNode;
    tmpFeatureLayer:TWMTS_FeatureLayer;
    tmpFeature:TAGeoFeature;
    idx,len,fidx, flen:integer;
begin
  TreeView_wmts_list.BeginUpdate;
  try
    root:=TreeView_wmts_list.Items.FindNodeWithText('要素图层');
    len:=root.Count;
    for idx:=len-1 downto 0 do begin
      TreeView_wmts_list.Items.Delete(root.Items[idx]);
    end;
    len:=WMTS_Client.FeatureLayerCount;
    for idx:=0 to len-1 do begin
      tmpFeatureLayer:=WMTS_Client.FeatureLayers[idx];
      flen:=tmpFeatureLayer.Features.Count;
      feature_layer:=TreeView_wmts_list.Items.AddChild(root, Format('%s (%d)',[tmpFeatureLayer.Title, flen]));
      feature_layer.Data:=tmpFeatureLayer;
      for fidx:=0 to flen-1 do begin
        tmpFeature:=TAGeoFeature(tmpFeatureLayer.Features.Items[fidx]);
        feature_node:=TreeView_wmts_list.Items.AddChild(feature_layer, tmpFeature.LabelText);
        feature_node.Data:=tmpFeature;
      end;
    end;
  finally
    TreeView_wmts_list.EndUpdate;
  end;
end;

initialization
  WMTS_Client:=TWMTS_Client.Create;

finalization
  WMTS_Client.Free;

end.

