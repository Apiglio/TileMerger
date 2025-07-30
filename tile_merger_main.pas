unit tile_merger_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, Menus,
  tile_merger_core, tile_merger_view, tile_merger_wmts_client;

type

  { TFormTileMerger }

  TFormTileMerger = class(TForm)
    Label_export: TLabel;
    MainMenu_TileMerger: TMainMenu;
    MenuItem_DownloadDiv01: TMenuItem;
    MenuItem_DownloadExport: TMenuItem;
    MenuItem_ViewDiv01: TMenuItem;
    MenuItem_ViewAutoFetch: TMenuItem;
    MenuItem_DownloadModeForce: TMenuItem;
    MenuItem_DownloadModeAuto: TMenuItem;
    MenuItem_DownloadModeManual: TMenuItem;
    MenuItem_ServerToken: TMenuItem;
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
    Splitter_MainV: TSplitter;
    StatusBar_TileMerger: TStatusBar;
    TreeView_wmts_list: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem_DownloadExportClick(Sender: TObject);
    procedure MenuItem_OptionLogClick(Sender: TObject);
    procedure MenuItem_ViewAutoFetchClick(Sender: TObject);
    procedure MenuItem_ViewShowGridClick(Sender: TObject);
    procedure MenuItem_ViewShowInfoClick(Sender: TObject);
    procedure TreeView_wmts_listSelectionChanged(Sender: TObject);
  private
    FTileViewer:TTileViewer;
  public

  end;

var
  FormTileMerger: TFormTileMerger;
  WMTS_Client:TWMTS_Client;

implementation
uses debugline, exporttiff;

{$R *.lfm}

{ TFormTileMerger }

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
  FTileViewer.CurrentLayer:=server.Layers[0];
  FTileViewer.CurrentTileMatrixSet:=server.TileMatrixSets[0];
  FTileViewer.AutoFetchTile:=true;

end;

procedure TFormTileMerger.MenuItem_DownloadExportClick(Sender: TObject);
begin
  Form_ExportTiff.Execute(FTileViewer, FTileViewer.CurrentLayer, FTileViewer.CurrentTileMatrixSet);
end;

procedure TFormTileMerger.MenuItem_OptionLogClick(Sender: TObject);
begin
  Form_Debug.Show;
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
  len:=FTileViewer.CurrentTileMatrixSet.TileMatrixCount;
  Form_Debug.AddMessage('[CurrentTileMatrixSet]'+FTileViewer.CurrentTileMatrixSet.Identifier);
  for idx:=0 to len-1 do
    with FTileViewer.CurrentTileMatrixSet.TileMatrixs[idx] do begin
      Form_Debug.AddMessage(Format('  Index  = %d',[idx]));
      Form_Debug.AddMessage(Format('    Scale  = %3.1f',[Scale]));
      Form_Debug.AddMessage(Format('    ColCnt = %d',[ColumnCount]));
      Form_Debug.AddMessage(Format('    RowCnt = %d',[RowCount]));
    end;
end;

initialization
  WMTS_Client:=TWMTS_Client.Create;

finalization
  WMTS_Client.Free;

end.

