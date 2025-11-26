unit exporttiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, tile_merger_wmts_client, tile_merger_view;

type

  { TForm_ExportTiff }

  TForm_ExportTiff = class(TForm)
    Button_MergeTiles: TButton;
    Button_DownloadTiles: TButton;
    Button_SaveFileName: TButton;
    Edit_Layer: TEdit;
    Edit_TileMatrixSet: TEdit;
    Edit_SaveFileName: TEdit;
    Label_Layer: TLabel;
    Label_TileMatrixSet: TLabel;
    Label_SaveFileName: TLabel;
    Label_TileMatrix: TLabel;
    ListBox_TileMatrix: TListBox;
    ProgressBar_Export: TProgressBar;
    SaveDialog_Export: TSaveDialog;
    procedure Button_DownloadTilesClick(Sender: TObject);
    procedure Button_MergeTilesClick(Sender: TObject);
    procedure Button_SaveFileNameClick(Sender: TObject);
  private
    PTileViewer:TTileViewer;
  public
    procedure Execute(aTileViewer:TTileViewer;aLayer:TWMTS_Layer;aTileMatrixSet:TWMTS_TileMatrixSet);
  end;

var
  Form_ExportTiff: TForm_ExportTiff;

implementation

{$R *.lfm}

procedure TForm_ExportTiff.Button_DownloadTilesClick(Sender: TObject);
var tmpTM:TWMTS_TileMatrix;
begin
  ForceDirectories(ExtractFileDir(Edit_SaveFileName.Text));
  tmpTM:=TWMTS_TileMatrix(ListBox_TileMatrix.Items.Objects[ListBox_TileMatrix.ItemIndex]);

  PTileViewer.TilePool.Clear;
  PTileViewer.ShowTiles(tmpTM.Scale);

end;

procedure TForm_ExportTiff.Button_MergeTilesClick(Sender: TObject);
begin
  PTileViewer.SaveToGeoTiff(Edit_SaveFileName.Text);
end;

procedure TForm_ExportTiff.Button_SaveFileNameClick(Sender: TObject);
var len:integer;
    fname:string;
begin
  with SaveDialog_Export do
    if Execute then begin
      fname:=FileName;
      len:=length(fname);
      if pos('.tif',lowercase(fname))=len-3 then delete(fname,len-3,4);
      Edit_SaveFileName.Caption:=fname;
    end;
end;

procedure TForm_ExportTiff.Execute(aTileViewer:TTileViewer;aLayer:TWMTS_Layer;aTileMatrixSet:TWMTS_TileMatrixSet);
var tmpTM:TWMTS_TileMatrix;
    len,idx:integer;
begin
  PTileViewer:=aTileViewer;
  Edit_Layer.Caption:=aLayer.Title;
  Edit_TileMatrixSet.Caption:=aTileMatrixSet.Identifier;
  ListBox_TileMatrix.Clear;
  len:=aTileMatrixSet.TileMatrixCount;
  for idx:=len-1 downto 0 do begin
    tmpTM:=aTileMatrixSet.TileMatrixs[idx];
    ListBox_TileMatrix.Items.AddObject(tmpTM.Identifier,tmpTM);
  end;
  ListBox_TileMatrix.ItemIndex:=0;
  ShowModal;

end;

end.
