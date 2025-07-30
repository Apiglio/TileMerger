unit exporttiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, tile_merger_wmts_client, tile_merger_view, tile_merger_core;

type

  { TForm_ExportTiff }

  TForm_ExportTiff = class(TForm)
    Button_ExportTiff: TButton;
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
    procedure Button_ExportTiffClick(Sender: TObject);
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

procedure TForm_ExportTiff.Button_ExportTiffClick(Sender: TObject);
var tmpTM:TWMTS_TileMatrix;
begin
  ForceDirectories(ExtractFileDir(Edit_SaveFileName.Text));
  tmpTM:=TWMTS_TileMatrix(ListBox_TileMatrix.Items.Objects[ListBox_TileMatrix.ItemIndex]);
  PTileViewer.Clear;
  PTileViewer.ShowTiles(tmpTM.Scale);
  PTileViewer.SaveToGeoTiff(Edit_SaveFileName.Text);
end;

procedure TForm_ExportTiff.Button_SaveFileNameClick(Sender: TObject);
begin
  with SaveDialog_Export do
    if Execute then begin
      Edit_SaveFileName.Caption:=FileName;
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
