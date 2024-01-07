unit tile_merger_wmts_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, DOM, XMLRead,
  Dialogs,
  tile_merger_core;

type

  TWMTS_Layer = class
  private
    FTitle:String;
    FIdentifier:String;
    FFormat:String;
    FURLTemplate:String;
  public
    property Title:String read FTitle;
  public
    function URL(aLevel:byte;aRow,aCol:integer):string;
  end;

  TWMTS_TileMatrix = class
  private
    FLevel:Byte;
    FScale:Double;
    FTileWidth,FTileHeight:Integer;
    FColumnCount,FRowCount:Int64;
    FTopLeft:TDoublePoint;
  end;

  TWMTS_TileMatrixSet = class
  private
    FTitle:String;
    FAbstract:String;
    FIdentifier:String;
    FTileMatrixList:TList;
  protected
    function GetTileMatrix(index:integer):TWMTS_TileMatrix;
    function GetTileMatrixCount:Integer;
  public
    property TileMatrixs[index:integer]:TWMTS_TileMatrix read GetTileMatrix;
    property TileMatrixCount:Integer read GetTileMatrixCount;
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  TWMTS_Service = class
  private
    FTitle:String;
    FLayerList:TList;
    FTileMatrixSetList:TList;
  protected
    function GetLayer(index:integer):TWMTS_Layer;
    function GetLayerCount:Integer;
    function GetTileMatrixSet(index:integer):TWMTS_TileMatrixSet;
    function GetTileMatrixSetCount:Integer;
  public
    property Layers[index:integer]:TWMTS_Layer read GetLayer;
    property LayerCount:Integer read GetLayerCount;
    property TileMatrixSets[index:integer]:TWMTS_TileMatrixSet read GetTileMatrixSet;
    property TileMatrixSetCount:Integer read GetTileMatrixSetCount;
    property Title:String read FTitle;
  public
    procedure LoadFromManifestXml(aUrl:string);
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  TWMTS_Client = class
  private
    FServiceList:TList;
  protected
    function GetService(index:integer):TWMTS_Service;
    function GetServiceCount:Integer;
  public
    property Services[index:integer]:TWMTS_Service read GetService;
    property ServiceCount:Integer read GetServiceCount;
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

implementation



{ TWMTS_Layer }

function TWMTS_Layer.URL(aLevel:byte;aRow,aCol:integer):string;
begin

end;


{ TWMTS_TileMatrixSet }

function TWMTS_TileMatrixSet.GetTileMatrix(index:integer):TWMTS_TileMatrix;
begin
  result:=nil;
  if index<0 then exit;
  if index>=FTileMatrixList.Count then exit;
  result:=TWMTS_TileMatrix(FTileMatrixList.Items[index]);
end;

function TWMTS_TileMatrixSet.GetTileMatrixCount:Integer;
begin
  result:=FTileMatrixList.Count;
end;

procedure TWMTS_TileMatrixSet.Clear;
begin
  while FTileMatrixList.Count>0 do begin
    TWMTS_TileMatrix(FTileMatrixList.Items[0]).Free;
    FTileMatrixList.Delete(0);
  end;
end;

constructor TWMTS_TileMatrixSet.Create;
begin
  inherited Create;
  FTileMatrixList:=TList.Create;
end;

destructor TWMTS_TileMatrixSet.Destroy;
begin
  Clear;
  FTileMatrixList.Free;
  inherited Destroy;
end;


{ TWMTS_Service }

function TWMTS_Service.GetLayer(index:integer):TWMTS_Layer;
begin
  result:=nil;
  if index<0 then exit;
  if index>=FLayerList.Count then exit;
  result:=TWMTS_Layer(FLayerList.Items[index]);
end;

function TWMTS_Service.GetLayerCount:Integer;
begin
  result:=FLayerList.Count;
end;

function TWMTS_Service.GetTileMatrixSet(index:integer):TWMTS_TileMatrixSet;
begin
  result:=nil;
  if index<0 then exit;
  if index>=FTileMatrixSetList.Count then exit;
  result:=TWMTS_TileMatrixSet(FTileMatrixSetList.Items[index]);
end;

function TWMTS_Service.GetTileMatrixSetCount:Integer;
begin
  result:=FTileMatrixSetList.Count;
end;

procedure TWMTS_Service.LoadFromManifestXml(aUrl:string);
var manifest:TMemoryStream;
    xml:TXMLDocument;
    node,content_node,tilematrix_node:TDOMNode;
    idx,len,mt_idx,mt_len,poss:integer;
    tmpLayer:TWMTS_Layer;
    tmpTileMatrix:TWMTS_TileMatrix;
    tmpTileMatrixSet:TWMTS_TileMatrixSet;
    px,py:string;
begin
  manifest:=TMemoryStream.Create;
  try
    TFPHTTPClient.SimpleGet(aUrl,manifest);
    if manifest.Size=0 then exit;
    xml:=TXMLDocument.Create;
    try
      manifest.Position:=0;
      ReadXMLFile(xml,manifest);
      //标题和基本的Server信息
      node:=xml.DocumentElement;
      node:=node.FindNode('ows:ServiceIdentification');
      node:=node.FindNode('ows:Title');
      FTitle:=node.FirstChild.NodeValue;
      //内容列表
      node:=xml.DocumentElement;
      node:=node.FindNode('Contents');
      len:=node.ChildNodes.Count;
      for idx:=0 to len-1 do begin
        content_node:=node.ChildNodes[idx];
        case content_node.NodeName of
          'Layer':begin
            tmpLayer:=TWMTS_Layer.Create;
            tmpLayer.FTitle:=content_node.FindNode('ows:Title').FirstChild.NodeValue;
            tmpLayer.FIdentifier:=content_node.FindNode('ows:Identifier').FirstChild.NodeValue;
            tmpLayer.FFormat:=content_node.FindNode('Format').FirstChild.NodeValue;
            tmpLayer.FURLTemplate:=content_node.FindNode('ResourceURL').Attributes.GetNamedItem('template').NodeValue;
            FLayerList.Add(tmpLayer);
          end;
          'TileMatrixSet':begin
            tmpTileMatrixSet:=TWMTS_TileMatrixSet.Create;
            tmpTileMatrixSet.FTitle:=content_node.FindNode('ows:Title').FirstChild.NodeValue;
            tmpTileMatrixSet.FAbstract:=content_node.FindNode('ows:Abstract').FirstChild.NodeValue;
            tmpTileMatrixSet.FIdentifier:=content_node.FindNode('ows:Identifier').FirstChild.NodeValue;
            mt_len:=content_node.ChildNodes.Count;
            for mt_idx:=0 to mt_len-1 do begin
              tilematrix_node:=content_node.ChildNodes[mt_idx];
              if tilematrix_node.NodeName<>'TileMatrix' then continue;
              tmpTileMatrix:=TWMTS_TileMatrix.Create;
              tmpTileMatrix.FLevel:=StrToInt(tilematrix_node.FindNode('ows:Identifier').FirstChild.NodeValue);
              tmpTileMatrix.FScale:=StrToFloat(tilematrix_node.FindNode('ScaleDenominator').FirstChild.NodeValue);
              tmpTileMatrix.FTileWidth:=StrToInt(tilematrix_node.FindNode('TileWidth').FirstChild.NodeValue);
              tmpTileMatrix.FTileHeight:=StrToInt(tilematrix_node.FindNode('TileHeight').FirstChild.NodeValue);
              tmpTileMatrix.FColumnCount:=StrToInt(tilematrix_node.FindNode('MatrixWidth').FirstChild.NodeValue);
              tmpTileMatrix.FRowCount:=StrToInt(tilematrix_node.FindNode('MatrixHeight').FirstChild.NodeValue);
              px:=tilematrix_node.FindNode('TopLeftCorner').FirstChild.NodeValue;
              py:=px;
              poss:=pos(' ',px);
              System.Delete(px,poss,length(px));
              System.Delete(py,1,poss);
              tmpTileMatrix.FTopLeft.x:=StrToFloat(px);
              tmpTileMatrix.FTopLeft.y:=StrToFloat(py);
              tmpTileMatrixSet.FTileMatrixList.Add(tmpTileMatrix);
            end;
            FTileMatrixSetList.Add(tmpTileMatrixSet);
          end;
        end;
      end;
    finally
      xml.Free;
    end;
  finally
    manifest.Free;
  end;
end;

procedure TWMTS_Service.Clear;
begin
  while FLayerList.Count>0 do begin
    TWMTS_Layer(FLayerList.Items[0]).Free;
    FLayerList.Delete(0);
  end;
  while FTileMatrixSetList.Count>0 do begin
    TWMTS_TileMatrixSet(FTileMatrixSetList.Items[0]).Free;
    FTileMatrixSetList.Delete(0);
  end;
end;

constructor TWMTS_Service.Create;
begin
  inherited Create;
  FLayerList:=TList.Create;
  FTileMatrixSetList:=TList.Create;
end;

destructor TWMTS_Service.Destroy;
begin
  Clear;
  FLayerList.Free;
  FTileMatrixSetList.Free;
  inherited Destroy;
end;



{ TWMTS_Client }

function TWMTS_Client.GetService(index:integer):TWMTS_Service;
begin
  result:=nil;
  if index<0 then exit;
  if index>=FServiceList.Count then exit;
  result:=TWMTS_Service(FServiceList.Items[index]);
end;

function TWMTS_Client.GetServiceCount:Integer;
begin
  result:=FServiceList.Count;
end;

procedure TWMTS_Client.Clear;
begin
  while FServiceList.Count>0 do begin
    TWMTS_Service(FServiceList.Items[0]).Free;
    FServiceList.Delete(0);
  end;
end;

constructor TWMTS_Client.Create;
const _wayback_ = 'https://wayback.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/WMTS/1.0.0/WMTSCapabilities.xml';
var tmpService:TWMTS_Service;
begin
  inherited Create;
  FServiceList:=TList.Create;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml(_wayback_);
  FServiceList.Add(tmpService);
end;

destructor TWMTS_Client.Destroy;
begin
  Clear;
  FServiceList.Free;
  inherited Destroy;
end;


initialization
  InitSSLInterface;

end.

