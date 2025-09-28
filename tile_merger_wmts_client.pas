unit tile_merger_wmts_client;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef UNIX}
  cthreads,
  {$endif}
  Classes, SysUtils, fphttpclient, openssl, DOM, XMLRead,
  Dialogs,
  tile_merger_core;

type

  TWMTS_TileMatrix = class;
  TWMTS_TileMatrixSet = class;

  TWMTS_Layer = class
  private
    FTitle:String;
    FIdentifier:String;
    FFormat:String;
    FURLTemplate:String;
    FService:TObject; //forward TWMTS_Service;
  protected
    function GetTileExtent:string;
  public
    property Title:String read FTitle;
    property Format:String read FFormat;
    property Service:TObject read FService;
    property TileExtent:string read GetTileExtent;
  public
    function URL(aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):string;
  end;

  TWMTS_TileMatrix = class
  private
    FParent:TWMTS_TileMatrixSet;
    FIdentifier:String;
    FScale:Double;
    FTileWidth,FTileHeight:Integer;
    FColumnCount,FRowCount:Int64;
    FLeftTop:TDoublePoint;
  public
    property LeftTop:TDoublePoint read FLeftTop;
    property Scale:Double read FScale;
    property Width:Integer read FTileWidth;
    property Height:Integer read FTileHeight;
    property ColumnCount:Int64 read FColumnCount;
    property RowCount:Int64 read FRowCount;
    property Identifier:String read FIdentifier;
  end;

  TWMTS_TileMatrixSet = class
  private
    FTitle:String;
    FAbstract:String;
    FIdentifier:String;
    FTileMatrixList:TList;
    FService:TObject; //forward TWMTS_Service;
  protected
    function GetTileMatrix(index:integer):TWMTS_TileMatrix;
    function GetTileMatrixCount:Integer;
  public
    property Identifier:String read FIdentifier;
    property TileMatrixs[index:integer]:TWMTS_TileMatrix read GetTileMatrix;
    property TileMatrixCount:Integer read GetTileMatrixCount;
    property Service:TObject read FService;
  public
    function BestFitTileMatrix(AScale:Double):TWMTS_TileMatrix; //根据地图比例尺选择最合适的TileMatrix
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  TWMTS_Service_Config = record
    url_replacement:record
      old_pattern:string;
      new_pattern:string;
    end;
    token:string;
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
    procedure LoadFromManifestXml(aUrl:string; ServiceConfig:TWMTS_Service_Config);
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

var
  ServiceConfig_Default:TWMTS_Service_Config;

implementation
uses tile_merger_view;


{ TWMTS_Layer }

function TWMTS_Layer.GetTileExtent:string;
begin
  case lowercase(FFormat) of
    'image/png':begin
      result:='png';
    end;
    'image/jpeg':begin
      result:='jpg';
    end;
    //'image/bmp':begin
    //  result:='bmp'; //这里并不确定有没有bmp的瓦片
    //end;
    else result:='dat';
  end;
end;

function TWMTS_Layer.URL(aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):string;
begin
  result:=FURLTemplate;
  result:=StringReplace(result,'{TileMatrixSet}',aTileMatrix.FParent.FIdentifier,[rfIgnoreCase]);
  result:=StringReplace(result,'{TileMatrix}',aTileMatrix.FIdentifier,[rfIgnoreCase]);
  result:=StringReplace(result,'{TileRow}',IntToStr(aRow),[rfIgnoreCase]);
  result:=StringReplace(result,'{TileCol}',IntToStr(aCol),[rfIgnoreCase]);
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

function TWMTS_TileMatrixSet.BestFitTileMatrix(AScale:Double):TWMTS_TileMatrix;
var idx,len:integer;
    tm_1,tm_2,tm_3,tm_4,tmpTM:TWMTS_TileMatrix;
    sca_1,sca_2,sca_3,sca_4,tmpScale:double;
begin
  result:=nil;
  len:=FTileMatrixList.Count;
  if len<1 then exit;
  sca_1:=webmercator_ms;  //min scale
  sca_2:=-1;              //max scale below (or equal to) AScale
  sca_3:=webmercator_ms;  //min scale above (or equal to) AScale
  sca_4:=-1;              //max scale
  tm_1:=nil;
  tm_2:=nil;
  tm_3:=nil;
  tm_4:=nil;
  idx:=0;
  while idx<len do begin
    tmpTM:=TWMTS_TileMatrix(FTileMatrixList[idx]);
    tmpScale:=tmpTM.FScale;
    if tmpScale>sca_4 then begin
      sca_4:=tmpScale;
      tm_4:=tmpTM;
    end;
    if tmpScale<sca_1 then begin
      sca_1:=tmpScale;
      tm_1:=tmpTM;
    end;
    if (tmpScale>sca_2) and (tmpScale<=AScale) then begin
      sca_2:=tmpScale;
      tm_2:=tmpTM;
    end;
    if (tmpScale<sca_3) and (tmpScale>=AScale) then begin
      sca_3:=tmpScale;
      tm_3:=tmpTM;
    end;
    inc(idx);
  end;
  result:=tm_2;
  if result=nil then result:=tm_1;
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

procedure TWMTS_Service.LoadFromManifestXml(aUrl:string; ServiceConfig:TWMTS_Service_Config);
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
    with TFPHTTPClient.Create(nil) do try
      AllowRedirect:=true;
      AddHeader('User-Agent','ArcGIS Client Using WinInet');
      Get(aUrl,manifest);
    finally
      Free;
    end;
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
            //天地图格式差很多，要专门处理 http://s0.fjmap.net/img_fj_2019/wmts
            //RESTful 和 KVP
            tmpLayer:=TWMTS_Layer.Create;
            tmpLayer.FTitle:=content_node.FindNode('ows:Title').FirstChild.NodeValue;
            tmpLayer.FIdentifier:=content_node.FindNode('ows:Identifier').FirstChild.NodeValue;
            tmpLayer.FFormat:=content_node.FindNode('Format').FirstChild.NodeValue;
            tmpLayer.FURLTemplate:=content_node.FindNode('ResourceURL').Attributes.GetNamedItem('template').NodeValue;
            with ServiceConfig.url_replacement do
              tmpLayer.FURLTemplate:=tmpLayer.FURLTemplate.Replace(old_pattern, new_pattern);
            tmpLayer.FService:=Self;
            FLayerList.Add(tmpLayer);
          end;
          'TileMatrixSet':begin
            tmpTileMatrixSet:=TWMTS_TileMatrixSet.Create;
            tmpTileMatrixSet.FIdentifier:=content_node.FindNode('ows:Identifier').FirstChild.NodeValue;
            if content_node.FindNode('ows:Abstract') <> nil then
              tmpTileMatrixSet.FAbstract:=content_node.FindNode('ows:Abstract').FirstChild.NodeValue;
            if content_node.FindNode('ows:Title') <> nil then
              tmpTileMatrixSet.FTitle:=content_node.FindNode('ows:Title').FirstChild.NodeValue
            else
              tmpTileMatrixSet.FTitle:=tmpTileMatrixSet.FIdentifier;
            tmpTileMatrixSet.FService:=Self;
            mt_len:=content_node.ChildNodes.Count;
            for mt_idx:=0 to mt_len-1 do begin
              tilematrix_node:=content_node.ChildNodes[mt_idx];
              if tilematrix_node.NodeName<>'TileMatrix' then continue;
              tmpTileMatrix:=TWMTS_TileMatrix.Create;
              tmpTileMatrix.FParent:=tmpTileMatrixSet;
              tmpTileMatrix.FIdentifier:=tilematrix_node.FindNode('ows:Identifier').FirstChild.NodeValue;
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
              tmpTileMatrix.FLeftTop.x:=StrToFloat(px);
              tmpTileMatrix.FLeftTop.y:=StrToFloat(py);
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
//const _wayback_ = 'https://wayback.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/WMTS/1.0.0/WMTSCapabilities.xml';
const _wayback_ = 'https://wayback-a.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/WMTS/1.0.0/WMTSCapabilities.xml';
var tmpService:TWMTS_Service;
    tmpServiceConfig:TWMTS_Service_Config;
begin
  inherited Create;
  FServiceList:=TList.Create;

  tmpServiceConfig.url_replacement.old_pattern:='//wayback.';
  tmpServiceConfig.url_replacement.new_pattern:='//wayback-a.';
  tmpServiceConfig.token:='';
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml(_wayback_, tmpServiceConfig);
  FServiceList.Add(tmpService);

  //tmpService:=TWMTS_Service.Create;
  //tmpService.LoadFromManifestXml('https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/1.0.0/WMTSCapabilities.xml', ServiceConfig_Default);
  //FServiceList.Add(tmpService);

  //tmpService:=TWMTS_Service.Create;
  //tmpService.LoadFromManifestXml('http://s0.fjmap.net:80/img_fj_2019/wmts');
  //FServiceList.Add(tmpService);

  //https://osmlab.github.io/wmts-osm/WMTSCapabilities.xml
  //https://ows.terrestris.de/osm/service?service=WMTS&request=GetCapabilities
  //https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/1.0.0/WMTSCapabilities.xml

end;

destructor TWMTS_Client.Destroy;
begin
  Clear;
  FServiceList.Free;
  inherited Destroy;
end;


initialization
  InitSSLInterface;
  with ServiceConfig_Default do begin
    token:='';
    url_replacement.old_pattern:='';
    url_replacement.new_pattern:='';
  end;


end.

