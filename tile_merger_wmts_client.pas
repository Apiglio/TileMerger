unit tile_merger_wmts_client;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef UNIX}
  cthreads,
  {$endif}
  Classes, SysUtils, fphttpclient, openssl, DOM, XMLRead,
  Dialogs, tile_merger_projection;

type

  TWMTS_TileMatrix = class;
  TWMTS_TileMatrixSet = class;

  TWMTS_Options = class
  protected
    FNamedDimensions:TStringList;
    FDefaultIndice:TStringList;
  public
    procedure AddValue(key,value:string;isDefault:boolean=false);
    function GetValue(key:string;idx:integer):string;
    function GetValueCount(key:string):integer;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  TWMTS_Layer = class
  private
    FTitle:String;
    FIdentifier:String;
    FFormat:String;
    FStyle:String;
    FURLTemplate:String;
    FService:TObject; //forward TWMTS_Service;
    FBoundingBox:TGeoRectangle;
    FDimension:TWMTS_Options;

  protected
    function GetTileExtent:string;
  public
    property Title:String read FTitle;
    property Format:String read FFormat;
    property Service:TObject read FService;
    property TileExtent:string read GetTileExtent;
  public
    function URL(aTileMatrix:TWMTS_TileMatrix;aRow,aCol:integer):string;
    constructor Create;
    destructor Destroy; override;
  end;

  TWMTS_TileMatrix = class
  private
    FParent:TWMTS_TileMatrixSet;
    FIdentifier:String;
    FScale:Double;
    FTileWidth,FTileHeight:Integer;
    FColumnCount,FRowCount:Int64;
    FLeftTop:TGeoPoint;
  public
    function GetTileRect(MatrixCol, MatrixRow: Integer):TGeoRectangle;
    function GetTileIndex(Point:TGeoPoint):TTileIndex;
  protected
    function GetMeterPerPixel:TGeoCoord;
    function GetUserAgent:String;
  public
    property LeftTop:TGeoPoint read FLeftTop;
    property Scale:Double read FScale;
    property Width:Integer read FTileWidth;
    property Height:Integer read FTileHeight;
    property ColumnCount:Int64 read FColumnCount;
    property RowCount:Int64 read FRowCount;
    property Identifier:String read FIdentifier;
    property MeterPerPixel:TGeoCoord read GetMeterPerPixel;
    property TileMatrixSet:TWMTS_TileMatrixSet read FParent;
    property UserAgent:String read GetUserAgent;
  end;

  TWMTS_TileMatrixSet = class
  private
    FTitle:String;
    FAbstract:String;
    FIdentifier:String;
    FSupportedCRS:String;
    FProjection:TProjection;
    FTileMatrixList:TList;
    FService:TObject; //forward TWMTS_Service;
  protected
    function GetTileMatrix(index:integer):TWMTS_TileMatrix;
    function GetTileMatrixCount:Integer;
    function GetMeterPerPixel:TGeoCoord;
    procedure SetMeterPerPixel(Value:TGeoCoord);
  public
    property Identifier:String read FIdentifier;
    property TileMatrixs[index:integer]:TWMTS_TileMatrix read GetTileMatrix;
    property TileMatrixCount:Integer read GetTileMatrixCount;
    property Service:TObject read FService;
    property Projection:TProjection read FProjection;
    property MeterPerPixel:TGeoCoord read GetMeterPerPixel write SetMeterPerPixel;
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
    fixed_meter_per_pixel:TGeoCoord;//0 means automatical
  end;

  TWMTS_Service = class
  private
    FTitle:String;
    FVersion:String;
    FLayerList:TList;
    FTileMatrixSetList:TList;
    FToken:String;
    FKvpUrl:String;
    FUserAgent:String;
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
    property Token:String read FToken;
    property KvpUrl:String read FKvpUrl;
    property UserAgent:String read FUserAgent write FUserAgent;
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
uses tile_merger_view, math;


{ TWMTS_Options }

procedure TWMTS_Options.AddValue(key,value:string;isDefault:boolean=false);
var idx:integer;
    pDimension:TStringList;
begin
  if FNamedDimensions.Find(key, idx) then begin
    pDimension:=TStringList(FNamedDimensions.Objects[idx]);
  end else begin
    pDimension:=TStringList.Create;
    FNamedDimensions.AddObject(key, pDimension);
    FDefaultIndice.AddObject(key, TObject(pint32(-1)));
  end;
  idx:=pDimension.Add(value);

  if isDefault then FDefaultIndice.Objects[idx]:=TObject(pint32(idx));
end;

function TWMTS_Options.GetValue(key:string;idx:integer):string;
var dim_idx:integer;
begin
  result:='';
  if FNamedDimensions.Find(key, dim_idx) then begin
    if (idx<0) or (idx>=dim_idx) then raise Exception.Create('invalid index: '+IntToStr(idx));
    result:=TStringList(FNamedDimensions.Objects[dim_idx]).Strings[idx];
  end else result:='';
end;

function TWMTS_Options.GetValueCount(key:string):integer;
var idx:integer;
begin
  result:=0;
  if FNamedDimensions.Find(key, idx) then result:=TStringList(FNamedDimensions.Objects[idx]).Count
  else raise Exception.Create('No such key: '+key);
end;

procedure TWMTS_Options.Clear;
var idx:integer;
begin
  for idx:=FNamedDimensions.Count-1 downto 0 do
    TStringList(FNamedDimensions.Objects[idx]).Free;
  FNamedDimensions.Clear;
  FDefaultIndice.Clear;
end;

constructor TWMTS_Options.Create;
begin
  inherited Create;
  FNamedDimensions:=TStringList.Create;
  FNamedDimensions.Sorted:=true;
  FDefaultIndice:=TStringList.Create;
  FDefaultIndice.Sorted:=true;
end;

destructor TWMTS_Options.Destroy;
begin
  Clear;
  FNamedDimensions.Free;
  FDefaultIndice.Free;
  inherited Destroy;
end;


{ TWMTS_Layer }

function TWMTS_Layer.GetTileExtent:string;
begin
  case lowercase(FFormat) of
    'image/png','tiles':begin
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
  result:=StringReplace(result,'{Layer}',FIdentifier,[rfIgnoreCase]);
  result:=StringReplace(result,'{Token}',TWMTS_Service(FService).FToken,[rfIgnoreCase]);
  result:=StringReplace(result,'{Version}',TWMTS_Service(FService).FVersion,[rfIgnoreCase]);
  result:=StringReplace(result,'{Format}',FFormat,[rfIgnoreCase]);
  result:=StringReplace(result,'{Style}',FStyle,[rfIgnoreCase]);

end;

constructor TWMTS_Layer.Create;
begin
  inherited Create;
  FDimension:=TWMTS_Options.Create;
end;

destructor TWMTS_Layer.Destroy;
begin
  FDimension.Free;
  inherited Destroy;
end;


{ TWMTS_TileMatrix }

function TWMTS_TileMatrix.GetTileRect(MatrixCol, MatrixRow: Integer):TGeoRectangle;
begin
  result:=FParent.Projection.GetWMTSTileRect(FLeftTop, FScale, FTileWidth, FTileHeight, MatrixCol, MatrixRow);
end;

function TWMTS_TileMatrix.GetTileIndex(Point:TGeoPoint):TTileIndex;
begin
  result:=FParent.Projection.GetWMTSTileIndex(FLeftTop, FScale, FTileWidth, FTileHeight, Point);
end;

function TWMTS_TileMatrix.GetMeterPerPixel:TGeoCoord;
begin
  result:=FParent.FProjection.MetterPerPixel;
end;

function TWMTS_TileMatrix.GetUserAgent:String;
begin
  result:=TWMTS_Service(FParent.Service).UserAgent;
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

function TWMTS_TileMatrixSet.GetMeterPerPixel:TGeoCoord;
begin
  result:=FProjection.MetterPerPixel;
end;

procedure TWMTS_TileMatrixSet.SetMeterPerPixel(Value:TGeoCoord);
begin
  FProjection.MetterPerPixel:=Value;
end;

function TWMTS_TileMatrixSet.BestFitTileMatrix(AScale:Double):TWMTS_TileMatrix;
var idx,len:integer;
    tm_1,tm_2,tm_3,tm_4,tmpTM:TWMTS_TileMatrix;
    sca_1,sca_2,sca_3,sca_4,tmpScale:double;
begin
  result:=nil;
  len:=FTileMatrixList.Count;
  if len<1 then exit;
  sca_1:=MaxDouble;       //min scale (webmercator_ms)
  sca_2:=-1;              //max scale below (or equal to) AScale
  sca_3:=MaxDouble;       //min scale above (or equal to) AScale
  sca_4:=-1;              //max scale (webmercator_ms)
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
    node,content_node,tilematrix_node,tmp_node,tm2_node:TDOMNode;
    idx,len,mt_idx,mt_len,poss:integer;
    tmpLayer:TWMTS_Layer;
    tmpTileMatrix:TWMTS_TileMatrix;
    tmpTileMatrixSet:TWMTS_TileMatrixSet;
    px,py,kvp_url,dimension_id:string;
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
    FToken:=ServiceConfig.token;
    xml:=TXMLDocument.Create;
    try
      manifest.Position:=0;
      ReadXMLFile(xml,manifest);
      //标题和基本的Server信息
      node:=xml.DocumentElement;
      node:=node.FindNode('ows:ServiceIdentification');
      node:=node.FindNode('ows:Title');
      FTitle:=node.FirstChild.NodeValue;
      node:=node.ParentNode;
      node:=node.FindNode('ows:ServiceTypeVersion');
      FVersion:=node.FirstChild.NodeValue;
      //读取KVP方法
      FKvpUrl:='';
      node:=xml.DocumentElement;
      node:=node.FindNode('ows:OperationsMetadata');
      len:=node.ChildNodes.Count;
      for idx:=0 to len-1 do begin
        content_node:=node.ChildNodes[idx];
        if content_node.NodeName<>'ows:Operation' then continue;
        if content_node.Attributes.GetNamedItem('name').NodeValue<>'GetTile' then continue;
        content_node:=content_node.FindNode('ows:DCP');
        content_node:=content_node.FindNode('ows:HTTP');
        content_node:=content_node.FindNode('ows:Get');
        kvp_url:=content_node.Attributes.GetNamedItem('xlink:href').FirstChild.NodeValue;
        content_node:=content_node.FindNode('ows:Constraint');
        content_node:=content_node.FindNode('ows:AllowedValues');
        content_node:=content_node.FindNode('ows:Value');
        if content_node.FirstChild.NodeValue='KVP' then
          FKvpUrl:=kvp_url+'Service=WMTS&request=GetTile&Version={Version}&Layer={Layer}&Style={Style}&TileMatrixSet={TileMatrixSet}&TileMatrix={TileMatrix}&TileRow={TileRow}&TileCol={TileCol}&Format={Format}&tk={Token}';
      end;
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
            tmpLayer.FStyle:=content_node.FindNode('Style').FindNode('ows:Identifier').FirstChild.NodeValue;
            tmp_node:=nil;
            tmp_node:=content_node.FindNode('Dimension');
            if tmp_node<>nil then begin
              dimension_id:=tmp_node.FindNode('ows:Identifier').FirstChild.NodeValue;
              {dims}mt_len:=tmp_node.ChildNodes.Count;
              for {dims}mt_idx:=0 to {dims}mt_len-1 do begin
                tm2_node:=tmp_node.ChildNodes[{dims}mt_idx];
                if tm2_node.NodeName='Value' then tmpLayer.FDimension.AddValue(dimension_id,tm2_node.FirstChild.NodeValue);
              end;
            end;
            if FKvpUrl='' then
              tmpLayer.FURLTemplate:=content_node.FindNode('ResourceURL').Attributes.GetNamedItem('template').NodeValue
            else
              tmpLayer.FURLTemplate:=FKvpUrl;
            with ServiceConfig.url_replacement do
              if old_pattern<>'' then
                tmpLayer.FURLTemplate:=tmpLayer.FURLTemplate.Replace(old_pattern, new_pattern);
            tmpLayer.FService:=Self;
            FLayerList.Add(tmpLayer);
          end;
          'TileMatrixSet':begin
            tmpTileMatrixSet:=TWMTS_TileMatrixSet.Create;
            tmpTileMatrixSet.FIdentifier:=content_node.FindNode('ows:Identifier').FirstChild.NodeValue;
            tmpTileMatrixSet.FSupportedCRS:=content_node.FindNode('ows:SupportedCRS').FirstChild.NodeValue;
            tmpTileMatrixSet.FProjection:=TProjection.CreateProjectionByText(tmpTileMatrixSet.FSupportedCRS);
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
              tmpTileMatrix.FLeftTop:=tmpTileMatrixSet.Projection.DecodeCoordinate(tmpTileMatrix.FLeftTop);
              //calculate for non-standard meter_per_pixel
              if ServiceConfig.fixed_meter_per_pixel=0 then with tmpTileMatrix do begin
                tmpTileMatrixSet.Projection.MetterPerPixel:=2*abs(FLeftTop.lng)/FTileWidth/FColumnCount/FScale;
              end else begin
                tmpTileMatrixSet.Projection.MetterPerPixel:=ServiceConfig.fixed_meter_per_pixel;
              end;
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
  FUserAgent:='ArcGIS Client Using WinInet';
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
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml(_wayback_, tmpServiceConfig);
  FServiceList.Add(tmpService);
  {
  //需要解决Time维度
  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='0e2c50def624b69b1dcb67f43f353c49';
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/1.0.0/WMTSCapabilities.xml', ServiceConfig_Default);
  FServiceList.Add(tmpService);
  }
  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='0e2c50def624b69b1dcb67f43f353c49';
  tmpServiceConfig.fixed_meter_per_pixel:=0.0002803138; //没招了就这样吧
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('http://s0.fjmap.net:80/img_fj_2019/wmts', tmpServiceConfig);
  FServiceList.Add(tmpService);
  tmpService.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';


  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='ef1e65139e1e3571ff1338d9a72e8142';
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('https://t0.tianditu.gov.cn/img_w/wmts?request=GetCapabilities&service=wmts', tmpServiceConfig);
  FServiceList.Add(tmpService);

  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='';
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('https://ows.terrestris.de/osm/service?service=WMTS&request=GetCapabilities', tmpServiceConfig);
  FServiceList.Add(tmpService);


  //需要解决Time维度
  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='0e2c50def624b69b1dcb67f43f353c49';
  tmpServiceConfig.fixed_meter_per_pixel:=0.0002803138; //没招了就这样吧
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('http://s0.fjmap.net/img_fj_2025_his/wmts', tmpServiceConfig);
  FServiceList.Add(tmpService);
  tmpService.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';


  //https://t0.tianditu.gov.cn/img_w/wmts?request=GetCapabilities&service=wmts
  //http://s0.fjmap.net:80/img_fj_2019/wmts
  //http://s0.fjmap.net/img_fj_2025_his/wmts
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
    fixed_meter_per_pixel:=0;
  end;


end.

