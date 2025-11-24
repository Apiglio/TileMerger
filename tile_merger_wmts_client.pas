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
  TWMTS_Layer = class;
  TWMTS_Parameter = class;
  TWMTS_ParameterValue = class;
  TWMTS_ParameterList = class;
  TWMTS_ParameterValueList = class;

  TWMTS_ParameterValue = class(TCollectionItem)
  private
    FValue:String;
  protected
    function GetOwner:TWMTS_ParameterValueList;
  public
    property Value:String read FValue write FValue;
    property Owner:TWMTS_ParameterValueList read GetOwner;
  end;

  TWMTS_ParameterValueList = class(TCollection)
  private
    FOwner:TWMTS_Parameter;
    FSelected:TWMTS_ParameterValue;
  public
    function Add(Value:String):TWMTS_ParameterValue;
    constructor Create(TheOwner:TWMTS_Parameter);
    property Selected:TWMTS_ParameterValue read FSelected write FSelected;
    property Owner:TWMTS_Parameter read FOwner;
  end;

  TWMTS_Parameter = class(TCollectionItem)
  private
    FTitle:String;
    FValueList:TWMTS_ParameterValueList;
  protected
    function GetOwner:TWMTS_ParameterList;
  public
    function Add(Value:String):TWMTS_ParameterValue;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Title:String read FTitle write FTitle;
    property ValueList:TWMTS_ParameterValueList read FValueList;
    property Owner:TWMTS_ParameterList read GetOwner;
  end;

  TWMTS_ParameterList = class(TCollection)
  private
    FOwner:TWMTS_Layer;
    FSelected:TWMTS_Parameter;
  protected
    function AddParameterIfNotExist(ParameterTitle:String):TWMTS_Parameter;
  public
    function Add(ParamterTitle:String):TWMTS_Parameter;
    constructor Create(TheOwner:TWMTS_Layer);
    property Parameter[ParameterTitle:String]:TWMTS_Parameter read AddParameterIfNotExist; default;
    property Selected:TWMTS_Parameter read FSelected write FSelected;
    property Owner:TWMTS_Layer read FOwner;
  public
    function GetURLParameter:string;
    function GetPathNameParameter:string;
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
    FParameterList:TWMTS_ParameterList;
    FTimeTags:array of TDateTime; //only for RESTful ISO8601 time option
    FTimeTagSelected:TDateTime;   //copy from the FTimesTags
    FUsingISO8601:Boolean;

  protected
    function GetTileExtent:string;
    function GetTimeTag(index:integer):TDateTime;
    function GetTimeTagCount:integer;
  public
    property Title:String read FTitle;
    property Format:String read FFormat;
    property Service:TObject read FService;
    property TileExtent:string read GetTileExtent;
    property ParameterList:TWMTS_ParameterList read FParameterList;
    property TimeTag[index:integer]:TDateTime read GetTimeTag;
    property TimeTagCount:Integer read GetTimeTagCount;
    property TimeTagSelected:TDateTime read FTimeTagSelected write FTimeTagSelected;
    property UsingISO8601:boolean read FUsingISO8601 write FUsingISO8601;
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
    FDisplayName:String;
  protected
    function GetLayer(index:integer):TWMTS_Layer;
    function GetLayerCount:Integer;
    function GetTileMatrixSet(index:integer):TWMTS_TileMatrixSet;
    function GetTileMatrixSetByName(idenifier:string):TWMTS_TileMatrixSet;
    function GetTileMatrixSetCount:Integer;
  public
    property Layers[index:integer]:TWMTS_Layer read GetLayer;
    property LayerCount:Integer read GetLayerCount;
    property TileMatrixSets[index:integer]:TWMTS_TileMatrixSet read GetTileMatrixSet;
    property TileMatrixSetByName[idenifier:string]:TWMTS_TileMatrixSet read GetTileMatrixSetByName;
    property TileMatrixSetCount:Integer read GetTileMatrixSetCount;
    property Title:String read FTitle;
    property Token:String read FToken;
    property KvpUrl:String read FKvpUrl;
    property UserAgent:String read FUserAgent write FUserAgent;
    property DisplayName:String read FDisplayName write FDisplayName;
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
uses tile_merger_view, math, dateutils;

function DateTimeToISO8601(dt:TDateTime):string;
begin
  result:=Format('%.4d-%.2d-%0.2dT%.2d:%.2d:%.2dZ',[YearOf(dt),MonthOf(dt),DayOf(dt),HourOf(dt),MinuteOf(dt),SecondOf(dt)]);
end;

function ISO8601ToDateTime(iso:string):TDateTime;
var dat,tim,yr,mt,dy,hr,mi,se:string;
    len:integer;
begin
  result:=0.0;
  //yyyy-mm-ddThh:mm:ssZ
  //yyyy-mm-dd
  len:=length(iso);
  if len=20 then begin
    tim:=iso;
    dat:=iso;
    Delete(tim,20,1);
    Delete(tim,1,11);
    Delete(dat,11,10);
    hr:=tim;
    mi:=tim;
    se:=tim;
    Delete(hr,3,7);
    Delete(mi,6,3);
    Delete(mi,1,3);
    Delete(se,1,6);
  end else if len=10 then begin
    dat:=iso;
    hr:='00';
    mi:='00';
    se:='00';
  end else begin
    exit;
  end;
  yr:=iso;
  mt:=iso;
  dy:=iso;
  Delete(yr,5,6+10);
  Delete(mt,8,3+10);
  Delete(mt,1,5);
  Delete(dy,11,10);
  Delete(dy,1,8);
  result:=dateutils.EncodeDateTime(StrToInt(yr),StrToInt(mt),StrToInt(dy),StrToInt(hr),StrToInt(mi),StrToInt(se),0);

end;

function ISORepeatingIntervalFindFirst(const ri:string; var dt1, dt2:TDateTime; var interval:string):boolean;
var st1,st2:string;
    t_pos:integer;
begin
  result:=false;
  t_pos:=pos('/',ri);
  if t_pos<=0 then exit;
  st1:=ri;
  delete(st1,t_pos,length(st1));
  st2:=ri;
  delete(st2,1,t_pos);
  t_pos:=pos('/',st2);
  if t_pos<=0 then exit;
  interval:=st2;
  delete(st2,t_pos,length(st2));
  delete(interval,1,t_pos);
  dt1:=ISO8601ToDateTime(st1);
  dt2:=ISO8601ToDateTime(st2);
  result:=true;
end;

{
function ISORepeatingIntervalFindNext(var current:TDateTime; const endUpWith:TDateTime; const interval:string):boolean;
var interval_str:string;
    NextDateTime:TDateTime;
begin
  result:=false;
  interval_str:=UpperCase(interval);
  if interval_str[1]<>'P' then exit else delete(interval_str,1,1);
  if interval_str[1]='T' then begin
    //...
  end else begin
    //...
  end;
end;
}

function ISORepeatingIntervalFindNext( var current:TDateTime; const endUpWith:TDateTime; const interval:string):boolean;
var s,datepart,timepart:string;
    years,months,days,hours,mins,secs:integer;
    function ExtractNumber(var s:string;ident:char):integer;
    var p:integer; tmp:string;
    begin
      p := Pos(ident, s);
      if p=0 then Exit(0);
      // extract number before ident
      tmp := Copy(s,1,p-1);
      Delete(s,1,p);
      Result := StrToIntDef(tmp, 0);
    end;
begin
  Result := False;
  s := UpperCase(interval);
  if (s='') or (s[1]<>'P') then Exit;
  Delete(s,1,1); // remove P
  years := 0; months := 0; days := 0;
  hours := 0; mins := 0; secs := 0;

  // Date part first
  if Pos('T', s) > 0 then begin
    // date part
    datepart := s;
    timepart := s;
    Delete(datepart, Pos('T', datepart), Length(datepart));
    Delete(timepart, 1, Pos('T', timepart));
    s := datepart;
    years  := ExtractNumber(s,'Y');
    months := ExtractNumber(s,'M');
    days   := ExtractNumber(s,'D');
    // time part
    s := timepart;
    Delete(s,1,0); // s starts with 'T', but we removed earlier
    hours := ExtractNumber(s,'H');
    mins  := ExtractNumber(s,'M');
    secs  := ExtractNumber(s,'S');
  end else begin
    // Only date part
    years  := ExtractNumber(s,'Y');
    months := ExtractNumber(s,'M');
    days   := ExtractNumber(s,'D');
  end;
  // Apply increments
  current := IncYear(current, years);
  current := IncMonth(current, months);
  current := IncDay(current, days);
  current := IncHour(current, hours);
  current := IncMinute(current, mins);
  current := IncSecond(current, secs);
  // check if past end
  if current > endUpWith then Exit;
  Result := True;
end;

{ TWMTS_ParameterValue }

function TWMTS_ParameterValue.GetOwner:TWMTS_ParameterValueList;
begin
  result:=Collection as TWMTS_ParameterValueList;
end;

{ TWMTS_ParameterValueList }

function TWMTS_ParameterValueList.Add(Value:String):TWMTS_ParameterValue;
begin
  result:=Inherited Add as TWMTS_ParameterValue;
  result.FValue:=Value;
  if Count=1 then Selected:=result;
end;

constructor TWMTS_ParameterValueList.Create(TheOwner:TWMTS_Parameter);
begin
  inherited Create(TWMTS_ParameterValue);
  FOwner:=TheOwner;
  FSelected:=nil;
end;


{ TWMTS_Parameter }

function TWMTS_Parameter.GetOwner:TWMTS_ParameterList;
begin
  result:=Collection as TWMTS_ParameterList;
end;

function TWMTS_Parameter.Add(Value:String):TWMTS_ParameterValue;
begin
  result:=FValueList.Add(Value);
end;

constructor TWMTS_Parameter.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FValueList:=TWMTS_ParameterValueList.Create(Self);
end;

destructor TWMTS_Parameter.Destroy;
begin
  FValueList.Free;
  inherited Destroy;
end;


{TWMTS_ParameterList}

function TWMTS_ParameterList.AddParameterIfNotExist(ParameterTitle:String):TWMTS_Parameter;
var tmpP:TCollectionItem;
begin
  for tmpP in Self do begin
    if TWMTS_Parameter(tmpP).FTitle = ParameterTitle then begin
      result:=TWMTS_Parameter(tmpP);
      exit;
    end;
  end;
  result:=Add(ParameterTitle);
end;

function TWMTS_ParameterList.Add(ParamterTitle:String):TWMTS_Parameter;
begin
  result:=Inherited Add as TWMTS_Parameter;
  result.FTitle:=ParamterTitle;
  if Count=1 then Selected:=result;
end;

constructor TWMTS_ParameterList.Create(TheOwner:TWMTS_Layer);
begin
  inherited Create(TWMTS_Parameter);
  FOwner:=TheOwner;
  FSelected:=nil;
end;

function TWMTS_ParameterList.GetURLParameter:string;
var tmpP:TCollectionItem;
    tmpV:TWMTS_ParameterValue;
begin
  result:='';
  for tmpP in Self do begin
    tmpV:=TWMTS_Parameter(tmpP).FValueList.Selected;
    if tmpV.Owner.Owner.Title='TileMatrixSet' then continue; //TMS暂时还是用老方法
    if tmpV=nil then continue;
    result:=result+Format('&%s=%s',[tmpV.Owner.Owner.Title,StringReplace(tmpV.Value,' ','%20',[rfReplaceAll])]);
  end;
end;

function TWMTS_ParameterList.GetPathNameParameter:string;
var tmpP:TCollectionItem;
    tmpV:TWMTS_ParameterValue;
    tmpLayer:TWMTS_Layer;
begin
  result:='';
  for tmpP in Self do begin
    tmpV:=TWMTS_Parameter(tmpP).FValueList.Selected;
    tmpLayer:=tmpV.Owner.Owner.Owner.Owner; //that's ridiculously ugly
    if tmpV.Owner.Owner.Title='TileMatrixSet' then continue; //TMS暂时还是用老方法
    if tmpV.Owner.Owner.Title='Time' then begin
      if tmpLayer.UsingISO8601 then begin
        result:=result+Format('&%s=%s',[tmpV.Owner.Owner.Title,DateTimeToISO8601(tmpLayer.TimeTagSelected)]);
        continue;
      end;
    end;
    if tmpV=nil then continue;
    result:=result+Format('&%s=%s',[tmpV.Owner.Owner.Title,tmpV.Value]);
  end;
  result:=StringReplace(result,':','_',[rfReplaceAll]);
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
function TWMTS_Layer.GetTimeTag(index:integer):TDateTime;
begin
  if (index<0) or (index>=Length(FTimeTags)) then result:=0
  else result:=FTimeTags[index];
end;

function TWMTS_Layer.GetTimeTagCount:integer;
begin
  result:=Length(FTimeTags);
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
  if pos('{time}',lowercase(result))>=0 then begin
    result:=StringReplace(result,'{Time}',DateTimeToISO8601(FTimeTagSelected),[rfIgnoreCase]);
  end else begin
    result:=result+ParameterList.GetURLParameter;
  end;
end;

constructor TWMTS_Layer.Create;
begin
  inherited Create;
  FParameterList:=TWMTS_ParameterList.Create(Self);
  SetLength(FTimeTags,0);
  FTimeTagSelected:=0.0;
end;

destructor TWMTS_Layer.Destroy;
begin
  SetLength(FTimeTags,0);
  FParameterList.Free;
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

function TWMTS_Service.GetTileMatrixSetByName(idenifier:string):TWMTS_TileMatrixSet;
var idx,len:integer;
begin
  len:=FTileMatrixSetList.Count;
  for idx:=0 to len-1 do begin
    result:=TWMTS_TileMatrixSet(FTileMatrixSetList.Items[idx]);
    if result.Identifier=idenifier then exit;
  end;
  result:=nil;
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
    //is_ISO8601:boolean;
    len_timetags:integer;
    dt1,dt2:TDateTime;
    TimeOption,ISO8601_Interval:string;
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
      FDisplayName:=FTitle;
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
            tmpLayer.UsingISO8601:=false;
            if tmp_node<>nil then begin
              dimension_id:=tmp_node.FindNode('ows:Identifier').FirstChild.NodeValue;
              if tmp_node.FindNode('ows:UOM')<>nil then begin
                //ISO8601
                tmpLayer.UsingISO8601:=true;
                {default_dt}tm2_node:=tmp_node.FindNode('ows:UOM');
                tmpLayer.FTimeTagSelected:=ISO8601ToDateTime({default_dt}tm2_node.FirstChild.NodeValue);
              end;
              {dims}mt_len:=tmp_node.ChildNodes.Count;
              for {dims}mt_idx:=0 to {dims}mt_len-1 do begin
                tm2_node:=tmp_node.ChildNodes[{dims}mt_idx];
                if tm2_node.NodeName='Value' then begin
                  TimeOption:=tm2_node.FirstChild.NodeValue;
                  tmpLayer.FParameterList.Parameter[dimension_id].Add(TimeOption);
                  len_timetags:=Length(tmpLayer.FTimeTags);
                  if ISORepeatingIntervalFindFirst(TimeOption,dt1,dt2,ISO8601_Interval) then begin
                    repeat
                      SetLength(tmpLayer.FTimeTags,len_timetags+1);
                      tmpLayer.FTimeTags[len_timetags]:=dt1;
                      inc(len_timetags);
                    until not ISORepeatingIntervalFindNext(dt1,dt2,ISO8601_Interval);
                  end;
                end;
              end;
            end;
            if FKvpUrl='' then
              tmpLayer.FURLTemplate:=content_node.FindNode('ResourceURL').Attributes.GetNamedItem('template').NodeValue
            else
              tmpLayer.FURLTemplate:=FKvpUrl;
            with ServiceConfig.url_replacement do
              if old_pattern<>'' then
                tmpLayer.FURLTemplate:=tmpLayer.FURLTemplate.Replace(old_pattern, new_pattern);
            //TileMatrixSetLink
            {tmsl}mt_len:=content_node.ChildNodes.Count;
            for {tmsl}mt_idx:=0 to {tmsl}mt_len-1 do begin
              tmp_node:=content_node.ChildNodes[mt_idx];
              if tmp_node.NodeName<>'TileMatrixSetLink' then continue;
              tm2_node:=tmp_node.FindNode('TileMatrixSet');
              if tm2_node=nil then continue;
              tmpLayer.ParameterList.Parameter['TileMatrixSet'].Add(tm2_node.FirstChild.NodeValue);
            end;
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

  //需要解决Time维度
  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='0e2c50def624b69b1dcb67f43f353c49';
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/1.0.0/WMTSCapabilities.xml', ServiceConfig_Default);
  FServiceList.Add(tmpService);

  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='0e2c50def624b69b1dcb67f43f353c49';
  tmpServiceConfig.fixed_meter_per_pixel:=0.0002803138; //没招了就这样吧
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('http://s0.fjmap.net:80/img_fj_2019/wmts', tmpServiceConfig);
  FServiceList.Add(tmpService);
  tmpService.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
  tmpService.DisplayName:='天地图福建 2019';

  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='ef1e65139e1e3571ff1338d9a72e8142';
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('https://t0.tianditu.gov.cn/img_w/wmts?request=GetCapabilities&service=wmts', tmpServiceConfig);
  FServiceList.Add(tmpService);
  tmpService.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
  tmpService.DisplayName:='天地图全国';

  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='';
  tmpServiceConfig.fixed_meter_per_pixel:=0;
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('https://ows.terrestris.de/osm/service?service=WMTS&request=GetCapabilities', tmpServiceConfig);
  FServiceList.Add(tmpService);
  tmpService.DisplayName:='Open Street Map (terrestris.de)';


  //需要解决Time维度
  tmpServiceConfig.url_replacement.old_pattern:='';
  tmpServiceConfig.url_replacement.new_pattern:='';
  tmpServiceConfig.token:='0e2c50def624b69b1dcb67f43f353c49';
  tmpServiceConfig.fixed_meter_per_pixel:=0.0002803138; //没招了就这样吧
  tmpService:=TWMTS_Service.Create;
  tmpService.LoadFromManifestXml('http://s0.fjmap.net/img_fj_2025_his/wmts', tmpServiceConfig);
  FServiceList.Add(tmpService);
  tmpService.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
  tmpService.DisplayName:='天地图福建 2025';


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

